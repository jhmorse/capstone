# ---- OVERVIEW ----
## This script builds and cleans the dataset for analysis.
 
# ---- Libraries ----
library(tidyverse)

# ---- Load Data ----
directory <- read_rds("rds/school_directory.rds")
fy_enrollment <- read_rds("rds/fy_enrollment.rds")
assessment <- read_rds("rds/assessment.rds")
graduation <- read_rds("rds/graduation.rds")

# ---- Helper functions

# Safe division
safe_div <- function(num, denom) {
  ifelse(is.na(denom) | is.na(num) | denom == 0, 0, num / denom)
}

# Convert a proportion to a logit
logit_prop <- function(p, eps = 1e-6) {
  # p <- p / 100    # Only necessary if p is a percent
  p <- pmin(pmax(p, eps), 1 - eps)   # avoid 0 and 1
  log(p / (1 - p))
}

# Inverse logit to get back to ratio or percentage
inv_logit_pct <- function(x) {
  (exp(x) / (1 + exp(x)))   # multiply by 100 to get percentage
}

# Weighted mean of logits for pooling across years
wmean_logit <- function(logit_vals, weights) {
  sum(logit_vals * weights, na.rm = TRUE) / 
    sum(weights, na.rm = TRUE)
}

# Above three functions combined in one
pool_logit_by_group <- function(data, group_var, pct_var, weight_var) {
  data %>%
    mutate(
      logit_val = logit_prop({{ pct_var }})
    ) %>%
    group_by({{ group_var }}) %>%
    summarize(
      pooled_logit = wmean_logit(logit_val, {{ weight_var }}),
      pooled_pct = inv_logit_pct(pooled_logit),
      total_weight = sum({{ weight_var }}, na.rm = TRUE),
      .groups = "drop"
    )
}


# ---- District Lookup ----
district_lookup <- directory %>%
  select(LEACode, LEAName, ESDCode, ESDName) %>%
  distinct() %>%
  rename(district_code = LEACode,
         district_name = LEAName,
         esd_code = ESDCode,
         esd_name = ESDName)

# ---- High School Graduation ----

# To correctly calculate weighted logit means in the
# college enrollment, we need to estimate the denominator,
# which is the total number of students that could be
# enrolling in college. For this analysis, I am using the
# total number of students graduating in the school year
# ending in the same year as enrollment.

# Get graduation rates by district
df_grad_year <- graduation %>%
  filter(
    organization_level == "District",
    student_group == "All Students",
    suppression == "No DAT",
    cohort == "Four Year"   # closest representation of actual graduation rate
  ) %>%
  mutate(
    # Use school_year to calculate actual graduation year
    grad_year = as.numeric(substr(school_year, 1, 4)) + 1
  ) %>%
  select(school_year, grad_year, district_code,
         beginning_grade9, final_cohort, graduate) %>%
  mutate(
    # Convert numeric columns to actual numbers
    beginning_grade9 = as.numeric(beginning_grade9),
    final_cohort = as.numeric(final_cohort),
    graduate = as.numeric(graduate)
  )


# ---- College Enrollment Data ----

## Use df_enroll_year for rates of enrollment by year.
## Use df_enroll_district for rates of enrollment by district,
## averaged across years.

# Filter for School districts and relevant columns
df_enroll_year <- fy_enrollment %>%
  filter(SchoolTTL == "District Wide",  # Districts only
         LEACode != "N/A",              # Valid districts only
         CohortYearTTL >= 2015,         # No school data is before 2015
         !is.na(Pct),                   # Drop anything with no results
         DemographicGroup == "All Students") %>%  
  select(CohortYearTTL, LEACode, DistrictTTL,
         PSEnrollLevel, Pct)

# Join college enrollment data to district lookup
df_enroll_year <- df_enroll_year %>%
  left_join(district_lookup,
            by = c("LEACode" = "district_code")) %>%
  select(CohortYearTTL, LEACode, district_name, 
         PSEnrollLevel, Pct) %>%
  rename(year = CohortYearTTL,
         district_code = LEACode,
         enrollment_level = PSEnrollLevel,
         percent_enrolled = Pct)

# Join graduation rates by District and by year.
# At this point, graduate is our denominator!
df_enroll_year <- df_enroll_year %>%
  inner_join(df_grad_year, 
             by = c("district_code" = "district_code",
                    "year" = "grad_year"))

# We can now use the denominator to summarize
# enrollment rates at the District level.

## HOWEVER:
## I am choosing NOT to use the weighted logit method. 
## Weighted Logits will inadvertently weight the size
## of the district. My intent here is to combine within
## a district, and we expect the number of graduates
## within a district to remain stable year over year.

# Group by District, with combined values by year.
# Use a Logit average to combine percentages.
df_enroll_district <- df_enroll_year %>%
  mutate(
    logit_val = qlogis(percent_enrolled)
    #logit_val = logit_prop(percent_enrolled)
  ) %>%
  group_by(district_code, district_name, enrollment_level) %>%
  summarise(
    # Sum the total graduates for the District
    graduate = sum(graduate, na.rm = TRUE),
    
    # Calculate the Logit Average
    pooled_logit = mean(logit_val, na.rm = TRUE),
    #pooled_logit = wmean_logit(logit_val, graduate),
    # Convert back to a ratio/percentage
    pooled_pct_enrolled = plogis(pooled_logit),
    #pooled_pct_enrolled = inv_logit_pct(pooled_logit),
    
    .groups = "drop"
  )





# *********************************************************  
# ---- SBAC Assessment Data ----

## Use df_assess_allgrades to get, by year, percent meeting standards
##   across all grades
## Use df_assess_dist_allgrades to get an aggregate of all years, 
##   percent meeting standards across all grades
## Use df_assess_final to get, by year, the most recent SBAC scores
##   (usually in 10th or 11th grade).
## Use df_assess_dist_final to get an aggregate of all years for
##   a district, showing percent met standards in final testing.
## Use df_assess_last2 to get, by year, an aggregate of the last two
##   SBAC scores for any school district (usually 10th and 11th).
## Use df_assess_dist_last2 to get an aggregate of all years for
##   each District of the last two SBAC scores.
## Use df_assess_wtd for a weighted aggregate score by district
##  for each year. Higher grade levels are weighted more.
## Use df_assess_dist_wtd for a weighted score at the district
##   level with all years aggregated together.


# Get the District Level data by year
df_assess_year <- assessment %>%
  filter(student_group == "All Students",
         organization_level == "District",
         test_administration == "SBAC") %>%
  select(school_year, district_code, district_name,
         grade_level, test_subject, 
         count_students_expected_to_test_incl_passed,
         count_met_standard, percent_met_standard,
         percent_level3, percent_level4
         ) %>%
  rename(count_tested = count_students_expected_to_test_incl_passed)


# *********************************************************  
# ---  ALL GRADES

# Get All Grades totals by district, and 
# spread ELA and Math into one row.
df_assess_allgrades <- df_assess_year %>%
  filter(grade_level == "All Grades") %>%
  rename(tested = count_tested,
         met = count_met_standard,
         pct_met = percent_met_standard,
         pct_level3 = percent_level3,
         pct_level4 = percent_level4) %>%
  select(-grade_level) %>%
  pivot_wider(names_from = test_subject, 
              values_from = c(tested, met, pct_met, pct_level3, pct_level4),
              names_glue = "{test_subject}_{.value}"
              )

# Replace NA with 0 for all variables
df_assess_allgrades[is.na(df_assess_allgrades)] <- 0

# Get an aggregated value by district across all years.
# In this case we have the raw numbers and can aggregate
# to calculate a percentage.
df_assess_dist_allgrades <- df_assess_allgrades %>%
  select(-ELA_pct_met, -Math_pct_met,
         -ELA_pct_level3, -ELA_pct_level4,
         -Math_pct_level3, -Math_pct_level4) %>%
  group_by(district_code, district_name) %>%
  summarise(ELA_tested = sum(ELA_tested, na.rm = TRUE),
            Math_tested = sum(Math_tested, na.rm = TRUE),
            ELA_met = sum(ELA_met, na.rm = TRUE),
            Math_met = sum(Math_met, na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(ELA_pct_met = safe_div(ELA_met, ELA_tested),
         Math_pct_met = safe_div(Math_met, Math_tested))

  
# *********************************************************  
# ---  FINAL SBAC GRADE (usually 11th or 10th grade)
  
# Get the data by district and by grade, 
# removing the All Grades aggregate.
df_assess_final <- df_assess_year %>%
  filter(grade_level != "All Grades") %>%  # JUST individual grades
  rename(tested = count_tested,
       met = count_met_standard,
       pct_met = percent_met_standard,
       pct_level3 = percent_level3,
       pct_level4 = percent_level4) %>%
  # Change the results by subject to columns
  pivot_wider(names_from = test_subject, 
            values_from = c(tested, met, pct_met, pct_level3, pct_level4),
            names_glue = "{test_subject}_{.value}"
            )

# Turn grade_level into an ordered factor
grade_levels <- sort(unique(df_assess_final$grade_level))
df_assess_final <- df_assess_final %>%
  mutate(grade_level = factor(grade_level, levels = grade_levels))

# Get the highest grade_level for each school district and year
df_assess_final <- df_assess_final %>%
  group_by(school_year, district_code, district_name) %>%
  slice_max(grade_level, n = 1, with_ties = FALSE) %>%
  ungroup()

# Replace NA with 0 for all variables
df_assess_final[is.na(df_assess_final)] <- 0

# Get aggregated value by District (includes all years).
df_assess_dist_final <- df_assess_final %>%
  select(school_year, district_code, district_name,
         ELA_tested, Math_tested, 
         ELA_met, Math_met) %>%
  group_by(district_code, district_name) %>%
  summarise(ELA_tested = sum(ELA_tested, na.rm = TRUE),
            Math_tested = sum(Math_tested, na.rm = TRUE),
            ELA_met = sum(ELA_met, na.rm = TRUE),
            Math_met = sum(Math_met, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(ELA_pct_met = safe_div(ELA_met, ELA_tested),
         Math_pct_met = safe_div(Math_met, Math_tested))

# *********************************************************  
# ---  LAST TWO SBAC GRADES (usually 11th & 10th grade)

# This looks much like the last, except we are getting
# the last 2 SBAC grades for the district in each year
# and adding them together.

# Get the data by district and by grade, 
# removing the All Grades aggregate.
df_assess_last2 <- df_assess_year %>%
  filter(grade_level != "All Grades") %>%  # JUST individual grades
  rename(tested = count_tested,
         met = count_met_standard,
         pct_met = percent_met_standard,
         pct_level3 = percent_level3,
         pct_level4 = percent_level4) %>%
  # Change the results by subject to columns
  pivot_wider(names_from = test_subject, 
              values_from = c(tested, met, pct_met, pct_level3, pct_level4),
              names_glue = "{test_subject}_{.value}"
  )

# Turn grade_level into an ordered factor
grade_levels <- sort(unique(df_assess_last2$grade_level))
df_assess_last2 <- df_assess_last2 %>%
  mutate(grade_level = factor(grade_level, levels = grade_levels))

# Get the highest grade_level for each school district and year
df_assess_last2 <- df_assess_last2 %>%
  group_by(school_year, district_code, district_name) %>%
  slice_max(grade_level, n = 2, with_ties = FALSE) %>%
  ungroup()

# Replace NA with 0 for all variables
df_assess_last2[is.na(df_assess_last2)] <- 0

# Aggregate those last two grade_levels
df_assess_last2 <- df_assess_last2 %>%
  group_by(school_year, district_code, district_name) %>%
  summarise(ELA_tested = sum(ELA_tested, na.rm = TRUE),
            Math_tested = sum(Math_tested, na.rm = TRUE),
            ELA_met = sum(ELA_met, na.rm = TRUE),
            Math_met = sum(Math_met, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(ELA_pct_met = safe_div(ELA_met, ELA_tested),
         Math_pct_met = safe_div(Math_met, Math_tested))
  
# Get aggregated value by District (includes all years).
df_assess_dist_last2 <- df_assess_last2 %>%
  select(school_year, district_code, district_name,
         ELA_tested, Math_tested, 
         ELA_met, Math_met) %>%
  group_by(district_code, district_name) %>%
  summarise(ELA_tested = sum(ELA_tested, na.rm = TRUE),
            Math_tested = sum(Math_tested, na.rm = TRUE),
            ELA_met = sum(ELA_met, na.rm = TRUE),
            Math_met = sum(Math_met, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(ELA_pct_met = safe_div(ELA_met, ELA_tested),
       Math_pct_met = safe_div(Math_met, Math_tested))
  

# *********************************************************  
# ---  WEIGHTED SBAC AGGREGATE GRADES

## This uses a Logit transformation of weighted scores. 
## I am weighting the grades so that later grades have 
## more influence on the overall SBAC score, indicating 
## more influence on college enrollment.

## Based on the weightings, this also balances districts
## that don't have SBAC scores in certain years. The grades
## they do have are weighted the same as that grade
## in other districts. Compare a district with 11th as the
## last grade vs. a district with 8th as the last grade.

## Using the weighted method, we can also retain % Level 3 & 4.

# Get the data by district and by grade, 
# removing the All Grades aggregate.
df_assess_wtd <- df_assess_year %>%
  filter(grade_level != "All Grades") %>%  # JUST individual grades
  rename(tested = count_tested,
         met = count_met_standard,
         pct_met = percent_met_standard,
         pct_level3 = percent_level3,
         pct_level4 = percent_level4) %>%
  # Change the results by subject to columns
  pivot_wider(names_from = test_subject, 
              values_from = c(tested, met, pct_met, pct_level3, pct_level4),
              names_glue = "{test_subject}_{.value}"
  )

# Replace NA with 0 for all variables
df_assess_wtd[is.na(df_assess_wtd)] <- 0

# 1. Define grade weights
grade_weights <- c(
  "03" = 1,
  "04" = 2,
  "05" = 3,
  "06" = 4,
  "07" = 5,
  "08" = 6,
  "10" = 7,
  "11" = 8
)

# ---- Logit Weights ----
# Calculate Logit values for each of the percentages
# along with weighted values for # tested.
df_assess_wtd <- df_assess_wtd %>%
  mutate(
    # Compute Logit values for ELA scores
    ELA_logit = logit_prop(ELA_pct_met),
    ELA_level3_logit = logit_prop(ELA_pct_level3),
    ELA_level4_logit = logit_prop(ELA_pct_level4),

    # Compute Logit values for Math scores
    Math_logit = logit_prop(Math_pct_met),
    Math_level3_logit = logit_prop(Math_pct_level3),
    Math_level4_logit = logit_prop(Math_pct_level4),

    # Normally, we would just use the # tested as the weight,
    # but here I want 11th grade to have more weight than
    # 3rd grade, so we are multiplying by grade_weights.
    grade_weight = grade_weights[grade_level],
    ELA_weight = grade_weight * ELA_tested,     # weight the size of the group tested
    Math_weight = grade_weight * Math_tested
  )

## IMPORTANT
## Assign current state of data set to df_assess_dist_wtd
## to allow for aggregating at district level across years.
df_assess_dist_wtd <- df_assess_wtd

# Group by year and school district, then
# Calculate the weighted logit scores for the grouped data.
# Convert the weighted logits back to ratios.
df_assess_wtd <- df_assess_wtd %>%
  group_by(school_year, district_code, district_name) %>%
  summarize(
    # Include total number of students tested
    ELA_tested = sum(ELA_tested),
    Math_tested = sum(Math_tested),
    
    # Generate weighted means for each Logit value.
    # This is what should be used for regression.
    ELA_weighted_logit = wmean_logit(ELA_logit, ELA_weight),
    ELA_level3_logit = wmean_logit(ELA_level3_logit, ELA_weight),
    ELA_level4_logit = wmean_logit(ELA_level4_logit, ELA_weight),
    
    Math_weighted_logit = wmean_logit(Math_logit, Math_weight),
    Math_level3_logit = wmean_logit(Math_level3_logit, Math_weight),
    Math_level4_logit = wmean_logit(Math_level4_logit, Math_weight),
    
    # Convert Logits back to proportions for charts and tables
    ELA_score = inv_logit_pct(ELA_weighted_logit),
    ELA_level3_score = inv_logit_pct(ELA_level3_logit),
    ELA_level4_score = inv_logit_pct(ELA_level4_logit),
    
    Math_score = inv_logit_pct(Math_weighted_logit),
    Math_level3_score = inv_logit_pct(Math_level3_logit),
    Math_level4_score = inv_logit_pct(Math_level4_logit),
    
    .groups = "drop"   # ungroup
  ) #%>%
  #select(school_year, district_code, district_name,
  #       ELA_score, ELA_level3_score, ELA_level4_score,
  #       Math_score, Math_level3_score, Math_level4_score)

# Convert anything that is not a number (NaN) to 0
df_assess_wtd <- df_assess_wtd %>%
  mutate(across(where(is.numeric), ~ ifelse(is.nan(.), 0, .)))



# Repeat the grouping process and the scores, but group at
# the district level, aggregating all years into a single
# score for each district.
df_assess_dist_wtd <- df_assess_dist_wtd %>%
  group_by(district_code, district_name) %>%
  summarize(
    # Include total number of students tested
    ELA_tested = sum(ELA_tested),
    Math_tested = sum(Math_tested),
    
    # Generate weighted means for each Logit value.
    # This is what should be used for regression.
    ELA_weighted_logit = wmean_logit(ELA_logit, ELA_weight),
    ELA_level3_logit = wmean_logit(ELA_level3_logit, ELA_weight),
    ELA_level4_logit = wmean_logit(ELA_level4_logit, ELA_weight),
    
    Math_weighted_logit = wmean_logit(Math_logit, Math_weight),
    Math_level3_logit = wmean_logit(Math_level3_logit, Math_weight),
    Math_level4_logit = wmean_logit(Math_level4_logit, Math_weight),

    # Convert Logits back to proportions for charts and tables
    ELA_score = inv_logit_pct(ELA_weighted_logit),
    ELA_level3_score = inv_logit_pct(ELA_level3_logit),
    ELA_level4_score = inv_logit_pct(ELA_level4_logit),
    
    Math_score = inv_logit_pct(Math_weighted_logit),
    Math_level3_score = inv_logit_pct(Math_level3_logit),
    Math_level4_score = inv_logit_pct(Math_level4_logit),
    
    .groups = "drop"
  ) #%>%
  #select(district_code, district_name,
  #       ELA_score, ELA_level3_score, ELA_level4_score,
  #       Math_score, Math_level3_score, Math_level4_score)

# Convert anything that is not a number (NaN) to 0
df_assess_dist_wtd <- df_assess_dist_wtd %>%
  mutate(across(where(is.numeric), ~ ifelse(is.nan(.), 0, .)))



# ---- Combined Enrollment and Assessment ----

# Start with simple view of District aggregates
# and Assessment scores for all grades.

#df_enroll_district
#df_assess_dist_allgrades
  
df <- df_assess_dist_allgrades %>%
  inner_join(df_enroll_district, 
            by = c("district_code" = "district_code")) %>%
  select(-district_name.y, -pooled_logit) %>%
  rename(district_name = district_name.x,
         percent_enrolled = pooled_pct_enrolled)


# Scatter plot of ELA_pct_met against percent_enrolled
# with color for Enrollment level.
ggplot(df, aes(x = ELA_pct_met, 
               y = percent_enrolled, 
               color = enrollment_level)) +
  geom_point()


ggplot(df, aes(x = Math_pct_met, 
               y = percent_enrolled, 
               color = enrollment_level)) +
  geom_point()


## Is there a relationship between enrollment and the 
## weighted assessment scores?

df2 <- df_assess_dist_wtd %>%
  inner_join(df_enroll_district, 
             by = c("district_code" = "district_code")) %>%
  select(-district_name.y, -pooled_logit) %>%
  rename(district_name = district_name.x,
         percent_enrolled = pooled_pct_enrolled)

# Scatter plot of ELA_score (the weighted test score across grades)
# against percent_enrolled with color for Enrollment level.
ggplot(df2, aes(x = ELA_score, 
               y = percent_enrolled, 
               color = enrollment_level)) +
  geom_point()



