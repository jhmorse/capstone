# ---- OVERVIEW ----
## This script builds and cleans the dataset for analysis.
 
# ---- Libraries ----
library(tidyverse)

# ---- Load Data ----
schools <- read_rds("rds/school_directory.rds")
districts <- read_rds("rds/district_directory.rds")
fy_enrollment <- read_rds("rds/fy_enrollment.rds")
assessment <- read_rds("rds/assessment.rds")

# ---- Helper functions ----

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
district_lookup <- schools %>%
  select(LEACode, LEAName, ESDCode, ESDName) %>%
  distinct() %>%
  rename(district_code = LEACode,
         district_name = LEAName,
         esd_code = ESDCode,
         esd_name = ESDName)



# ---- College Enrollment Data ----

## Use df_enroll_year for rates of enrollment by year.

## Get Enrollment Percentage by enrollment year,
## and by School District. Include All Students
## and Gender breakdown. Track enrollment for 
## 4-Year college only.

# Filter for School districts and relevant columns
df_enroll_year <- fy_enrollment %>%
  filter(SchoolTTL == "District Wide",  # Districts only
         LEACode != "N/A",              # Valid districts only
         CohortYearTTL >= 2015,         # No school data is before 2015
         !is.na(Pct),                   # Drop anything with no results
         PSEnrollLevel == "4 Year",     # 4 -Year College Enrollment
         DemographicGroup %in% c("All Students", "Gender")) %>%  
  select(CohortYearTTL, LEACode, DistrictTTL,
         DemographicValue, PSEnrollLevel, Pct)

# Join college enrollment data to district lookup.
# The inner_join ensures ONLY valid and traceable
# school districts are identified.
df_enroll_year <- df_enroll_year %>%
  inner_join(district_lookup,
            by = c("LEACode" = "district_code")) %>%
  select(CohortYearTTL, LEACode, district_name, 
         #PSEnrollLevel, 
         DemographicValue, Pct) %>%
  rename(year = CohortYearTTL,
         district_code = LEACode,
         #enrollment_level = PSEnrollLevel,
         demographic = DemographicValue,
         percent_enrolled = Pct)

# Change the values in demographic to columns
# with Pct as the values.

df_enroll_year <- df_enroll_year %>%
  pivot_wider(names_from = demographic, 
              values_from = percent_enrolled,
              names_glue = "{demographic}"
  ) %>%
  rename(enrollment_all = All,
         enrollment_female = Female,
         enrollment_male = Male)




# ---- SBAC Assessment Data - Base Data ----

## Use df_assess_allgrades to get, by year, percent meeting standards
##   across all grades
## Use df_assess_final to get, by year, the most recent SBAC scores
##   (usually in 10th or 11th grade).
## Use df_assess_last2 to get, by year, an aggregate of the last two
##   SBAC scores for any school district (usually 10th and 11th).
## Use df_assess_wtd for a weighted aggregate score by district
##  for each year. Higher grade levels are weighted more.


# Get the District Level data by year
df_assess_year <- assessment %>%
  filter(
         student_group %in% c("All Students", "Female", "Low-Income"),
         organization_level == "District",
         test_administration == "SBAC"
         ) %>%
  # Second half of school year as number to match enrollment data
  mutate(year = as.numeric(substr(school_year, 1, 4)) + 1) %>%
  # Reduce to columns of interest
  select(school_year, year, county, district_code, district_name,
         grade_level, student_group, test_subject, 
         count_students_expected_to_test_incl_passed,
         #count_met_standard, 
         #percent_level3, percent_level4,
         percent_met_standard
         ) %>%
  rename(cnt_tested = count_students_expected_to_test_incl_passed,
         #cnt_met = count_met_standard,
         #pct_lvl3 = percent_level3,
         #pct_lvl4 = percent_level4,
         pct_met = percent_met_standard)


# ---- SBAC Assessment Data - All Grades ----

# Get All Grades totals by district, and 
# spread ELA and Math into one row.
df_assess_allgrades <- df_assess_year %>%
  filter(grade_level == "All Grades") %>%
  select(-grade_level) %>%
  pivot_wider(names_from = test_subject, 
              values_from = c(cnt_tested, pct_met),
              names_glue = "{test_subject}_{.value}"
              )

# Replace NA with 0 for all variables
df_assess_allgrades[is.na(df_assess_allgrades)] <- 0

# Replace values in student_group with different values
df_assess_allgrades <- df_assess_allgrades %>%
  mutate(student_group = case_when(
    student_group == "All Students" ~ "All",
    student_group == "Low-Income" ~ "LI",
    #student_group == "Non-Low Income" ~ "NonLI",
    #student_group == "Male" ~ "Male",
    student_group == "Female" ~ "Female")
  )

# Pivot based on the demographics in student_group
df_assess_allgrades <- df_assess_allgrades %>%
  pivot_wider(
    names_from = student_group,
    values_from = c(ELA_cnt_tested, Math_cnt_tested, ELA_pct_met, Math_pct_met),
    names_glue = "{student_group}_{.value}"
  )

# Create ratios of Female and LI taking test,
# then drop unneeded columns.
df_assess_allgrades <- df_assess_allgrades %>%
  rename(ela_tested = All_ELA_cnt_tested,
         math_tested = All_Math_cnt_tested) %>%
  mutate(ela_li_ratio = safe_div(LI_ELA_cnt_tested, ela_tested),
         ela_female_ratio = safe_div(Female_ELA_cnt_tested, ela_tested),
         math_li_ratio = safe_div(LI_Math_cnt_tested, math_tested),
         math_female_ratio = safe_div(Female_Math_cnt_tested, math_tested)
         ) %>%
  select(-LI_ELA_cnt_tested, -Female_ELA_cnt_tested,
         -LI_Math_cnt_tested, -Female_Math_cnt_tested) %>%
  rename(ela_met = All_ELA_pct_met,
         ela_li_met = LI_ELA_pct_met,
         ela_female_met = Female_ELA_pct_met,
         math_met = All_Math_pct_met,
         math_li_met = LI_Math_pct_met,
         math_female_met = Female_Math_pct_met)
  


# ---- SBAC Assessment Data - Last Grade ----

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


# ---- SBAC Assessment Data - Last 2 Grades ----

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
  

# ---- SBAC Assessment Data - Weighted Aggregate ----

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

## NOT USED
## Assign current state of data set to df_assess_dist_wtd
## to allow for aggregating at district level across years.
#df_assess_dist_wtd <- df_assess_wtd

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




# *******************************************
## Temp working space
dfTemp <- assessment %>%
  filter(#student_group %in% c("All Students", "Female"),
         student_group_type %in% c("All", "Gender"),
         organization_level == "District",
         test_administration == "SBAC")
View(dfTemp)




