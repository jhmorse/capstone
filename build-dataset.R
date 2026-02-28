# ---- OVERVIEW ----
## This script builds and cleans the datasets for analysis.
 
## IMPORTANT
## Decisions I am making:
## 1. Filter to only SBAC test administration.
## 2. Filter to only "All Students" student group.
## 3. ???? Remove rows where count_met_standard is "NULL".

## DISTRICT LEVEL DATA
## Creating District Level Scores
## that are a consolidation of grade
## level scores for the SBAC ELA results.


# ---- Libraries ----
library(tidyverse)

# ---- Load Data ----
schools <- read_rds("rds/school_directory.rds")
districts <- read_rds("rds/district_directory.rds")
fy_enrollment <- read_rds("rds/fy_enrollment.rds")
assessment <- read_rds("rds/assessment.rds")
census <- read_rds("rds/census.rds")

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
         count_met_standard, 
         #percent_level3, 
         percent_level4,
         percent_met_standard
         ) %>%
  rename(cnt_tested = count_students_expected_to_test_incl_passed,
         cnt_met = count_met_standard,
         #pct_lvl3 = percent_level3,
         pct_lvl4 = percent_level4,
         pct_met = percent_met_standard) %>%
  # Get the count meeting level 4 standard
  mutate(cnt_lvl4 = (pct_lvl4 * cnt_tested))



# ---- SBAC Assessment Data - All Grades ----

# Get All Grades totals by district, and 
# spread ELA and Math into one row.
df_assess_allgrades <- df_assess_year %>%
  filter(grade_level == "All Grades") %>%
  select(-grade_level, -cnt_met, -cnt_lvl4) %>%
  pivot_wider(names_from = test_subject, 
              values_from = c(cnt_tested, pct_met, pct_lvl4),
              names_glue = "{test_subject}_{.value}"
              )

# Replace NA with 0 for all variables
#df_assess_allgrades[is.na(df_assess_allgrades)] <- 0

# Replace values in student_group with different values
df_assess_allgrades <- df_assess_allgrades %>%
  mutate(student_group = case_when(
    student_group == "All Students" ~ "All",
    student_group == "Low-Income" ~ "LI",
    student_group == "Female" ~ "Female")
  )

# Pivot based on the demographics in student_group
df_assess_allgrades <- df_assess_allgrades %>%
  pivot_wider(
    names_from = student_group,
    values_from = c(ELA_cnt_tested, Math_cnt_tested, 
                    ELA_pct_met, Math_pct_met,
                    ELA_pct_lvl4, Math_pct_lvl4),
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
         -LI_Math_cnt_tested, -Female_Math_cnt_tested,
         # Remove pct_met for LI and Female
         -LI_ELA_pct_met, -Female_ELA_pct_met,
         -LI_Math_pct_met, -Female_Math_pct_met,
         -LI_ELA_pct_lvl4, -Female_ELA_pct_lvl4,
         -LI_Math_pct_lvl4, -Female_Math_pct_lvl4
         ) %>%
  rename(ela_met = All_ELA_pct_met,
         #ela_li_met = LI_ELA_pct_met,
         #ela_female_met = Female_ELA_pct_met,
         #math_li_met = LI_Math_pct_met,
         #math_female_met = Female_Math_pct_met,
         math_met = All_Math_pct_met,
         ela_lvl4 = All_ELA_pct_lvl4,
         math_lvl4 = All_Math_pct_lvl4)
  


# ---- SBAC Assessment Data - Last Grade ----

# Get the data by district and by grade, 
# removing the All Grades aggregate.
df_assess_final <- df_assess_year %>%
  select(-cnt_met, -cnt_lvl4) %>%   # Won't use for this data set
  filter(grade_level != "All Grades") %>%  # JUST individual grades
  # Change the results by subject to columns
  pivot_wider(names_from = test_subject, 
            #values_from = c(tested, met, pct_met, pct_level3, pct_level4),
            values_from = c(cnt_tested, pct_met, pct_lvl4),
            names_glue = "{test_subject}_{.value}"
            )

# Turn grade_level into an ordered factor
grade_levels <- sort(unique(df_assess_final$grade_level))
df_assess_final <- df_assess_final %>%
  mutate(grade_level = factor(grade_level, levels = grade_levels))

# Get the highest grade_level for each school district and year
df_assess_final <- df_assess_final %>%
  group_by(school_year, district_code, district_name, student_group) %>%
  slice_max(grade_level, n = 1, with_ties = FALSE) %>%
  ungroup()

# Replace NA with 0 for all variables
#df_assess_final[is.na(df_assess_final)] <- 0

# Replace values in student_group with different values
df_assess_final <- df_assess_final %>%
  mutate(student_group = case_when(
    student_group == "All Students" ~ "All",
    student_group == "Low-Income" ~ "LI",
    student_group == "Female" ~ "Female")
  )

# Remove grade_level
df_assess_final <- df_assess_final %>%
  select(-grade_level)

# Pivot based on the demographics in student_group
df_assess_final <- df_assess_final %>%
  pivot_wider(
    names_from = student_group,
    values_from = c(ELA_cnt_tested, Math_cnt_tested, 
                    ELA_pct_met, Math_pct_met,
                    ELA_pct_lvl4, Math_pct_lvl4),
    names_glue = "{student_group}_{.value}"
  )

# Create ratios of Female and LI taking test,
# then drop unneeded columns.
df_assess_final <- df_assess_final %>%
  rename(ela_tested = All_ELA_cnt_tested,
         math_tested = All_Math_cnt_tested) %>%
  mutate(ela_li_ratio = safe_div(LI_ELA_cnt_tested, ela_tested),
         ela_female_ratio = safe_div(Female_ELA_cnt_tested, ela_tested),
         math_li_ratio = safe_div(LI_Math_cnt_tested, math_tested),
         math_female_ratio = safe_div(Female_Math_cnt_tested, math_tested)
  ) %>%
  select(-LI_ELA_cnt_tested, -Female_ELA_cnt_tested,
         -LI_Math_cnt_tested, -Female_Math_cnt_tested,
         # Remove pct_met for LI and Female
         -LI_ELA_pct_met, -Female_ELA_pct_met,
         -LI_Math_pct_met, -Female_Math_pct_met,
         -LI_ELA_pct_lvl4, -Female_ELA_pct_lvl4,
         -LI_Math_pct_lvl4, -Female_Math_pct_lvl4
  ) %>%
  rename(ela_met = All_ELA_pct_met,
         #ela_li_met = LI_ELA_pct_met,
         #ela_female_met = Female_ELA_pct_met,
         math_met = All_Math_pct_met,
         #math_li_met = LI_Math_pct_met,
         #math_female_met = Female_Math_pct_met
         ela_lvl4 = All_ELA_pct_lvl4,
         math_lvl4 = All_Math_pct_lvl4
         )


# ---- SBAC Assessment Data - Last 2 Grades ----

# This looks much like the last, except we are getting
# the last 2 SBAC grades for the district in each year
# and adding them together.

# Get the data by district and by grade, 
# removing the All Grades aggregate.
df_assess_last2 <- df_assess_year %>%
  filter(grade_level != "All Grades") %>%  # JUST individual grades
  # Change the results by subject to columns
  pivot_wider(names_from = test_subject, 
              values_from = c(cnt_tested, cnt_met, pct_met,
                              cnt_lvl4, pct_lvl4),
              names_glue = "{test_subject}_{.value}"
  )

# Turn grade_level into an ordered factor
grade_levels <- sort(unique(df_assess_last2$grade_level))
df_assess_last2 <- df_assess_last2 %>%
  mutate(grade_level = factor(grade_level, levels = grade_levels))

# Get the highest 2 grade_level values for each school district and year
df_assess_last2 <- df_assess_last2 %>%
  group_by(school_year, year, county,
           district_code, district_name, 
           student_group) %>%
  slice_max(grade_level, n = 2, with_ties = FALSE) %>%
  ungroup()

# Replace NA with 0 for all variables
#df_assess_last2[is.na(df_assess_last2)] <- 0

# Aggregate those last two grade_levels
df_assess_last2 <- df_assess_last2 %>%
  # Drop percentages, we will calcualte after aggregating
  select(-ELA_pct_met, -Math_pct_met,
         -ELA_pct_lvl4, -Math_pct_lvl4) %>%
  # Note when we group, we eliminate the grade_level
  group_by(school_year, year, county,
           district_code, district_name, 
           student_group) %>%
  # aggregate the count tested and count meeting expectations
  summarise(ELA_cnt_tested = sum(ELA_cnt_tested, na.rm = TRUE),
            Math_cnt_tested = sum(Math_cnt_tested, na.rm = TRUE),
            ELA_cnt_met = sum(ELA_cnt_met, na.rm = TRUE),
            Math_cnt_met = sum(Math_cnt_met, na.rm = TRUE),
            ELA_cnt_lvl4 = sum(ELA_cnt_lvl4, na.rm = TRUE),
            Math_cnt_lvl4 = sum(Math_cnt_lvl4, na.rm = TRUE)) %>%
  ungroup()

# Calculate the percent met based on aggregated values  
df_assess_last2 <- df_assess_last2 %>%
  mutate(ELA_pct_met = safe_div(ELA_cnt_met, ELA_cnt_tested),
         Math_pct_met = safe_div(Math_cnt_met, Math_cnt_tested),
         ELA_pct_lvl4 = safe_div(ELA_cnt_lvl4, ELA_cnt_tested),
         Math_pct_lvl4 = safe_div(Math_cnt_lvl4, Math_cnt_tested)) %>%
  # Drop the count tested now that we have percentages
  select(-ELA_cnt_met, -Math_cnt_met,
         -ELA_cnt_lvl4, -Math_cnt_lvl4)

## We now have our aggregated values for last 2
## grades with percent meeting standards.

# Replace values in student_group with different values
df_assess_last2 <- df_assess_last2 %>%
  mutate(student_group = case_when(
    student_group == "All Students" ~ "All",
    student_group == "Low-Income" ~ "LI",
    student_group == "Female" ~ "Female")
  )

# Pivot based on the demographics in student_group
df_assess_last2 <- df_assess_last2 %>%
  pivot_wider(
    names_from = student_group,
    values_from = c(ELA_cnt_tested, Math_cnt_tested, 
                    ELA_pct_met, Math_pct_met,
                    ELA_pct_lvl4, Math_pct_lvl4),
    names_glue = "{student_group}_{.value}"
  )

# Create ratios of Female and LI taking test,
# then drop unneeded columns.
df_assess_last2 <- df_assess_last2 %>%
  rename(ela_tested = All_ELA_cnt_tested,
         math_tested = All_Math_cnt_tested) %>%
  mutate(ela_li_ratio = safe_div(LI_ELA_cnt_tested, ela_tested),
         ela_female_ratio = safe_div(Female_ELA_cnt_tested, ela_tested),
         math_li_ratio = safe_div(LI_Math_cnt_tested, math_tested),
         math_female_ratio = safe_div(Female_Math_cnt_tested, math_tested)
  ) %>%
  select(-LI_ELA_cnt_tested, -Female_ELA_cnt_tested,
         -LI_Math_cnt_tested, -Female_Math_cnt_tested,
         # Remove pct_met for LI and Female
         -LI_ELA_pct_met, -Female_ELA_pct_met,
         -LI_Math_pct_met, -Female_Math_pct_met,
         -LI_ELA_pct_lvl4, -Female_ELA_pct_lvl4,
         -LI_Math_pct_lvl4, -Female_Math_pct_lvl4
         ) %>%
  rename(ela_met = All_ELA_pct_met,
         #ela_li_met = LI_ELA_pct_met,
         #ela_female_met = Female_ELA_pct_met,
         math_met = All_Math_pct_met,
         #math_li_met = LI_Math_pct_met,
         #math_female_met = Female_Math_pct_met
         ela_lvl4 = All_ELA_pct_lvl4,
         math_lvl4 = All_Math_pct_lvl4
         )


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
  select(-cnt_met, -cnt_lvl4) %>%    # Not used for this calculation
  filter(grade_level != "All Grades") %>%  # JUST individual grades
  # Change the results by subject to columns
  pivot_wider(names_from = test_subject, 
              values_from = c(cnt_tested, pct_met, pct_lvl4),
              names_glue = "{test_subject}_{.value}"
  )

# Replace NA with 0 for all variables
#df_assess_wtd[is.na(df_assess_wtd)] <- 0

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
    ELA_logit_lvl4 = logit_prop(ELA_pct_lvl4),

    # Compute Logit values for Math scores
    Math_logit = logit_prop(Math_pct_met),
    Math_logit_lvl4 = logit_prop(Math_pct_lvl4),

    # Normally, we would just use the # tested as the weight,
    # but here I want 11th grade to have more weight than
    # 3rd grade, so we are multiplying by grade_weights.
    grade_weight = grade_weights[grade_level],
    ELA_weight = grade_weight * ELA_cnt_tested,     # weight the size of the group tested
    Math_weight = grade_weight * Math_cnt_tested
  )

# Group by year and school district, then
# Calculate the weighted logit scores for the grouped data.
# Convert the weighted logits back to ratios.
df_assess_wtd <- df_assess_wtd %>%
  group_by(school_year, year, county,
           district_code, district_name, 
           student_group) %>%
  summarize(
    # Include total number of students tested
    ELA_cnt_tested = sum(ELA_cnt_tested),
    Math_cnt_tested = sum(Math_cnt_tested),
    
    # Generate weighted means for each Logit value.
    # This is what should be used for regression.
    ELA_weighted_logit = wmean_logit(ELA_logit, ELA_weight),
    ELA_weighted_logit_lvl4 = wmean_logit(ELA_logit_lvl4, ELA_weight),
    Math_weighted_logit = wmean_logit(Math_logit, Math_weight),
    Math_weighted_logit_lvl4 = wmean_logit(Math_logit_lvl4, Math_weight),

    # Convert Logits back to proportions for charts and tables
    ELA_score = inv_logit_pct(ELA_weighted_logit),
    ELA_score_lvl4 = inv_logit_pct(ELA_weighted_logit_lvl4),
    Math_score = inv_logit_pct(Math_weighted_logit),
    Math_score_lvl4 = inv_logit_pct(Math_weighted_logit_lvl4),

    .groups = "drop"   # ungroup
  ) %>%
  # drop the weighted logit values, keep just the scores
  select(-ELA_weighted_logit, -ELA_weighted_logit_lvl4,
         -Math_weighted_logit, -Math_weighted_logit_lvl4)

# Convert anything that is not a number (NaN) to 0
df_assess_wtd <- df_assess_wtd %>%
  mutate(across(where(is.numeric), ~ ifelse(is.nan(.), 0, .)))


## Now we have the weighted scores that can be used
## just like the percent met standard values.

# Replace values in student_group with different values
df_assess_wtd <- df_assess_wtd %>%
  mutate(student_group = case_when(
    student_group == "All Students" ~ "All",
    student_group == "Low-Income" ~ "LI",
    student_group == "Female" ~ "Female")
  )

# Pivot based on the demographics in student_group
df_assess_wtd <- df_assess_wtd %>%
  pivot_wider(
    names_from = student_group,
    values_from = c(ELA_cnt_tested, Math_cnt_tested, 
                    ELA_score, Math_score,
                    ELA_score_lvl4, Math_score_lvl4
                    ),
    names_glue = "{student_group}_{.value}"
  )

# Create ratios of Female and LI taking test,
# then drop unneeded columns.
df_assess_wtd <- df_assess_wtd %>%
  rename(ela_tested = All_ELA_cnt_tested,
         math_tested = All_Math_cnt_tested) %>%
  mutate(ela_li_ratio = safe_div(LI_ELA_cnt_tested, ela_tested),
         ela_female_ratio = safe_div(Female_ELA_cnt_tested, ela_tested),
         math_li_ratio = safe_div(LI_Math_cnt_tested, math_tested),
         math_female_ratio = safe_div(Female_Math_cnt_tested, math_tested)
  ) %>%
  select(-LI_ELA_cnt_tested, -Female_ELA_cnt_tested,
         -LI_Math_cnt_tested, -Female_Math_cnt_tested,
         # Remove pct_met for LI and Female
         -LI_ELA_score, -Female_ELA_score,
         -LI_Math_score, -Female_Math_score,
         -LI_ELA_score_lvl4, -Female_ELA_score_lvl4,
         -LI_Math_score_lvl4, -Female_Math_score_lvl4
  ) %>%
  rename(ela_score = All_ELA_score,
         #ela_li_score = LI_ELA_score,
         #ela_female_score = Female_ELA_score,
         math_score = All_Math_score,
         #math_li_score = LI_Math_score,
         #math_female_score = Female_Math_score
         ela_score_lvl4 = All_ELA_score_lvl4,
         math_score_lvl4 = All_Math_score_lvl4
         )


# ---- Join Enrollment to Assessment ----

## Create final data sets to be used for
## analysis and regression.

# Enrollment and All Grades
dfCapAllGrades <- df_enroll_year %>%
  inner_join(df_assess_allgrades,
             by = c("district_code" = "district_code",
                    "year" = "year")) %>%
  select(-district_name.y) %>%
  rename(district_name = district_name.x)

# Enrollment and Last Grade
dfCapLastGrade <- df_enroll_year %>%
  inner_join(df_assess_final,
             by = c("district_code" = "district_code",
                    "year" = "year")) %>%
  select(-district_name.y) %>%
  rename(district_name = district_name.x)

# Enrollment and Last 2 Grades
dfCapLast2Grades <- df_enroll_year %>%
  inner_join(df_assess_last2,
             by = c("district_code" = "district_code",
                    "year" = "year")) %>%
  select(-district_name.y) %>%
  rename(district_name = district_name.x)

# Enrollment and Weighted Grades
dfCapWeighted <- df_enroll_year %>%
  inner_join(df_assess_wtd,
             by = c("district_code" = "district_code",
                    "year" = "year")) %>%
  select(-district_name.y) %>%
  rename(district_name = district_name.x)


#write_csv(dfCapAllGrades, "csv/enroll_assess_allgrades.csv")
#write_csv(dfCapWeighted, "csv/enroll_assess_weighted.csv")

# ---- Select Census Variables ----

# select specific columns from core census data
dfCensus <- census %>%
  select(year, district_code, district_name,
         population = DP03_0001E,
         civ_labor_force = DP03_0003E,  # employed + unemployed
         employed = DP03_0004E,
         employment_rate = DP03_0004PE,
         unemployed = DP03_0005E, 
         unemployment_rate = DP03_0005PE,
         female_population = DP03_0010E,
         households = DP03_0051E,
         med_income_household = DP03_0062E,
         families = DP03_0075E,
         med_income_families = DP03_0086E,
         civ_population = DP03_0095E,
         health_insurance = DP03_0096E, 
         health_insurance_rate = DP03_0096PE # of civ_population
         )

# The "rate" variables are percentages. 
# For consistency we will convert to ratios.
dfCensus <- dfCensus %>%
  mutate(employment_rate = employment_rate / 100,
         unemployment_rate = unemployment_rate / 100,
         health_insurance_rate = health_insurance_rate / 100) %>%
  # We have num of females in population, convert as ratio to total population
  mutate(female_rate = safe_div(female_population, population))


# ---- Join Census to Enrollment & Assessment ----

# Enrollment and Last Grade
dfCapLastGrade <- dfCapLastGrade %>%
  left_join(dfCensus, by = c("district_code", "year")) %>%
  # drop duplicate district_name.y and rename district_name.x to district_name
  select(-district_name.y) %>%
  rename(district_name = district_name.x) %>%
  # Reorder columns
  select(year, school_year, district_code, 
         district_name, county, everything())
