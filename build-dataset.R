# ---- OVERVIEW ----
# This script builds and cleans the datasets for analysis.

# ---- Libraries ----
library(tidyverse)

# ---- Load Data ----
schools <- read_rds("rds/school_directory.rds")
districts <- read_rds("rds/district_directory.rds")
fy_enrollment <- read_rds("rds/fy_enrollment.rds")
assessment <- read_rds("rds/assessment.rds")
census <- read_rds("rds/census.rds")
census_education <- read_rds("rds/census_education.rds")
census_education_county <- read_rds("rds/census_education_county.rds")

# ---- Helper functions ----

# Safe division
safe_div <- function(num, denom) {
  ifelse(is.na(denom) | is.na(num) | denom == 0, 0, num / denom)
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
         DemographicValue, Pct) %>%
  rename(year = CohortYearTTL,
         district_code = LEACode,
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
  mutate(female_rate = safe_div(female_population, population)) %>%
  # Convert median income to thousands of dollars for easier interpretation
  mutate(med_income_household = med_income_household / 1000)

# Join census education data to census data, 
# keeping only the columns of interest.
dfCensus <- dfCensus %>%
  left_join(census_education,
            by = c("year", "district_code")) %>%
  select(-district_name.y, -pop_18to24, -bachelor_18to24,
         -pop_25over, -bachelor_25over, -pop_total, -bachelor_total) %>%
  rename(district_name = district_name.x)


# ---- SBAC Assessment Data - Base Data ----

# Get the District Level data by year
df_assess_year <- assessment %>%
  filter(
    student_group %in% c("All Students", "Female", "Low-Income"),
    organization_level == "District",
    test_administration == "SBAC"
  ) %>%
  # Second half of school year as number to match enrollment data
  mutate(year_test = as.numeric(substr(school_year, 1, 4)) + 1) %>%
  # Reduce to columns of interest
  select(school_year, year_test, county, district_code, district_name,
         grade_level, student_group, test_subject, 
         count_students_expected_to_test_incl_passed,
         count_met_standard, 
         percent_level4,
         percent_met_standard
  ) %>%
  rename(cnt_tested = count_students_expected_to_test_incl_passed,
         cnt_met = count_met_standard,
         pct_lvl4 = percent_level4,
         pct_met = percent_met_standard) %>%
  # Get the count meeting level 4 standard
  mutate(cnt_lvl4 = (pct_lvl4 * cnt_tested))


# ---- SBAC Adjusted Last Grade ----

## Shift the Last Grade data by one year, accounting
## for the fact that SBAC testing occurred in 11th grade
## and enrollment in college would occur after 12th grade.
## Also, we will restrict this data to districts and years
## where the last time tests were given was in 
## 8th, 10th, or 11th grade.

# Get grade level data only and pivot test subjects to columns
df_assess_final_adj <- df_assess_year %>%
  select(-cnt_met, -cnt_lvl4) %>%   # Won't use for this data set
  filter(grade_level != "All Grades") %>%  # JUST individual grades
  # Change the results by subject to columns
  pivot_wider(names_from = test_subject, 
              values_from = c(cnt_tested, pct_met, pct_lvl4),
              names_glue = "{test_subject}_{.value}"
  )

# Turn grade_level into an ordered factor
grade_levels <- sort(unique(df_assess_final_adj$grade_level))
df_assess_final_adj <- df_assess_final_adj %>%
  mutate(grade_level = factor(grade_level, levels = grade_levels))

# Get the highest grade_level for each school district and year
df_assess_final_adj <- df_assess_final_adj %>%
  group_by(school_year, district_code, district_name, student_group) %>%
  slice_max(grade_level, n = 1, with_ties = FALSE) %>%
  ungroup()

# Limit the data set to districts where last SBAC was in
# 11th, 10th, or 8th grade. This is about 91% of total rows (0.9084).
df_assess_final_adj <- df_assess_final_adj %>%
  filter(grade_level %in% c("08", "10", "11"))

# Create a year_grad variable that shifts the year based on last grade
# where SBAC was given. This will be used to join to enrollment data.
df_assess_final_adj <- df_assess_final_adj %>%
  mutate(year_grad = case_when(
    grade_level == "08" ~ year_test + 4,   # Shift 8th
    grade_level == "10" ~ year_test + 2,   # Shift 10th
    grade_level == "11" ~ year_test + 1,   # Shift 11th
    TRUE ~ year_test                       # No shift for other grades
  )) %>%
  select(school_year, year_test, year_grad, everything())


# Replace values in student_group with different values
df_assess_final_adj <- df_assess_final_adj %>%
  mutate(student_group = case_when(
    student_group == "All Students" ~ "All",
    student_group == "Low-Income" ~ "LI",
    student_group == "Female" ~ "Female")
  )

# Remove grade_level
df_assess_final_adj <- df_assess_final_adj %>%
  select(-grade_level)

# Pivot based on the demographics in student_group
df_assess_final_adj <- df_assess_final_adj %>%
  pivot_wider(
    names_from = student_group,
    values_from = c(ELA_cnt_tested, Math_cnt_tested, 
                    ELA_pct_met, Math_pct_met,
                    ELA_pct_lvl4, Math_pct_lvl4),
    names_glue = "{student_group}_{.value}"
  )

# Create ratios of Female and LI taking test,
# then drop unneeded columns.
df_assess_final_adj <- df_assess_final_adj %>%
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
         math_met = All_Math_pct_met,
         ela_lvl4 = All_ELA_pct_lvl4,
         math_lvl4 = All_Math_pct_lvl4
  )


##### IMPORTANT STEP
## For duplicate district_code and year_grad combinations,
## keep the row with the most recent year_test.
df_assess_final_adj <- df_assess_final_adj %>%
  group_by(district_code, year_grad) %>%
  slice_max(year_test, n = 1, with_ties = FALSE) %>%
  ungroup()

# Indicate shift in time from test to graduation/enrollment.
# To be used as a control variable in regression.
df_assess_final_adj <- df_assess_final_adj %>%
  mutate(grade_shift = year_grad - year_test) %>%
  select(school_year, year_test, year_grad, grade_shift, everything())


# ---- Join Adjusted Last Grade and Enrollment ----

dfCapLastGradeAdj <- df_enroll_year %>%
  inner_join(df_assess_final_adj,
             by = c("district_code" = "district_code",
                    "year" = "year_grad")) %>%
  select(-district_name.y) %>%
  rename(district_name = district_name.x)

# ---- Join Adjusted Last Grade and Census ----

dfCapLastGradeAdj <- dfCapLastGradeAdj %>%
  left_join(dfCensus, by = c("district_code", "year")) %>%
  # drop duplicate district_name.y and rename district_name.x to district_name
  select(-district_name.y) %>%
  rename(district_name = district_name.x) %>%
  # Reorder columns
  select(year, school_year, year_test, grade_shift, district_code, 
         district_name, county, everything())




