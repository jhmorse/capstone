## Code removed from other files,
## as it was determined unneeded.
## Retained here just in case I change my mind.


# ---- Removed from load-data.R ----

## Enrollment Data
# Get list of files at path location
files <- list.files(glue("{path}enrollment/"), full.names = TRUE)
# Load all files into a list of data frames
data_list <- lapply(files, load_data)
names(data_list) <- basename(files)

# Clean up the column names using janitor::clean_names
data_list <- lapply(data_list, janitor::clean_names)

# There was only one file, so we can just
# access the first data frame.
enrollment <- data_list[[1]]
# Save to RDS format
enrollment <- remove_rownames(enrollment)
saveRDS(enrollment, file = glue("{rds_path}enrollment.rds"))




## Graduation Data
# Get list of files at path location
files <- list.files(glue("{path}graduation/"), full.names = TRUE)
# Load all files into a list of data frames
data_list <- lapply(files, load_data)
names(data_list) <- basename(files)

# Column clean up for graduation:
# Loop through the data frames in data_list and
# remove the columns continuing and dropout
data_list <- lapply(data_list, function(df) {
  df <- remove_column(df, "continuing")
  df <- remove_column(df, "dropout")
  return(df)
})

# Loop through the data frames and rename columns
data_list <- lapply(data_list, function(df) {
  df <- rename_column(df, "ESDOrganizationID", "ESDOrganizationId")
  df <- rename_column(df, "SchoolOrganizationid", "SchoolOrganizationId")
  df <- rename_column(df, "suppression", "Suppression")
  df <- rename_column(df, "DAT", "Suppression")
  df <- rename_column(df, "TransferredIn", "TransferIn")
  df <- rename_column(df, "Final Cohort", "FinalCohort")
  df <- rename_column(df, "DataAsof", "DataAsOf")
  df <- rename_column(df, "BegginingGrade9", "BeginningGrade9")
  return(df)
})

# Clean up the column names using janitor::clean_names
data_list <- lapply(data_list, janitor::clean_names)

# Loop through the list and perform an rbind to combine into single data frame
graduation <- do.call(rbind, data_list)
# Save to RDS format
graduation <- remove_rownames(graduation)
saveRDS(graduation, file = glue("{rds_path}graduation.rds"))


## Discipline
# Get list of files at path location
files <- list.files(glue("{path}discipline/"), full.names = TRUE)
# Load all files into a list of data frames
data_list <- lapply(files, load_data)
names(data_list) <- basename(files)

# Convert a single column in the third data frame
# into a POSIXct type value with a given format.
# The DateExtracted in df3 comes through as a character string
data_list[[3]]$DateExtracted <- as.POSIXct(data_list[[3]]$DateExtracted, 
                                           format = "%Y %b %d %I:%M:%S %p")

# Clean up the column names using janitor::clean_names
data_list <- lapply(data_list, janitor::clean_names)

# Loop through the list and perform an rbind to combine into single data frame
discipline <- do.call(rbind, data_list)
# Save to RDS format
discipline <- remove_rownames(discipline)
saveRDS(discipline, file = glue("{rds_path}discipline.rds"))


## SQSS
# Get list of files at path location
files <- list.files(glue("{path}sqss/"), full.names = TRUE)
# Load all files into a list of data frames
data_list <- lapply(files, load_data)
names(data_list) <- basename(files)

# Loop through the data frames in data_list and
# remove columns not consistent across files
data_list <- lapply(data_list, function(df) {
  df <- remove_column(df, "OrganizationName")
  df <- remove_column(df, "ESDOrganizationID")
  df <- remove_column(df, "Label")
  df <- remove_column(df, "Percent")
  return(df)
})

# Loop through the data frames and rename columns
data_list <- lapply(data_list, function(df) {
  df <- rename_column(df, "SchoolOrganizationid", "SchoolOrganizationId")
  df <- rename_column(df, "Measures", "Measure")
  df <- rename_column(df, "DATReason", "Suppression")
  df <- rename_column(df, "DAT_Reason", "Suppression")
  df <- rename_column(df, "APCourseNumber", "NumberTakingAP")
  df <- rename_column(df, "IBCourseNumber", "NumberTakingIB")
  df <- rename_column(df, "CIHSCourseNumber", "NumberTakingCollegeInTheHighSchool")
  df <- rename_column(df, "CambridgeCourseNumber", "NumberTakingCambridge")
  df <- rename_column(df, "CTECourseNumber", "NumberTakingCTETechPrep")
  df <- rename_column(df, "RunningStartCourseNumber", "NumberTakingRunningStart")
  df <- rename_column(df, "APCoursePercent", "PercentTakingAP")
  df <- rename_column(df, "IBCoursePercent", "PercentTakingIB")
  df <- rename_column(df, "CIHSCoursePercent", "PercentTakingCollegeInTheHighSchool")
  df <- rename_column(df, "CambridgeCoursePercent", "PercentTakingCambridge")
  df <- rename_column(df, "CTECoursePercent", "PercentTakingCTETechPrep")
  df <- rename_column(df, "RunningStartCoursePercent", "PercentTakingRunningStart")
  return(df)
})

# Clean up the column names using janitor::clean_names
data_list <- lapply(data_list, janitor::clean_names)

# Reorder the columns based on one of the data frames
template <- data_list[[1]]   # pick a df with the correct order
template_order <- names(template)   # get the list of columns
# Reorder each data frame in the list
data_list <- lapply(data_list, function(x) x[template_order])

# Loop through the list and perform an rbind to combine into single data frame
sqss <- do.call(rbind, data_list)
# Save to RDS format
sqss <- remove_rownames(sqss)
saveRDS(sqss, file = glue("{rds_path}sqss.rds"))


## WSIF
# Get list of files at path location
files <- list.files(glue("{path}wsif/"), full.names = TRUE)
# Load all files into a list of data frames
data_list <- lapply(files, load_data)
names(data_list) <- basename(files)

# Loop through the data frames in data_list and
# remove columns not consistent across files
data_list <- lapply(data_list, function(df) {
  df <- remove_column(df, "2022_TitleI")
  df <- remove_column(df, "2023_TitleI")
  df <- remove_column(df, "Growth Combined Decile")
  df <- remove_column(df, "Growth_Combined_Decile")
  df <- remove_column(df, "AL_ELA_NumberofStudents")
  df <- remove_column(df, "AL_ELA_Avg_Level")
  df <- remove_column(df, "AL_ELA_Decile")
  df <- remove_column(df, "AL_Math_NumberofStudents")
  df <- remove_column(df, "AL_Math_Avg_Level")
  df <- remove_column(df, "AL_Math_Decile")
  df <- remove_column(df, "Other_Academic_Combined_Decile")
  df <- remove_column(df, "Support Tier")
  df <- remove_column(df, "Support_Tier")
  df <- remove_column(df, "2022_Support_Tier")
  df <- remove_column(df, "2023_Annual_Identification")
  df <- remove_column(df, "2024_Annual_Identification")
  df <- remove_column(df, "2017_Support_Tier")
  df <- remove_column(df, "2022_Cycle_Support")
  df <- remove_column(df, "2023_Cycle_Support")
  df <- remove_column(df, "2023_Cycle3_Support")
  return(df)
})

# Loop through the data frames and rename columns
data_list <- lapply(data_list, function(df) {
  df <- rename_column(df, "2022_Score", "final_school_score")
  df <- rename_column(df, "2023_Score", "final_school_score")
  df <- rename_column(df, "2024_Score", "final_school_score")
  return(df)
})

# Clean up the column names using janitor::clean_names
data_list <- lapply(data_list, janitor::clean_names)

# Reorder the columns based on the 2024 data
template <- data_list[[6]]   # pick a df with the correct order
template_order <- names(template)   # get the list of columns
# Reorder each data frame in the list
data_list <- lapply(data_list, function(x) x[template_order])

# Loop through the list and perform an rbind to combine into single data frame
wsif <- do.call(rbind, data_list)
# Save to RDS format
wsif <- remove_rownames(wsif)
saveRDS(wsif, file = glue("{rds_path}wsif.rds"))


# ---- Removed from load-outcome_data.R ----

earnings <- read_csv(glue("{path}earnings_by_industry.csv"))
fy_enrollment_sector <- read_csv(glue("{path}first_year_enrollment_by_sector.csv"))
persistence <- read_csv(glue("{path}persistence_retention.csv"))
ps_completion <- read_csv(glue("{path}post_secondary_completion.csv"))
dual_credit <- read_csv(glue("{path}dual_credit.csv"))

saveRDS(fy_enrollment_sector, glue("{rds_path}fy_enrollment_sector.rds"))
saveRDS(persistence, glue("{rds_path}persistence.rds"))
saveRDS(ps_completion, glue("{rds_path}ps_completion.rds"))
saveRDS(dual_credit, glue("{rds_path}dual_credit.rds"))


# ---- Removed from build-dataset.R ----

graduation <- read_rds("rds/graduation.rds")

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

## Use df_enroll_district for rates of enrollment by district,
## averaged across years.


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

## ALSO NOT INCLUDING Remedial coursetaking

# ---- Remedial English Classes ----

# Get the Remedial English courses for All, Female, and Male.
df_remedial <- remedial %>%
  filter(SchoolTTL == "District Wide",  # Districts only
         LEACode != "N/A",              # Valid districts only
         LEACode != "51760",
         CohortYearTTL >= 2015,         # No school data is before 2015
         PSEnrollLevel == "4 Year",     # 4 -Year College Enrollment
         DemographicGroup %in% c("All Students", "Gender"),
         RemedialType == "English") %>%
  select(CohortYearTTL, LEACode, DemographicValue, Pct) %>%
  rename(year = CohortYearTTL,
         district_code = LEACode,
         demographic = DemographicValue,
         percent_remedial = Pct)

# Pivot the demographics into columns
df_remedial <- df_remedial %>%
  pivot_wider(names_from = demographic,
              values_from = percent_remedial,
              names_glue = "remedial_{demographic}")

# Join the Remedial English class data
# to the enrollment data.
df_enroll_year <- df_enroll_year %>%
  left_join(df_remedial, by = c("district_code" = "district_code",
                                "year" = "year")
  )



# ---- SBAC Assessment Data ----

## Use df_assess_dist_allgrades to get an aggregate of all years, 
##   percent meeting standards across all grades
## Use df_assess_dist_final to get an aggregate of all years for
##   a district, showing percent met standards in final testing.
## Use df_assess_dist_last2 to get an aggregate of all years for
##   each District of the last two SBAC scores.
## Use df_assess_dist_wtd for a weighted score at the district
##   level with all years aggregated together.

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


