library(readr)
library(tibble)
library(glue)
library(janitor)

# ---- Helper Functions ----

# Function to load data from a CSV file
load_data <- function(file_path) {
  if (!file.exists(file_path)) {
    stop(glue("File not found: {file_path}"))
  }
  
  data <- read_csv(file_path)
  return(data)
}

path <- "../data/"
rds_path <- "rds/"

# Function to remove a column if it exists
remove_column <- function(df, col_name) {
  if (col_name %in% colnames(df)) {
    df[[col_name]] <- NULL
  }
  return(df)
}

# Function to rename a column if it exists
rename_column <- function(df, old_name, new_name) {
  if (old_name %in% colnames(df)) {
    colnames(df)[colnames(df) == old_name] <- new_name
  }
  return(df)
}


# ---- College Enrollment Data ----
fy_enrollment <- read_csv(glue("{path}first_year_enrollment.csv"))
saveRDS(fy_enrollment, glue("{rds_path}fy_enrollment.rds"))


# ---- School Districts & Schools ----

wa_school_directory <- read_csv(glue("../data/wa_school_directory.csv"))
wa_district_directory <- read_csv(glue("../data/wa_district_directory.csv"))

saveRDS(wa_school_directory, glue("{rds_path}school_directory.rds"))
saveRDS(wa_district_directory, glue("{rds_path}district_directory.rds"))



# ---- Assessment Data ----

# Get list of files at path location
files <- list.files(glue("{path}assessment/"), full.names = TRUE)
# Load all files into a list of data frames
data_list <- lapply(files, load_data)
names(data_list) <- basename(files)

# Column clean up
# Loop through the data frames in data_list and
# remove columns not consistent across files
data_list <- lapply(data_list, function(df) {
  df <- remove_column(df, "Test Administration (group)")
  df <- remove_column(df, "Percent Taking Alternative Assessment")
  df <- remove_column(df, "Count Foundational Grade-Level Knowledge And Above")
  df <- remove_column(df, "Percent Foundational Grade-Level Knowledge And Above")
  return(df)
})

# Loop through the data frames and rename columns
data_list <- lapply(data_list, function(df) {
  df <- rename_column(df, "DAT", "Suppression")
  df <- rename_column(df, "ESDOrganizationID", "ESDOrganizationId")
  df <- rename_column(df, "SchoolOrganizationid", "SchoolOrganizationId")
  df <- rename_column(df, "Percent No Score", "PercentNoScore")
  df <- rename_column(df, "Percent Participation", "PercentParticipation")
  
  df <- rename_column(df, 
                      "Count of students expected to test including previously passed", 
                      "CountStudentsExpectedToTest_InclPassed")
  df <- rename_column(df, 
                      "Count of Students Expected to Test (included previously passed)", 
                      "CountStudentsExpectedToTest_InclPassed")
  df <- rename_column(df, 
                      "Count of Students Expected to Test (including previously passed)", 
                      "CountStudentsExpectedToTest_InclPassed")
  df <- rename_column(df, 
                      "Count Consistent Grade Level Knowledge And Above", 
                      "CountMetStandard")
  df <- rename_column(df, 
                      "Percent Consistent Grade Level Knowledge And Above", 
                      "PercentMetStandard")
  df <- rename_column(df, "Percent Consistent Tested Only", "PercentMetTestedOnly")
  return(df)
})

# Move the DataAsOf column to be the last column in each data frame
data_list <- lapply(data_list, function(df) {
  if ("DataAsOf" %in% colnames(df)) {
    df <- df[, c(setdiff(names(df), "DataAsOf"), "DataAsOf")]
  }
  return(df)
})

# Clean up the column names using janitor::clean_names
data_list <- lapply(data_list, janitor::clean_names)

# Loop through the list and perform an rbind to combine into single data frame
assessment <- do.call(rbind, data_list)

# There are several numeric columns that came in as character.
# These need to be converted.
numeric_columns <- c("count_of_students_expected_to_test",
                     "count_students_expected_to_test_incl_passed",
                     "count_met_standard",
                     "percent_level1", "percent_level2", 
                     "percent_level3", "percent_level4",
                     "percent_met_tested_only", "percent_no_score",
                     "percent_participation")
assessment <- assessment %>%
  # Remove all rows where count_met_standard == NULL
  filter(suppression == "None",
         count_met_standard != "NULL") %>%
  # Convert the "71.3%" style characters into numbers
  mutate(percent_met_standard = parse_number(percent_met_standard) / 100) %>%
  # convert columns to numeric where appropriate
  mutate(across(all_of(numeric_columns), as.numeric)) %>%
  # Convert NA to 0 across these numeric columns
  mutate(across(all_of(numeric_columns), ~replace_na(., 0)))

# Save to RDS format
assessment <- remove_rownames(assessment)
saveRDS(assessment, file = glue("{rds_path}assessment.rds"))



