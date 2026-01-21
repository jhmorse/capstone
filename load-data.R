library(readr)
library(tibble)
library(glue)
library(janitor)

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



##################
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



##################
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



##################
## Assessment Data

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
# Save to RDS format
assessment <- remove_rownames(assessment)
saveRDS(assessment, file = glue("{rds_path}assessment.rds"))



#############
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



#######
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



#######
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






############################################################
# Get number of rows and columns for each data frame in the list
sapply(data_list, dim)
View(data_list[[1]])

rbind(data_list[[3]], data_list[[4]])
# What columns are in df1 but not in df2?
setdiff(names(data_list[[5]]), names(data_list[[6]]))

