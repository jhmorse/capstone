library(readr)
library(dplyr)

## NOTE: DAT = Disclosure Avoidance Technique.
## Rows with non-NA values in DAT or Suppression
## Could be flagged with a single non-null value
## for easy filtering. Any significant differences
## caused by non-null need to be identified.


##################
## Enrollment data

# Load the data from an RDS
enrollment <- read_rds("rds/enrollment.rds")

# Get a random sample of 10 rows from the dataframe
set.seed(123)  # For reproducibility
enrollment_sample <- enrollment[sample(nrow(enrollment), 10), ]
# Write the sample to a CSV file
write_csv(enrollment_sample, "validate/enrollment_sample.csv")

## NOTES
## Data aligned
## Blank values in original appear as NA for the following:
## SchoolCode, SchoolOrganizationid, CurrentSchoolType, DAT
## With the exception of DAT, these are associated with District Totals.


##################
## Assessment data

# Load the data from an RDS
assessment <- read_rds("rds/assessment.rds")

# Get a random sample of 10 rows from the dataframe
set.seed(123)  # For reproducibility
assessment_sample <- assessment[sample(nrow(assessment), 10), ]
# Write the sample to a CSV file
write_csv(assessment_sample, "validate/assessment_sample.csv")

## NOTES
## Data aligned
## However, half the rows pulled with this seed have Suppression or DAT values.

##################
## Graduation data
# Load the data from an RDS
graduation <- read_rds("rds/graduation.rds")

# because of high level of suppresssion in this data set,
# need to select No DAT from the Suppression field
graduation_sample <- graduation %>%
  filter(suppression == "No DAT")

# Get a random sample of 10 rows from the dataframe
set.seed(123)  # For reproducibility
graduation_sample <- graduation_sample[sample(nrow(graduation_sample), 10), ]
# Write the sample to a CSV file
write_csv(graduation_sample, "validate/graduation_sample.csv")

## Alignment in data
## Note the need to filter on rows with no suppression/DAT
## to ensure rows with actual data.



##################
## Discipline data

# Load the data from an RDS
discipline <- read_rds("rds/discipline.rds")

# because of high level of suppresssion in this data set,
# need to select None from the discipline_dat_notes field
discipline_sample <- discipline %>%
  filter(discipline_dat_notes == "None")

# Get a random sample of 10 rows from the dataframe
set.seed(123)  # For reproducibility
discipline_sample <- discipline_sample[sample(nrow(discipline_sample), 10), ]
# Write the sample to a CSV file
write_csv(discipline_sample, "validate/discipline_sample.csv")

## NOTES
## Alignment in data


##################
## WSIF data

# Load the data from an RDS
wsif <- read_rds("rds/wsif.rds")

# Get a random sample of 10 rows from the dataframe
set.seed(123)  # For reproducibility
wsif_sample <- wsif[sample(nrow(wsif), 10), ]
# Write the sample to a CSV file
write_csv(wsif_sample, "validate/wsif_sample.csv")

## NOTES
## Data is aligned.
## Some years have NULL instead of NA values for blanks.


##################
## SQSS data

# Load the data from an RDS
sqss <- read_rds("rds/sqss.rds")

# because of high level of suppresssion in this data set,
# need to select None from the discipline_dat_notes field
sqss_sample <- sqss %>%
  filter(measure == "Dual Credit",
         suppression %in% c("None", "No Suppression"))
  

# Get a random sample of 10 rows from the dataframe
set.seed(123)  # For reproducibility
sqss_sample <- sqss_sample[sample(nrow(sqss_sample), 10), ]
# Write the sample to a CSV file
write_csv(sqss_sample, "validate/sqss_sample.csv")

## NOTES
## &&& Analysis not complete
