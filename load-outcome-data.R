library(tidyverse)
library(glue)

path <- "../data/outcomes/"
rds_path <- "rds/"

earnings <- read_csv(glue("{path}earnings_by_industry.csv"))
fy_enrollment <- read_csv(glue("{path}first_year_enrollment.csv"))
fy_enrollment_sector <- read_csv(glue("{path}first_year_enrollment_by_sector.csv"))
persistence <- read_csv(glue("{path}persistence_retention.csv"))
ps_completion <- read_csv(glue("{path}post_secondary_completion.csv"))
remedial <- read_csv(glue("{path}remedial_coursetaking.csv"))
dual_credit <- read_csv(glue("{path}dual_credit.csv"))

wa_school_directory <- read_csv(glue("../data/wa_school_directory.csv"))

##################################################################
## Save to RDS files
saveRDS(fy_enrollment, glue("{rds_path}fy_enrollment.rds"))
saveRDS(fy_enrollment_sector, glue("{rds_path}fy_enrollment_sector.rds"))
saveRDS(persistence, glue("{rds_path}persistence.rds"))
saveRDS(ps_completion, glue("{rds_path}ps_completion.rds"))
saveRDS(remedial, glue("{rds_path}remedial.rds"))
saveRDS(dual_credit, glue("{rds_path}dual_credit.rds"))
saveRDS(wa_school_directory, glue("{rds_path}school_directory.rds"))

dfTemp <- remedial %>%
  select(RemedialType) %>%
  group_by(RemedialType) %>%
  summarize(n = n())
dfTemp

dfTemp <- fy_enrollment_sector %>%
  select(PSEnrollLevel, PSOrganizationLocationTTL, PSOrgnOwnershipTypeTTL) %>%
  group_by(PSEnrollLevel, PSOrganizationLocationTTL, PSOrgnOwnershipTypeTTL) %>%
  summarize(n = n())
dfTemp


# Get a random sample of 200 rows from the dual-credit dataset
set.seed(123)  # For reproducibility
dfSample <- dual_credit %>%
  sample_n(200)

# Convert columns to appropriate types
ps_enrollment <- ps_enrollment %>%
  mutate(LEACode = as.character(LEACode),
         SchoolCode = as.double(SchoolCode))

ps_enrollment_school <- ps_enrollment %>%
  left_join(wa_school_directory,
            by = c("LEACode" = "LEACode",
                   "SchoolCode" = "SchoolCode")) %>%
  select(LEACode, SchoolCode, SchoolName, GradeCategory) %>%
  distinct()

unique(fy_enrollment$PSEnrollLevel)
unique(fy_enrollment_sector$CohortYearTTL)
