## Data sets used for tables and charts in the Appendix.

library(tidyverse)

wasubject <- c(
  rep("Assessment", 8),
  "Enrollment",
  rep("Graduation", 4)
)
wadataset <- c(
  "Report Card Assessment Data 2014-15 School Year",
  "Report Card Assessment Data 2015-16 School Year",
  "Report Card Assessment Data 2016-17 School Year",
  "Report Card Assessment Data 2017-18 School Year",
  "Report Card Assessment Data 2018-19 School Year",
  "Report Card Assessment Data 2021-22 School Year",
  "Report Card Assessment Data 2022-23 School Year",
  "Report Card Assessment Data 2023-24 School Year",
  "Report Card Enrollment from 2014-15 to Current Year",
  "Report Card Graduation 2014-15 to 2020-2021",
  "Report Card Graduation 2021-22",
  "Report Card Graduation 2022-23",
  "Report Card Graduation 2023-24"
)

description <- c(
  rep("Assessment description", 8),
  "Enrollment descritpion",
  rep("Graduation description", 4)
)

# Build df with list of files downloaded from data.wa.gov.
df <- tibble::tibble(
  Subject = wasubject,
  Datasets = wadataset,
  Description = description
)
saveRDS(df, "rds/datasets_list.rds")



