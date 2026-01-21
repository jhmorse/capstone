library(tidyverse)
library(glue)

## School Code Look up file

# Read in an RDS data file with all schools
school_lookup <- read_rds("rds/enrollment.rds") |>
  dplyr::select(county, district_code, district_name,
                school_code, school_name, 
                school_organizationid) |>
  dplyr::distinct()

dftemp <- school_lookup |>
  filter(county == "Snohomish")
