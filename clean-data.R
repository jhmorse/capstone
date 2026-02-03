library(tidyverse)
library(glue)

## School Code Look up file

path <- "../data/"
rds_path <- "rds/"

wa_school_directory <- read_csv(glue("{path}wa_school_directory.csv"))

df <- assessment %>%
  select(test_administration, test_subject) %>%
  distinct()



# INCOMPLETE: Build school feeder paths 

wa_school_paths <- wa_school_directory %>%
  select(LEACode, LEAName, SchoolCode, SchoolName, 
         LowestGrade, HighestGrade, GradeCategory)

elem <- wa_school_paths %>% filter(GradeCategory == "Elementary School")
mid  <- wa_school_paths %>% filter(GradeCategory %in% c("Middle School", "Jr High") )
high <- wa_school_paths %>% filter(GradeCategory == "High School")
k12 <- wa_school_paths %>% filter(GradeCategory %in% c("K-12", "PK-12")) 

wa_school_paths %>%
  select(GradeCategory) %>%
  distinct()

# Skeleton: one row per elementary, with NA middle/high to be filled
feeder_skeleton <- elem %>%
  transmute(
    district_code,
    district_name,
    elem_school_code = school_code,
    elem_school_name = school_name,
    mid_school_code  = NA_character_,
    mid_school_name  = NA_character_,
    high_school_code = NA_character_,
    high_school_name = NA_character_,
    feeder_type      = "strict",
    notes            = NA_character_
  )  
