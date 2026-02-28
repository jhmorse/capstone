library(tidyverse)
library(glue)

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


# ---- District Lookup ----

# Check to see if the district_lookup data frame exists
if (!exists("district_lookup")) {
  # If not, load the school directory and create the district lookup
  wa_school_directory <- read_csv(glue("../data/wa_school_directory.csv"))
  
  district_lookup <- wa_school_directory %>%
    select(LEACode, LEAName, ESDCode, ESDName) %>%
    distinct() %>%
    rename(district_code = LEACode,
           district_name = LEAName,
           esd_code = ESDCode,
           esd_name = ESDName)
}

# Need to implement 12 school district name changes
# to match the district names in the census data.
district_lookup <- district_lookup %>%
  # Convert any ALL CAPS to Normal Case (CASHMERE)
  mutate(district_name = str_to_title(district_name)) %>%
  # Convert certain "School District" to "Public Schools"
  mutate(district_name = if_else(district_name == "Fife School District", 
                                 "Fife Public Schools", district_name)) %>%
  mutate(district_name = if_else(district_name == "Longview School District", 
                                 "Longview Public Schools", district_name)) %>%
  # Lopez Island to Lopez
  mutate(district_name = if_else(district_name == "Lopez Island School District", 
                                 "Lopez School District", district_name)) %>%
  mutate(district_name = if_else(district_name == "Mary M Knight School District", 
                                 "Mary M. Knight School District", district_name)) %>%
  mutate(district_name = if_else(district_name == "Spokane School District", 
                                 "Spokane Public Schools", district_name)) %>%
  mutate(district_name = if_else(district_name == "Star School District No. 054", 
                                 "Star School District", district_name)) %>%
  mutate(district_name = if_else(district_name == "Steilacoom Hist. School District", 
                                 "Steilacoom Historical School District", district_name)) %>%
  mutate(district_name = if_else(district_name == "Sumner-Bonney Lake School District", 
                                 "Sumner School District", district_name)) %>%
  mutate(district_name = if_else(district_name == "Tacoma School District", 
                                 "Tacoma Public Schools", district_name)) %>%
  mutate(district_name = if_else(district_name == "Vancouver School District", 
                                 "Vancouver Public Schools", district_name)) %>%
  mutate(district_name = if_else(district_name == "Yelm School District", 
                                 "Yelm Community Schools", district_name)) %>%
  mutate(district_name = if_else(district_name == "Lacrosse School District", 
                                 "LaCrosse School District", district_name)) %>%
  mutate(district_name = if_else(district_name == "Mccleary School District", 
                                 "McCleary School District", district_name))
  

# ---- Single Census File Load ----
filename <- "ACSDP5Y2024.DP03-Data.csv"
dfCensus <- read_csv(glue("{path}census/{filename}"))

dfCensus <- dfCensus %>%
  select(-GEO_ID) %>%
  rename(district_name = NAME) %>%
  filter(!district_name %in% c("Geographic Area Name", "Washington")) %>%
  # Convert every column except the first two columns
  # to numeric values.
  mutate(across(-c(district_name), as.numeric)) %>%
  # Strip ", Washington" off the end of all values
  # in the district_name column.
  mutate(district_name = str_replace(district_name, ", Washington", ""))

# Attempt to join the data to the district directory
dfCensus <- dfCensus %>%
  left_join(district_lookup, by = "district_name") %>%
  select(-esd_code, -esd_name) %>%
  # Reorder columns to have district_code first, then district_name, 
  # then geo_id, then the rest of the columns.
  select(district_code, district_name, everything())


# ---- Census Data Load ----

# Get list of files at path location
files <- list.files(glue("{path}census/"), full.names = TRUE)

# Load all files into a list of data frames
data_list <- lapply(files, load_data)
names(data_list) <- basename(files)

# Clean up columns and rows:
# Loop through the data frames in data_list and
# make changes as needed.
data_list <- lapply(data_list, function(df) {
  # Remove GEO_ID
  df <- remove_column(df, "GEO_ID")
  
  # Rename NAME
  df <- rename_column(df, "NAME", "district_name")
  
  return(df)
})

data_list <- lapply(data_list, function(df) {
  # Filter out unwanted rows
  df <- df %>%
    filter(!district_name %in% c("Geographic Area Name", "Washington")) %>%
    # Convert every column except the first two columns
    # to numeric values.
    mutate(across(-c(district_name), as.numeric)) %>%
    # Strip ", Washington" off the end of all values
    # in the district_name column.
    mutate(district_name = str_replace(district_name, ", Washington", ""))
  
  return(df)
}) 

# Add the year column to the data frame based
# on the file name. The imap() function provides
# the name in nm.
data_list <- imap(data_list, function(df, nm) {
  # Parse the year from the file name. The file name
  # looks like "ACSDP5Y2023.DP03-Data.csv" and the year
  # is 2023. We can use a regular expression to 
  # extract the year.
  the_year <- as.numeric(str_extract(nm, "\\d{4}"))
  
  # Add the year as a new column
  df <- df %>%
    mutate(year = the_year)
  
  return(df)
})

# Join the data frames in the list to
# the district lookup to get the district codes.
data_list <- lapply(data_list, function(df) {
  df <- df %>%
    left_join(district_lookup, by = "district_name") %>%
    select(-esd_code, -esd_name) %>%
    # Reorder columns to have district_code first, then district_name,
    # then geo_id, then the rest of the columns.
    select(year, district_code, district_name, everything())
  return(df)
})


# Loop through the list and perform 
# an rbind to combine into single data frame
census <- do.call(rbind, data_list)
# drop the rownames from the data frame
census <- remove_rownames(census)

# Store the data frame as an RDS
saveRDS(census, glue("{rds_path}census.rds"))



