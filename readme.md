# Capstone Project for Johns Hopkins University

## Advanced Academic Programs - Data Analysis and Policy

John Morse, Candidate for the Master of Science degree in Data Analysis and Policy

## Abstract

To be written...

## Steps to Reproduce

1.  Ensure RDS files are available in the \rds subdirectory.

2.  Run the code in build-dataset.R. This will load required RDS files and build data frames needed for analysis.

## Description of Files

| File | Purpose |
|------------------------------------|------------------------------------|
| load-data.R | Load source data from downloaded CSV files, combine into single data frame for each subject, then save as RDS file. |
| load-outcome-data.R | Load source data from downloaded CSV files for Outcome variables, then save as RDS file. |
| build-dataset.R | Required to load and prepare data for analysis. Creates several data frames and fields as described in the Data and Methodology section of the file document. |
| analysis1 | Primary analysis file where data is examined and results are calculated. Results are reproduced in the Quarto document used to produce the final project report. |
| appendix | Builds data sets used for tables and charts that appear in the Appendix section. |
