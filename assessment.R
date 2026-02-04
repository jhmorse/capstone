library(tidyverse)

assessment <- read_rds("rds/assessment.rds")

# ---- IMPORTANT ----
## Decisions I am making:
## 1. Filter to only SBAC test administration.
## 2. Filter to only "All Students" student group.
## 3. Remove rows where count_met_standard is "NULL".
## 4. Convert NA in numeric columns to 0.




# ---- DISTRICT LEVEL DATA ----
## Creating District Level Scores
## that are a consolidation of grade
## level scores for the SBAC ELA results.

# 
# 1. Define grade weights
# 
grade_weights <- c(
  "03" = 1,
  "04" = 2,
  "05" = 3,
  "06" = 4,
  "07" = 5,
  "08" = 6,
  "10" = 7,
  "11" = 8
)

# 
# 2. Prepare data
# 

district_scores <- assessment %>%
  filter(
    organization_level == "District",
    test_administration == "SBAC",
    test_subject == "ELA",
    student_group == "All Students",
    grade_level %in% names(grade_weights)
  ) %>%
  mutate(
    n_tested = count_students_expected_to_test_incl_passed,
    logit = log(percent_met_standard / (1 - percent_met_standard)), # Compute the logit
    grade_weight = grade_weights[grade_level],
    combined_weight = grade_weight * n_tested     # weight the size of the group tested
  ) %>%
  group_by(district_code, school_year) %>%
  summarize(
    weighted_logit = sum(logit * combined_weight, na.rm = TRUE) /   # weight the logit
      sum(combined_weight, na.rm = TRUE),
    district_score = 1 / (1 + exp(-weighted_logit)),   # convert back to proportion
    .groups = "drop"
  )

# 
# 3. Get the District rows with All Grades
# 
dfDistrict <- assessment %>%
  filter(organization_level == "District",
         grade_level == "All Grades",
         test_administration == "SBAC",
         test_subject == "ELA",
         student_group == "All Students"
  )

# Join scores back into All Grades
dfDistrict <- dfDistrict %>%
#  left_join(district_scores, by=c("district_code", "school_year")) %>%
  mutate(
    # fallback: if no grade_level score exists, use original proportion
    district_score = if_else(
      is.na(district_score),
      percent_met_standard,
      district_score
    )
  )



# ---- School Level Data ----

dfSchools <- assessment %>%
  filter(organization_level == "School",
         test_administration == "SBAC",
         student_group == "All Students",
         grade_level == "All Grades")

## Moved this code to the data load section for assessment
# dfSchools <- dfSchools %>%
#   # Remove all rows where count_met_standard == NULL
#   filter(suppression == "None",
#          count_met_standard != "NULL") %>%
#   # Convert the "71.3%" style characters into numbers
#   mutate(percent_met_standard = parse_number(percent_met_standard) / 100)
# 
# # There are several numeric columns that came in as character.
# # These need to be converted.
# numeric_columns <- c("count_of_students_expected_to_test",
#                      "count_students_expected_to_test_incl_passed",
#                      "count_met_standard",
#                      "percent_level1", "percent_level2", 
#                      "percent_level3", "percent_level4",
#                      "percent_met_tested_only", "percent_no_score",
#                      "percent_participation")
# dfSchools <- dfSchools %>%
#   # convert to numeric
#   mutate(across(all_of(numeric_columns), as.numeric)) %>%
#   # Convert NA to 0 across these numeric columns
#   mutate(across(all_of(numeric_columns), ~replace_na(., 0)))


## LET'S DO SOME DENSITY PLOTS

# ---- Density plot of percent_met_standard ----

ggplot(dfSchools) +
  # Met standard with 3 or 4
  geom_density(aes(x = percent_met_standard),
               fill = "blue", alpha = 0.5) +
  # Received a 4
  geom_density(aes(x = percent_level4), 
               fill = "green", alpha = 0.5) +
  # Received a 3
  geom_density(aes(x = percent_level3), 
               fill = "red", alpha = 0.5) +
  theme_minimal()

dfELA <- filter(dfSchools, test_subject == "ELA")
dfMath <- filter(dfSchools, test_subject == "Math")

# ---- ELA vs. Math ----
ggplot() +
  geom_density(data=dfELA, 
               aes(x = percent_met_standard, fill="ELA"), alpha = 0.7) +
  geom_vline(xintercept = median(dfELA$percent_met_standard),
             linetype="dashed", color = "black") +
  geom_density(data=dfMath,
               aes(x = percent_met_standard, fill="Math"), alpha = 0.7) +
  geom_vline(xintercept = median(dfMath$percent_met_standard),
             linetype="dashed", color = "gray") +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_fill_manual(
    values = c("ELA" = "darkblue", "Math" = "darkred"),
    name = "SBAC Test Subject"
  ) +
  # Annotate not used right now
  #annotate("label", x = Inf, y = Inf,
  #         label = "High growth customers!",
  #         hjust = 1.5, vjust = 2.0, 
  #         size = 4, fill = "white", alpha = 0.8) +
  theme_minimal()

# ---- Series of Density Plots by year ----
# Create a data set with median values for each subject
median_permet <- dfSchools %>%
  group_by(school_year, test_subject) %>%
  summarize(xbar = median(percent_met_standard, na.rm=TRUE))
median_permet$y <- 0.3  # Used to position label

# facet_grid by year and by test_subject
p <- ggplot(data = dfSchools, 
            mapping = aes(x = percent_met_standard, fill = test_subject))
p + geom_density(alpha = 0.6, 
                 mapping = aes(y = after_stat(scaled))) + 
  geom_vline(data = median_permet, aes(xintercept = xbar),
             color = "white", linewidth = 0.5) +
  scale_x_continuous(breaks = c(.25, .50, .75), labels = scales::percent) +
  scale_y_continuous(labels = NULL) +
  scale_fill_manual(values = c("darkblue", "darkred"),
                    name = "Test Subject") +
  #guides(fill = "none") +   # remove legend
  labs(title = "Distribution of SBAC Scores Meeting Standards",
       subtitle = "Meeting Standards requires a score of 3 or 4 on the test.",
       x = "Percent Meeting Standard",
       y = "Density (scaled)",
       caption = "No testing administered during 2019-20 or 2020-2021 school years due to COVID outbreak."
       ) +
  #facet_grid(school_year ~ test_subject, switch = "y")
  facet_grid(school_year ~ ., switch = "y")


# ---- Series of Density plots by Grade Level

# Define new data set with grade levels broken out
# and focused on ELA test results.
dfGrades <- assessment %>%
  filter(organization_level == "School",
         test_administration == "SBAC",
         test_subject == "ELA",
         student_group == "All Students")

# Turn the grade_level into a factor with proper ordering
grade_levels <- c("03", "04", "05", "06", "07", "08",
                  "10", "11", "All Grades")
dfGrades <- dfGrades %>%
  mutate(grade_level = factor(grade_level, levels = grade_levels))

# Create a data set with median values for each grade
median_grade <- dfGrades %>%
  group_by(grade_level) %>%
  summarize(xbar = median(percent_met_standard, na.rm=TRUE))
median_grade$y <- 0.3  # Used to position label

# Facet Grid by Grade level
p <- ggplot(data = dfGrades, 
            mapping = aes(x = percent_met_standard))
p + geom_density(alpha = 1, fill = "darkblue", 
                 mapping = aes(y = after_stat(scaled))) + 
  geom_vline(data = median_grade, aes(xintercept = xbar),
             color = "white", linewidth = 0.5) +
  scale_x_continuous(breaks = c(.25, .50, .75), labels = scales::percent) +
  scale_y_continuous(labels = NULL) +
  #guides(fill = "none") +   # remove legend
  labs(title = "Distribution of ELA SBAC Scores by Grade Level",
       subtitle = "Meeting Standards requires a score of 3 or 4 on the test.",
       x = "Percent Meeting Standard",
       y = "Density (scaled)",
       caption = "No testing administered during 2019-20 or 2020-2021 school years due to COVID outbreak."
  ) +
  facet_grid(grade_level ~ ., switch = "y")



# ---- SBAC Scores by Grade and by District ----
## This section looks at School District trends by grade level.

# Get a data set at District level by Grade
dfDistrictGrade <- assessment %>%
  filter(organization_level == "District",
         test_administration == "SBAC",
         test_subject == "ELA",
         student_group == "All Students")

# Get a list of all districts that have a percent_met_standard
# value for grade 11
has_grade_11 <- dfDistrictGrade %>%
  filter(grade_level == "11",
         !is.na(percent_met_standard)) %>%
  pull(district_code) %>%
  unique()
# Filter dfDistrictGrade to only those districts
dfDistrictGrade <- dfDistrictGrade %>%
  filter(district_code %in% has_grade_11)

# Turn the grade_level into a factor with proper ordering
grade_levels <- dfDistrictGrade %>%
  pull(grade_level) %>%
  unique() %>%
  sort()
dfDistrictGrade <- dfDistrictGrade %>%
  mutate(grade_level = factor(grade_level, 
                              levels = grade_levels))

# Get a unique list of district codes
district_sample <- dfDistrictGrade %>%
  pull(district_code) %>%
  unique() %>%
  sort()
# Take a random sample of codes
set.seed(123)  # For reproducibility
district_sample <- sample(district_sample, size = 50)
# Use the random sample to filter dfDistrictGrade
dfPlot <- dfDistrictGrade %>%
  filter(district_code %in% district_sample) %>%
  filter(grade_level != "All Grades") %>%
  group_by(district_code, district_name, grade_level) %>%
  summarize(
    percent_met_standard = median(percent_met_standard),
    .groups = "drop"
  )
  
districts <- as.factor(sort(unique(dfPlot$district_name)))

p <- ggplot(dfPlot,
            aes(x = grade_level, y = district_name,
                fill = percent_met_standard)) +
  geom_tile() +
  scale_x_discrete(position = "top") +
  scale_y_discrete(position = "left",
                   limits=rev(districts)) +
  scale_fill_gradient(low = "white", high = "#08306B",
                      name = "Percent Met Standard",
                      label = scales::percent) +
  theme_minimal() +
  theme(
    legend.position = "top",
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    #axis.text.x = element_text(angle = 45, hjust=0, vjust = 0.2),
    axis.text.y = element_text(size=rel(1.0)),
    plot.title = element_text(size=rel(1.5), face="bold")
  ) +
  labs(title = "Percent Met Standard by District and by Grade",
     caption = "Darker colors = higher percentages.",
)
  
p

View(dfPlot %>% filter(grade_level == "11"))

nrow(assessment %>% filter(organization_level == "School"))
