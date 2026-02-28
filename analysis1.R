library(tidyverse)


# ---- District Level SBAC Score Matrix ----

# Start with simple view of District aggregates
# and Assessment scores across all 4 methods.

#df_enroll_district
#df_assess_dist_final
#df_assess_dist_last2
#df_assess_dist_allgrades
#df_assess_dist_wtd

# Join enrollment to ELA Percent Met for All Grades
df <- df_enroll_district %>%
  inner_join(df_assess_dist_allgrades,
             by = c("district_code" = "district_code")) %>%
  select(-district_name.y, -pooled_logit) %>%
  rename(district_name = district_name.x,
         percent_enrolled = pooled_pct_enrolled)

# Filter to just the elements we want for the first plot
dfPlot <- df %>%
  select(district_code, enrollment_level, percent_enrolled, ELA_pct_met) %>%
  rename(ELA_allgrades = ELA_pct_met)

# Join the data to ELA Percent Met for Final Score
dfPlot <- dfPlot %>%
  inner_join(df_assess_dist_final,
             by = c("district_code" = "district_code")) %>%
  select(district_code, enrollment_level, percent_enrolled, ELA_allgrades,
         ELA_pct_met) %>%
  rename(ELA_finalscore = ELA_pct_met)

# Join the data to ELA Percent Met for Last 2 Scores
dfPlot <- dfPlot %>%
  inner_join(df_assess_dist_last2,
             by = c("district_code" = "district_code")) %>%
  select(district_code, enrollment_level, percent_enrolled, ELA_allgrades,
         ELA_finalscore, ELA_pct_met) %>%
  rename(ELA_last2 = ELA_pct_met)

# Join the data to the Weighted Score
dfPlot <- dfPlot %>%
  inner_join(df_assess_dist_wtd, 
             by = c("district_code" = "district_code")) %>%
  select(district_code, enrollment_level, percent_enrolled, 
         ELA_finalscore, ELA_last2, ELA_allgrades, ELA_score) %>%
  rename(ELA_weighted = ELA_score)


## Plot all on single matrix of plots

predictors <- c("ELA_finalscore", "ELA_last2", "ELA_allgrades", "ELA_weighted")
enroll_factor <- c("4 Year", "2 Year / CTC", "Not Enrolled")

dfPlotLong <- dfPlot %>%
  pivot_longer(
    cols = all_of(predictors),
    names_to = "predictor",
    values_to = "x_value"
  ) %>%
  mutate(
    predictor = factor(predictor, levels = predictors),
    enrollment_level = factor(enrollment_level, levels = enroll_factor)
  )

# Save to RDS
saveRDS(dfPlotLong, "rds/scatter_matrix.rds")

ggplot(dfPlotLong, aes(x = x_value, y = percent_enrolled)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE,
              linewidth = 0.4, color = "steelblue") +
  facet_grid(enrollment_level ~ predictor, 
             scales = "free_x",
             switch = "y") +
  labs(
    title = "Percent Enrolled vs. ELA Score",
    x = "ELA Score",
    y = "Enrollment Rate"
  ) +
  theme_bw() +
  theme(
    strip.background = element_rect(fill = "gray90"),
    strip.text.y = element_text(angle = 0, face = "bold")
  )



# ---- Series of Density Plots of Enrollment by Year ----
# Create a data set with median values for each subject
median_per_enroll <- df_enroll_year %>%
  group_by(year, enrollment_level) %>%
  summarize(xbar = median(percent_enrolled, na.rm=TRUE))
median_permet$y <- 0.3  # Used to position label

# facet_grid by year and by test_subject
p <- ggplot(data = df_enroll_year, 
            mapping = aes(x = percent_enrolled, fill = enrollment_level))
p + geom_density(alpha = 0.6, 
                 mapping = aes(y = after_stat(scaled))) + 
  geom_vline(data = median_per_enroll, aes(xintercept = xbar),
             color = "white", linewidth = 0.5) +
  scale_x_continuous(breaks = c(.25, .50, .75), labels = scales::percent) +
  scale_y_continuous(labels = NULL) +
  scale_fill_manual(values = c("darkblue", "darkred", "black"),
                    name = "Enrollment Type") +
  labs(title = "Distribution of Enrollment by Year",
       x = "Percent",
       y = "Density (scaled)",
       caption = "Enrollment from 2015 to 2023."
  ) +
  facet_grid(year ~ ., switch = "y")

# ---- Boxplots of 4 Year Enrollment by Year ----
# Boxplots of 4 year enrollment by year
ggplot(data = df_enroll_year,
            mapping = aes(x = factor(year), y = enrollment_all)) +
  geom_boxplot() +
  labs(
    title = "Distribution of District Level Enrollment Percentages",
    subtitle = "Enrollment in 4 Year College",
    x = "Enrollment Year",
    y = "Percent Enrolled"
  )


# ---- Boxplots of 2 Year Enrollment by Year ----
# Boxplots of 2 year enrollment by year
ggplot(data = df_enroll_year %>% filter(enrollment_level == "2 Year / CTC"),
       mapping = aes(x = factor(year), y = percent_enrolled)) +
  geom_boxplot() +
  labs(
    title = "Distribution of District Level Enrollment Percentages",
    subtitle = "Enrollment in 2 Year College or CTC",
    x = "Enrollment Year",
    y = "Percent Enrolled"
  )

  
# ---- Boxplots of ELA SBAC Results ----
  
# Boxplots of ELA Assessment
ggplot(data = df_assess_allgrades,
            mapping = aes(x = factor(school_year), y = ELA_pct_met)) +
  geom_boxplot() +
  labs(
    title = "Distribution of Percent Students Meeting SBAC ELA Standards",
    subtitle = "SBAC Results Captured at School District Level",
    x = "School Year",
    y = "Percent Meeting Standards"
  )
