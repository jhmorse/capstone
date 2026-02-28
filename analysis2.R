library(tidyverse)
library(plm)
library(jtools)


# Create a pdata.frame with entity and time indexes
pdata_allgrades <- pdata.frame(dfCapAllGrades, index = c("district_code", "year"))
pdata_weighted <- pdata.frame(dfCapWeighted, index = c("district_code", "year"))



# Entity and time fixed effects model
fe_twoway <- plm(enrollment_all ~ ela_met,
                 data = pdata_allgrades,
                 model = "within",
                 effect = "twoways")
summary(fe_twoway)
# Both district and year fixed effects are removed

library(lmtest)
#library(sandwich)

# Two-way clustering (common practice)
coeftest(fe_twoway,
         vcov = vcovDC(fe_twoway,
                       type = "HC1"))

# Create Pooled OLS
pool <- plm(enrollment_all ~ ela_met,
            data = pdata_allgrades,
            model = "pooling")

coeftest(pool,
         vcov = vcovDC(pool,
                       type = "HC1"))

export_summs(fe_twoway)
export_summs(pool)

# Compare the two-way fixed to the pooled
pFtest(fe_twoway, pool)


# ----------------------------------------

# Quadratic model - All Grades
model_quad_all <- plm(
  enrollment_all ~ ela_met + I(ela_met^2),
  data = pdata_allgrades,
  model = "within",
  effect = "twoways"
)
export_summs(model_quad_all)

# Quadratic model - Weighted
model_quad_wtd <- plm(
  enrollment_all ~ ela_score + I(ela_score^2),
  data = pdata_weighted,
  model = "within",
  effect = "twoways"
)
export_summs(model_quad_wtd)

## Is ela_li_met highly colinear with ela_met?
## If so, it should NOT be included.
cor(dfCapWeighted$ela_score, dfCapWeighted$ela_li_ratio,
    use = "complete.obs")
# Returns 0.759, so likely highly co-linear
library(car)
ols_model <- lm(
  enrollment_all ~ ela_score + ela_li_ratio,
  data = dfCapWeighted
)
vif(ols_model)


model_quad_wtd <- plm(
  enrollment_all ~ ela_score + I(ela_score^2) + ela_li_ratio + ela_female_ratio,
  data = pdata_weighted,
  model = "within",
  effect = "twoways"
)
export_summs(model_quad_wtd)

vif(model_quad_wtd)

# -------------------------------------------------
## Create 4 two-ways fixed effects models.

# Create a pdata.frame with entity and time indexes
pdata_allgrades <- pdata.frame(dfCapAllGrades, 
                               index = c("district_code", "year"))
pdata_lastgrade <- pdata.frame(dfCapLastGrade, 
                               index = c("district_code", "year"))
pdata_last2grades <- pdata.frame(dfCapLast2Grades, 
                               index = c("district_code", "year"))
pdata_weighted <- pdata.frame(dfCapWeighted, 
                              index = c("district_code", "year"))

# Create 4 models
model_allgrades <- plm(
  enrollment_all ~ ela_met + I(ela_met^2),
  data = pdata_allgrades,
  model = "within",
  effect = "twoways"
)

model_lastgrade <- plm(
  enrollment_all ~ ela_met + I(ela_met^2),
  data = pdata_lastgrade,
  model = "within",
  effect = "twoways"
)

model_last2grades <- plm(
  enrollment_all ~ ela_met + I(ela_met^2),
  data = pdata_last2grades,
  model = "within",
  effect = "twoways"
)

model_weighted <- plm(
  enrollment_all ~ ela_score + I(ela_score^2),
  data = pdata_weighted,
  model = "within",
  effect = "twoways"
)

export_summs(model_allgrades,
             model_lastgrade,
             model_last2grades,
             model_weighted)
