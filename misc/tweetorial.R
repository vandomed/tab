# Released 'tab' v. 4.1.1 for creating statistical tables in R. It's not the first package for summary tables but it is the best (jk jk). Figured I'd drop a #tweetorial #rstats #epitwitter (1/11)

# Notable features relative to similar R packages:
# (1) Neatly formats factors, polynomial terms, and interactions
# (2) Compatible with random samples and complex surveys
# (3) Creates tables for fitted GLM's, GEE's, and Cox models. (2/11)

# To install from GitHub and load, run `devtools::install_github("vandomed/tab")` and then `library("tab")` (3/11)

# The package includes a toy dataset called `tabdata` with 300 observations (4/11)
head(tabdata)

# `tabmulti` compares characteristics across levels of a factor variable (5/11)
tabmulti(Age + Sex + Race + BMI ~ Group, data = tabdata) %>% kable()

# `tabglm` summarizes fitted GLM's (it recognizes this one is a logistic regression) (6/11)
fit <- glm(death_1yr ~ Age + Sex + Race + Group, data = tabdata, family = "binomial")
fit %>% tabglm() %>% kable()

# Variable labels can be modified like this: (7/11)
fit %>% tabglm(xvarlabels = list(Age = "Age (years)", Race = "Race/ethnicity")) %>% kable()

# `tabcoxph` summarizes fitted Cox models. P-values cause global warming so here I request just Beta (SE) and HR (95% CI). (8/11)
fit <- coxph(Surv(time, delta) ~ Age + Sex + Race + Group, data = tabdata)
fit %>% tabcoxph(columns = c("beta.se", "hr.ci")) %>% kable()

# `tabglm` and `tabcoxph` also work with fitted survey GLM's and Cox models, respectively. Here I summarize a survey-weighted linear regression with some higher-order terms, using NHANES 03-04 data on adults age 18-39. (9/11)
library("survey")
design <- svydesign(
  data = tabsvydata,
  ids = ~sdmvpsu,
  strata = ~sdmvstra,
  weights = ~wtmec2yr,
  nest = TRUE
)
fit <- svyglm(BMI ~ poly(Age, 2, raw = TRUE) + Sex + Race + Sex*Race,
              design = subset(design, Age %in% 18: 39))
fit %>% tabglm() %>% kable()

# Easiest way to export to Word is to set `print.html = TRUE`, and then open (e.g. in Chrome) and copy/paste from there. (10/11)
# Show picture of table in Chrome
fit %>% tabglm(print.html = TRUE)

# Please clap (gif?) (11/11)
