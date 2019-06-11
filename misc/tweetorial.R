# Updated 'tab' package for creating statistical tables in R, and figured I'd do a tweetorial. Main features: auto-formats factors variables and higher-order terms; works on random samples and complex surveys; works on GLM's, GEE's, and Cox PH's.

# Here's a basic Table 1.
tabmulti(Age + Sex + Race + BMI ~ Group, data = tabdata) %>% kable()

# Here's a GLM (tabglm recognizes it as a logistic regression)
fit <- glm(death_1yr ~ Age + Sex + Race + Group, data = tabdata, family = "binomial")
fit %>% tabglm() %>% kable()

# You can add variable labels if you like
fit %>% tabglm(xvarlabels = list(Age = "Age (years)", Race = "Race/ethnicity")) %>% kable()

# Here's a Cox model, with p-values removed for submitting to enlightened journals
fit <- coxph(Surv(time, delta) ~ Age + Sex + Race + Group, data = tabdata)
fit %>% tabcoxph(columns = c("beta.se", "hr.ci")) %>% kable()

# Here's a survey-weighted linear regression with some higher-order terms. Data is from NHANES 2003-2004 (adults age 18-39)
design <- svydesign(
  data = tabsvydata,
  ids = ~sdmvpsu,
  strata = ~sdmvstra,
  weights = ~wtmec2yr,
  nest = TRUE
)
design <- subset(design, Age >= 18 & Age <= 39)
fit <- svyglm(BMI ~ poly(Age, 2, raw = TRUE) + Sex + Race + Sex*Race, design = design)
fit %>% tabglm() %>% kable()




# Here's a Cox proportional hazards model, with some higher-order terms that get formatted neatly. Data is for adults age 18-29 in NHANES 2003-2004.



# Here's a complex survey Cox PH, with p-values to please enlightened journal editors
design <- svydesign(
  data = tabsvydata,
  ids = ~sdmvpsu,
  strata = ~sdmvstra,
  weights = ~wtmec2yr,
  nest = TRUE
)
fit <- svycoxph(Surv(time, event) ~ Age + Sex + Race, design = design)
fit %>% tabcoxph(columns = c("beta.se", "hr.ci")) %>% kable()
