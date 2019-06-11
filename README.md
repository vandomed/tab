Summary Tables with 'tab'
================
Dane Van Domelen <br> <vandomed@gmail.com>
2019-06-11

<!-- README.md is generated from README.Rmd. Please edit that file -->
Installation
------------

You can install and load **tab** from GitHub via the following code:

``` r
devtools::install_github("vandomed/tab")
library("tab")
```

Functions
---------

The purpose of **tab** is to make it easier to create tables for papers, including Table 1's showing characteristics of the sample and summary tables for fitted regression models. Currently, the following functions are included:

-   `tabmeans` compares means in two or more groups.
-   `tabmedians` compares medians in two or more groups.
-   `tabfreq` compares frequencies in two or more groups.
-   `tabmulti` compares multiple variables in two or more groups.
-   `tabmeans.svy`, `tabmedians.svy`, `tabfreq.svy`, and `tabmulti.svy` serve the same purposes as the above functions, but for complex survey data.
-   `tabglm` summarizes generalized linear models (GLM's) fit via `glm` or `survey::svyglm`.
-   `tabgee` summarizes generalized estimating equation models (GEE's) fit via `gee::gee`.
-   `tabcoxph` summarizes Cox Proportional Hazards models fit via `survival::coxph` or `survey::svycoxph`.

Table 1's
---------

<!-- A toy dataset called `tabdata` is included in **tab** package. It is a data frame with 15 variables and 300 observations. Let's take a look: -->
<!-- ```{r toydata} -->
<!-- library("tab") -->
<!-- head(tabdata) -->
<!-- ``` -->
You can use `tabmulti` to compare characteristics across levels of a factor variable, e.g. here comparing age, sex, and race by treatment group in the toy dataset `tabdata`.

``` r
tabmulti(Age + Sex + Race ~ Group, data = tabdata) %>% kable()
```

| Variable             | Control    | Treatment  | P         |
|:---------------------|:-----------|:-----------|:----------|
| Age, M (SD)          | 70.5 (5.3) | 69.5 (5.9) | 0.15      |
| Sex, n (%)           |            |            | &lt;0.001 |
|     Female           | 93 (68.4)  | 62 (38.5)  |           |
|     Male             | 43 (31.6)  | 99 (61.5)  |           |
| Race, n (%)          |            |            | 0.29      |
|     White            | 46 (34.1)  | 65 (39.6)  |           |
|     Black            | 36 (26.7)  | 52 (31.7)  |           |
|     Mexican American | 21 (15.6)  | 19 (11.6)  |           |
|     Other            | 32 (23.7)  | 28 (17.1)  |           |

To illustrate some options, we can request `Age` and `Race` to print as `Age (years)` and `Race/ethnicity`, compare medians rather than means for age, and include the sample sizes in the column headings:

``` r
tabmulti(Age + Sex + Race ~ Group, data = tabdata, 
         yvarlabels = list(Age = "Age (years)", Race = "Race/ethnicity"), 
         ymeasures = c("median", "freq", "freq"), 
         listwise.deletion = TRUE, 
         n.headings = TRUE) %>% kable()
```

| Variable                  | Control (n = 134) | Treatment (n = 158) | P         |
|:--------------------------|:------------------|:--------------------|:----------|
| Age (years), Median (IQR) | 70.0 (9.8)        | 69.0 (11.0)         | 0.19      |
| Sex, n (%)                |                   |                     | &lt;0.001 |
|     Female                | 92 (68.7)         | 60 (38.0)           |           |
|     Male                  | 42 (31.3)         | 98 (62.0)           |           |
| Race/ethnicity, n (%)     |                   |                     | 0.26      |
|     White                 | 46 (34.3)         | 64 (40.5)           |           |
|     Black                 | 36 (26.9)         | 50 (31.6)           |           |
|     Mexican American      | 21 (15.7)         | 17 (10.8)           |           |
|     Other                 | 31 (23.1)         | 27 (17.1)           |           |

Regression tables
-----------------

#### GLM's

Logistic regression for 1-year mortality vs. age, sex, and treatment, with the binary factor variables displayed in a "compressed" format:

``` r
fit <- glm(death_1yr ~ Age + Sex + Group, data = tabdata, family = binomial)
fit %>% tabglm(factor.compression = 5) %>% kable()
```

| Variable  | Beta (SE)    | OR (95% CI)       | P    |
|:----------|:-------------|:------------------|:-----|
| Intercept | -2.02 (1.76) | -                 | 0.25 |
| Age       | 0.02 (0.02)  | 1.02 (0.97, 1.07) | 0.50 |
| Male      | 0.11 (0.29)  | 1.12 (0.63, 1.97) | 0.70 |
| Treatment | -0.04 (0.29) | 0.96 (0.54, 1.69) | 0.88 |

#### GEE's

GEE for high blood pressure (measured at 3 time points longitudinally) vs. various predictors, with some higher-order terms:

``` r
tabdata2 <- reshape(data = tabdata,
                    varying = c("bp.1", "bp.2", "bp.3", "highbp.1", "highbp.2", "highbp.3"),
                    timevar = "bp.visit", direction = "long")
tabdata2 <- tabdata2[order(tabdata2$id), ]
fit <- gee(highbp ~ poly(Age, 2, raw = TRUE) + Sex + Race + Race*Sex,
           id = id, data = tabdata2, family = "binomial", corstr = "unstructured")
fit %>% tabgee(data = tabdata2) %>% kable()
```

| Variable                  | Beta (SE)     | OR (95% CI)       | P    |
|:--------------------------|:--------------|:------------------|:-----|
| Intercept                 | -3.10 (14.84) | -                 | 0.83 |
| Age                       | 0.06 (0.43)   | 1.06 (0.46, 2.45) | 0.89 |
| Age squared               | -0.00 (0.00)  | 1.00 (0.99, 1.01) | 0.88 |
| Sex                       |               |                   |      |
|    Female (ref)           | -             | -                 | -    |
|    Male                   | 0.48 (0.29)   | 1.61 (0.91, 2.84) | 0.10 |
| Race                      |               |                   |      |
|    White (ref)            | -             | -                 | -    |
|    Black                  | 0.04 (0.32)   | 1.04 (0.56, 1.95) | 0.90 |
|    Mexican American       | 0.13 (0.38)   | 1.14 (0.55, 2.39) | 0.72 |
|    Other                  | -0.83 (0.37)  | 0.43 (0.21, 0.89) | 0.02 |
| Sex by Race               |               |                   |      |
|    Male, Black            | 0.23 (0.42)   | 1.26 (0.55, 2.87) | 0.58 |
|    Male, Mexican American | 0.27 (0.54)   | 1.31 (0.46, 3.75) | 0.61 |
|    Male, Other            | 1.11 (0.51)   | 3.05 (1.12, 8.25) | 0.03 |

Note that we had to set `data = tabdata2` here, because `gee` objects don't store all of the information on factor variables (unlike `glm` objects).

#### Cox proportional hazards

Survival model for mortality vs. predictors, again compressing the factor variables, and requesting slightly differnet columns (i.e. no p-values):

``` r
library("survival")
fit <- coxph(Surv(time = time, event = delta) ~ Age + Sex + Group, data = tabdata)
fit %>% tabcoxph(factor.compression = 5, columns = c("beta", "hr.ci")) %>% kable()
```

| Variable  | Beta  | HR (95% CI)       |
|:----------|:------|:------------------|
| Age       | 0.03  | 1.03 (1.00, 1.06) |
| Male      | 0.01  | 1.01 (0.74, 1.39) |
| Treatment | -0.05 | 0.95 (0.69, 1.30) |

Complex survey data
-------------------

The functions in **tab** can also accommodate complex survey data. To illustrate with the included dataset `tabsvydata` (which is data from NHANES 2003-2004, except for the made-up variables `time` and `event`), here's a Table 1:

``` r
library("survey")
design <- svydesign(
  data = tabsvydata,
  ids = ~sdmvpsu,
  strata = ~sdmvstra,
  weights = ~wtmec2yr,
  nest = TRUE
)
tabmulti.svy(Age + Race + BMI ~ Sex, design = design) %>% kable()
```

| Variable               | Female      | Male        | P         |
|:-----------------------|:------------|:------------|:----------|
| Age, M (SD)            | 37.0 (22.5) | 34.8 (21.7) | &lt;0.001 |
| Race, % (SE)           |             |             | 0.08      |
|     Non-Hispanic White | 69.7 (3.7)  | 69.6 (3.8)  |           |
|     Non-Hispanic Black | 13.2 (2.0)  | 11.9 (1.9)  |           |
|     Mexican American   | 8.6 (2.1)   | 9.8 (2.2)   |           |
|     Other              | 8.4 (1.0)   | 8.8 (1.3)   |           |
| BMI, M (SD)            | 26.4 (7.5)  | 26.0 (6.4)  | 0.11      |

And here's a linear regression:

``` r
fit <- svyglm(BMI ~ Age + Sex + Race, design = design)
fit %>% tabglm(factor.compression = 3) %>% kable()
```

| Variable                 | Beta (SE)    | 95% CI         | P         |
|:-------------------------|:-------------|:---------------|:----------|
| Intercept                | 20.95 (0.34) | (20.27, 21.62) | &lt;0.001 |
| Age                      | 0.14 (0.00)  | (0.13, 0.15)   | &lt;0.001 |
| Female (ref)             | -            | -              | -         |
| Male                     | -0.07 (0.23) | (-0.51, 0.37)  | 0.76      |
| Non-Hispanic White (ref) | -            | -              | -         |
| Non-Hispanic Black       | 1.91 (0.23)  | (1.46, 2.35)   | &lt;0.001 |
| Mexican American         | 1.06 (0.30)  | (0.47, 1.66)   | 0.006     |
| Other                    | -1.09 (0.33) | (-1.73, -0.45) | 0.007     |

Exporting tables, e.g. to Word
------------------------------

All of the functions in **tab** have an argument called `print.html` which can be used to export tables to word processors. Setting `print.html = TRUE` will result in a HTML table being output to your current working directory. You can open the table (e.g. in Chrome) and copy/paste into your report.

Options for printing in R
-------------------------

I used **knitr**'s `kable` function for the examples here, but other approaches should also work (e.g. **xtable**'s `xtable` or **pandoc**'s `pandoc.table`).

References
----------

Lumley, Thomas. 2019. *Survey: Analysis of Complex Survey Samples*. <https://CRAN.R-project.org/package=survey>.

Lumley, Thomas, and others. 2004. “Analysis of Complex Survey Samples.” *Journal of Statistical Software* 9 (1): 1–19.

R by Thomas Lumley, Vincent J Carey. Ported to, and Brian Ripley. Note that maintainers are not available to give advice on using a package they did not author. 2015. *Gee: Generalized Estimation Equation Solver*. <https://CRAN.R-project.org/package=gee>.

Terry M. Therneau, and Patricia M. Grambsch. 2000. *Modeling Survival Data: Extending the Cox Model*. New York: Springer.

Therneau, Terry M. 2015. *A Package for Survival Analysis in S*. <https://CRAN.R-project.org/package=survival>.

Xie, Yihui. 2014. “Knitr: A Comprehensive Tool for Reproducible Research in R.” In *Implementing Reproducible Computational Research*, edited by Victoria Stodden, Friedrich Leisch, and Roger D. Peng. Chapman; Hall/CRC. <http://www.crcpress.com/product/isbn/9781466561595>.

———. 2015. *Dynamic Documents with R and Knitr*. 2nd ed. Boca Raton, Florida: Chapman; Hall/CRC. <https://yihui.name/knitr/>.

———. 2018. *Knitr: A General-Purpose Package for Dynamic Report Generation in R*. <https://yihui.name/knitr/>.
