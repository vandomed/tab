Summary Tables with ‘tab’
================
Dane Van Domelen <br> <vandomed@gmail.com>
2020-03-28

<!-- README.md is generated from README.Rmd. Please edit that file -->

## Installation

You can install and load **tab** from GitHub via the following code:

``` r
devtools::install_github("vandomed/tab")
library("tab")
```

## Functions

The main purpose of **tab** is to create neatly formatted summary tables
for papers and presentations. The following functions are included:

  - `glm_v` prints a GLM summary table to the RStudio Viewer
  - `tabglm` summarizes generalized linear models (GLM’s) fit via `glm`
    or `survey::svyglm`
  - `tabgee` summarizes generalized estimating equation models (GEE’s)
    fit via `gee::gee`
  - `tabcoxph` summarizes Cox Proportional Hazards models fit via
    `survival::coxph` or `survey::svycoxph`
  - `tabmulti` compares variables across two or more groups, e.g. to
    create a “Table 1”
  - `tabmulti.svy` does the same thing as `tabmulti` but for complex
    survey data

## Regression summaries with just 2 extra keystrokes

To summarize a fitted generalized linear model, simply call `glm_v` as
you would `glm`. The result will be a formatted summary table printed to
the RStudio Viewer. Here’s an example for logistic regression:

``` r
glm_v(
  death_1yr ~ poly(Age, 2, raw = TRUE) + Sex * BMI, 
  data = tabdata, 
  family = binomial
)
```

![Figure](vignettes/logistic.PNG)

From here, you can “snip” the summary table and save it as a figure (as
I did for this README) or copy directly from the Viewer and paste
outside of R.

For more flexibility, see `tabglm`. That function lets you control
things like what columns to present, how categorical predictors are
presented, and so on.

## Summary tables for continuous and categorical variables

You can use `tabmulti` to summarize variables across two or more groups,
using a formula interface. Here’s an example:

``` r
tabmulti(Age + Sex + Race + BMI ~ Group, data = tabdata)
```

![Figure](vignettes/tabmulti.PNG)

## Compatibility with Markdown/Knitr

The functions all return `kable` objects, so they should work perfectly
well in R Markdown and knitr documents.

<!-- ## Exporting tables, e.g. to Word -->

<!-- All of the functions in **tab** have an argument called `print.html` which can  -->

<!-- be used to export tables to word processors. Setting `print.html = TRUE` will  -->

<!-- result in a HTML table being output to your current working directory. You can  -->

<!-- open the table (e.g. in Chrome) and copy/paste into your report. -->

<!-- ## Options for printing in R -->

<!-- I used **knitr**'s `kable` function for the examples here, but other approaches  -->

<!-- should also work (e.g. **xtable**'s `xtable` or **pandoc**'s `pandoc.table`). -->

## References

<div id="refs" class="references">

<div id="ref-knitr3">

Xie, Yihui. 2014. “Knitr: A Comprehensive Tool for Reproducible Research
in R.” In *Implementing Reproducible Computational Research*, edited by
Victoria Stodden, Friedrich Leisch, and Roger D. Peng. Chapman;
Hall/CRC. <http://www.crcpress.com/product/isbn/9781466561595>.

</div>

<div id="ref-knitr2">

———. 2015. *Dynamic Documents with R and Knitr*. 2nd ed. Boca Raton,
Florida: Chapman; Hall/CRC. <https://yihui.name/knitr/>.

</div>

<div id="ref-knitr1">

———. 2018. *Knitr: A General-Purpose Package for Dynamic Report
Generation in R*. <https://yihui.name/knitr/>.

</div>

</div>
