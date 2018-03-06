Summary Tables with 'tab'
================
Dane Van Domelen <br> <vandomed@gmail.com>
2018-03-06

<!-- README.md is generated from README.Rmd. Please edit that file -->
Scope
-----

The purpose of **tab** is to make it easier to create tables for papers, including Table 1's showing characteristics of the sample and summary tables for fitted regression models. Currently, the following functions are included:

-   *tabmeans* compares means in two or more groups.
-   *tabmedians* compares medians in two or more groups.
-   *tabfreq* compares frequencies in two or more groups.
-   *tabmulti* compares multiple variables in two or more groups.
-   *tabglm* summarizes a generalized linear model (GLM) fit via *glm*.
-   *tabgee* summarizes a generalized estimating equation (GEE) fit via *gee*.
-   *tabcox* summarizes a Cox Proportional Hazards (Cox PH) model fit via *coxph* in **survival**.

Creating a Table 1
------------------

A toy dataset called `tabdata` is included in the **tab** package. It is a data frame with 15 variables and 300 observations. Let's take a look:

``` r
library("tab")
data(tabdata)
dim(tabdata)
#> [1] 300  15
head(tabdata)
#>   ID     Group Age    Sex  Race  BMI time delta death_1yr  bp.1  bp.2
#> 1  1   Control  63 Female Black 28.6 1512     1         0 121.4 123.4
#> 2  2 Treatment  74 Female White 27.2 2987     1         0 145.5 168.9
#> 3  3 Treatment  70   Male Black 25.5 1468     0         0 138.4 134.6
#> 4  4 Treatment  78   Male Other 24.3  691     1         0 128.3 124.2
#> 5  5 Treatment  73   Male White 25.8  477     1         0 163.3 133.2
#> 6  6   Control  61 Female White 21.9 3380     1         0 137.6 130.3
#>    bp.3 highbp.1 highbp.2 highbp.3
#> 1 118.3        0        0        0
#> 2 149.5        1        1        1
#> 3 131.2        0        0        0
#> 4 131.1        0        0        0
#> 5 138.4        1        0        0
#> 6 141.5        0        0        1
```

Here is how you can use *tabmulti* to generate a Table 1 comparing characteristics of the treatment and control groups.

``` r
(table1 <- tabmulti(data = tabdata, 
                    xvarname = "Group", 
                    yvarnames = c("Age", "Sex", "Race")))
#>       Variable             Control      Treatment    P       
#>  [1,] "Age, M (SD)"        "70.5 (5.3)" "69.5 (5.9)" "0.15"  
#>  [2,] "Sex, n (%)"         ""           ""           "<0.001"
#>  [3,] "  Female"           "93 (68.4)"  "62 (38.5)"  ""      
#>  [4,] "  Male"             "43 (31.6)"  "99 (61.5)"  ""      
#>  [5,] "Race, n (%)"        ""           ""           "0.29"  
#>  [6,] "  White"            "46 (34.1)"  "65 (39.6)"  ""      
#>  [7,] "  Black"            "36 (26.7)"  "52 (31.7)"  ""      
#>  [8,] "  Mexican American" "21 (15.6)"  "19 (11.6)"  ""      
#>  [9,] "  Other"            "32 (23.7)"  "28 (17.1)"  ""
```

*tabmulti* created a character matrix, but it doesn't look like a table yet.

Exporting to Word
-----------------

If you want to get the table into Microsoft Word, here are two approaches:

1.  Install/load **Kmisc** and run `write.cb(table1)` to copy the table to your clipboard. Paste the result into Word, then highlight the text and go to `Insert -> Table -> Convert Text to Table... OK`.

2.  Add `print.html = TRUE` to the above *tabmulti* function call. That will result in a .html file being written to your working directory. It should appear as a neat table when you open it (e.g. in Google Chrome), and you can copy/paste it into Word.

If you want it to display it in a **knitr** document, you can add `latex = TRUE` and then use various approaches...

Printing in R
-------------

**printr**

I think the easiest approach is to simply load the **printr** package. Loading it results in R output printing more neatly, which includes character matrices showing up as neat tables.

``` r
library("printr")
(table1 <- tabmulti(data = tabdata, 
                    xvarname = "Group", 
                    yvarnames = c("Age", "Sex", "Race"), 
                    latex = TRUE))
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

``` r
detach("package:printr", unload = TRUE)
```

(I detached **printr** so R reverts to its usual output display format for the rest of the vignette.)

If you want to add table options, e.g. a caption or non-default column alignment, you can use `kable` from the **knitr** package (e.g. try `kable(table1, align = "lrrr", caption = "Table 1.", format = "html")`.

**knitr's kable function**

Another approach:

``` r
library("knitr")
kable(table1,
      caption = "Table 1a. Characteristics (created by tabmulti/kable).", 
      align = 'lrrr', 
      format = "html")
```

<table>
<caption>
Table 1a. Characteristics (created by tabmulti/kable).
</caption>
<thead>
<tr>
<th style="text-align:left;">
Variable
</th>
<th style="text-align:right;">
Control
</th>
<th style="text-align:right;">
Treatment
</th>
<th style="text-align:right;">
P
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
Age, M (SD)
</td>
<td style="text-align:right;">
70.5 (5.3)
</td>
<td style="text-align:right;">
69.5 (5.9)
</td>
<td style="text-align:right;">
0.15
</td>
</tr>
<tr>
<td style="text-align:left;">
Sex, n (%)
</td>
<td style="text-align:right;">
</td>
<td style="text-align:right;">
</td>
<td style="text-align:right;">
&lt;0.001
</td>
</tr>
<tr>
<td style="text-align:left;">
    Female
</td>
<td style="text-align:right;">
93 (68.4)
</td>
<td style="text-align:right;">
62 (38.5)
</td>
<td style="text-align:right;">
</td>
</tr>
<tr>
<td style="text-align:left;">
    Male
</td>
<td style="text-align:right;">
43 (31.6)
</td>
<td style="text-align:right;">
99 (61.5)
</td>
<td style="text-align:right;">
</td>
</tr>
<tr>
<td style="text-align:left;">
Race, n (%)
</td>
<td style="text-align:right;">
</td>
<td style="text-align:right;">
</td>
<td style="text-align:right;">
0.29
</td>
</tr>
<tr>
<td style="text-align:left;">
    White
</td>
<td style="text-align:right;">
46 (34.1)
</td>
<td style="text-align:right;">
65 (39.6)
</td>
<td style="text-align:right;">
</td>
</tr>
<tr>
<td style="text-align:left;">
    Black
</td>
<td style="text-align:right;">
36 (26.7)
</td>
<td style="text-align:right;">
52 (31.7)
</td>
<td style="text-align:right;">
</td>
</tr>
<tr>
<td style="text-align:left;">
    Mexican American
</td>
<td style="text-align:right;">
21 (15.6)
</td>
<td style="text-align:right;">
19 (11.6)
</td>
<td style="text-align:right;">
</td>
</tr>
<tr>
<td style="text-align:left;">
    Other
</td>
<td style="text-align:right;">
32 (23.7)
</td>
<td style="text-align:right;">
28 (17.1)
</td>
<td style="text-align:right;">
</td>
</tr>
</tbody>
</table>
<br>

**xtable's xtable function**

Another option is the **xtable** package/function (requires adding `results = "asis"` as a chunk option!):

``` r
library("xtable")
print(xtable(table1, 
             caption = "Table 1b. Characteristics (created by tabmulti/xtable).", 
             align = 'llrrr'), 
      type = "html", 
      include.rownames = FALSE)
```

<table border="1">
<caption align="top">
Table 1b. Characteristics (created by tabmulti/xtable).
</caption>
<tr>
<th>
Variable
</th>
<th>
Control
</th>
<th>
Treatment
</th>
<th>
P
</th>
</tr>
<tr>
<td>
Age, M (SD)
</td>
<td align="right">
70.5 (5.3)
</td>
<td align="right">
69.5 (5.9)
</td>
<td align="right">
0.15
</td>
</tr>
<tr>
<td>
Sex, n (%)
</td>
<td align="right">
</td>
<td align="right">
</td>
<td align="right">
&lt;0.001
</td>
</tr>
<tr>
<td>
    Female
</td>
<td align="right">
93 (68.4)
</td>
<td align="right">
62 (38.5)
</td>
<td align="right">
</td>
</tr>
<tr>
<td>
    Male
</td>
<td align="right">
43 (31.6)
</td>
<td align="right">
99 (61.5)
</td>
<td align="right">
</td>
</tr>
<tr>
<td>
Race, n (%)
</td>
<td align="right">
</td>
<td align="right">
</td>
<td align="right">
0.29
</td>
</tr>
<tr>
<td>
    White
</td>
<td align="right">
46 (34.1)
</td>
<td align="right">
65 (39.6)
</td>
<td align="right">
</td>
</tr>
<tr>
<td>
    Black
</td>
<td align="right">
36 (26.7)
</td>
<td align="right">
52 (31.7)
</td>
<td align="right">
</td>
</tr>
<tr>
<td>
    Mexican American
</td>
<td align="right">
21 (15.6)
</td>
<td align="right">
19 (11.6)
</td>
<td align="right">
</td>
</tr>
<tr>
<td>
    Other
</td>
<td align="right">
32 (23.7)
</td>
<td align="right">
28 (17.1)
</td>
<td align="right">
</td>
</tr>
</table>
**pander's *pandoc.table* function**

And finally the *pandoc.table* function in **pander** (also requires `results = "asis"`):

``` r
library("pander")
pandoc.table(table1, 
             caption = "Table 1c. Characteristics (created by tabmulti/pandoc.table).", 
             style = "rmarkdown", 
             justify = 'lrrr', 
             split.tables = Inf)
```

| Variable             |     Control|   Treatment|          P|
|:---------------------|-----------:|-----------:|----------:|
| Age, M (SD)          |  70.5 (5.3)|  69.5 (5.9)|       0.15|
| Sex, n (%)           |            |            |  &lt;0.001|
|     Female           |   93 (68.4)|   62 (38.5)|           |
|     Male             |   43 (31.6)|   99 (61.5)|           |
| Race, n (%)          |            |            |       0.29|
|     White            |   46 (34.1)|   65 (39.6)|           |
|     Black            |   36 (26.7)|   52 (31.7)|           |
|     Mexican American |   21 (15.6)|   19 (11.6)|           |
|     Other            |   32 (23.7)|   28 (17.1)|           |

More on *tabmulti*
------------------

Recall the *tabmulti* function call from above:

``` r
table1 <- tabmulti(data = tabdata, 
                   xvarname = "Group", 
                   yvarnames = c("Age", "Sex", "Race"), 
                   latex = TRUE)
```

I specified the data frame, the name of the group variable, and the names of the variables I wanted to compare. By default, *tabmulti* treats each Y variable as continuous if it is numeric and takes on 5 or more unique values, and categorical otherwise. It compares means for continuous variables and frequencies for categorical variables.

Internally, *tabmulti* called *tabmeans* for the first comparison and *tabfreq* for the second and third. We could have created the same table using these functions and *rbind*:

``` r
table1b <- rbind(tabmeans(x = tabdata$Group, y = tabdata$Age, latex = TRUE), 
                 tabfreq(x = tabdata$Group, y = tabdata$Sex, latex = TRUE), 
                 tabfreq(x = tabdata$Group, y = tabdata$Race, latex = TRUE))
all(table1 == table1b)
#> [1] TRUE
```

Let's go through some more options. The `columns` input controls what columns are shown, with the default `columns = c("xgroups", "p")` requesting a column for each `x` level and the p-value (from t-test or ANOVA). Since we have missing values and *tabmulti* uses pairwise deletion by default, let's add a sample size column, and why not also throw in a column for the overall sample statistics.

``` r
table1 <- tabmulti(data = tabdata, 
                   xvarname = "Group", 
                   yvarnames = c("Age", "Sex", "Race"),
                   columns = c("n", "overall", "xgroups", "p"),
                   latex = TRUE)
kable(table1, 
      caption = "Table 1d. Characteristics of sample.", 
      align = 'lrrrrr', 
      format = "html")
```

<table>
<caption>
Table 1d. Characteristics of sample.
</caption>
<thead>
<tr>
<th style="text-align:left;">
Variable
</th>
<th style="text-align:right;">
N
</th>
<th style="text-align:right;">
Overall
</th>
<th style="text-align:right;">
Control
</th>
<th style="text-align:right;">
Treatment
</th>
<th style="text-align:right;">
P
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
Age, M (SD)
</td>
<td style="text-align:right;">
296
</td>
<td style="text-align:right;">
69.9 (5.7)
</td>
<td style="text-align:right;">
70.5 (5.3)
</td>
<td style="text-align:right;">
69.5 (5.9)
</td>
<td style="text-align:right;">
0.15
</td>
</tr>
<tr>
<td style="text-align:left;">
Sex, n (%)
</td>
<td style="text-align:right;">
297
</td>
<td style="text-align:right;">
</td>
<td style="text-align:right;">
</td>
<td style="text-align:right;">
</td>
<td style="text-align:right;">
&lt;0.001
</td>
</tr>
<tr>
<td style="text-align:left;">
    Female
</td>
<td style="text-align:right;">
</td>
<td style="text-align:right;">
155 (52.2)
</td>
<td style="text-align:right;">
93 (68.4)
</td>
<td style="text-align:right;">
62 (38.5)
</td>
<td style="text-align:right;">
</td>
</tr>
<tr>
<td style="text-align:left;">
    Male
</td>
<td style="text-align:right;">
</td>
<td style="text-align:right;">
142 (47.8)
</td>
<td style="text-align:right;">
43 (31.6)
</td>
<td style="text-align:right;">
99 (61.5)
</td>
<td style="text-align:right;">
</td>
</tr>
<tr>
<td style="text-align:left;">
Race, n (%)
</td>
<td style="text-align:right;">
299
</td>
<td style="text-align:right;">
</td>
<td style="text-align:right;">
</td>
<td style="text-align:right;">
</td>
<td style="text-align:right;">
0.29
</td>
</tr>
<tr>
<td style="text-align:left;">
    White
</td>
<td style="text-align:right;">
</td>
<td style="text-align:right;">
111 (37.1)
</td>
<td style="text-align:right;">
46 (34.1)
</td>
<td style="text-align:right;">
65 (39.6)
</td>
<td style="text-align:right;">
</td>
</tr>
<tr>
<td style="text-align:left;">
    Black
</td>
<td style="text-align:right;">
</td>
<td style="text-align:right;">
88 (29.4)
</td>
<td style="text-align:right;">
36 (26.7)
</td>
<td style="text-align:right;">
52 (31.7)
</td>
<td style="text-align:right;">
</td>
</tr>
<tr>
<td style="text-align:left;">
    Mexican American
</td>
<td style="text-align:right;">
</td>
<td style="text-align:right;">
40 (13.4)
</td>
<td style="text-align:right;">
21 (15.6)
</td>
<td style="text-align:right;">
19 (11.6)
</td>
<td style="text-align:right;">
</td>
</tr>
<tr>
<td style="text-align:left;">
    Other
</td>
<td style="text-align:right;">
</td>
<td style="text-align:right;">
60 (20.1)
</td>
<td style="text-align:right;">
32 (23.7)
</td>
<td style="text-align:right;">
28 (17.1)
</td>
<td style="text-align:right;">
</td>
</tr>
</tbody>
</table>
For age, often the range is more informative than the SD. We can display M (min-max) rather than M (SD) but setting the *tabmeans* input `parenth = "sd"`. To pass this argument through *tabmulti*, we use the `means.list` argument:

``` r
table1 <- tabmulti(data = tabdata, 
                   xvarname = "Group", 
                   yvarnames = c("Age", "Sex", "Race"),
                   columns = c("n", "overall", "xgroups", "p"),
                   means.list = list(parenth = "minmax"),
                   latex = TRUE)
kable(table1, 
      caption = "Table 1e. Characteristics of sample.", 
      align = 'lrrrrr', 
      format = "html")
```

<table>
<caption>
Table 1e. Characteristics of sample.
</caption>
<thead>
<tr>
<th style="text-align:left;">
Variable
</th>
<th style="text-align:right;">
N
</th>
<th style="text-align:right;">
Overall
</th>
<th style="text-align:right;">
Control
</th>
<th style="text-align:right;">
Treatment
</th>
<th style="text-align:right;">
P
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
Age, M (min, max)
</td>
<td style="text-align:right;">
296
</td>
<td style="text-align:right;">
69.9 (60, 80)
</td>
<td style="text-align:right;">
70.5 (60, 79)
</td>
<td style="text-align:right;">
69.5 (60, 80)
</td>
<td style="text-align:right;">
0.15
</td>
</tr>
<tr>
<td style="text-align:left;">
Sex, n (%)
</td>
<td style="text-align:right;">
297
</td>
<td style="text-align:right;">
</td>
<td style="text-align:right;">
</td>
<td style="text-align:right;">
</td>
<td style="text-align:right;">
&lt;0.001
</td>
</tr>
<tr>
<td style="text-align:left;">
    Female
</td>
<td style="text-align:right;">
</td>
<td style="text-align:right;">
155 (52.2)
</td>
<td style="text-align:right;">
93 (68.4)
</td>
<td style="text-align:right;">
62 (38.5)
</td>
<td style="text-align:right;">
</td>
</tr>
<tr>
<td style="text-align:left;">
    Male
</td>
<td style="text-align:right;">
</td>
<td style="text-align:right;">
142 (47.8)
</td>
<td style="text-align:right;">
43 (31.6)
</td>
<td style="text-align:right;">
99 (61.5)
</td>
<td style="text-align:right;">
</td>
</tr>
<tr>
<td style="text-align:left;">
Race, n (%)
</td>
<td style="text-align:right;">
299
</td>
<td style="text-align:right;">
</td>
<td style="text-align:right;">
</td>
<td style="text-align:right;">
</td>
<td style="text-align:right;">
0.29
</td>
</tr>
<tr>
<td style="text-align:left;">
    White
</td>
<td style="text-align:right;">
</td>
<td style="text-align:right;">
111 (37.1)
</td>
<td style="text-align:right;">
46 (34.1)
</td>
<td style="text-align:right;">
65 (39.6)
</td>
<td style="text-align:right;">
</td>
</tr>
<tr>
<td style="text-align:left;">
    Black
</td>
<td style="text-align:right;">
</td>
<td style="text-align:right;">
88 (29.4)
</td>
<td style="text-align:right;">
36 (26.7)
</td>
<td style="text-align:right;">
52 (31.7)
</td>
<td style="text-align:right;">
</td>
</tr>
<tr>
<td style="text-align:left;">
    Mexican American
</td>
<td style="text-align:right;">
</td>
<td style="text-align:right;">
40 (13.4)
</td>
<td style="text-align:right;">
21 (15.6)
</td>
<td style="text-align:right;">
19 (11.6)
</td>
<td style="text-align:right;">
</td>
</tr>
<tr>
<td style="text-align:left;">
    Other
</td>
<td style="text-align:right;">
</td>
<td style="text-align:right;">
60 (20.1)
</td>
<td style="text-align:right;">
32 (23.7)
</td>
<td style="text-align:right;">
28 (17.1)
</td>
<td style="text-align:right;">
</td>
</tr>
</tbody>
</table>
Technically the range is the difference between the min and the max, not the min and the max, but if you prefer the label `"M (range)"`, you could specify the `text.label` input: `means.list = list(parenth = "minmax", text.label = "M (range)")`.

These are some of the options you have access to with *tabmulti* and the underlying functions it calls. A complete list of options are described in the help files for *tabmulti*, *tabmeans*, *tabmedians*, and *tabfreq*. The help files also have some different examples.

Regression summaries
--------------------

### Linear regression

Suppose we want to summarize a linear regression of BMI on age, sex, race, and treatment group. You could use *kable*, *xtable*, or *pandoc.table* to print a summary table like this:

``` r
fit <- glm(BMI ~ Age + Sex + Race + Group, data = tabdata)
print(xtable(fit, 
             caption = "Table 2a. Linear regression fit (created by xtable)."), 
      type = "html")
```

<table border="1">
<caption align="top">
Table 2a. Linear regression fit (created by xtable).
</caption>
<tr>
<th>
</th>
<th>
Estimate
</th>
<th>
Std. Error
</th>
<th>
t value
</th>
<th>
Pr(&gt;|t|)
</th>
</tr>
<tr>
<td align="right">
(Intercept)
</td>
<td align="right">
22.1009
</td>
<td align="right">
1.6340
</td>
<td align="right">
13.53
</td>
<td align="right">
0.0000
</td>
</tr>
<tr>
<td align="right">
Age
</td>
<td align="right">
0.0102
</td>
<td align="right">
0.0231
</td>
<td align="right">
0.44
</td>
<td align="right">
0.6591
</td>
</tr>
<tr>
<td align="right">
SexMale
</td>
<td align="right">
-0.1800
</td>
<td align="right">
0.2723
</td>
<td align="right">
-0.66
</td>
<td align="right">
0.5090
</td>
</tr>
<tr>
<td align="right">
RaceBlack
</td>
<td align="right">
0.3362
</td>
<td align="right">
0.3183
</td>
<td align="right">
1.06
</td>
<td align="right">
0.2918
</td>
</tr>
<tr>
<td align="right">
RaceMexican American
</td>
<td align="right">
0.6283
</td>
<td align="right">
0.4204
</td>
<td align="right">
1.49
</td>
<td align="right">
0.1361
</td>
</tr>
<tr>
<td align="right">
RaceOther
</td>
<td align="right">
0.0204
</td>
<td align="right">
0.3613
</td>
<td align="right">
0.06
</td>
<td align="right">
0.9550
</td>
</tr>
<tr>
<td align="right">
GroupTreatment
</td>
<td align="right">
3.0966
</td>
<td align="right">
0.2755
</td>
<td align="right">
11.24
</td>
<td align="right">
0.0000
</td>
</tr>
</table>
But this isn't how a regression table in a paper typically looks. A few issues:

-   P-value column should have a simpler heading.
-   P-values should not print as 0.
-   For factor variables, levels should be printed more cleanly.

Let's try *tabglm*:

``` r
table2 <- tabglm(fit = fit, 
                 latex = TRUE)
kable(table2, 
      caption = "Table 2b. Linear regression fit (created by tabglm/kable).", 
      align = 'lrr', 
      format = "html")
```

<table>
<caption>
Table 2b. Linear regression fit (created by tabglm/kable).
</caption>
<thead>
<tr>
<th style="text-align:left;">
Variable
</th>
<th style="text-align:right;">
Beta (SE)
</th>
<th style="text-align:right;">
P
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
Intercept
</td>
<td style="text-align:right;">
22.10 (1.63)
</td>
<td style="text-align:right;">
&lt;0.001
</td>
</tr>
<tr>
<td style="text-align:left;">
Age
</td>
<td style="text-align:right;">
0.01 (0.02)
</td>
<td style="text-align:right;">
0.66
</td>
</tr>
<tr>
<td style="text-align:left;">
Sex
</td>
<td style="text-align:right;">
</td>
<td style="text-align:right;">
</td>
</tr>
<tr>
<td style="text-align:left;">
    Female (ref)
</td>
<td style="text-align:right;">
--
</td>
<td style="text-align:right;">
--
</td>
</tr>
<tr>
<td style="text-align:left;">
    Male
</td>
<td style="text-align:right;">
-0.18 (0.27)
</td>
<td style="text-align:right;">
0.51
</td>
</tr>
<tr>
<td style="text-align:left;">
Race
</td>
<td style="text-align:right;">
</td>
<td style="text-align:right;">
</td>
</tr>
<tr>
<td style="text-align:left;">
    White (ref)
</td>
<td style="text-align:right;">
--
</td>
<td style="text-align:right;">
--
</td>
</tr>
<tr>
<td style="text-align:left;">
    Black
</td>
<td style="text-align:right;">
0.34 (0.32)
</td>
<td style="text-align:right;">
0.29
</td>
</tr>
<tr>
<td style="text-align:left;">
    Mexican American
</td>
<td style="text-align:right;">
0.63 (0.42)
</td>
<td style="text-align:right;">
0.14
</td>
</tr>
<tr>
<td style="text-align:left;">
    Other
</td>
<td style="text-align:right;">
0.02 (0.36)
</td>
<td style="text-align:right;">
0.95
</td>
</tr>
<tr>
<td style="text-align:left;">
Group
</td>
<td style="text-align:right;">
</td>
<td style="text-align:right;">
</td>
</tr>
<tr>
<td style="text-align:left;">
    Control (ref)
</td>
<td style="text-align:right;">
--
</td>
<td style="text-align:right;">
--
</td>
</tr>
<tr>
<td style="text-align:left;">
    Treatment
</td>
<td style="text-align:right;">
3.10 (0.28)
</td>
<td style="text-align:right;">
&lt;0.001
</td>
</tr>
</tbody>
</table>
If you don't like all the white space, you can set `compress.factors = TRUE` to omit rows with factor variable names (and left-align factor levels). By default, the input `omit.refgroups` has the same value as `compress.factors`, and `omit.refgroups = TRUE` omits reference group rows.

``` r
table2 <- tabglm(fit = fit, 
                 compress.factors = TRUE,
                 latex = TRUE)
kable(table2, 
      caption = "Table 2c. Linear regression fit (created by tabglm/kable).", 
      align = 'lrr', 
      format = "html")
```

<table>
<caption>
Table 2c. Linear regression fit (created by tabglm/kable).
</caption>
<thead>
<tr>
<th style="text-align:left;">
Variable
</th>
<th style="text-align:right;">
Beta (SE)
</th>
<th style="text-align:right;">
P
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
Intercept
</td>
<td style="text-align:right;">
22.10 (1.63)
</td>
<td style="text-align:right;">
&lt;0.001
</td>
</tr>
<tr>
<td style="text-align:left;">
Age
</td>
<td style="text-align:right;">
0.01 (0.02)
</td>
<td style="text-align:right;">
0.66
</td>
</tr>
<tr>
<td style="text-align:left;">
Male
</td>
<td style="text-align:right;">
-0.18 (0.27)
</td>
<td style="text-align:right;">
0.51
</td>
</tr>
<tr>
<td style="text-align:left;">
Black
</td>
<td style="text-align:right;">
0.34 (0.32)
</td>
<td style="text-align:right;">
0.29
</td>
</tr>
<tr>
<td style="text-align:left;">
Mexican American
</td>
<td style="text-align:right;">
0.63 (0.42)
</td>
<td style="text-align:right;">
0.14
</td>
</tr>
<tr>
<td style="text-align:left;">
Other
</td>
<td style="text-align:right;">
0.02 (0.36)
</td>
<td style="text-align:right;">
0.95
</td>
</tr>
<tr>
<td style="text-align:left;">
Treatment
</td>
<td style="text-align:right;">
3.10 (0.28)
</td>
<td style="text-align:right;">
&lt;0.001
</td>
</tr>
</tbody>
</table>
Maybe you're submitting to one of those enlightened journals that thinks comparing confidence intervals to 0 is totally different than comparing p-values to 0.05. *tabglm* can do confidence intervals:

``` r
table2 <- tabglm(fit = fit, 
                 columns = c("beta.se", "betaci"),
                 compress.factors = TRUE,
                 latex = TRUE)
kable(table2, 
      caption = "Table 2d. Linear regression fit (created by tabglm/kable).", 
      align = 'lrr', 
      format = "html")
```

<table>
<caption>
Table 2d. Linear regression fit (created by tabglm/kable).
</caption>
<thead>
<tr>
<th style="text-align:left;">
Variable
</th>
<th style="text-align:right;">
Beta (SE)
</th>
<th style="text-align:right;">
95% CI for Beta
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
Intercept
</td>
<td style="text-align:right;">
22.10 (1.63)
</td>
<td style="text-align:right;">
18.90, 25.30
</td>
</tr>
<tr>
<td style="text-align:left;">
Age
</td>
<td style="text-align:right;">
0.01 (0.02)
</td>
<td style="text-align:right;">
-0.04, 0.06
</td>
</tr>
<tr>
<td style="text-align:left;">
Male
</td>
<td style="text-align:right;">
-0.18 (0.27)
</td>
<td style="text-align:right;">
-0.71, 0.35
</td>
</tr>
<tr>
<td style="text-align:left;">
Black
</td>
<td style="text-align:right;">
0.34 (0.32)
</td>
<td style="text-align:right;">
-0.29, 0.96
</td>
</tr>
<tr>
<td style="text-align:left;">
Mexican American
</td>
<td style="text-align:right;">
0.63 (0.42)
</td>
<td style="text-align:right;">
-0.20, 1.45
</td>
</tr>
<tr>
<td style="text-align:left;">
Other
</td>
<td style="text-align:right;">
0.02 (0.36)
</td>
<td style="text-align:right;">
-0.69, 0.73
</td>
</tr>
<tr>
<td style="text-align:left;">
Treatment
</td>
<td style="text-align:right;">
3.10 (0.28)
</td>
<td style="text-align:right;">
2.56, 3.64
</td>
</tr>
</tbody>
</table>
### Logistic regression

Summarizing a fitted logistic regression model with *tabglm* is very similar. For 1-year mortality vs. age, age squared, sex, race, and treatment group:

``` r
fit <- glm(death_1yr ~ poly(Age, 2, raw = TRUE) + Sex + Race + Group, 
           data = tabdata, family = "binomial")
table3 <- tabglm(fit = fit, 
                 compress.factors = "binary", 
                 latex = TRUE)
kable(table3, 
      caption = "Table 3. Logistic regression fit (created by tabglm/kable).", 
      align = 'lrrr', 
      format = "html")
```

<table>
<caption>
Table 3. Logistic regression fit (created by tabglm/kable).
</caption>
<thead>
<tr>
<th style="text-align:left;">
Variable
</th>
<th style="text-align:right;">
Beta (SE)
</th>
<th style="text-align:right;">
OR (95% CI)
</th>
<th style="text-align:right;">
P
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
Intercept
</td>
<td style="text-align:right;">
-20.28 (25.57)
</td>
<td style="text-align:right;">
--
</td>
<td style="text-align:right;">
0.43
</td>
</tr>
<tr>
<td style="text-align:left;">
Age
</td>
<td style="text-align:right;">
0.54 (0.73)
</td>
<td style="text-align:right;">
1.72 (0.42, 7.43)
</td>
<td style="text-align:right;">
0.46
</td>
</tr>
<tr>
<td style="text-align:left;">
Age squared
</td>
<td style="text-align:right;">
-0.00 (0.01)
</td>
<td style="text-align:right;">
1.00 (0.99, 1.01)
</td>
<td style="text-align:right;">
0.48
</td>
</tr>
<tr>
<td style="text-align:left;">
Male
</td>
<td style="text-align:right;">
0.14 (0.29)
</td>
<td style="text-align:right;">
1.15 (0.64, 2.05)
</td>
<td style="text-align:right;">
0.64
</td>
</tr>
<tr>
<td style="text-align:left;">
Race
</td>
<td style="text-align:right;">
</td>
<td style="text-align:right;">
</td>
<td style="text-align:right;">
</td>
</tr>
<tr>
<td style="text-align:left;">
    White (ref)
</td>
<td style="text-align:right;">
--
</td>
<td style="text-align:right;">
--
</td>
<td style="text-align:right;">
--
</td>
</tr>
<tr>
<td style="text-align:left;">
    Black
</td>
<td style="text-align:right;">
-0.92 (0.38)
</td>
<td style="text-align:right;">
0.40 (0.19, 0.83)
</td>
<td style="text-align:right;">
0.02
</td>
</tr>
<tr>
<td style="text-align:left;">
    Mexican American
</td>
<td style="text-align:right;">
0.16 (0.42)
</td>
<td style="text-align:right;">
1.17 (0.50, 2.68)
</td>
<td style="text-align:right;">
0.70
</td>
</tr>
<tr>
<td style="text-align:left;">
    Other
</td>
<td style="text-align:right;">
0.04 (0.37)
</td>
<td style="text-align:right;">
1.04 (0.50, 2.16)
</td>
<td style="text-align:right;">
0.91
</td>
</tr>
<tr>
<td style="text-align:left;">
Treatment
</td>
<td style="text-align:right;">
0.04 (0.30)
</td>
<td style="text-align:right;">
1.04 (0.58, 1.89)
</td>
<td style="text-align:right;">
0.89
</td>
</tr>
</tbody>
</table>
Notice that the second-order term was labeled appropriately, and *tabglm* recognized `fit` as a logistic regression and thus by default added a OR (95% CI) column. Additionally, the binary Sex and Group variables was displayed as single rows, while the other factor variable, Race, was shown in a more expanded format. This was the result of setting `compress.factors = "binary"` (`compress.factors` can be `TRUE`, `FALSE`, or `"binary"`).

### GEEs

To summarize a fitted GEE, we can convert `tabdata` from wide to long format, fit a GEE, and then call *tabgee*. Here's a table for blood pressure vs. age, sex, race, BMI, and treatment group, with columns for Beta, SE, Z, and P:

``` r
tabdata2 <- reshape(data = tabdata,
                    varying = c("bp.1", "bp.2", "bp.3", "highbp.1",
                                "highbp.2", "highbp.3"),
                    timevar = "bp.visit", 
                    direction = "long")
tabdata2 <- tabdata2[order(tabdata2$id), ]
fit <- gee(bp ~ Age + Sex + Race + BMI + Group, 
           id = id, 
           data = tabdata2,
           corstr = "unstructured")
#>          (Intercept)                  Age              SexMale 
#>         111.89775246           0.04056654           4.16389689 
#>            RaceBlack RaceMexican American            RaceOther 
#>           0.15475299           1.16832912           0.01975425 
#>                  BMI       GroupTreatment 
#>           0.63487889           3.65538664
table4 <- tabgee(fit = fit, 
                 columns = c("beta", "se", "z", "p"),
                 compress.factors = "binary", 
                 data = tabdata2, 
                 latex = TRUE)
kable(table4, 
      caption = "Table 4. GEE fit (created by tabgee/kable).", 
      align = 'lrrr', 
      format = "html")
```

<table>
<caption>
Table 4. GEE fit (created by tabgee/kable).
</caption>
<thead>
<tr>
<th style="text-align:left;">
Variable
</th>
<th style="text-align:right;">
Beta
</th>
<th style="text-align:right;">
SE
</th>
<th style="text-align:right;">
Z
</th>
<th style="text-align:left;">
P
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
Intercept
</td>
<td style="text-align:right;">
111.71
</td>
<td style="text-align:right;">
5.45
</td>
<td style="text-align:right;">
20.50
</td>
<td style="text-align:left;">
&lt;0.001
</td>
</tr>
<tr>
<td style="text-align:left;">
Age
</td>
<td style="text-align:right;">
0.04
</td>
<td style="text-align:right;">
0.07
</td>
<td style="text-align:right;">
0.65
</td>
<td style="text-align:left;">
0.51
</td>
</tr>
<tr>
<td style="text-align:left;">
Male
</td>
<td style="text-align:right;">
4.16
</td>
<td style="text-align:right;">
0.75
</td>
<td style="text-align:right;">
5.54
</td>
<td style="text-align:left;">
&lt;0.001
</td>
</tr>
<tr>
<td style="text-align:left;">
Race
</td>
<td style="text-align:right;">
</td>
<td style="text-align:right;">
</td>
<td style="text-align:right;">
</td>
<td style="text-align:left;">
</td>
</tr>
<tr>
<td style="text-align:left;">
    White (ref)
</td>
<td style="text-align:right;">
--
</td>
<td style="text-align:right;">
--
</td>
<td style="text-align:right;">
--
</td>
<td style="text-align:left;">
--
</td>
</tr>
<tr>
<td style="text-align:left;">
    Black
</td>
<td style="text-align:right;">
0.19
</td>
<td style="text-align:right;">
0.87
</td>
<td style="text-align:right;">
0.22
</td>
<td style="text-align:left;">
0.83
</td>
</tr>
<tr>
<td style="text-align:left;">
    Mexican American
</td>
<td style="text-align:right;">
1.13
</td>
<td style="text-align:right;">
1.27
</td>
<td style="text-align:right;">
0.89
</td>
<td style="text-align:left;">
0.37
</td>
</tr>
<tr>
<td style="text-align:left;">
    Other
</td>
<td style="text-align:right;">
0.03
</td>
<td style="text-align:right;">
1.09
</td>
<td style="text-align:right;">
0.03
</td>
<td style="text-align:left;">
0.98
</td>
</tr>
<tr>
<td style="text-align:left;">
BMI
</td>
<td style="text-align:right;">
0.64
</td>
<td style="text-align:right;">
0.16
</td>
<td style="text-align:right;">
3.87
</td>
<td style="text-align:left;">
&lt;0.001
</td>
</tr>
<tr>
<td style="text-align:left;">
Treatment
</td>
<td style="text-align:right;">
3.65
</td>
<td style="text-align:right;">
0.98
</td>
<td style="text-align:right;">
3.71
</td>
<td style="text-align:left;">
&lt;0.001
</td>
</tr>
</tbody>
</table>
### Cox PH

And finally, to summarize a fitted Cox PH model for survival vs. covariates, with default settings:

``` r
tabdata <- tabdata[complete.cases(tabdata), ]
fit <- coxph(Surv(time = tabdata$time, event = tabdata$delta) ~ 
               Age + Sex + Race + Group, 
             data = tabdata)
table5 <- tabcox(fit = fit, 
                 latex = TRUE)
kable(table5, 
      caption = "Table 5. Cox PH fit (created by tabcox/kable).", 
      align = 'lrrr', 
      format = "html")
```

<table>
<caption>
Table 5. Cox PH fit (created by tabcox/kable).
</caption>
<thead>
<tr>
<th style="text-align:left;">
Variable
</th>
<th style="text-align:right;">
Beta (SE)
</th>
<th style="text-align:right;">
HR (95% CI)
</th>
<th style="text-align:right;">
P
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
Age
</td>
<td style="text-align:right;">
0.05 (0.01)
</td>
<td style="text-align:right;">
1.05 (1.02, 1.08)
</td>
<td style="text-align:right;">
0.001
</td>
</tr>
<tr>
<td style="text-align:left;">
Sex
</td>
<td style="text-align:right;">
</td>
<td style="text-align:right;">
</td>
<td style="text-align:right;">
</td>
</tr>
<tr>
<td style="text-align:left;">
    Female (ref)
</td>
<td style="text-align:right;">
--
</td>
<td style="text-align:right;">
--
</td>
<td style="text-align:right;">
--
</td>
</tr>
<tr>
<td style="text-align:left;">
    Male
</td>
<td style="text-align:right;">
0.11 (0.18)
</td>
<td style="text-align:right;">
1.12 (0.79, 1.58)
</td>
<td style="text-align:right;">
0.54
</td>
</tr>
<tr>
<td style="text-align:left;">
Race
</td>
<td style="text-align:right;">
</td>
<td style="text-align:right;">
</td>
<td style="text-align:right;">
</td>
</tr>
<tr>
<td style="text-align:left;">
    White (ref)
</td>
<td style="text-align:right;">
--
</td>
<td style="text-align:right;">
--
</td>
<td style="text-align:right;">
--
</td>
</tr>
<tr>
<td style="text-align:left;">
    Black
</td>
<td style="text-align:right;">
-0.98 (0.22)
</td>
<td style="text-align:right;">
0.38 (0.24, 0.58)
</td>
<td style="text-align:right;">
&lt;0.001
</td>
</tr>
<tr>
<td style="text-align:left;">
    Mexican American
</td>
<td style="text-align:right;">
0.07 (0.26)
</td>
<td style="text-align:right;">
1.07 (0.64, 1.78)
</td>
<td style="text-align:right;">
0.80
</td>
</tr>
<tr>
<td style="text-align:left;">
    Other
</td>
<td style="text-align:right;">
-0.08 (0.22)
</td>
<td style="text-align:right;">
0.92 (0.59, 1.43)
</td>
<td style="text-align:right;">
0.71
</td>
</tr>
<tr>
<td style="text-align:left;">
Group
</td>
<td style="text-align:right;">
</td>
<td style="text-align:right;">
</td>
<td style="text-align:right;">
</td>
</tr>
<tr>
<td style="text-align:left;">
    Control (ref)
</td>
<td style="text-align:right;">
--
</td>
<td style="text-align:right;">
--
</td>
<td style="text-align:right;">
--
</td>
</tr>
<tr>
<td style="text-align:left;">
    Treatment
</td>
<td style="text-align:right;">
0.13 (0.18)
</td>
<td style="text-align:right;">
1.14 (0.80, 1.61)
</td>
<td style="text-align:right;">
0.48
</td>
</tr>
</tbody>
</table>
Closing comments
----------------

-   I suggest **printr** with *kable* for alignment, captions, etc.
-   Working on functions for complex survey data.
-   Feel free to collaborate on [GitHub](https://github.com/vandomed/tab)!

References
----------

Dahl, David B. 2016. *Xtable: Export Tables to Latex or Html*. <https://CRAN.R-project.org/package=xtable>.

Daróczi, Gergely, and Roman Tsegelskyi. 2017. *Pander: An R ’Pandoc’ Writer*. <http://rapporter.github.io/pander>.

Terry M. Therneau, and Patricia M. Grambsch. 2000. *Modeling Survival Data: Extending the Cox Model*. New York: Springer.

Therneau, Terry M. 2015. *A Package for Survival Analysis in S*. <https://CRAN.R-project.org/package=survival>.

Xie, Yihui. 2014. “Knitr: A Comprehensive Tool for Reproducible Research in R.” In *Implementing Reproducible Computational Research*, edited by Victoria Stodden, Friedrich Leisch, and Roger D. Peng. Chapman; Hall/CRC. <http://www.crcpress.com/product/isbn/9781466561595>.

———. 2015. *Dynamic Documents with R and Knitr*. 2nd ed. Boca Raton, Florida: Chapman; Hall/CRC. <https://yihui.name/knitr/>.

———. 2017. *Printr: Automatically Print R Objects to Appropriate Formats According to the ’Knitr’ Output Format*. <https://CRAN.R-project.org/package=printr>.

———. 2018. *Knitr: A General-Purpose Package for Dynamic Report Generation in R*. <https://yihui.name/knitr/>.
