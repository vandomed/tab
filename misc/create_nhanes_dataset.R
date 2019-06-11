# June 9, 2019
# Dane Van Domelen

# R code to create dataset for survey functions in tab

# Load 2003-2004 demographics file
library("nhanesdata")
library("dplyr")
data(demo_c)
data(bmx_c)
df <- left_join(demo_c, bmx_c)

n <- nrow(df)
names(df) <- tolower(names(df))
df$rate <- 1 / (pmax(1, 75 * 12 - df$ridageyr * 12))
set.seed(123)
df$time_death <- rexp(n = n, rate = df$rate)
df$time_censoring <- rexp(n = n, rate = 1 / 10)
df <- df %>% dplyr::transmute(
  seqn,
  wtmec2yr,
  sdmvpsu,
  sdmvstra,
  Age = ridageyr,
  Sex = recode(riagendr, `1` = "Male", `2` = "Female"),
  Race = recode_factor(ridreth2, `1` = "Non-Hispanic White", `2` = "Non-Hispanic Black", `3` = "Mexican American",
                `4` = "Other", `5` = "Other"),
  BMI = bmxbmi,
  time = pmin(time_death, time_censoring),
  event = ifelse(time_death <= time_censoring, 1, 0)
)
table(df$Race)
locs <- which(df$Age + df$time >= 105)
df$time[locs] <- 105 - df$Age[locs]
hist(df$Age + df$time)

tabsvydata <- df
save(tabsvydata, file = "C:/Users/Dane/Google Drive/github/tab/data/tabsvydata.rda",
     version = 2)
