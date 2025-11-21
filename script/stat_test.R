install.packages("performance")

library(tidyverse) 
# library(lubridate) 
library(scales) 
library(readxl) 
library(skimr) 
library(janitor) 
library(patchwork)
library(lme4)       # for mixed models
library(lmerTest)   # adds p-values
library(emmeans)    # for post-hoc comparisons
library(performance) # for model diagnostics
library(ggplot2)
library(dplyr)

#data in
mahadi.df<- read_excel("Data/porewater_all_data_24_25_plot.xlsx") |> clean_names()

unique(mahadi.df$crop)

# the issue is how to make WIU and ISU on the same plot!! UGGGH Nicholas...
mahadi.df <-  mahadi.df |>
  mutate(site_type = paste(farm, type, sep="_"))


# the issue is how to make WIU and ISU on the same plot!! UGGGH Nicholas...
mahadi.df <-  mahadi.df |>
  mutate(site_type = paste(farm, type, sep="_"))
mahadi.df <-  mahadi.df |>
  mutate(type = as.factor(type),
         crop = as.factor(crop),
         site_type = as_factor(site_type)) |> 
  mutate(type = fct_relevel(type, "S", "L")) |> 
  mutate(crop = fct_relevel(crop, "F" ,  "CR", "AR", "GPC", "WPC", "PCRO" ),
         site_type = fct_relevel(site_type, "ISU_S", "ISU_L",  "WIU_S", "WIU_L"))


# Dr. Heller---------
df_s<- mahadi.df|> filter(type =="S")
#filtered df for just the short values, ran repeated measures
model <- aov(
  porewater_no3_mgl ~ year +
    farm %in% year + 
    block %in% farm +
    crop +
    year:crop +
    farm:crop +
    Error(plot / original_coll_date),
  data = df_s
)
#lots of significant terms, complex model, many missing data points, completely unbalanced
summary(model)
resids <- model$Within$residuals
#Shapiro_wilk##### on residuals, 
shapiro.test(resids)


### try to run linear mixed model#####
#Mixed models do:
  
#  allow missing repeated measurements

#allow unbalanced data

#allow different number of observations per plot

#allow date as random

#allow block → plot → date nested random effects

#allow fixed effects farm, year, crop, depth



library(lme4)
library(lmerTest)

df<- mahadi.df

######factor conversion-------
df$farm  <- factor(df$farm)
df$year  <- factor(df$year)
df$crop  <- factor(df$crop)
df$block <- factor(df$block)
df$plot  <- factor(df$plot)
df$type  <- factor(df$type)
df$original_coll_date <- as.Date(df$original_coll_date) #to see how does NO₃ change over time?

#LMM fit in the dataset####
mod <- lmer(
  porewater_no3_mgl ~ farm * year * crop * type + original_coll_date +
    (1 | farm:year:block) +
    (1 | farm:year:block:plot),
  data = df
)

summary(mod)
plot(residuals(mod))
qqnorm(residuals(mod)); qqline(residuals(mod))
###### data not normal

#as not normal and positively skewed, try to fit GLMM with gamma distribution
library(glmmTMB)

####remove negative value
# Check for negative values
summary(df$porewater_no3_mgl)
min(df$porewater_no3_mgl)
df[df$porewater_no3_mgl <= 0, ]


mod_gamma <- glmmTMB(
  porewater_no3_mgl ~ farm * year * crop * type + original_coll_date +
    (1 | farm:year:block) +
    (1 | farm:year:block:plot),
  family = Gamma(link="log"),
  data = df
)
summary(mod_gamma)

#check residuals
plot(residuals(mod_gamma, type="pearson") ~ fitted(mod_gamma))
abline(h = 0, col="red"))


#very complex model try to simple it
#4 way anova####
mod1 <- update(mod_gamma, . ~ . - farm:year:crop:type)
anova(mod_gamma, mod1, test = "Chisq") ## not fit p value >0.05

#1 Test farm:year:crop












####error for negative value
# Check for negative values
sum(dat$porewater_no3_mgl < 0, na.rm = TRUE)
# See which rows have negative values
dat[dat$porewater_no3_mgl < 0 & !is.na(dat$porewater_no3_mgl_log), ]
# Remove negative values before modeling
dat_clean <- filter(dat, porewater_no3_mgl > 0)










#Q_Q plot, resids not normal.
qqnorm(resids)
qqline(mahadi.df$porewater_no3_mgl, col = "red")

#-----Nitrate-nitrogen--------------
####Normality test#####

#Histogram
hist(mahadi.df$porewater_no3_mgl)

#Shapiro_wilk#####
shapiro.test(mahadi.df$porewater_no3_mgl)
shapiro.test(mahadi.df$porewater_no3_mgl_log)

#Q_Q plot
qqnorm(mahadi.df$porewater_no3_mgl)
qqline(mahadi.df$porewater_no3_mgl, col = "red")

#---data not normal

#as raw data not normal, trying to transform the data_log transformation to run parametric test
####convert to log
mahadi.df <-  mahadi.df |>
  mutate(porewater_drp_mgl_log = log(porewater_drp_mgl+1),
         porewater_no3_mgl_log = log(porewater_no3_mgl+1))

####Normality test#####

#Histogram
hist(mahadi.df$porewater_no3_mgl_log) #seems normal #lets see other tests

#Shapiro_wilk#####
shapiro.test(mahadi.df$porewater_no3_mgl_log) #data not normal

#Q_Q plot
qqnorm(mahadi.df$porewater_no3_mgl_log)
qqline(mahadi.df$porewater_no3_mgl_log, col = "red") #data not normal


# Overall data not normal need to run non parametric tests

#Generalized linear mixed model has been used for water quality data. 
#####GLMMs- Generalized linear mixed model
install.packages("lme4")      # run once
install.packages("performance")  # for checks (optional)
install.packages("glmmTMB")
########
library(lme4)
library(performance)
library(glmmTMB)
library(emmeans)


#GLMMs test

# Make a site-specific date factor and a per-plot ID [As have missing samples and dates could be different ]
dat <- mahadi.df
dat$SiteDate <- interaction(dat$farm, dat$original_coll_date, drop = TRUE)
dat$PlotID   <- interaction(dat$farm, dat$block, dat$plot, drop = TRUE)


# Filter for Nitrate-N
#nitrate <- subset(dat, Analyte == "porewater_no3_mgl_log")

# Fit GLMM with Gamma distribution
model_nitrate_glmm <- glmmTMB(
  porewater_no3_mgl ~ crop * type * year +
    (1 | farm) + (1 | block) + (1 | PlotID) + (1 | SiteDate),
  data = dat,
  family = Gamma(link = "log")
)

####error for negative value
# Check for negative values
sum(dat$porewater_no3_mgl < 0, na.rm = TRUE)
# See which rows have negative values
dat[dat$porewater_no3_mgl < 0 & !is.na(dat$porewater_no3_mgl_log), ]
# Remove negative values before modeling
dat_clean <- filter(dat, porewater_no3_mgl > 0)


# Fit GLMM with Gamma distribution with clean data [removed negative value]
model_nitrate_glmm <- glmmTMB(
  porewater_no3_mgl_log ~ crop * type * year +
    (1 | farm) + (1 | block) + (1 | PlotID) + (1 | SiteDate),
  data = dat_clean,
  family = Gamma(link = "log")
)

####
summary(model_nitrate_glmm)
anova(model_nitrate_glmm)



#######date from row to collumn######
unique_dates <- unique(dat$original_coll_date)
unique_dates




