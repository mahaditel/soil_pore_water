# 1. Load necessary libraries
library(readxl)
library(lme4)
library(lmerTest)
library(dplyr)
library(car)
library(emmeans)
library(tidyverse)
library(multcompView)
library(DHARMa)
library(lme4)


#2. load data
df<- read.csv("Data/porewater_all data_ISU_WIU_24_25_L_S.csv")


#3. data cleaning and preparation

#convert variables into factor
df<- df |> mutate(crop = factor(crop, levels = c("F", "CR", "AR", "GPC", "WPC", "PCRO")),
                  type = as.factor(type),
                  year = as.factor(year),
                  block= as.factor(block))

#removing missing points
summary(df)
df<- df %>% filter(!is.na(crop))

#4. GLMM_ISU_S
#df2 data includes ISU, 2024| 2025| S
#filtering Isu data for 2024 and 2025, only short lysimeter data

df2<- df %>% filter(farm == "ISU",
                    type == "S")

#Check how many zeros
mean(df2$porewater_no3_mgl == 0)



# Visual Check
ggplot(df2, aes(x = crop, y = porewater_no3_mgl, fill = crop)) +
  geom_boxplot() +
  #facet_wrap(~year)+
  geom_jitter(width = 0.2, alpha = 0.5) +
  labs(
    title = "Porewater Nitrate by Crop (April-May 2024-2025)",
    y = "Nitrate (mg/L)"
  ) +
  theme_minimal()


#Fit GLMM model
#fixed effect crop (We want to know effect of CC on no3 )
#random effect block and year
model_glmm <- glmer(
  porewater_no3_mgl ~ crop + (1 | block) + (1| year),
  data = df2,
  family = Gamma(link = "log"),
  control = glmerControl(
    optimizer = "bobyqa",
    optCtrl = list(maxfun = 2e5)
  )
)

summary(model_glmm)

####assumptions ##Dharma

sim_glmm <- simulateResiduals(model_glmm, n = 1000)
plot(sim_glmm) #this model it meets the assumptions


####likelihood test

model_null <- glmer(
  porewater_no3_mgl ~ (1 | block) + (1 | year),
  data = df2,
  family = Gamma(link="log"),
  control = glmerControl(optimizer="bobyqa")
)

anova(model_null, model_glmm, test = "Chisq") #Crop has a highly significant effect on porewater NO₃.


####The coefficients are on the log scale
#this number will help in explanation (percent comparison)
exp(fixef(model_glmm))

####emmeans calculation
emm<- emmeans(model_glmm, ~ crop, type = "response")
pairs(emm, adjust = "tukey")
multcomp::cld(emm, Letters = letters)

###Get numbers for the figure/table
emm
emm_df <- as.data.frame(emm)

###plot

ggplot(emm_df, aes(x = crop, y = response)) +
  geom_point(size = 3) +
  geom_errorbar(
    aes(ymin = asymp.LCL, ymax = asymp.UCL),
    width = 0.2
  ) +
  ylab("Porewater NO₃ (mg L⁻¹)") +
  xlab("Crop") +
  theme_classic()




