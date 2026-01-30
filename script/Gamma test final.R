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

df1<- df %>% filter(farm == "ISU",
                    type == "S")

#Check how many zeros
mean(df1$porewater_no3_mgl == 0)



# Visual Check
ggplot(df1, aes(x = crop, y = porewater_no3_mgl, fill = crop)) +
  geom_boxplot() +
  #facet_wrap(~year)+
  geom_jitter(width = 0.2, alpha = 0.5) +
  labs(
    title = "Porewater Nitrate by Crop (April-May 2024-2025) ISU Short",
    y = "Nitrate (mg/L)"
  ) +
  theme_minimal()


#Fit GLMM model
#fixed effect crop (We want to know effect of CC on no3 )
#random effect block and year
model_glmm_isu_s <- glmer(
  porewater_no3_mgl ~ crop + (1 | block) + (1| year),
  data = df1,
  family = Gamma(link = "log"),
  control = glmerControl(
    optimizer = "bobyqa",
    optCtrl = list(maxfun = 2e5)
  )
)

summary(model_glmm_isu_s)



####assumptions 
plot(model_glmm_isu_s)
qqnorm(resid(model_glmm_isu_s))
qqline(resid(model_glmm_isu_s))

##Dharma
sim_glmm_isu_s <- simulateResiduals(model_glmm_isu_s, n = 1000)
plot(sim_glmm_isu_s) #this model it meets the assumptions


####likelihood test

model_null <- glmer(
  porewater_no3_mgl ~ (1 | block) + (1 | year),
  data = df1,
  family = Gamma(link="log"),
  control = glmerControl(optimizer="bobyqa")
)

anova(model_null, model_glmm_isu_s, test = "Chisq") #Crop has a highly significant effect on porewater NO₃.


####The coefficients are on the log scale
#this number will help in explanation (percent comparison)
exp(fixef(model_glmm_isu_s))

####emmeans calculation
emm_isu_s<- emmeans(model_glmm_isu_s, ~ crop, type = "response")
pairs(emm_isu_s, adjust = "tukey")
multcomp::cld(emm_isu_s, Letters = letters)

###Get numbers for the figure/table
emm_isu_s
emm_df_isu_s <- as.data.frame(emm_isu_s)

###plot

ggplot(emm_df_isu_s, aes(x = crop, y = response)) +
  geom_point(size = 3) +
  geom_errorbar(
    aes(ymin = asymp.LCL, ymax = asymp.UCL),
    width = 0.2
  ) +
  labs(title ="Porewater Nitrate by Crop (April-May 2024-2025) ISU short")+
  ylab("Porewater NO₃ (mg L⁻¹)") +
  xlab("Crop") +
  theme_classic()




########ISU_L######
#4. GLMM_ISU_S
#df2 data includes ISU, 2024| 2025| S
#filtering Isu data for 2024 and 2025, only short lysimeter data

df2<- df %>% filter(farm == "ISU",
                    type == "L")

#Check how many zeros
mean(df1$porewater_no3_mgl == 0)



# Visual Check
ggplot(df2, aes(x = crop, y = porewater_no3_mgl, fill = crop)) +
  geom_boxplot() +
  #facet_wrap(~year)+
  geom_jitter(width = 0.2, alpha = 0.5) +
  labs(
    title = "Porewater Nitrate by Crop (April-May 2024-2025) ISU long",
    y = "Nitrate (mg/L)"
  ) +
  theme_minimal()


#Fit GLMM model
#fixed effect crop (We want to know effect of CC on no3 )
#random effect block and year
model_glmm_isu_l <- glmer(
  porewater_no3_mgl ~ crop + (1 | block) + (1| year),
  data = df2,
  family = Gamma(link = "log"),
  control = glmerControl(
    optimizer = "bobyqa",
    optCtrl = list(maxfun = 2e5)
  )
)

summary(model_glmm_isu_l)


####assumptions 
plot(model_glmm_isu_l)
qqnorm(resid(model_glmm_isu_l))
qqline(resid(model_glmm_isu_l))

##Dharma

sim_glmm_isu_l <- simulateResiduals(model_glmm_isu_l, n = 1000)
plot(sim_glmm_isu_l) #this model it meets the assumptions


####likelihood test

model_null_isu_l <- glmer(
  porewater_no3_mgl ~ (1 | block) + (1 | year),
  data = df2,
  family = Gamma(link="log"),
  control = glmerControl(optimizer="bobyqa")
)

anova(model_null_isu_l, model_glmm_isu_l, test = "Chisq") #Crop has a highly significant effect on porewater NO₃.


####The coefficients are on the log scale
#this number will help in explanation (percent comparison)
exp(fixef(model_glmm_isu_l))

####emmeans calculation
emm_isu_l<- emmeans(model_glmm_isu_l, ~ crop, type = "response")
pairs(emm_isu_l, adjust = "tukey")
multcomp::cld(emm_isu_l, Letters = letters)

###Get numbers for the figure/table
emm_isu_l
emm_df_isu_l <- as.data.frame(emm_isu_l)

###plot

ggplot(emm_df_isu_l, aes(x = crop, y = response)) +
  geom_point(size = 3) +
  geom_errorbar(
    aes(ymin = asymp.LCL, ymax = asymp.UCL),
    width = 0.2
  ) +
  labs(title ="Porewater Nitrate by Crop (April-May 2024-2025) ISU long")+
  ylab("Porewater NO₃ (mg L⁻¹)") +
  xlab("Crop") +
  theme_classic()
