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



#############################################################################################################
#checking original data set that has 432 data points
df3 <- read_xlsx("Data/porewater_all_data_24_25_plot.xlsx")
summary(df3$original_coll_date)
unique(df3$original_coll_date)
df3 %>%
  filter(original_coll_date == as.Date("2025-03-05")) %>%
  nrow() #### 2025-03-05 was removed due to considering April and may. so, observation number 272

#############################################################################################################

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



###checking the unique points in the variable
unique(df$ month)
unique(df$ year)
unique(df$ crop)
unique(df$ block)
unique(df$farm)
unique(df$original_coll_date)
unique(df$month_name)
summary(df)




#filtering Isu data for 2024 and 2025, only short lysimeter data
df2<- df %>% filter(farm == "ISU",
                   type == "S")

#save the data frame
write.csv(df2, "Data/porewater_ISU_24_25_shallow.csv", row.names = FALSE)



################################################################################
#test-1
#filtering #ISU data for #2024, only #short lysimeter data
df3<- df %>% filter(farm == "ISU",
                    type == "S",
                    year == "2024")

# 4. Visual Check
ggplot(df3, aes(x = crop, y = porewater_no3_mgl, fill = crop)) +
  geom_boxplot() +
  geom_jitter(width = 0.2, alpha = 0.5) +
  labs(
    title = "Porewater Nitrate by Crop (April-May 2024)",
    y = "Nitrate (mg/L)"
  ) +
  theme_minimal()


###plot histogram to see the pattern of the data
hist(df3$porewater_no3_mgl) #right skewed


#raw data does not suitable for analysis need log transformation 
df3 <-  df3 %>%  
  mutate(log_no3 = log10(porewater_no3_mgl +1),
         log_drp = log10(porewater_drp_mgl +1))

###plot histogram to see the pattern of the data
hist(df3$log_no3) #right skewed


#5. test-1 : Fit the Linear Mixed Model
# Fixed effect: crop
# Random effect: (1| block) -> Accounts for the 4 blocks
# Note: nitrate data may be right-skewed consider log transformation:
# model <- lmer(log(porewater_no3_mgl) ~ crop + (1|block), data = df_clean)
model<- lmer(log_no3 ~ crop + (1| block) , data = df3)

#5. Check Assumptions (Normality of residuals)
# If these look non-normal, switch to the log-transformed model mentioned above
plot(model) # Heteroscedasticity check # egads it is really bad - may try log...
plot(model)
qqnorm(resid(model))
qqline(resid(model))

#check Dharma
sim <- simulateResiduals(model)
plot(sim)

#5.test-2 (GLMM)

#df2 data includes ISU, 2024| 2025| S
#filtering Isu data for 2024 and 2025, only short lysimeter data
df2<- df %>% filter(farm == "ISU",
                    type == "S")
#Check how many zeros
mean(df2$porewater_no3_mgl == 0)

#model
library(lme4)

model_glmm <- glmer(
  porewater_no3_mgl ~ crop + (1 | block) + (1 | year),
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
plot(sim_glmm) #in this model it meets the assumptions



#5.test-3
#Filtering data for analysis
###data filter for ISU, 2024|2025|S|L

df1<- df %>% filter(farm %in% "ISU")

#Check how many zeros
mean(df1$porewater_no3_mgl == 0)


model_glmm <- glmer(
  porewater_no3_mgl ~ crop * type + (1 | block) + (1 | year),
  data = df1,
  family = Gamma(link = "log"),
  control = glmerControl(
    optimizer = "bobyqa",
    optCtrl = list(maxfun = 2e5)
  )
)

summary(model_glmm)

####assumptions ##Dharma

sim_glmm <- simulateResiduals(model_glmm, n = 1000)
plot(sim_glmm) #assumptions not perfectly met trying in a different way
plot(simulateResiduals(model_glmm_add)) ##assumptions not perfectly met trying in a different way
#Data looks normal but data distribution looks odd.

#5. test- 4 GLMM
#Depth (type) effects often vary by block or year
model_glmm_rs <- glmer(
  porewater_no3_mgl ~ crop * type + (1 + type | block) + (1 | year),
  data = df1,
  family = Gamma(link = "log"),
  control = glmerControl(optimizer = "bobyqa",
                         optCtrl = list(maxfun = 2e5))
)

plot(simulateResiduals(model_glmm_rs)) ##assumptions not perfectly met trying in a different way

###
model_glmm_add <- glmer(
  porewater_no3_mgl ~ crop + type + (1 | block) + (1 | year),
  data = df1,
  family = Gamma(link = "log")
)

####assumptions ##Dharma
plot(simulateResiduals(model_glmm_add)) ##assumptions not perfectly met trying in a different way
#Data looks normal but data distribution looks odd.




#6. As assumptions do not meet after including the "type"
#removing it from the model to make it simple
##df2 data includes ISU, 2024| 2025| S
#filtering Isu data for 2024 and 2025, only short lysimeter data
df2<- df %>% filter(farm == "ISU",
                    type == "S")
#Check how many zeros
mean(df2$porewater_no3_mgl == 0)

#model
library(lme4)

model_glmm <- glmer(
  porewater_no3_mgl ~ crop + (1 | block) + (1 | year),
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
plot(sim_glmm) #in this model it meets the assumptions



#7. As assumptions do not meet after including the "type"
#removing it from the model to make it simple
##df2 data includes ISU, 2024| 2025| L
#filtering Isu data for 2024 and 2025, only short lysimeter data
df4<- df %>% filter(farm == "ISU",
                    type == "L")
#Check how many zeros
mean(df4$porewater_no3_mgl == 0)

#model
library(lme4)

model_glmm <- glmer(
  porewater_no3_mgl ~ crop + (1 | block) + (1 | year),
  data = df4,
  family = Gamma(link = "log"),
  control = glmerControl(
    optimizer = "bobyqa",
    optCtrl = list(maxfun = 2e5)
  )
)

summary(model_glmm)

####assumptions ##Dharma

sim_glmm <- simulateResiduals(model_glmm, n = 1000)
plot(sim_glmm) # normal but variance look different
#for only ISU_L try model 


library(glmmTMB)
library(DHARMa)

# ISU, type L dataset (example filter)
df_ISU_L <- df1 %>% dplyr::filter(site == "ISU", type == "L")

m_ISU_L_tmb <- glmmTMB(
  porewater_no3_mgl ~ crop + (1|block) + (1|year),
  data = df_ISU_L,
  family = Gamma(link = "log"),
  dispformula = ~ crop   # allow variance to differ by crop
)

summary(m_ISU_L_tmb)

# DHARMa check
sim <- simulateResiduals(m_ISU_L_tmb, n = 1000)
plot(sim)




#8. As assumptions do not meet after including the "type"
#removing it from the model to make it simple
##df2 data includes WIU, 2024| 2025| S
#filtering Isu data for 2024 and 2025, only short lysimeter data
df5<- df %>% filter(farm == "WIU",
                    type == "S")
#Check how many zeros
mean(df5$porewater_no3_mgl == 0)

#model
library(lme4)

model_glmm <- glmer(
  porewater_no3_mgl ~ crop + (1 | block) + (1 | year),
  data = df5,
  family = Gamma(link = "log"),
  control = glmerControl(
    optimizer = "bobyqa",
    optCtrl = list(maxfun = 2e5)
  )
)

summary(model_glmm)

####assumptions ##Dharma

sim_glmm <- simulateResiduals(model_glmm, n = 1000)
plot(sim_glmm) #in this model it meets the assumptions




#9. As assumptions do not meet after including the "type"
#removing it from the model to make it simple
##df2 data includes WIU, 2024| 2025| L
#filtering Isu data for 2024 and 2025, only short lysimeter data
df6<- df %>% filter(farm == "WIU",
                    type == "L")
#Check how many zeros
mean(df6$porewater_no3_mgl == 0)

#model
library(lme4)

model_glmm <- glmer(
  porewater_no3_mgl ~ crop + (1 | block) + (1 | year),
  data = df6,
  family = Gamma(link = "log"),
  control = glmerControl(
    optimizer = "bobyqa",
    optCtrl = list(maxfun = 2e5)
  )
)

summary(model_glmm)

####assumptions ##Dharma

sim_glmm <- simulateResiduals(model_glmm, n = 1000)
plot(sim_glmm) #in this model it meets the assumptions
