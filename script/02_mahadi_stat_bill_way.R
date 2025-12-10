##install packages
install.packages("multcompView")

# 1. Load necessary libraries
library(lme4)
library(lmerTest)
library(car)
library(emmeans)
library(tidyverse)
library(multcompView)

options(contrasts = c("contr.sum", "contr.poly"))
# options(contrasts = c("contr.treatment", "contr.poly"))

# 2. Load and Clean Data
# Assuming your file is in the working directory
df <- read_csv("Data/porewater_all_data_24_25_plot_isu_shallow.csv")

#raw data does not suitable for analysis need log transformation 
df <-  df %>%  
  mutate(log_no3 = log10(porewater_no3_mgl +1),
         log_drp = log10(porewater_drp_mgl +1))
####crop relable
df <- df %>% 
  mutate(crop = factor(crop, levels = c ("F", "CR", "AR", "GPC", "WPC", "PCRO") ))

# Filter for Year 2024 and Months April (4) & May (5)
df_clean <- df %>%
  filter(year == 2024, month %in% c(4, 5)) %>%
  mutate(
    block = as.factor(block),
    crop = as.factor(crop),
  ) %>%
  na.omit() # Remove rows with missing nitrate data




# 3. Visual Check
ggplot(df_clean, aes(x = crop, y = porewater_no3_mgl, fill = crop)) +
  geom_boxplot() +
  geom_jitter(width = 0.2, alpha = 0.5) +
  labs(
    title = "Porewater Nitrate by Crop (April-May 2024)",
    y = "Nitrate (mg/L)"
  ) +
  theme_minimal()

# 4. Fit the Linear Mixed Model
# Fixed effect: crop
# Random effect: (1 | block) -> Accounts for the 4 blocks
# Note: nitrate data may be right-skewed consider log transformation:
# model <- lmer(log(porewater_no3_mgl) ~ crop + (1|block), data = df_clean)

model <- lmer(log_no3 ~ crop + (1 | block), data = df_clean)

# 5. Check Assumptions (Normality of residuals)
# If these look non-normal, switch to the log-transformed model mentioned above
plot(model) # Heteroscedasticity check # looks good now
qqnorm(resid(model))
qqline(resid(model)) # Normality check - not that it is not that bad !!!!



# 6. Run the F-test (ANOVA)
# Using car::Anova for Type II Wald F tests
# (Type II is generally safer than Type I for unbalanced data)
anova_results <- car::Anova(model, type = "III", test.statistic = "F")
print(anova_results)

# 7. Post-hoc Analysis (Pairwise Comparisons)
# If the Anova shows a significant effect for 'crop', run this:
emmeans_results <- emmeans(model, ~crop)
emmeans_results

#data frame
first_emm_df <- as.data.frame(emmeans_results)

#log back transform for plot
first_emm_df <- first_emm_df %>% 
  mutate( 
    no3 = (10^emmean)-1,
    se_plus = ((10^(emmean+SE))-1),
    se_minus =   ((10^(emmean-SE))-1)
  )
first_emm_df %>% 
  ggplot(aes(x=crop, y = no3))+
  geom_point()+
  geom_errorbar(aes(ymin = se_minus, ymax= se_plus))

# Contrast pairwise comparisons (Tukey adjustment is default)
pairs(emmeans_results)




# Optional: View the Letter Display (Compact Letter Display) to see groupings
# requires 'multcomp' or 'multcompView' package usually, but recent emmeans does it strictly:

multcomp::cld(emmeans_results, Letters = letters)


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# Filter for Year 2024 and 2025 and Months April (4) & May (5)
df_2425 <- df %>%
  filter(year %in% c(2024, 2025), month %in% c(4, 5)) %>%
  mutate(
    block = as.factor(block),
    crop = as.factor(crop),
    year = as.factor(year) # IMPORTANT: Treat year as a category
    
  ) %>%
  na.omit()

# 3. Visual Check
ggplot(df_2425, aes(x = crop, y = log_no3, fill = year)) +
  geom_boxplot() +
  geom_jitter(width = 0.2, alpha = 0.5) +
  labs(
    title = "Porewater Nitrate by Crop (April-May 2024)",
    y = "Nitrate (mg/L)"
  ) +
  theme_minimal()

# 4. Fit the Linear Mixed Model
# Fixed effect: crop
# Random effect: (1 | block) -> Accounts for the 4 blocks
# Note: nitrate data may be right-skewed consider log transformation:
# model <- lmer(log(porewater_no3_mgl) ~ crop + (1|block), data = df_clean)
# plot is now repeated and needs to be a random effect as its not independent
model_multi <- lmer(
  log_no3 ~ crop * year + (1 | block ),
  data = df_2425
)

# 5. Check Assumptions (Normality of residuals)
# If these look non-normal, switch to the log-transformed model mentioned above
plot(model_multi) # Heteroscedasticity check # egads it is really bad - may try log...
plot(model_multi)
qqnorm(resid(model_multi))
qqline(resid(model_multi))
# Assumptions meet


summary(model_multi)

# 5. Run the F-test (ANOVA)
# Look at the 'crop:year' interaction row.
# If P < 0.05, the effect of crop depends on the year.
anova_2425 <- car::Anova(model_multi, type = "III", test.statistic = "F")
print(anova_2425)
summary(anova_2425)

# 6. Post-hoc Analysis (Compare Crops WITHIN each Year)
# The "| year" part tells R to run the crop comparisons separately for 2024 and 2025.
emm_interaction <- emmeans(model_multi, ~ crop | year)
emm_2425 <- summary(emm_interaction)
emm_2425
# Pairwise comparisons with Tukey adjustment
pairs(emm_interaction)

# Optional: Compact Letter Display by Year
# This generates letters (a, b, ab) for each year separately
multcomp::cld(emm_interaction, Letters = letters)

second_emm_df <- emm_2425 %>% 
  mutate( 
    no3 = (10^emmean)-1,
    se_plus = ((10^(emmean+SE))-1),
    se_minus =   ((10^(emmean-SE))-1)
  )
second_emm_df %>% 
  ggplot(aes(x=crop, y = no3, color = year))+
  geom_point()+
  geom_errorbar(aes(ymin = se_minus, ymax= se_plus))



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# NOW TO do this with shallow and deep...
df2 <- read_csv("Data/porewater_isu_s_and_l_24_25_plot.csv")


df_sl <- df2 %>%
  mutate(month = month(original_coll_date)) %>%
  # Filter for April (4) and May (5) in 2024 and 2025
  filter(year %in% c(2024, 2025), month %in% c(4, 5)) %>%
  filter(!is.na(crop)) %>% 
  # Define factors
  mutate(
    block = as.factor(block),
    plot = as.factor(plot), # Important for the random effect!
    crop = factor(crop, levels = c("F", "CR", "AR", "GPC", "WPC", "PCRO")),
    year = as.factor(year),
    log_no3 = log10(porewater_no3_mgl +1),
    log_drp = log10(porewater_drp_mgl +1)
  ) %>%
  mutate(
    factor(type, levels = c( "L", "S"))
  ) %>%
  na.omit()

# 3. Visual Check (Interaction Plot)
# It is critical to visualize the 3-way interaction (Crop x Year x Depth)
ggplot(df_sl, aes(x = crop, y = porewater_no3_mgl, fill = type)) +
  geom_boxplot() +
  facet_wrap(~year) + # Split by year
  labs(
    title = "Nitrate by Crop and Depth (S vs L) across Years",
    y = "Nitrate (mg/L)",
    fill = "Depth"
  ) +
  theme_minimal()

# 4. Fit the Linear Mixed Model (Split-Plot Design)
# Fixed Effects: crop * year * type (We want to know if depth patterns change by crop or year)
# Random Effects: (1|block/plot)
#   -> (1|block): Accounts for the block differences.
#   -> (1|block:plot): Accounts for the fact that 'Shallow' and 'Deep' come from the SAME plot.
#      This is the "whole plot error" term required for split-plot designs.

model_depth <- lmer(
  log_no3 ~ crop * year * type + (1 | block),
  data = df_sl
)

# 5. Check Assumptions
plot(model_depth)
qqnorm(resid(model_depth))
qqline(resid(model_depth)) #Seems normal 

# 6. ANOVA (Type III is best for interactions)
anova_depth <- car::Anova(model_depth, type = "III", test.statistic = "F")
print(anova_depth)

# njh playimg with random year, too
model_depthh <- lmer(
  log_no3 ~ crop * (1 | year) * type + (1 | block),
  data = df_sl
)
anova_depthh <- car::Anova(model_depthh, type = "III", test.statistic = "F")
print(anova_depthh)

# 7. Post-hoc Analysis
# Case A: If the 3-way interaction (crop:year:type) is SIGNIFICANT:
# We look at everything together.
# depends on how to analyze
emm_3way <- emmeans(model_depth, ~ type * crop * year)
multcomp::cld(emm_3way, Letters = letters)

# or
emm_3way2 <- emmeans(model_depth, ~ type | crop | year)
multcomp::cld(emm_3way2, Letters = letters)

#interaction type*year
emm_sl <- emmeans(model_depth, ~ type * year)
emm_sl_df <- summary(emm_sl)
multcomp::cld(emm_sl, Letters = letters)

emm_sl_df <- emm_sl_df %>% 
  mutate( 
    no3 = (10^emmean)-1,
    se_plus = ((10^(emmean+SE))-1),
    se_minus =   ((10^(emmean-SE))-1)
  )
emm_sl_df %>% 
  ggplot(aes(x=type, y = no3, color = year))+
  geom_point()+
  geom_errorbar(aes(ymin = se_minus, ymax= se_plus))

emm_crop_sl_yr <- emmeans(model_depth, ~ type * year *crop)
emm_sl_yr_df <- summary(emm_crop_sl_yr)
multcomp::cld(emm_crop_sl_yr, Letters = letters)

emm_sl_yr_df <- emm_sl_yr_df %>% 
  mutate( 
    no3 = (10^emmean)-1,
    se_plus = ((10^(emmean+SE))-1),
    se_minus =   ((10^(emmean-SE))-1)
  )%>%
  mutate(
    type = fct_relevel(type, "S", "L"))

emm_sl_yr_df %>% 
  ggplot(aes(x=crop, y = no3, color = type))+
  geom_point(position = position_dodge2(width = 0.3))+
  geom_errorbar(aes(ymin = se_minus, ymax= se_plus),
                position = position_dodge2(width = 0.3),
                width = 0.3) +
  facet_grid(year~.)#, scales = "free"

levels(emm_sl_yr_df$type)

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# farms
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# NOW TO do this with shallow and deep...
library(readxl)
df_farm<- read_excel("Data/porewater_all_data_24_25_plot.xlsx")


df_farm <- df_farm %>%
  mutate(month = month(original_coll_date)) %>%
  # Filter for April (4) and May (5) in 2024 and 2025
  filter(year %in% c(2024, 2025), month %in% c(4, 5)) %>%
  filter(!is.na(crop)) %>% 
  # Define factors
  mutate(
    farm = as.factor(farm),
    block = as.factor(block),
    plot = as.factor(plot), # Important for the random effect!
    crop = factor(crop, levels = c("F", "CR", "AR", "GPC", "PCRO", "WPC")),
    type = as.factor(type),
    year = as.factor(year),
    log_no3 = log10(porewater_no3_mgl +1),
    log_drp = log10(porewater_drp_mgl +1)
  ) %>%
  mutate(
    type = fct_relevel(type, "S", "L")) %>%
  na.omit()
levels(df_farm$farm)


# 3. Visual Check (Interaction Plot)
# It is critical to visualize the 3-way interaction (Crop x Year x Depth)
ggplot(df_farm, aes(x = crop, y = porewater_no3_mgl, fill = type)) +
  geom_boxplot() +
  facet_wrap(~year) + # Split by year
  labs(
    title = "Nitrate by Crop and Depth (S vs L) across Years",
    y = "Nitrate (mg/L)",
    fill = "Depth"
  ) +
  theme_minimal() + 
  facet_grid(farm~ year)

# 4. Fit the Linear Mixed Model (Split-Plot Design)
# Fixed Effects: crop * year * type (We want to know if depth patterns change by crop or year)
# Random Effects: (1|block/plot)
#   -> (1|block): Accounts for the block differences.
#   -> (1|block:plot): Accounts for the fact that 'Shallow' and 'Deep' come from the SAME plot.
#      This is the "whole plot error" term required for split-plot designs.

model_depth_farm <- lmer(
  log_no3 ~ crop * year * type *  farm  + (1 | block),
  data = df_farm
)
summary(model_depth_farm)
# 5. Check Assumptions
plot(model_depth_farm)
qqnorm(resid(model_depth))
qqline(resid(model_depth)) #seems normal

# 6. ANOVA (Type III is best for interactions)
anova_farm <- car::Anova(model_depth_farm, type = "III", test.statistic = "F")
print(anova_farm)


# 7. Post-hoc Analysis
# Case A: If the 3-way interaction (crop:year:type) is SIGNIFICANT:
# We look at everything together.
# depends on how to analyze
emm_farm <- emmeans(model_depth_farm, ~  crop * year | farm  )
emm_farm_df <- summary(emm_farm)
multcomp::cld(emm_farm, Letters = letters)


emm_farm_df <- emm_farm_df %>% 
  mutate( 
    no3 = (10^emmean)-1,
    se_plus = ((10^(emmean+SE))-1),
    se_minus =   ((10^(emmean-SE))-1)
  )

emm_farm_df %>% 
  ggplot(aes(x=crop, y = no3, color = crop))+ #, shape = type
  geom_point(position = position_dodge2(width = 0.3))+
  geom_errorbar(aes(ymin = se_minus, ymax= se_plus),
                position = position_dodge2(width = 0.3),
                width = 0.3) +
  facet_grid(farm~ year)


# depends on how to analyze
emm_farm_yr_type <- emmeans(model_depth_farm, ~  type * year * farm  )
emm_farm_yr_type_df <- summary(emm_farm_yr_type)
multcomp::cld(emm_farm_yr_type, Letters = letters)


emm_farm_yr_type_df <- emm_farm_yr_type_df %>% 
  mutate( 
    no3 = (10^emmean)-1,
    se_plus = ((10^(emmean+SE))-1),
    se_minus =   ((10^(emmean-SE))-1)
  )

emm_farm_yr_type_df %>% 
  ggplot(aes(x=farm, y = no3, color = type))+ #, shape = type
  geom_point(position = position_dodge2(width = 0.3))+
  geom_errorbar(aes(ymin = se_minus, ymax= se_plus),
                position = position_dodge2(width = 0.3),
                width = 0.3) +
  facet_grid(.~ year)


