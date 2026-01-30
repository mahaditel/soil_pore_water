


# 1. Load necessary libraries
library(readxl)
library(lme4)
library(lmerTest)
library(car)
library(emmeans)
library(tidyverse)
library(multcompView)
library(DHARMa)
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
  log_no3 ~ crop * type + (1 | block) + (1| year),
  data = df_sl
)

print(model_depth)
###Check the assumption with darma package


simulationoutput<-
  simulateResiduals(fittedModel = model_depth, 500, plot = TRUE )


# 5. Check Assumptions
plot(model_depth)
qqnorm(resid(model_depth))
qqline(resid(model_depth)) #Seems normal 

# 6. ANOVA (Type III is best for interactions)
anova_depth <- car::Anova(model_depth, type = "III", test.statistic = "F")
print(anova_depth)
 
#Save ANOVA table
write.csv(as.data.frame(anova_depth), "output/anova_type3_results.csv", row.names = TRUE)



# 7. Post-hoc Analysis
# We look at everything together.
# depends on how to analyze
emm_3way <- emmeans(model_depth, ~ crop)
multcomp::cld(emm_3way, Letters = letters)
emm_sl_df <- summary(emm_3way)
summary(emm_3way)

####back log transformation
emm_sl_df <- emm_sl_df %>% 
  mutate( 
    no3 = (10^emmean)-1,
    se_plus = ((10^(emmean+SE))-1),
    se_minus =   ((10^(emmean-SE))-1)
  )

#visualization
emm_sl_df %>% 
  ggplot(aes(x=crop, y = no3))+
  geom_point()+
  geom_errorbar(aes(ymin = se_minus, ymax= se_plus))


