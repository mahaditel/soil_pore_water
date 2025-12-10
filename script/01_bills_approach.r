# install packages
install.packages("multcompView")

# 1. Load necessary libraries
library(lme4)
library(lmerTest)
library(car)
library(emmeans)
library(tidyverse)
library(multcompView)

# 2. Load and Clean Data
# Assuming your file is in the working directory
df <- read_csv("Data/porewater_all_data_24_25_plot_isu_shallow.csv")

# Filter for Year 2024 and Months April (4) & May (5)
df_clean <- df %>%
  filter(year == 2024, month %in% c(4, 5)) %>%
  mutate(
    block = as.factor(block),
    crop = as.factor(crop),

    porewater_no3_mgl = as.numeric(porewater_no3_mgl)
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

model <- lmer(porewater_no3_mgl ~ crop + (1 | block), data = df_clean)

# 5. Check Assumptions (Normality of residuals)
# If these look non-normal, switch to the log-transformed model mentioned above
plot(model) # Heteroscedasticity check # egads it is really bad - may try log...
qqnorm(resid(model))
qqline(resid(model)) # Normality check - not that it is not that bad other than a few outliers!!!!

# 6. Run the F-test (ANOVA)
# Using car::Anova for Type II Wald F tests
# (Type II is generally safer than Type I for unbalanced data)
anova_results <- car::Anova(model, type = "III", test.statistic = "F")
print(anova_results)

# 7. Post-hoc Analysis (Pairwise Comparisons)
# If the Anova shows a significant effect for 'crop', run this:
emmeans_results <- emmeans(model, ~crop)

# Contrast pairwise comparisons (Tukey adjustment is default)
pairs(emmeans_results)

# Optional: View the Letter Display (Compact Letter Display) to see groupings
# requires 'multcomp' or 'multcompView' package usually, but recent emmeans does it strictly:

multcomp::cld(emmeans_results, Letters = letters)


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# Filter for Year 2024 and Months April (4) & May (5)
df_2425 <- df %>%
  filter(year %in% c(2024, 2025), month %in% c(4, 5)) %>%
  mutate(
    block = as.factor(block),
    crop = as.factor(crop),
    year = as.factor(year), # IMPORTANT: Treat year as a category
    porewater_no3_mgl = as.numeric(porewater_no3_mgl)
  ) %>%
  na.omit()

# 3. Visual Check
ggplot(df_2425, aes(x = crop, y = porewater_no3_mgl, fill = year)) +
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
  porewater_no3_mgl ~ crop * year + (1 | block / plot),
  data = df_2425
)

# 5. Check Assumptions (Normality of residuals)
# If these look non-normal, switch to the log-transformed model mentioned above
plot(model_multi) # Heteroscedasticity check # egads it is really bad - may try log...
plot(model_multi)
qqnorm(resid(model_multi))
qqline(resid(model_multi))
# NOTE this is where its not normal and may need to log the data

# 5. Run the F-test (ANOVA)
# Look at the 'crop:year' interaction row.
# If P < 0.05, the effect of crop depends on the year.
anova_2425 <- car::Anova(model_multi, type = "III", test.statistic = "F")
print(anova_2425)

# 6. Post-hoc Analysis (Compare Crops WITHIN each Year)
# The "| year" part tells R to run the crop comparisons separately for 2024 and 2025.
emm_interaction <- emmeans(model_multi, ~ crop * year)

# Pairwise comparisons with Tukey adjustment
pairs(emm_interaction)

# Optional: Compact Letter Display by Year
# This generates letters (a, b, ab) for each year separately
multcomp::cld(emm_interaction, Letters = letters)

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# NOW TO do this with shallow and deep...
df2 <- read_csv("Data/porewater_isu_s_and_l_24_25_plot.csv")


df_sl <- df2 %>%
  mutate(month = month(original_coll_date)) %>%
  # Filter for April (4) and May (5) in 2024 and 2025
  filter(year %in% c(2024, 2025), month %in% c(4, 5)) %>%
  # Define factors
  mutate(
    block = as.factor(block),
    plot = as.factor(plot), # Important for the random effect!
    crop = as.factor(crop),
    year = as.factor(year),
    porewater_no3_mgl = as.numeric(porewater_no3_mgl)
  ) %>%
  mutate(
    factor(type, levels = c("S", "L"))
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
  porewater_no3_mgl ~ crop * year * type + (1 | block / plot),
  data = df_sl
)

# 5. Check Assumptions
plot(model_depth)
qqnorm(resid(model_depth))
qqline(resid(model_depth))

# 6. ANOVA (Type III is best for interactions)
anova_depth <- car::Anova(model_depth, type = "III", test.statistic = "F")
print(anova_depth)

# 7. Post-hoc Analysis
# Case A: If the 3-way interaction (crop:year:type) is SIGNIFICANT:
# We look at everything together.
# depends on how to analyze
emm_3way <- emmeans(model_depth, ~ type * crop * year)
multcomp::cld(emm_3way, Letters = letters)

# or
emm_3way2 <- emmeans(model_depth, ~ type | crop | year)
multcomp::cld(emm_3way2, Letters = letters)
