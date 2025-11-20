# load libraries
library(janitor)
library(readxl)
library(tidyverse)
library(patchwork)

# read in the file from data / mahadi
mahadi.df <- read_excel("data/NREC Lysimeter Results.xlsx") |> clean_names()

mahadi.df <- mahadi.df |> 
  mutate(crop = if_else(crop=="FPC", "WPC", crop))

mahadi.df <-  mahadi.df |>
  mutate(type = as.factor(type),
         crop = as.factor(crop)) |> 
  mutate(type = fct_relevel(type, "S", "L")) |> 
  mutate(crop = fct_relevel(crop, "F", "WPC", "GPC", "AR", "CR", "PCRO" ))

# this will show the levels
levels(mahadi.df$crop)

isu.pplot<-mahadi.df |> 
  filter(farm == "ISU") |>
  ggplot(aes(y =porewater_drp_ugl, x = as.factor(crop), color = type, fill = type)) + 
  stat_summary(fun = mean, geom = "col",  # Changed to geom_col() for better bar orientation
               position = position_dodge(width = 0.9), alpha = 0.5) + 
  stat_summary(fun.data = mean_se, geom = "errorbar", 
               width = 0.2, position = position_dodge(width = 0.9)) +  # Adding error bars for mean SE
  scale_color_manual(
    name = "Lysimeter \nDepth",          # Legend title
    labels = c(
      "45cm", "90cm"),          # Set labels matching the type column
    values = c("coral", "cyan3")         # Set colors for '90cm' and '45cm'
  ) + 
  scale_fill_manual(
    name = "Lysimeter \nDepth",          # Legend title
    labels = c(
      "45cm", "90cm"),          # Set labels matching the type column
    values = c("coral", "cyan3")         # Set colors for '90cm' and '45cm'
  ) + 
  coord_cartesian(xlim = c(0, 230)) +     # Adjust xlim to fit Nitrate-N range
  labs(title = "A) ISU", x = "DRP(μg/L)", y = "Crop") +  # Correct axis labels
  theme_classic() + 
  geom_vline(xintercept = c(0, 50, 100, 150, 200), linetype = "dashed", color = "black",alpha=0.2)  # Add dashed vertical lines
isu.pplot

isu.pplot<-mahadi.df |> 
  filter(farm == "ISU") |>
  ggplot(aes(x = porewater_drp_ugl, y =crop, color = type, fill = type)) + 
  stat_summary(fun = mean, geom = "col",  # Changed to geom_col() for better bar orientation
               position = position_dodge(width = 0.9), alpha = 0.5) + 
  stat_summary(fun.data = mean_se, geom = "errorbar", 
               width = 0.2, position = position_dodge(width = 0.9)) +  # Adding error bars for mean SE
  scale_color_manual(
    name = "Lysimeter \nDepth",          # Legend title
    labels = c(
      "45cm", "90cm"),          # Set labels matching the type column
    values = c("coral", "cyan3")         # Set colors for '90cm' and '45cm'
  ) + 
  scale_fill_manual(
    name = "Lysimeter \nDepth",          # Legend title
    labels = c(
      "45cm", "90cm"),          # Set labels matching the type column
    values = c("coral", "cyan3")         # Set colors for '90cm' and '45cm'
  ) + 
  coord_cartesian(xlim = c(0, 230)) +     # Adjust xlim to fit DRP range
  labs(title = "A) ISU", y = "Crop", x = "DRP (μg/L)") +  # Correct axis labels
  theme_classic() + 
  geom_vline(xintercept = c(0, 50, 100, 150, 200), linetype = "dashed", color = "black", alpha = 0.2) +  # Add dashed vertical lines
  coord_flip()  # Flip the axes
isu.pplot


isu.pplot<-mahadi.df |> 
  filter(farm == "ISU") |>
  ggplot(aes(y =porewater_drp_ugl, x = as.factor(crop), color = type, fill = type)) + 
  stat_summary(fun = mean, geom = "col",  # Changed to geom_col() for better bar orientation
               position = position_dodge(width = 0.9), alpha = 0.5) + 
  stat_summary(fun.data = mean_se, geom = "errorbar", 
               width = 0.2, position = position_dodge(width = 0.9)) +  # Adding error bars for mean SE
  scale_color_manual(
    name = "Lysimeter \nDepth",          # Legend title
    labels = c(
      "45cm", "90cm"),          # Set labels matching the type column
    values = c("coral", "cyan3")         # Set colors for '90cm' and '45cm'
  ) + 
  scale_fill_manual(
    name = "Lysimeter \nDepth",          # Legend title
    labels = c(
      "45cm", "90cm"),          # Set labels matching the type column
    values = c("coral", "cyan3")         # Set colors for '90cm' and '45cm'
  ) + 
  coord_cartesian(xlim = c(0, 230)) +     # Adjust xlim to fit Nitrate-N range
  labs(title = "A) ISU", x = "DRP(μg/L)", y = "Crop") +  # Correct axis labels
  theme_classic() + 
  geom_vline(xintercept = c(0, 50, 100, 150, 200), linetype = "dashed", color = "black",alpha=0.2)  # Add dashed vertical lines
isu.pplot

wiu.pplot <- mahadi.df |> 
  filter(farm == "WIU") |>
  ggplot(aes(x = porewater_no3_mgl, y = as.factor(crop), color = type, fill = type)) + 
  stat_summary(fun = mean, geom = "col",  # Changed to geom_col() for better bar orientation
               position = position_dodge(width = 0.9), alpha = 0.5) + 
  stat_summary(fun.data = mean_se, geom = "errorbar", 
               width = 0.2, position = position_dodge(width = 0.9)) +  # Adding error bars for mean SE
  scale_color_manual(
    name = "Lysimeter \nDepth",          # Legend title
    labels = c("45cm", "90cm"),          # Set labels matching the type column
    values = c("coral", "cyan3")         # Set colors for '90cm' and '45cm'
  ) + 
  scale_fill_manual(
    name = "Lysimeter \nDepth",          # Legend title
    labels = c("45cm", "90cm"),          # Set labels matching the type column
    values = c("coral", "cyan3")         # Set colors for '90cm' and '45cm'
  ) + 
  coord_cartesian(xlim = c(0, 15)) +     # Adjust xlim to fit Nitrate-N range
  labs(title = "B) WIU", x = "DRP(μg/L)", y = "Crop") +  # Correct axis labels
  theme_classic() + 
  geom_vline(xintercept = c(0, 5, 10, 15), linetype = "dashed", color = "black", alpha=0.2) +  # Add dashed vertical lines
  coord_flip()  # Flip axes
wiu.pplot


isu.pplot + theme_classic(base_size = 26,)+ 
  theme( strip.text = element_blank(), strip.background = element_blank())+
  wiu.pplot + theme_classic(base_size = 26,)+ 
  theme( axis.title.y=element_blank(), axis.text.y=element_blank(), strip.background = element_blank()) +
  plot_layout(ncol = 2, guides = "collect") +
  NULL  

####
library(patchwork)

# Adjust ISU plot with log scale
isu.pplot <- mahadi.df |> 
  filter(farm == "ISU") |>
  ggplot(aes(y =porewater_drp_ugl, x = as.factor(crop), color = type, fill = type)) + 
  stat_summary(fun = mean, geom = "col",  
               position = position_dodge(width = 0.9), alpha = 0.5) + 
  stat_summary(fun.data = mean_se, geom = "errorbar", 
               width = 0.2, position = position_dodge(width = 0.9)) +  
  scale_color_manual(
    name = "Lysimeter \nDepth",          
    labels = c("45cm", "90cm"),          
    values = c("coral", "cyan3")         
  ) + 
  scale_fill_manual(
    name = "Lysimeter \nDepth",          
    labels = c("45cm", "90cm"),          
    values = c("coral", "cyan3")         
  ) + 
  scale_y_log10(limits = c(1, 300)) +     # Applying log scale and adjusting limits
  labs(title = "A) ISU", x = "Crop", y = "DRP(μg/L)") +  
  theme_classic() + 
  geom_vline(xintercept = c(1, 10, 100, 200), linetype = "dashed", color = "black", alpha=0.2)

# Adjust WIU plot with log scale
wiu.pplot <- mahadi.df |> 
  filter(farm == "WIU") |>
  ggplot(aes(x = porewater_no3_mgl, y = as.factor(crop), color = type, fill = type)) + 
  stat_summary(fun = mean, geom = "col",  
               position = position_dodge(width = 0.9), alpha = 0.5) + 
  stat_summary(fun.data = mean_se, geom = "errorbar", 
               width = 0.2, position = position_dodge(width = 0.9)) +  
  scale_color_manual(
    name = "Lysimeter \nDepth",          
    labels = c("45cm", "90cm"),          
    values = c("coral", "cyan3")         
  ) + 
  scale_fill_manual(
    name = "Lysimeter \nDepth",          
    labels = c("45cm", "90cm"),          
    values = c("coral", "cyan3")         
  ) + 
  scale_x_log10(limits = c(1, 20)) +     # Applying log scale and adjusting limits
  labs(title = "B) WIU", x = "NO3-N (mg/L)", y = "Crop") +  
  theme_classic() + 
  geom_vline(xintercept = c(1,10,100), linetype = "dashed", color = "black", alpha=0.2) +
  coord_flip()
wiu.pplot
# Combine the two plots side by side
isu.pplot + theme_classic(base_size = 26) + 
  theme(strip.text = element_blank(), strip.background = element_blank()) +
  wiu.pplot + theme_classic(base_size = 26) + 
  theme(axis.title.y = element_blank(), axis.text.y = element_blank(), strip.background = element_blank()) +
  plot_layout(ncol = 2, guides = "collect") +
  NULL

