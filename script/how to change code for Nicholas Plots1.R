# # install packages
# install.packages("devtools") # install new things from developmental sources
# install.packages("tidyverse") # dplyr and piping and ggplot etc
# install.packages("lubridate") # dates and times
# install.packages("readxl") # read in excel files
# install.packages("janitor") # clean up excel imports
# install.packages("patchwork") # arrange multiple plots per page
# install.packages("skimr") # quick summary stats
# install.packages("plotly") # cool ggplot things
# install.packages("scales") # scales on ggplot axes

# you will load a subset of these each time you run R 
library(tidyverse) 
# library(lubridate) 
library(scales) 
library(readxl) 
library(skimr) 
library(janitor) 
library(patchwork)
# library(dplyr)
# library(dplyr)
# library(ggplot2)
# library(dplyr)
# library(ggplot2)

# read in the file from data / mahadi  #clean_names() to fix error in the variables names
mahadi.df <- read_excel("Data/NREC Lysimeter Results.xlsx") |> clean_names()

#to see the treatments
mahadi.df$crop
length(unique(mahadi.df$crop))
summarise(mahadi.df)
unique(mahadi.df$crop)

#to rename or mutate name to another one
mahadi.df <- mahadi.df |> 
  mutate(crop = if_else(crop=="FPC", "WPC", crop))

# change to log scale in dataframe
mahadi.df <-  mahadi.df |>
  mutate(porewater_drp_ugl_log10 = log10(porewater_drp_ugl+1),
         porewater_nh3_mgl_log10 = log10(porewater_nh3_mgl+1),
         porewater_no3_mgl_log10 = log10(porewater_no3_mgl+1))

# the issue is how to make WIU and ISU on the same plot!! UGGGH Nicholas...
mahadi.df <-  mahadi.df |>
  mutate(site_type = paste(farm, type, sep="_"))


mahadi.df <-  mahadi.df |>
  mutate(type = as.factor(type),
         crop = as.factor(crop),
         site_type = as_factor(site_type)) |> 
  mutate(type = fct_relevel(type, "S", "L")) |> 
  mutate(crop = fct_relevel(crop, "F" , "PCRO" ,  "CR", "AR", "GPC", "WPC" ),
         site_type = fct_relevel(site_type, "ISU_S", "ISU_L",  "WIU_S", "WIU_L"))

# this will show the levels
levels(mahadi.df$site_type)

# make a simple plot just of the ISU data
nitrate.plot <- mahadi.df |>
  # filter(farm == "ISU") |>
  ggplot(aes(x = crop, y = porewater_no3_mgl_log10, 
             fill=site_type, color=site_type)) +
  stat_summary(
    fun = mean, geom = "bar",
    linewidth = .4,
    position = position_dodge(width = .9)) +  # Bars with dodge width 0.9
  stat_summary(
    fun.data = mean_se, geom = "errorbar",
    linewidth = 0.8, color = "black",
    width = 0.3,  # Make the error bar caps narrower
    position = position_dodge(width = .9)) +  # Error bars dodged similarly
  labs(
    x = "Year",
    y = "Nitrate-N (log10+1 mg/L)"
  ) +
  scale_x_discrete(labels = c(
    "PCRO" = "PEA", 
    "CR" = "Cereal Rye", 
    "AR" = "Annual Rye", 
    "GPC" = "Golden PC", 
    "WPC" = "WT PC", 
    "F" = "Fallow"
  )) +
  scale_color_manual(name = "Lysimeter \nDepth",
                     label = c(ISU_L = "ISU Deep", 
                               ISU_S = "ISU Shallow",
                               WIU_L = "WIU Deep",
                               WIU_S = "WIU Shallow"),
                     values = c(
                       ISU_L = "red4", 
                       ISU_S = "red1",
                       WIU_L = "purple4",
                       WIU_S = "purple1"),
                       ) +
  scale_fill_manual(name = "Lysimeter \nDepth",
                     label = c(ISU_L = "ISU Deep", 
                               ISU_S = "ISU Shallow",
                               WIU_L = "WIU Deep",
                               WIU_S = "WIU Shallow"),
                     values = c(
                       ISU_L = "red4", 
                       ISU_S = "red1",
                       WIU_L = "purple4",
                       WIU_S = "purple1"),
  ) +
  theme_classic()
nitrate.plot

