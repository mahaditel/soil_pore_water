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
library(dplyr)
library(ggplot2)



# Make a new default theme
# Run this and it will store it as an object for use later
theme_poster <- function(base_size = 28, base_family = "sans")
{theme(
  axis.line = element_line(linewidth  = 1, linetype = "solid"), 
  axis.ticks = element_line(colour = "black"),
  axis.title = element_text(size = 24, face = "bold"), 
  axis.text = element_text(colour = "black"),
  axis.text.x = element_text(size = 22, face = "bold"),
  axis.text.y = element_text(size = 24, face="bold"),
  plot.title = element_text(face = "bold"),
  legend.text = element_text(size = 18, face = "bold"), 
  legend.title = element_text(size = 24, face = "bold"), 
  legend.key = element_rect(fill = NA),
  legend.position = "right",
  legend.background = element_rect(fill = NA),
  panel.grid.major = element_line(linetype = "blank"),
  panel.grid.minor = element_line(linetype = "blank"),
  panel.background = element_rect(fill = NA))
}


# read in the file from data / mahadi  #clean_names() to fix error in the variables names
mahadi.df <- read_excel("Data/NREC biomass,2024.xlsx") |> clean_names()

mahadi.df <-  mahadi.df |>
  mutate(crop = as.factor(crop)) |> 
  mutate(crop = fct_relevel(crop, "PCRO", "CR", "AR", "GPC", "WPC", "F" ))
#to see the treatments
mahadi.df$crop
length(unique(mahadi.df$crop))
summarise(mahadi.df)
unique(mahadi.df$crop)
levels(mahadi.df$crop)  

mahadi.df |> 
  filter(farm == "ISU") |>
  ggplot(aes(x =net_biomass, y = as.factor(crop))) + 
  stat_summary(fun = mean, geom = "col",  # Changed to geom_col() for better bar orientation
               position = position_dodge(width = 0.8), alpha = 0.5) + 
  stat_summary(fun.data = mean_se, geom = "errorbar", 
               width = 0.2, position = position_dodge(width = 0.8))
isu.aplot
