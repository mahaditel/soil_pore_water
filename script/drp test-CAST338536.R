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
mahadi.df <- read_excel("Data/porewater_all_data_24_25.xlsx") |> clean_names()


#to see the treatments
mahadi.df$crop
length(unique(mahadi.df$crop))
summarise(mahadi.df)
unique(mahadi.df$crop)

#mutate FPC into WPC
mahadi.df <- mahadi.df |> 
  mutate(crop = if_else(crop== "FPC", "WPC" , crop))


mahadi.df <-  mahadi.df |>
  mutate(site_type = paste(farm, type, sep="_"))

library(dplyr)

mahadi.df <- mahadi.df |>
  mutate(drp_cat = case_when(
    porewater_drp_mgl < 0.05 ~ "L",
    porewater_drp_mgl <= 0.1 ~ "M",  # already know it's >= 0.04
    TRUE ~ "H"  # catches everything else (> 0.1)
  ))



