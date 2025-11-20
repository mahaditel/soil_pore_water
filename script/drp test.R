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

pc<- mahadi.df |> year |> percentile_range(porewater_drp_mgl, 0.90)

lower_percentile <- 0.05  # 5th percentile
upper_percentile <- 0.95  # 95th percentile

percent90_data_2024 <- mahadi.df |> 
  filter(year == 2024) |> 
  pull(porewater_drp_mgl) |> 
  as.numeric() |>  # Convert to numeric
  quantile(probs = c(0.05,0.95))

percent90_data_2025 <- mahadi.df |> 
  filter(year == 2025) |> 
  pull(porewater_drp_mgl) |> 
  as.numeric() |>  # Convert to numeric
  quantile(probs = c(0.05,0.95))

upper_bound_2025 <- mahadi.df |> 
  filter(year == 2025) |> 
  pull(porewater_drp_mgl) |> 
  as.numeric() |>  # Convert to numeric
  quantile(probs = upper_percentile, na.rm = TRUE)


library(dplyr)

mahadi.df <- mahadi.df |>
  mutate(drp_cat = case_when(
    porewater_drp_mgl < 0.04 ~ "low",
    porewater_drp_mgl <= 0.1 ~ "moderate",  # already know it's >= 0.04
    TRUE ~ "high"  # catches everything else (> 0.1)
  ))



