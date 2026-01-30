# 1. Load necessary libraries
library(readxl)  #import excel
library(tidyverse) #data cleaning, manipulation, visualization, and analysis (e.g., ggplot2, dplyr, tidyr).
library(dplyr) #data filter, select, mutate, summarize, arrange
library(skimr) #Quick, clean summary statistics of data sets
library(janitor) #fix column names, remove empty rows/columns, tabulations
library(patchwork) #Combine multiple ggplot2 plots into one figure layout
library(scales) #Control axis labels, breaks, transformations, and formatting in plots (percent, log, comma)

# 2. load data
df<- read.csv("Data/porewater_all_data_24_25_plot_isu_shallow.csv")

#3. data cleaning and manipulation
##Giving number on dates
df <- df %>%
  mutate(date_num = case_when(
    original_coll_date == "2024-04-10" ~ 1,
    original_coll_date == "2024-04-20" ~ 2,
    original_coll_date == "2024-04-24" ~ 3,
    original_coll_date == "2024-04-30" ~ 4
  ))

## adding month name
df<- df %>% 
  mutate(month_name = case_when(
    month == "3" ~ "March",
    month == "4" ~ "April",
    month == "5" ~ "May"
  ))

#convert variables into factor
df<- df |> mutate(crop = factor(crop, levels = c("F", "CR", "AR", "GPC", "WPC", "PCRO")),
                  type = as.factor(type),
                  year = as.factor(year),
                  month = as.factor(month),
                  farm = as.factor(farm),
                  date_num =as.factor(date_num))


unique(df$ month)
unique(df$ year)
unique(df$ crop)
unique(df$ block)
unique(df$farm)
unique(df$month_name)
summary(df$month)

#4. filter 2024 data
# Filter 2024 data
df_24 <- df %>% filter(year %in% c(2024),
                       month %in% c(4,5),
                       farm == "ISU")
#clean data removing NA/ missing values
df_24_clean <- df_24 %>% filter(!is.na(farm),
                                !is.na(month),
                                !is.na(year),
                                !is.na(crop))
#5. ploting
#ploting by month of collection                                                              
ggplot(df_24_clean, aes(x = month_name, y = porewater_no3_mgl, color = crop)) +
  geom_boxplot() +
  #facet_wrap(~crop)+
  labs(title = "2024 Porewater Nitrate-nitrogen at 45cm depth at ISU",
       x = "Month of sample collection",
       y = "Nitrate (mg/L)")
 

#ploting by month of collection                                                             
ggplot(df_24_clean, aes(x= date_num, y = porewater_no3_mgl, color =crop)) +
  geom_boxplot() +
  #facet_wrap(~crop)+
  labs( title = "2024 Porewater Nitrate-nitrogen at 45cm depth at ISU",
        x= "Sample collection events",
        y= "Nitrate (mg/L)")

