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

# Set up graphs so its less code throughout
# Theme for Graphs
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
  legend.text = element_text(size = 15, face = "bold"), 
  legend.title = element_text(size = 24, face = "bold"), 
  legend.key = element_rect(fill = NA),
  legend.position = "right",
  legend.background = element_rect(fill = NA),
  panel.grid.major = element_line(linetype = "blank"),
  panel.grid.minor = element_line(linetype = "blank"),
  panel.background = element_rect(fill = NA))
}



# read in the file from data / mahadi  #clean_names() to fix error in the variables names
mahadi.df <- read_excel("Data/porewater_all_data_24_25.xlsx") |> clean_names()

#to see the treatments
mahadi.df$crop
length(unique(mahadi.df$crop))
summarise(mahadi.df)
unique(mahadi.df$crop)

#FPC to WPC
mahadi.df<- mahadi.df|>
  mutate(crop = if_else(crop == "FPC", "WPC", crop))



# the issue is how to make WIU and ISU on the same plot!! UGGGH Nicholas...
mahadi.df <-  mahadi.df |>
  mutate(site_type = paste(farm, type, sep="_"))

mahadi.df <- mahadi.df |>
  mutate(porewater_drp_mgl = as.numeric(porewater_drp_mgl),
         porewater_no3_mgl = as.numeric(porewater_no3_mgl))  # Add this line

mahadi.df <-  mahadi.df |>
  mutate(type = as.factor(type),
         crop = as.factor(crop),
         site_type = as_factor(site_type)) |> 
  mutate(type = fct_relevel(type, "S", "L")) |> 
  mutate(crop = fct_relevel(crop, "F" ,  "CR", "AR", "GPC", "WPC", "PCRO" ),
         site_type = fct_relevel(site_type, "ISU_S", "ISU_L",  "WIU_S", "WIU_L"))

# this will show the levels
levels(mahadi.df$site_type)



####Nitrate----###
# make a simple plot just of the ISU data
nitrate.plot <- mahadi.df |>
   #filter(farm == "ISU") |>
  ggplot(aes(x = crop, y = porewater_no3_mgl, 
             fill=site_type, color=site_type)) +
  stat_summary(
    fun = mean, geom = "bar",
    aes(group = site_type),  # Add this
    linewidth = .4,
    position = position_dodge(width = .9)) +
  stat_summary(
    fun.data = mean_se, geom = "errorbar",
    aes(group = site_type),  # Add this
    linewidth = 0.8, color = "black",
    width = 0.3,
    position = position_dodge(width = .9)) +  # Error bars dodged similarly
  labs(
    x = "",
    y = expression(bold("Nitrate-N (mg L"^-1*")"))
  ) +
  scale_x_discrete(labels = c(
    "PCRO" = "Pea, Clover,\nRadish, Oat", 
    "CR" = "Cereal\nRye", 
    "AR" = "Annual\nRye", 
    "GPC" = "Golden\nPennycress", 
    "WPC" = "Wild\nPennycress", 
    "F" = "Fallow"
  )) +
  scale_color_manual(name = "Lysimeter \nDepth",
                     label = c(ISU_L = "ISU 90cm", 
                               ISU_S = "ISU 45cm",
                               WIU_L = "WIU 90cm",
                               WIU_S = "WIU 45cm"),
                     values = c(
                       ISU_L = "red4", 
                       ISU_S = "red1",
                       WIU_L = "purple4",
                       WIU_S = "purple1"),
  ) +
  scale_fill_manual(name = "Lysimeter \nDepth",
                    label = c(ISU_L = "ISU 90cm", 
                              ISU_S = "ISU 45cm",
                              WIU_L = "WIU 90cm",
                              WIU_S = "WIU 45cm"),
                    values = c(
                      ISU_L = "red4", 
                      ISU_S = "red1",
                      WIU_L = "purple4",
                      WIU_S = "purple1"),
  ) +
  scale_y_continuous(
    breaks = seq(0, 30, by = 5),
    limits = c(0, 30),
    expand = c(0, 0)
  ) +
  geom_hline(yintercept = c(0,10,20, 30), 
             linetype = "dashed", color = "black", alpha=0.2) +
  theme_poster()+
  labs(title = "2025: Nitrate-N")+
  theme(plot.title = element_text(hjust = 0.5, size = 24))  # Center the title
nitrate.plot


ggsave(nitrate.plot, file="figures/nitrate-n-f_25.png", units = "in", 
       width = 15, height = 6)


####DRP----###
# make a simple plot just of the ISU data
drp.plot <- mahadi.df |>
  filter(year == "2025") |>
  ggplot(aes(x = crop, y = porewater_drp_mgl, 
             fill=site_type, color=site_type)) +
  stat_summary(
    fun = mean, geom = "bar",
    aes(group = site_type),  # Add this
    linewidth = .4,
    position = position_dodge(width = .9)) +
  stat_summary(
    fun.data = mean_se, geom = "errorbar",
    aes(group = site_type),  # Add this
    linewidth = 0.8, color = "black",
    width = 0.3,
    position = position_dodge(width = .9)) +  # Error bars dodged similarly
  labs(
    x = "",
    y = expression(bold("DRP (mg L"^-1*")"))
  ) +
  scale_x_discrete(labels = c(
    "PCRO" = "Pea, Clover,\nRadish, Oat", 
    "CR" = "Cereal\nRye", 
    "AR" = "Annual\nRye", 
    "GPC" = "Golden\nPennycress", 
    "WPC" = "Wild\nPennycress", 
    "F" = "Fallow"
  )) +
  scale_color_manual(name = "Lysimeter \nDepth",
                     label = c(ISU_L = "ISU 90cm", 
                               ISU_S = "ISU 45cm",
                               WIU_L = "WIU 90cm",
                               WIU_S = "WIU 45cm"),
                     values = c(
                       ISU_L = "red4", 
                       ISU_S = "red1",
                       WIU_L = "purple4",
                       WIU_S = "purple1"),
  ) +
  scale_fill_manual(name = "Lysimeter \nDepth",
                    label = c(ISU_L = "ISU 90cm", 
                              ISU_S = "ISU 45cm",
                              WIU_L = "WIU 90cm",
                              WIU_S = "WIU 45cm"),
                    values = c(
                      ISU_L = "red4", 
                      ISU_S = "red1",
                      WIU_L = "purple4",
                      WIU_S = "purple1"),
  ) +
  scale_y_continuous(
    breaks = seq(0, 4, by = 0.1),
    limits = c(0, 1),
    expand = c(0, 0)
  ) +
  geom_hline(yintercept = c(0,0.05, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6), 
             linetype = "dashed", color = "black", alpha=0.2) +
  theme_poster()+
  labs(title = "2025: DRP")+
  theme(plot.title = element_text(hjust = 0.5, size = 24))  # Center the title
drp.plot


ggsave(nitrate.plot, file="figures/drp-f_25.png", units = "in", 
       width = 15, height = 6)

# First, check the range of your data
summary(mahadi.df$porewater_drp_mgl)

# or
max(mahadi.df$porewater_drp_mgl, na.rm = TRUE)

#data grater than 1
mahadi.df |> 
  filter(porewater_drp_mgl > 0.5) |> 
  select(crop, site_type, porewater_drp_mgl)






# Set up graphs so its less code throughout
# Theme for Graphs
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
  legend.text = element_text(size = 15, face = "bold"), 
  legend.title = element_text(size = 24, face = "bold"), 
  legend.key = element_rect(fill = NA),
  legend.position = "right",
  legend.background = element_rect(fill = NA),
  panel.grid.major = element_line(linetype = "blank"),
  panel.grid.minor = element_line(linetype = "blank"),
  panel.background = element_rect(fill = NA))
}



# read in the file from data / mahadi  #clean_names() to fix error in the variables names
mahadi.df <- read_excel("Data/porewater_all_data_24_25.xlsx") |> clean_names()

#to see the treatments
mahadi.df$crop
length(unique(mahadi.df$crop))
summarise(mahadi.df)
unique(mahadi.df$crop)

#mutate FPC to WPC

mahadi.df <- mahadi.df |>
  mutate(crop = if_else(crop == "FPC", "WPC", crop))




# the issue is how to make WIU and ISU on the same plot!! UGGGH Nicholas...
mahadi.df <-  mahadi.df |>
  mutate(site_type = paste(farm, type, sep="_"))

mahadi.df <- mahadi.df |>
  mutate(porewater_drp_mgl = as.numeric(porewater_drp_mgl),
         porewater_no3_mgl = as.numeric(porewater_no3_mgl))  # Add this line

mahadi.df <-  mahadi.df |>
  mutate(type = as.factor(type),
         crop = as.factor(crop),
         site_type = as_factor(site_type)) |> 
  mutate(type = fct_relevel(type, "S", "L")) |> 
  mutate(crop = fct_relevel(crop, "F" ,  "CR", "AR", "GPC", "WPC", "PCRO" ),
         site_type = fct_relevel(site_type, "ISU_S", "ISU_L",  "WIU_S", "WIU_L"))

# this will show the levels
levels(mahadi.df$site_type)

####Nitrate----###
# make a simple plot just of the ISU data
nitrate.plot <- mahadi.df |>
  filter(year == "2024") |>
  ggplot(aes(x = crop, y = porewater_drp_mgl, 
             fill=site_type, color=site_type)) +
  stat_summary(
    fun = mean, geom = "bar",
    aes(group = site_type),  # Add this
    linewidth = .4,
    position = position_dodge(width = .9)) +
  stat_summary(
    fun.data = mean_se, geom = "errorbar",
    aes(group = site_type),  # Add this
    linewidth = 0.8, color = "black",
    width = 0.3,
    position = position_dodge(width = .9)) +  # Error bars dodged similarly
  labs(
    x = "",
    y = expression(bold("Nitrate-N (mg L"^-1*")"))
  ) +
  scale_x_discrete(labels = c(
    "PCRO" = "Pea, Clover,\nRadish, Oat", 
    "CR" = "Cereal\nRye", 
    "AR" = "Annual\nRye", 
    "GPC" = "Golden\nPennycress", 
    "WPC" = "Wild\nPennycress", 
    "F" = "Fallow"
  )) +
  scale_color_manual(name = "Lysimeter \nDepth",
                     label = c(ISU_L = "ISU 90cm", 
                               ISU_S = "ISU 45cm",
                               WIU_L = "WIU 90cm",
                               WIU_S = "WIU 45cm"),
                     values = c(
                       ISU_L = "red4", 
                       ISU_S = "red1",
                       WIU_L = "purple4",
                       WIU_S = "purple1"),
  ) +
  scale_fill_manual(name = "Lysimeter \nDepth",
                    label = c(ISU_L = "ISU 90cm", 
                              ISU_S = "ISU 45cm",
                              WIU_L = "WIU 90cm",
                              WIU_S = "WIU 45cm"),
                    values = c(
                      ISU_L = "red4", 
                      ISU_S = "red1",
                      WIU_L = "purple4",
                      WIU_S = "purple1"),
  ) +
  scale_y_continuous(
    breaks = seq(0, 30, by = 5),
    limits = c(0, 30),
    expand = c(0, 0)
  ) +
  geom_hline(yintercept = c(0,10,20, 30), 
             linetype = "dashed", color = "black", alpha=0.2) +
  theme_poster()+
  labs(title = "2025: Nitrate-N")+
  theme(plot.title = element_text(hjust = 0.5, size = 24))  # Center the title
nitrate.plot


ggsave(nitrate.plot, file="figures/nitrate-n-f_25.png", units = "in", 
       width = 15, height = 6)


# First, check the range of your data
summary(mahadi.df$porewater_drp_mgl)

# or
max(mahadi.df$porewater_drp_mgl, na.rm = TRUE)

#data grater than 30
mahadi.df |> 
  filter(year == "2025")|>
  filter(porewater_no3_mgl > 80) |> 
  select(crop, site_type, porewater_no3_mgl)

##########
##########Correct missing value
max(mahadi.df$porewater_no3_mgl[mahadi.df$year == "2025"], na.rm = TRUE)

####Nitrate----###
# make a simple plot just of the ISU data
nitrate.plot <- mahadi.df |>
  filter(year == "2025") |>
  ggplot(aes(x = crop, y = porewater_no3_mgl, 
             fill=site_type, color=site_type)) +
  stat_summary(
    fun = mean, geom = "bar",
    aes(group = site_type),  # Add this
    linewidth = .4,
    position = position_dodge(width = .9)) +
  stat_summary(
    fun.data = mean_se, geom = "errorbar",
    aes(group = site_type),  # Add this
    linewidth = 0.8, color = "black",
    width = 0.3,
    position = position_dodge(width = .9)) +  # Error bars dodged similarly
  labs(
    x = "",
    y = expression(bold("Nitrate-N (mg L"^-1*")"))
  ) +
  scale_x_discrete(labels = c(
    "PCRO" = "Pea, Clover,\nRadish, Oat", 
    "CR" = "Cereal\nRye", 
    "AR" = "Annual\nRye", 
    "GPC" = "Golden\nPennycress", 
    "WPC" = "Wild\nPennycress", 
    "F" = "Fallow"
  )) +
  scale_color_manual(name = "Lysimeter \nDepth",
                     label = c(ISU_L = "ISU 90cm", 
                               ISU_S = "ISU 45cm",
                               WIU_L = "WIU 90cm",
                               WIU_S = "WIU 45cm"),
                     values = c(
                       ISU_L = "red4", 
                       ISU_S = "red1",
                       WIU_L = "purple4",
                       WIU_S = "purple1"),
  ) +
  scale_fill_manual(name = "Lysimeter \nDepth",
                    label = c(ISU_L = "ISU 90cm", 
                              ISU_S = "ISU 45cm",
                              WIU_L = "WIU 90cm",
                              WIU_S = "WIU 45cm"),
                    values = c(
                      ISU_L = "red4", 
                      ISU_S = "red1",
                      WIU_L = "purple4",
                      WIU_S = "purple1"),
  ) +
  scale_y_continuous(
    breaks = seq(0, 100, by = 10),  # Adjust max based on your data
    limits = c(0, 100),  # or whatever your max is + a bit of space
    expand = c(0, 0)
  ) +
  geom_hline(yintercept = seq(0, 100, by = 10), 
             linetype = "dashed", color = "black", alpha = 0.2) +
  theme_poster()+
  labs(title = "2025: Nitrate-N")+
  theme(plot.title = element_text(hjust = 0.5, size = 24))  # Center the title
nitrate.plot

##########
##########Correct missing value 2024
max(mahadi.df$porewater_no3_mgl[mahadi.df$year == "2024"], na.rm = TRUE)

####Nitrate----###
# make a simple plot just of the ISU data
nitrate.plot <- mahadi.df |>
  filter(year == "2024") |>
  ggplot(aes(x = crop, y = porewater_no3_mgl, 
             fill=site_type, color=site_type)) +
  stat_summary(
    fun = mean, geom = "bar",
    aes(group = site_type),  # Add this
    linewidth = .4,
    position = position_dodge(width = .9)) +
  stat_summary(
    fun.data = mean_se, geom = "errorbar",
    aes(group = site_type),  # Add this
    linewidth = 0.8, color = "black",
    width = 0.3,
    position = position_dodge(width = .9)) +  # Error bars dodged similarly
  labs(
    x = "",
    y = expression(bold("Nitrate-N (mg L"^-1*")"))
  ) +
  scale_x_discrete(labels = c(
    "PCRO" = "Pea, Clover,\nRadish, Oat", 
    "CR" = "Cereal\nRye", 
    "AR" = "Annual\nRye", 
    "GPC" = "Golden\nPennycress", 
    "WPC" = "Wild\nPennycress", 
    "F" = "Fallow"
  )) +
  scale_color_manual(name = "Lysimeter \nDepth",
                     label = c(ISU_L = "ISU 90cm", 
                               ISU_S = "ISU 45cm",
                               WIU_L = "WIU 90cm",
                               WIU_S = "WIU 45cm"),
                     values = c(
                       ISU_L = "red4", 
                       ISU_S = "red1",
                       WIU_L = "purple4",
                       WIU_S = "purple1"),
  ) +
  scale_fill_manual(name = "Lysimeter \nDepth",
                    label = c(ISU_L = "ISU 90cm", 
                              ISU_S = "ISU 45cm",
                              WIU_L = "WIU 90cm",
                              WIU_S = "WIU 45cm"),
                    values = c(
                      ISU_L = "red4", 
                      ISU_S = "red1",
                      WIU_L = "purple4",
                      WIU_S = "purple1"),
  ) +
  scale_y_continuous(
    breaks = seq(0, 80, by = 10),  # Adjust max based on your data
    limits = c(0, 80),  # or whatever your max is + a bit of space
    expand = c(0, 0)
  ) +
  geom_hline(yintercept = seq(0, 100, by = 10), 
             linetype = "dashed", color = "black", alpha = 0.2) +
  theme_poster()+
  labs(title = "2025: Nitrate-N")+
  theme(plot.title = element_text(hjust = 0.5, size = 24))  # Center the title
nitrate.plot

#####
####Nitrate----###
# Plot for 2024 data
nitrate.plot <- mahadi.df |>
  filter(year == "2024") |>
  ggplot(aes(x = crop, y = porewater_no3_mgl, 
             fill = site_type, color = site_type)) +
  stat_summary(
    fun = mean, geom = "bar",
    aes(group = site_type),
    linewidth = 0.4,
    position = position_dodge(width = 0.9)) +
  stat_summary(
    fun.data = mean_se, geom = "errorbar",
    aes(group = site_type),
    linewidth = 0.8, color = "black",
    width = 0.3,
    position = position_dodge(width = 0.9)) +
  labs(
    x = "",
    y = expression(bold("Nitrate-N (mg L"^-1*")"))
  ) +
  scale_x_discrete(labels = c(
    "PCRO" = "Pea, Clover,\nRadish, Oat", 
    "CR" = "Cereal\nRye", 
    "AR" = "Annual\nRye", 
    "GPC" = "Golden\nPennycress", 
    "WPC" = "Wild\nPennycress", 
    "F" = "Fallow"
  )) +
  scale_color_manual(
    name = "Lysimeter\nDepth",
    labels = c(ISU_L = "ISU 90cm", 
               ISU_S = "ISU 45cm",
               WIU_L = "WIU 90cm",
               WIU_S = "WIU 45cm"),
    values = c(
      ISU_L = "red4", 
      ISU_S = "red1",
      WIU_L = "purple4",
      WIU_S = "purple1")
  ) +
  scale_fill_manual(
    name = "Lysimeter\nDepth",
    labels = c(ISU_L = "ISU 90cm", 
               ISU_S = "ISU 45cm",
               WIU_L = "WIU 90cm",
               WIU_S = "WIU 45cm"),
    values = c(
      ISU_L = "red4", 
      ISU_S = "red1",
      WIU_L = "purple4",
      WIU_S = "purple1")
  ) +
  scale_y_continuous(
    breaks = seq(0, 80, by = 10),
    limits = c(0, 80),
    expand = c(0, 0)
  ) +
  geom_hline(
    yintercept = seq(0, 80, by = 10),  # Changed from 100 to 80
    linetype = "dashed", 
    color = "black", 
    alpha = 0.2
  ) +
  theme_poster() +
  labs(title = "2024: Nitrate-N") +  # Changed from 2025 to 2024
  theme(plot.title = element_text(hjust = 0.5, size = 24))

nitrate.plot


###
# Check for NA values in 2024 nitrate data
mahadi.df |>
  filter(year == "2024") |>
  filter(is.na(porewater_no3_mgl)) |>
  nrow()

# Check for values above 80 mg/L in 2024
mahadi.df |>
  filter(year == "2024") |>
  filter(porewater_no3_mgl > 80) |>
  select(crop, site_type, porewater_no3_mgl)

# Check the actual maximum to be sure
max(mahadi.df$porewater_no3_mgl[mahadi.df$year == "2024"], na.rm = TRUE)

# Check if there are any non-finite values (Inf, -Inf, NaN)
mahadi.df |>
  filter(year == "2024") |>
  filter(!is.finite(porewater_no3_mgl)) |>
  select(crop, site_type, porewater_no3_mgl)

# Check if the crop levels exist in your filtered data
mahadi.df |>
  filter(year == "2024") |>
  count(crop)

# Check if any site_type levels are missing data
mahadi.df |>
  filter(year == "2024") |>
  count(crop, site_type) |>
  print(n = 30)

# Check which combinations have very small sample sizes
mahadi.df |>
  filter(year == "2024") |>
  count(crop, site_type) |>
  filter(n < 3)  # Combinations with fewer than 3 observations
