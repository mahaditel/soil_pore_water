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

# # Install these one time and run the library  - use in R studio
# install.packages("ggThemeAssist") 
# # helps reformat code - only run library one time
# install.packages("styler") # allows you to reformat code to look like a pro!!
# 
# library(ggThemeAssist)
# library(styler) 

# # Install for stats
# install.packages("car") # stats and ANOVA - essential 
# install.packages("emmeans") # estimated marginal means for unbalanced designs 
# install.packages("multcomView") # paired comparisons - note this will interfear with DPLYR!!
# install.packages("Rmisc") # stats 
# install.packages("Hmisc") # stats install.packages("broom") # output models cleanly 



##########################################
########################################
#####################################
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

#mutate(type = fct_relevel(type, "S", "L")) to sequence S first then L
#mutate(crop = fct_relevel(crop, "F", "WPC", "GPC", "AR", "CR", "PCRO" )) to sequence 

mahadi.df <-  mahadi.df |>
  mutate(type = as.factor(type),
         crop = as.factor(crop)) |> 
  mutate(type = fct_relevel(type, "S", "L")) |> 
  mutate(crop = fct_relevel(crop, "PCRO", "CR", "AR", "GPC", "WPC", "F" ))

# this will show the levels
levels(mahadi.df$crop)


# make a simple plot just of the ISU data
isu.plot<- mahadi.df |>
  filter(farm == "ISU") |>
  ggplot(aes(x = crop, y = porewater_no3_mgl, 
             group=1, fill=type,color = type)) +
  stat_summary(
    fun = mean, geom = "point",
    shape = 23, size = 5, #fill = "white",
    position = position_dodge(width = 0.5)
  ) +
  stat_summary(
    fun = mean, geom = "line",
    linewidth = 1, #fill = "white",
    position = position_dodge(width = 0.5)
  ) +
  stat_summary(
    fun.data = mean_se, geom = "errorbar",
    linewidth = 1,
    width = 0.2, position = position_dodge(width = 0.5)
  )  +
  facet_grid(type ~ ., labeller = labeller(type = c(L = "Deep", S = "Shallow"))) + # doen as row and column
  labs(
    title = "ISU Data",
    x = "Year",
    y = "Nitrate-N (mg/L)"
  ) + 
  coord_cartesian(ylim=c(0,30))+
  scale_color_manual(name = "Lysimeter \nDepth",
                     label = c(L="Deep", S="Shallow"),
                     values = c(L="black", S="green"))+
  scale_fill_manual(name = "Lysimeter \nDepth",
                    label = c(L="Deep", S="Shallow"),
                    values = c(L="black", S="green"))+
  theme_classic()+
  theme(strip.background = element_blank())
isu.plot


wiu.plot<- mahadi.df |>
  filter(farm == "WIU") |>
  ggplot(aes(x = crop, y = porewater_no3_mgl, 
             group=1, fill=type,color = type)) +
  stat_summary(
    fun = mean, geom = "point",
    shape = 23, size = 5, #fill = "white",
    position = position_dodge(width = 0.5)
  ) +
  stat_summary(
    fun = mean, geom = "line",
    linewidth = 1, #fill = "white",
    position = position_dodge(width = 0.5)
  ) +
  stat_summary(
    fun.data = mean_se, geom = "errorbar",
    linewidth = 1,
    width = 0.2, position = position_dodge(width = 0.5)
  ) +
  coord_cartesian(ylim=c(0,30))+
  facet_grid(type ~ ., labeller = labeller(type = c(L = "Deep", S = "Shallow"))) + # doen as row and column
  labs(
    title = "WIU Data",
    x = "Year",
    y = "Nitrate-N (mg/L)"
  ) +
  scale_color_manual(name = "Lysimeter \nDepth",
                     label = c(L="Deep", S="Shallow"),
                     values = c(L="black", S="green"))+
  scale_fill_manual(name = "Lysimeter \nDepth",
                    label = c(L="Deep", S="Shallow"),
                    values = c(L="black", S="green"))+
  theme_classic()+
  theme(strip.background = element_blank())
wiu.plot

#theme_classic(base_size = 26,) to remove grid line and base font increase
final.plot <-
  isu.plot + theme_classic(base_size = 26,)+ 
  theme( strip.text = element_blank(), strip.background = element_blank())+
  wiu.plot + theme_classic(base_size = 26,)+ 
  theme( axis.title.y=element_blank(), axis.text.y=element_blank(), strip.background = element_blank()) +
  plot_layout(ncol = 2, guides = "collect") +
  NULL
final.plot
print(final.plot)


ggsave(final.plot, file="figures/Nitrate-N.pdf", units = "in", 
       width = 16, height = 6)

#### phosphorus###############
##############################
# make a simple plot just of the ISU data
isu.plot<- mahadi.df |>
  filter(farm == "ISU") |>
  ggplot(aes(x = crop, y = porewater_drp_ugl, 
             group=1, fill=type,color = type)) +
  stat_summary(
    fun = mean, geom = "point",
    shape = 23, size = 5, #fill = "white",
    position = position_dodge(width = 0.5)
  ) +
  stat_summary(
    fun = mean, geom = "line",
    linewidth = 1, #fill = "white",
    position = position_dodge(width = 0.5)
  ) +
  stat_summary(
    fun.data = mean_se, geom = "errorbar",
    linewidth = 1,
    width = 0.2, position = position_dodge(width = 0.5)
  )  +
  facet_grid(type ~ ., labeller = labeller(type = c(L = "Deep", S = "Shallow"))) + # doen as row and column
  labs(
    title = "ISU Data",
    x = "Cover Crops",
    y = "Dissolved Reactive Phosphorus (μg/L)"
  ) + 
  coord_cartesian(ylim=c(10,230))+
  scale_color_manual(name = "Lysimeter \nDepth",
                     label = c(L="Deep", S="Shallow"),
                     values = c(L="black", S="green"))+
  scale_fill_manual(name = "Lysimeter \nDepth",
                    label = c(L="Deep", S="Shallow"),
                    values = c(L="black", S="green"))+
  theme_classic()+
  theme(strip.background = element_blank())
isu.plot


wiu.plot<- mahadi.df |>
  filter(farm == "WIU") |>
  ggplot(aes(x = crop, y = porewater_drp_ugl, 
             group=1, fill=type,color = type)) +
  stat_summary(
    fun = mean, geom = "point",
    shape = 23, size = 5, #fill = "white",
    position = position_dodge(width = 0.5)
  ) +
  stat_summary(
    fun = mean, geom = "line",
    linewidth = 1, #fill = "white",
    position = position_dodge(width = 0.5)
  ) +
  stat_summary(
    fun.data = mean_se, geom = "errorbar",
    linewidth = 1,
    width = 0.2, position = position_dodge(width = 0.5)
  )  +
  facet_grid(type ~ ., labeller = labeller(type = c(L = "Deep", S = "Shallow"))) + # doen as row and column
  labs(
    title = "WIU Data",
    x = "Cover Crops",
    y = "Dissolved Reactive Phosphorus (μg/L)"
  ) + 
  coord_cartesian(ylim=c(10,50))+
  scale_color_manual(name = "Lysimeter \nDepth",
                     label = c(L="Deep", S="Shallow"),
                     values = c(L="black", S="green"))+
  scale_fill_manual(name = "Lysimeter \nDepth",
                    label = c(L="Deep", S="Shallow"),
                    values = c(L="black", S="green"))+
  theme_classic()+
  theme(strip.background = element_blank())
wiu.plot

#theme_classic(base_size = 26,) to remove grid line and base font increase
final.plot <-
  isu.plot + theme_classic(base_size = 26,)+ 
  theme( strip.text = element_blank(), strip.background = element_blank())+
  wiu.plot + theme_classic(base_size = 26,)+ 
  theme( axis.title.y=element_blank(), axis.text.y=element_blank(), strip.background = element_blank()) +
  plot_layout(ncol = 2, guides = "collect") +
  NULL
final.plot
print(final.plot)


ggsave(final.plot, file="figures/Dissolved Reactive Phosphorus.pdf", units = "in", 
       width = 16, height = 6)

############ammonia#########
###########################
isu.plot<- mahadi.df |>
  filter(farm == "ISU") |>
  ggplot(aes(x = crop, y = porewater_nh3_mgl, 
             group=1, fill=type,color = type, group= type)) +
  stat_summary(
    fun = mean, geom = "point",
    shape = 23, size = 5, #fill = "white",
    position = position_dodge(width = 0.5)
  ) +
  stat_summary(
    fun = mean, geom = "line",
    linewidth = 1, #fill = "white",
    position = position_dodge(width = 0.5)
  ) +
  stat_summary(
    fun.data = mean_se, geom = "errorbar",
    linewidth = 1,
    width = 0.2, position = position_dodge(width = 0.5)
  )  +
  facet_grid(type ~ ., labeller = labeller(type = c(L = "Deep", S = "Shallow"))) + # doen as row and column
  labs(
    title = "ISU Data",
    x = "Cover Crops",
    y = "Ammonia (mg/L)"
  ) + 
  coord_cartesian(ylim=c(0,1))+
  scale_color_manual(name = "Lysimeter \nDepth",
                     label = c(L="Deep", S="Shallow"),
                     values = c(L="black", S="green"))+
  scale_fill_manual(name = "Lysimeter \nDepth",
                    label = c(L="Deep", S="Shallow"),
                    values = c(L="black", S="green"))+
  theme_classic()+
  theme(strip.background = element_blank())
isu.plot


wiu.plot<- mahadi.df |>
  filter(farm == "WIU") |>
  ggplot(aes(x = crop, y = porewater_nh3_mgl, 
             group=1, fill=type,color = type)) +
  stat_summary(
    fun = mean, geom = "point",
    shape = 23, size = 5, #fill = "white",
    position = position_dodge(width = 0.5)
  ) +
  stat_summary(
    fun = mean, geom = "line",
    linewidth = 1, #fill = "white",
    position = position_dodge(width = 0.5)
  ) +
  stat_summary(
    fun.data = mean_se, geom = "errorbar",
    linewidth = 1,
    width = 0.2, position = position_dodge(width = 0.5)
  )  +
  facet_grid(type ~ ., labeller = labeller(type = c(L = "Deep", S = "Shallow"))) + # doen as row and column
  labs(
    title = "WIU Data",
    x = "Cover Crops",
    y = "Ammonia (mg/L)"
  ) + 
  coord_cartesian(ylim=c(0,0.15))+
  scale_color_manual(name = "Lysimeter \nDepth",
                     label = c(L="Deep", S="Shallow"),
                     values = c(L="black", S="green"))+
  scale_fill_manual(name = "Lysimeter \nDepth",
                    label = c(L="Deep", S="Shallow"),
                    values = c(L="black", S="green"))+
  theme_classic()+
  theme(strip.background = element_blank())
wiu.plot

#theme_classic(base_size = 26,) to remove grid line and base font increase
final.plot <-
  isu.plot + theme_classic(base_size = 26,)+ 
  theme( strip.text = element_blank(), strip.background = element_blank())+
  wiu.plot + theme_classic(base_size = 26,)+ 
  theme( axis.title.y=element_blank(), axis.text.y=element_blank(), strip.background = element_blank()) +
  plot_layout(ncol = 2, guides = "collect") +
  NULL
final.plot
print(final.plot)


ggsave(final.plot, file="figures/ammonia.pdf", units = "in", 
       width = 16, height = 6)



############################
#############################
mahadi.df |> 
  filter(farm == "ISU") |>
  ggplot(aes(x = as.factor(crop), y = porewater_no3_mgl, color = type, group = type)) + 
  stat_summary(fun = mean, geom = "boxplot", 
               shape = 23, size = 3, fill = "white",
               position = position_dodge(width = 0.2)) +
  # Dotted shadow line
  #stat_summary(fun = mean, geom = "line", 
               #linetype = "dotted",
               #linewidth = .7, position = position_dodge(width = 0.2)) +
  # Solid main line
  #stat_summary(fun.data = mean_se, geom = "errorbar",
               #width = 0.2, position = position_dodge(width = 0.2)) +
  # Shadow axis lines
  geom_hline(yintercept = c(0, 10, 20, 30), 
             linetype = "dashed", color = "grey", linewidth = 0.009) +
  scale_color_manual(
    name = "Lysimeter \nDepth",         # Change legend title
    labels = c(S = "Shallow", L = "Deep"), # Relabel 'S' and 'L'
    values = c(S = "coral", L = "cyan3")   # Assign colors to 'Shallow' and 'Deep'
  )+ 
  coord_cartesian(ylim=c(0,30))+
  labs(title = "ISU", x = "Crop", y = "Nitrate-N (mg/L)") +
  theme_classic()

isu.plot


##############################
wiu.plot<-mahadi.df |> 
  filter(farm == "WIU") |>
  ggplot(aes(x = as.factor(crop), y = porewater_no3_mgl, color = type, group = type)) + 
  stat_summary(fun = mean, geom = "point", 
               shape = 23, size = 3, fill = "white",
               position = position_dodge(width = 0.2)) +
  # Dotted shadow line
  stat_summary(fun = mean, geom = "line", 
               linetype = "dotted",
               linewidth = .7, position = position_dodge(width = 0.2)) +
  # Solid main line
  stat_summary(fun.data = mean_se, geom = "errorbar",
               width = 0.2, position = position_dodge(width = 0.2)) +
  # Shadow axis lines
  geom_hline(yintercept = c(0, 10, 20, 30), 
             linetype = "dashed", color = "grey", linewidth = 0.009) +
  scale_color_manual(
    name = "Lysimeter \nDepth",         # Change legend title
    labels = c(S = "Shallow", L = "Deep"), # Relabel 'S' and 'L'
    values = c(S = "coral", L = "cyan3")   # Assign colors to 'Shallow' and 'Deep'
  )+ 
  coord_cartesian(ylim=c(0,30))+
  labs(title = "WIU", x = "Crop", y = "Nitrate-N (mg/L)") +
  theme_classic()

wiu.plot


final.plot <-
  isu.plot + theme_classic(base_size = 26,)+ 
  theme( strip.text = element_blank(), strip.background = element_blank())+
  wiu.plot + theme_classic(base_size = 26,)+ 
  theme( axis.title.y=element_blank(), axis.text.y=element_blank(), strip.background = element_blank()) +
  plot_layout(ncol = 2, guides = "collect") +
  NULL
final.plot

ggsave(final.plot, file="figures/nitrate-n-dot.pdf", units = "in", 
       width = 16, height = 6)


# Final Poster code ------

####################
########################
##############################
#################################
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

#mutate(type = fct_relevel(type, "S", "L")) to sequence S first then L
#mutate(crop = fct_relevel(crop, "F", "WPC", "GPC", "AR", "CR", "PCRO" )) to sequence 

mahadi.df <-  mahadi.df |>
  mutate(type = as.factor(type),
         crop = as.factor(crop)) |> 
  mutate(type = fct_relevel(type, "S", "L")) |> 
  mutate(crop = fct_relevel(crop, "PCRO", "CR", "AR", "GPC", "WPC", "F" ))

# this will show the levels
levels(mahadi.df$crop)

scale_color <- scale_color_manual(
  name = "Lysimeter \nDepth",          # Legend title
  labels = c(
    "45cm", "90cm"),          # Set labels matching the type column
  values = c("coral", "cyan3")         # Set colors for '90cm' and '45cm'
)

scale_fill <- scale_fill_manual(
  name = "Lysimeter \nDepth",          # Legend title
  labels = c(
    "45cm", "90cm"),          # Set labels matching the type column
  values = c("coral", "cyan3")         # Set colors for '90cm' and '45cm'
)


#####################
isu.nplot <- mahadi.df |> 
  filter(farm == "ISU") |>
  ggplot(aes(x = porewater_no3_mgl, y = crop, color = type, fill = type)) + 
  stat_summary(fun = mean, geom = "col",  # Changed to geom_col() for better bar orientation
               position = position_dodge(width = 0.9), alpha = 0.5) + 
  stat_summary(fun.data = mean_se, geom = "errorbar", 
               width = 0.2, position = position_dodge(width = 0.9)) +  # Adding error bars for mean SE
  scale_color + 
  scale_fill + 
  coord_cartesian(xlim = c(0, 30)) +     # Adjust xlim to fit Nitrate-N range
  labs(title = "A) ISU", x = "Nitrate-N (mg/L)", y = "Crop") +  # Correct axis labels
  theme_classic() + 
  geom_vline(xintercept = c(0, 10, 20, 30), linetype = "dashed", color = "black",alpha=0.2)  # Add dashed vertical lines
isu.nplot


########################################################
wiu.nplot<-mahadi.df |> 
  filter(farm == "WIU") |>
  ggplot(aes(x = porewater_no3_mgl, y = as.factor(crop), color = type, fill = type)) + 
  stat_summary(fun = mean, geom = "col",  # Changed to geom_col() for better bar orientation
               position = position_dodge(width = 0.9), alpha = 0.5) + 
  stat_summary(fun.data = mean_se, geom = "errorbar", 
               width = 0.2, position = position_dodge(width = 0.9)) +  # Adding error bars for mean SE
  scale_color + 
  scale_fill + 
  coord_cartesian(xlim = c(0, 30)) +     # Adjust xlim to fit Nitrate-N range
  labs(title = "B) WIU", x = "Nitrate-N (mg/L)", y = "Crop") +  # Correct axis labels
  theme_classic() + 
  geom_vline(xintercept = c(0, 10, 20, 30), linetype = "dashed", color = "black",alpha=0.2)  # Add dashed vertical lines
wiu.nplot


final.nplot <-
  isu.nplot + theme_classic(base_size = 26)+ 
  theme( strip.text = element_blank(), strip.background = element_blank())+
  wiu.nplot + theme_classic(base_size = 26)+ 
  theme( axis.title.y=element_blank(), axis.text.y=element_blank(), strip.background = element_blank()) +
  plot_layout(ncol = 2, guides = "collect") +
  # plot_annotation(tag_levels = "A", tag_suffix = ")")
  NULL
final.nplot

ggsave(final.nplot, file="figures/nitrate-n-dot.png", units = "in", 
       width = 16, height = 6)



#############ppppppppp# # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
isu.pplot<-mahadi.df |> 
  filter(farm == "ISU") |>
  ggplot(aes(x =porewater_drp_ugl, y = as.factor(crop), color = type, fill = type)) + 
  stat_summary(fun = mean, geom = "col",  # Changed to geom_col() for better bar orientation
               position = position_dodge(width = 0.9), alpha = 0.5) + 
  stat_summary(fun.data = mean_se, geom = "errorbar", 
               width = 0.2, position = position_dodge(width = 0.9)) +  # Adding error bars for mean SE
  scale_color + 
  scale_fill + 
  coord_cartesian(xlim = c(0, 230)) +     # Adjust xlim to fit Nitrate-N range
  labs(title = "A) ISU", x = "DRP(μg/L)", y = "Crop") +  # Correct axis labels
  theme_classic() + 
  geom_vline(xintercept = c(0, 50, 100, 150, 200), linetype = "dashed", color = "black",alpha=0.2)  # Add dashed vertical lines
isu.pplot


########################################################
wiu.pplot<-mahadi.df |> 
  filter(farm == "WIU") |>
  ggplot(aes(x = porewater_no3_mgl, y = as.factor(crop), color = type, fill = type)) + 
  stat_summary(fun = mean, geom = "col",  # Changed to geom_col() for better bar orientation
               position = position_dodge(width = 0.9), alpha = 0.5) + 
  stat_summary(fun.data = mean_se, geom = "errorbar", 
               width = 0.2, position = position_dodge(width = 0.9)) +  # Adding error bars for mean SE
  scale_color + 
  scale_fill + 
  coord_cartesian(xlim = c(0, 15)) +     # Adjust xlim to fit Nitrate-N range
  labs(title = "B) WIU", x = "DRP(μg/L)", y = "Crop") +  # Correct axis labels
  theme_classic() + 
  geom_vline(xintercept = c(0, 5, 10, 15), linetype = "dashed", color = "black",alpha=0.2)  # Add dashed vertical lines
wiu.pplot


final.pplot <-
  isu.pplot + theme_classic(base_size = 26,)+ 
  theme( strip.text = element_blank(), strip.background = element_blank())+
  wiu.pplot + theme_classic(base_size = 26,)+ 
  theme( axis.title.y=element_blank(), axis.text.y=element_blank(), strip.background = element_blank()) +
  plot_layout(ncol = 2, guides = "collect") +
  NULL
final.pplot

ggsave(final.pplot, file="figures/nitrate-p-dot.png", units = "in", 
       width = 16, height = 6)

#############ammonia###################
#########################################
#####################
isu.aplot<-mahadi.df |> 
  filter(farm == "ISU") |>
  ggplot(aes(x =porewater_nh3_mgl, y = as.factor(crop), color = type, fill = type)) + 
  stat_summary(fun = mean, geom = "col",  # Changed to geom_col() for better bar orientation
               position = position_dodge(width = 0.9), alpha = 0.5) + 
  stat_summary(fun.data = mean_se, geom = "errorbar", 
               width = 0.2, position = position_dodge(width = 0.9)) +  # Adding error bars for mean SE
  scale_color + 
  scale_fill +  
  coord_cartesian(xlim = c(0, 1)) +     # Adjust xlim to fit Nitrate-N range
  labs(title = "A) ISU", x = "Ammonia(mg/L)", y = "Crop") +  # Correct axis labels
  theme_classic() + 
  geom_vline(xintercept = c(0, 0.25, 0.50, 0.75, 1.0), linetype = "dashed", color = "black",alpha=0.2)  # Add dashed vertical lines
isu.aplot


########################################################
wiu.aplot<-mahadi.df |> 
  filter(farm == "WIU") |>
  ggplot(aes(x = porewater_nh3_mgl, y = as.factor(crop), color = type, fill = type)) + 
  stat_summary(fun = mean, geom = "col",  # Changed to geom_col() for better bar orientation
               position = position_dodge(width = 0.9), alpha = 0.5) + 
  stat_summary(fun.data = mean_se, geom = "errorbar", 
               width = 0.2, position = position_dodge(width = 0.9)) +  # Adding error bars for mean SE
  scale_color + 
  scale_fill +  
  coord_cartesian(xlim = c(0, 0.15)) +     # Adjust xlim to fit Nitrate-N range
  labs(title = "B) WIU", x = "Ammonia(mg/L)", y = "Crop") +  # Correct axis labels
  theme_classic() + 
  geom_vline(xintercept = c(0, 0.05, 0.10, 0.15), linetype = "dashed", color = "black",alpha=0.2)  # Add dashed vertical lines
wiu.aplot


final.aplot <-
  isu.aplot + theme_classic(base_size = 26,)+ 
  theme( strip.text = element_blank(), strip.background = element_blank())+
  wiu.aplot + theme_classic(base_size = 26,)+ 
  theme( axis.title.y=element_blank(), axis.text.y=element_blank(), strip.background = element_blank()) +
  plot_layout(ncol = 2, guides = "collect") +
  NULL
final.aplot

ggsave(final.aplot, file="figures/nitrate-a-dot.png", units = "in", 
       width = 16, height = 6)

