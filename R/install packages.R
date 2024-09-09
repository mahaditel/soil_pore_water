# install packages
install.packages("devtools") # install new things from developmental sources
install.packages("tidyverse") # dplyr and piping and ggplot etc
install.packages("lubridate") # dates and times
install.packages("readxl") # read in excel files
install.packages("janitor") # clean up excel imports
install.packages("patchwork") # arrange multiple plots per page
install.packages("skimr") # quick summary stats
install.packages("plotly") # cool ggplot things
install.packages("scales") # scales on ggplot axes

# you will load a subset of these each time you run R 
library(tidyverse) 
library(lubridate) 
library(scales) 
library(readxl) 
library(skimr) 
library(janitor) 
library(patchwork)


# Install these one time and run the library  - use in R studio
install.packages("ggThemeAssist") # helps reformat code - only run library one time
install.packages("styler") # allows you to reformat code to look like a pro!!

library(ggThemeAssist)
library(styler) 

# Install for stats
install.packages("car") # stats and ANOVA - essential 
install.packages("emmeans") # estimated marginal means for unbalanced designs 
install.packages("multcomView") # paired comparisons - note this will interfear with DPLYR!!
install.packages("Rmisc") # stats 
install.packages("Hmisc") # stats install.packages("broom") # output models cleanly 




