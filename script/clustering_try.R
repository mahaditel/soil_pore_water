##install packages
install.packages("multcompView")
install.packages("fastDummies")

# 1. Load necessary libraries
library(lme4)
library(lmerTest)
library(car)
library(emmeans)
library(tidyverse)
library(multcompView)
library(fastDummies)

options(contrasts = c("contr.sum", "contr.poly"))
# options(contrasts = c("contr.treatment", "contr.poly"))

# 2. Load and Clean Data
# Assuming your file is in the working directory
df <- read_csv("Data/porewater_isu_s_and_l_24_25_plot.csv")

df<- df %>% mutate(block=as.factor(block),
                   crop= as.factor(crop),
                   type = as.factor(type))

#select the number of the variables
df1 <- df %>% 
  select(block, crop, porewater_no3_mgl, porewater_drp_mgl) %>% 
  drop_na()

## optional: scale variables so they have similar weight

X_scaled <- scale(df1 %>% select(porewater_no3_mgl, porewater_drp_mgl))

set.seed(123)



###decide the number of cluster____test
# Compute WSS for k = 1 to 10
wss <- sapply(1:10, function(k){
  kmeans(X_scaled, centers = k, nstart = 25)$tot.withinss
})

####plot
# Plot elbow curve
plot(1:10, wss,
     type = "b", pch = 19, frame = FALSE,
     xlab = "Number of clusters K",
     ylab = "Total within-clusters sum of squares (WSS)",
     main = "Elbow Method") ### 4 cluster seems perfect as after 4 it started to faltered 


######
k <- 4  # choose number of clusters
km_res <- kmeans(X_scaled, centers = k, nstart = 25)

print(km_res)

#add new column
df1$cluster <- factor(km_res$cluster)

####plot the non heretical clusters_no3
ggplot(df1, aes(x = block , y = porewater_no3_mgl)) +
  geom_point(aes(color = crop, shape = cluster), size = 3) +
  labs(
    color = "crop",
    shape = "Cluster",
    x = "block",
    y = "no3",
    title = "Non-hierarchical clustering (k-means) on NO3 & DRP"
  ) +
  theme_minimal()

####plot the non heretical clusters_drp
ggplot(df1, aes(x = block , y = porewater_drp_mgl)) +
  geom_point(aes(color = crop, shape = cluster), size = 3) +
  labs(
    color = "crop",
    shape = "Cluster",
    x = "block",
    y = "drp",
    title = "Non-hierarchical clustering (k-means) on NO3 & DRP"
  ) +
  theme_minimal()

####plot the non heretical clusters_crop
ggplot(df1, aes(x = crop , y = porewater_no3_mgl)) +
  geom_point(aes(color = crop, shape = cluster), size = 3) +
  labs(
    color = "block",
    shape = "Cluster",
    x = "block",
    y = "no3",
    title = "Non-hierarchical clustering (k-means) on NO3 & DRP"
  ) +
  theme_minimal()
  