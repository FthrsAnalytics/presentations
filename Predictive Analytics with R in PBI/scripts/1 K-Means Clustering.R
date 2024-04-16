#Load Packages
library(readxl)
library(tidyverse)

#Import Data
#This needs to point to the location of the Source Data.xlsx file
file_path <- "C:/Users/jfthr/Documents/GitHub/presentations/Predictive Analytics with R in PBI/data/Excel Source Data.xlsx"

data_set <- read_excel(file_path) %>% 
  select(Id, Total_Revenue, Income, Selected_By_Percent, Days_Worked, Aff_Client_Meetings, Client_Meetings, Admin, Position_Short)

data_set$Position_Short <- factor(data_set$Position_Short, levels = c("As", "JA", "A", "SA"), ordered = TRUE)

#Manipulate Data
model_data <- data_set %>% 
  filter(Days_Worked > 180) %>% 
  mutate(rev_per_Q = Total_Revenue / Days_Worked * 90) %>%
  select(-Days_Worked, -Total_Revenue)

#Create Clusters
set.seed(237)

cluster_model <- kmeans(model_data[,2:6,8], 3)

model_data$cluster <- as.factor(cluster_model$cluster)
