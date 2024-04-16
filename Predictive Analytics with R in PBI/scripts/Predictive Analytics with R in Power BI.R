#Packages must be installed before loading
#Current Line or Selection can be executed with (Ctrl + Enter)
#Load Packages
library(corrplot)
library(readxl)
library(rpart)
library(rpart.plot)
library(tidyverse)

#Import Data
#This needs to point to the location of the Source Data.xlsx file
file_path <- "C:/Users/jfthr/Desktop/R in PBI/Excel Source Data.xlsx"

data_set <- read_excel(file_path, sheet = "Source") %>% 
  select(Id, Total_Revenue, Income, Selected_By_Percent, Days_Worked, Aff_Client_Meetings, Client_Meetings, Admin, Reviews, Position_Short)
data_set$Position_Short <- factor(data_set$Position_Short, levels = c("As", "JA", "A", "SA"), ordered = TRUE)

#Inspect Data - We did this in Power BI
data_set %>%
  select(-Id, -Position_Short) %>% 
  plot()

data_set %>%
  select(Total_Revenue, Income, Selected_By_Percent, Days_Worked) %>% 
  plot()

#Correlation plot
data_set %>%
  select(-Id, -Position_Short) %>%
  mutate(Reviews1 = Reviews * -1) %>% 
  select(-Reviews) %>% 
  cor() %>% 
  corrplot(type = "lower")

#Density Plot - rev_per_Q
data_set %>% 
  mutate(rev_per_Q = Total_Revenue / Days_Worked * 90) %>%
  ggplot(aes(x=rev_per_Q)) +
  geom_density(fill =1)

#Inspect rev_per_Q Outliers
data_set %>% 
  mutate(rev_per_Q = Total_Revenue / Days_Worked * 90) %>%
  select(Id, rev_per_Q, Total_Revenue, Days_Worked) %>% 
  arrange(desc(rev_per_Q)) %>% 
  head(20)

#Filtered Density Plot - rev_per_Q
data_set %>% 
  filter(Days_Worked > 180) %>% 
  mutate(rev_per_Q = Total_Revenue / Days_Worked * 90) %>%
  ggplot(aes(x=rev_per_Q)) +
  geom_density(fill = 1)

#Create model_data data set
model_data <- data_set %>% 
  filter(Days_Worked > 180) %>% 
  mutate(rev_per_Q = Total_Revenue / Days_Worked * 90) %>%
  select(-Days_Worked, -Total_Revenue, -Reviews)

str(model_data)

#K-Means Clustering
model_data$Position_Short <- as.numeric(model_data$Position_Short)
set.seed(237)
cluster_model <- kmeans(model_data[,2:7], 3)

#Add K-Means Clustering Model Score to Data Set
model_data$cluster <- as.factor(cluster_model$cluster)

#Visualize K-Means Clustering
model_data %>% 
  gather(-Id, -cluster, -rev_per_Q, key = "var", value = "value") %>% 
  ggplot(aes(x = value, y = rev_per_Q, col = cluster, size = 1)) +
  geom_point(position = "jitter") +
  facet_wrap(~var, scale = "free")

#Create Linear Regression
linear_model <- lm(rev_per_Q ~ Income + Aff_Client_Meetings + Client_Meetings + Admin, data = model_data)

#Add Linear Regression Model Score to Data Set
model_data$score_lm <- predict(linear_model, model_data)

#Visualize Linear Regression Model
model_data %>% 
  gather(Income, Aff_Client_Meetings, Client_Meetings, Admin, key = "var", value = "value") %>% 
  ggplot(aes(x = value, y = rev_per_Q)) +
  geom_point(position = "jitter") +
  geom_smooth(method = lm) +
  facet_wrap(~var, scale = "free")

#Bin rev_per_Q variable for Decision Tree
rpQ_bin <- cut(model_data$rev_per_Q,
                breaks = c(0, 2.5, 5.5, 100),
                labels = c("Low", "Medium", "High"),
                right = FALSE,
                include.lowest = TRUE
)

#Add Binned rev_per_Q to Data Set
model_data$rpQ_bin <- rpQ_bin

#Create Decision Tree
decision_tree <- rpart(rpQ_bin ~ Income + Selected_By_Percent + Aff_Client_Meetings + Client_Meetings + Admin + Position_Short, data = model_data, method = "class", cp = 0.03)

#Add Decision Tree Model Score to Data Set
model_data$score_tree <- predict(decision_tree, model_data, type = "class", cp = 0.03)

#Visualize Decision Tree 
rpart.plot(decision_tree)
