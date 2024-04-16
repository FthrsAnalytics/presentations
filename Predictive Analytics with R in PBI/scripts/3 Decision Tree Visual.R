#Load Packages
library(rpart)
library(rpart.plot)

#Manipulate
model_data <- dataset

#Create Categorical
rpQ_bin <- cut(model_data$rev_per_Q,
                breaks = c(0, 2.5, 5.5, 100),
                labels = c("Low", "Medium", "High"),
                right = FALSE,
                include.lowest = TRUE
)
model_data$rpQ_bin <- rpQ_bin

#Decision Tree
decision_tree <- rpart(rpQ_bin ~ Income + Selected_By_Percent + Aff_Client_Meetings + Client_Meetings + Admin + Position_Short, data = model_data, method = "class", cp = 0.03)

#Visualize
rpart.plot(decision_tree)