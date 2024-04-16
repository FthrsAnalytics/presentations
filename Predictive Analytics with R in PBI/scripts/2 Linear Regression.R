model_data <- dataset
linear_model <- lm(rev_per_Q ~ Income + Aff_Client_Meetings + Client_Meetings + Admin, data = model_data)
model_data$score_lm <- predict(linear_model, model_data)