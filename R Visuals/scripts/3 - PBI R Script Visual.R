# Visualize Data
ggplot(dataset, aes(x = Price, y = Points)) +
  geom_point(aes(color = Position), position = "jitter") +
  geom_smooth(method = lm, se = FALSE)
