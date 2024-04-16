# Load Packages
library(tidyverse)
library(readxl)

# Import Data
file_path <- "C:/Users/jfthr/Documents/GitHub/presentations/"
xl_path <- "R Visuals/data/FPL Source Data.xlsx"

raw_data <- read_excel(paste0(file_path, xl_path), sheet = "Sheet1")

# Manipulate Data
data <- raw_data %>%
  select(Player, Team, Position, Price, Selected, Points, MP)

data1 <- data %>%
  mutate(PerMP = MP / 3420) %>%
  arrange(desc(PerMP))

data2 <- data1 %>%
  mutate(PointsPerMP = Points / MP, PointsPer90 = PointsPerMP * 90) %>%
  arrange(desc(PointsPerMP))

avgpoints <- data2 %>%
  filter(Points!= 0) %>%
  group_by(Position, Price) %>%
  summarise(AvgPoints = mean(Points)) %>%
  ungroup()

avgteampoints <- data2 %>%
  filter(Points!= 0) %>%
  group_by(Team, Position, Price) %>%
  summarise(AvgTeamPoints = mean(Points)) %>%
  ungroup()

avgmp <- data2 %>%
  filter(Points!= 0) %>%
  group_by(Position, Price) %>%
  summarise(AvgMP = mean(MP)) %>%
  ungroup()

avgteammp <- data2 %>%
  filter(Points!= 0) %>%
  group_by(Team, Position, Price) %>%
  summarise(AvgTeamMP = mean(MP)) %>%
  ungroup()

# Final Data Set
data3 <- left_join(data2, avgpoints, by = c("Position", "Price"))
data3 <- left_join(data3, avgteampoints, by = c("Team", "Position", "Price"))
data3 <- left_join(data3, avgmp, by = c("Position", "Price"))
data3 <- left_join(data3, avgteammp, by = c("Team", "Position", "Price"))