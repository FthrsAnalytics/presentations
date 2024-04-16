##Load packages needed for manipulation and visualization
library(readxl)
library(tidyverse)

##Define the examplefile connection string
examplefile <- ##Change this to the location of the Example Files .xlsx file
##It should be something like: "C:/Downloads/Prep Your Data/Example Files.xlsx""

##View all of the sheets in the Example File .xlsx file
excel_sheets(examplefile)

##Read data from Excel to example1 data frame
example1 <- read_excel(examplefile, sheet = "Example1")

View(example1)

#Manipulate example1 using dplyr verbs to create ex1_transform data frame
ex1_transform <- example1 %>% 
  select(Id, Total_Revenue, Days_Worked, Position) %>% #Select the four columns for analysis
  filter(Days_Worked > 365) %>% #Filter employees with 1+ years of tenure
  mutate(Rev_per_Day = Total_Revenue / Days_Worked) %>% #Create Revenue per Day column
  group_by(Position) %>% #Group employees by their position
  summarise(Avg_rpd = mean(Rev_per_Day)) %>% #Calculate the average revenue per day for each position
  arrange(desc(Avg_rpd)) #Order the data frame by average revenue per day in descending order

##Visualize ex1_transform
ggplot(ex1_transform, aes(x=Position, y=Avg_rpd, fill = Position)) +
  geom_bar(stat = "Identity") +
  labs(title = "Average revenue per day", 
       subtitle = "Employees with 1+ years of tenure", 
       y = "Average revenue per day")
