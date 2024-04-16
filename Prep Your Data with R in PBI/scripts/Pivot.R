##Load packages needed for manipulation and visualization
library(readxl)
library(tidyverse)

##Define the examplefile connection string
examplefile <- ##Change this to the location of the Example Files .xlsx file
##It should be something like: "C:/Downloads/Prep Your Data/Example Files.xlsx"

##Read data from Excel to example2 data frame
example2 <- read_excel(examplefile, sheet = "Example2")

View(example2)

##Manipulate example2 using spread() and lag() to create ex2_transform data frame
ex2_transform <- example2 %>% 
  group_by(CustomerNum, Category) %>%
  spread(Category, SumBalance) %>% #Pivot Category column, fill with values from SumBalance column
  arrange(BusinessDate, .by_group = TRUE) %>% #Order rows by date within Customer grouping
  mutate_at(.vars = vars("Investable Assets", "Loan"), .funs = list(change = ~. - lag(.))) %>% #Create change in IA and Loan columns by subtracting value from previous row
  select(-`Investable Assets`, -Loan) %>% #Remove balance columns
  filter(BusinessDate == max(BusinessDate)) #Remove all rows except for most recent date

##Visualize ex2_transform
ex2_transform %>% 
  ggplot(aes(x=Loan_change, y=`Investable Assets_change`, col = as.factor(CustomerNum))) +
  geom_point(size = 5) +
  labs(title = "Change in investable assets vs loans", 
       col = "Customer", 
       x = "Change in loan", 
       y = "Change in investable assets")
