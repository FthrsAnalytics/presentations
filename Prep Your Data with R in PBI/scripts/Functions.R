##Load packages needed for manipulation and visualization
library(readxl)
library(tidyverse)

##Create folderpath: the location of GL statements
folderpath <- ##Change this to the location of the Source Files folder
##It should be something like: "C:/Downloads/Prep Your Data/Source Files"

exampleExcel <- paste(folderpath, "2017-12-31.xlsx", sep = "/")

exampleGL <- read_excel(exampleExcel, sheet = "CAD", col_names = FALSE)

View(exampleGL)

#1. Clean up the example GL statement (2017-12-31.xlsx)
exampleGL %>% 
  select(...1, ...2, ...12) %>% #Select Columns
  rename("GL" =  ...1,"Date" = ...2, "Balance" = ...12) %>%  #Rename Columns
  mutate("GL" = as.integer(GL)) %>% #Change GL to integer will introduce errors by coercion
  fill(GL) %>% #Fill Down GL
  filter(!is.na(Date), !is.na(Balance)) %>% #Filter Out Null Rows
  mutate("Date" = as.Date(as.integer(Date), origin = "1899-12-30"), "Balance" = as.integer(Balance)) #Change Data Types

#2. Create list of GL statements to clean
filelist <- paste(folderpath,list.files(folderpath), sep = "/")

#3. Convert step 1 into a function
fn_example_transform <- function(x) {
  read_excel(x, sheet = "CAD", col_names = FALSE) %>% 
    select(...1, ...2, ...12) %>%
    rename("GL" =  ...1,"Date" = ...2, "Balance" = ...12) %>% 
    mutate("GL" = as.integer(GL)) %>%
    fill(GL) %>%
    filter(!is.na(Date), !is.na(Balance)) %>%
    mutate("Date" = as.integer(Date)) %>% 
    mutate("Date" = as.Date(Date, origin = "1899-12-30")) %>%
    mutate("Balance" = as.integer(Balance))
}

#4. Apply the function to each GL statement
example3 <- map_dfr(filelist, fn_example_transform)

#5. Merge GL categories to the appended GL statements
Categories <- read_excel(examplefile, sheet = "Example3Merge")

ex3_transform <- example3 %>% 
  inner_join(Categories, by = "GL")

##Visualize ex3_transform
ex3_transform %>% 
  group_by(Date, Heading) %>% 
  summarise(Total_Balance = sum(Balance)) %>% 
  ggplot(aes(x = Date, y = Total_Balance, col = Heading)) +
  geom_line(size = 2) +
  labs(title = "GL Balances",
       col = "Category",
       x = "2018",
       y = "Category Balance")
