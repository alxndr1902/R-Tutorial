library("tidyverse")

# view data
?msleep
glimpse(msleep)
View(msleep)

# rename a variable
msleep
msleep %>% 
  rename("conserv" = "conservation") %>%  # renaming conservation column to conserve  
  glimpse()

# reorder variables
msleep %>% 
  select(vore, name, everything()) #reorder vore, name, then else column

# change a variable type
class(msleep$vore) # vore is chr type
msleep$vore <- as.factor(msleep$vore) #change vore column to factor type 
glimpse(msleep)

msleep %>% 
  mutate(vore = as.character(vore)) %>%  #change vore column to character
  glimpse()

# select variables to work with
names(msleep) # show all variables
msleep %>% 
  select(2:4, # choose 2, 3, and 4 variables
         awake, 
         starts_with("sleep"), # choose any variable that starts with "sleep" word
         contains("wt")) # chose any variable that contains "wt"

# Filter and arrange data
unique(msleep$order)

msleep %>% 
  filter((order == "Carnivora" | # filter order column that contain "Carnivora" or
            order == "Primates") & # contain Primates and
           sleep_total > 8) %>%  # sleep total more than 8
  select(name, order, sleep_total) %>% 
  arrange(-sleep_total) %>% # arrange the data from biggest sleep total to lowest
  View
  
msleep %>% 
  filter(order %in% c("Carnivora", "Primates") & # same as above
           sleep_total > 8) %>% 
  select(name, order, sleep_total) %>% 
  arrange(order) %>% # arrange order column alphabetically
  View

# Change Observations (mutate)
msleep %>% 
  mutate(brainwt = brainwt * 1000) %>% 
  View

msleep %>% 
  mutate(brainwt_in_grams = brainwt * 1000) %>% # createa a new column named brainwt_in_grams
  View

# Conditional changes(if_else)
msleep$brainwt
msleep$brainwt > 0.1

size_of_brain <- msleep %>% 
  select(name, brainwt) %>% 
  drop_na(brainwt) %>% # remove n/a data in brainwt column
  mutate(brain_size = if_else(brainwt > 0.01, # create a column named brain_size
                              "large", # "large" if brainwt more than 0.01
                              "small")) # otherwise it's "small"
size_of_brain

# Recode data and rename a variable

size_of_brain %>% 
  mutate(brain_size = recode(brain_size,
                             "large" = 1,
                             "small" = 2))

# reshape the data from long format to wide format
library(gapminder)
View(gapminder)

data <- select(gapminder, country, year, lifeExp)
View(data)

wide_data <- data %>% 
  # this will make a new column made from each value in year column
  pivot_wider(names_from = year, values_from = lifeExp)
view(wide_data)

# reshape data from wide format to long format
long_data <- wide_data %>% 
  pivot_longer(2:13, 
               names_to = "year",
               values_to = "lifeExp")
View(long_data)
