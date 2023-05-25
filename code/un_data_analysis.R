library(tidyverse)

#Read in data
gapminder_data <- read_csv("data/gapminder_data.csv")

#-----What is the mean life expectancy?------
#summarize()

summarize(gapminder_data, averageLifeExp= mean(lifeExp))

gapminder_data %>% summarize(averageLifeExp =mean(lifeExp)) ##using pipe operator, but not save yet

gapminder_data_summarized <- gapminder_data %>%  ##save the result
  summarize(averageLifeExp =mean(lifeExp))


#What is the mean population for the most recent year?
gapminder_data_pop <- gapminder_data %>%  
  summarize(averagePop =mean(pop))

#What is the mean population and the mean life expectancy  for the most recent year?
gapminder_data %>% summarize(averageLifeExp =mean(lifeExp), averagePop =mean(pop))


#What is the mean life expectancy for the most recent year?------
#filter()
#max()

gapminder_data %>% 
  summarize(mostYear =max(year))

gapminder_data %>% 
  filter(year == 2007) %>%  #== is it equal to?
  summarize(meanLifeExp = mean(lifeExp))

#Doing in one step
gapminder_data %>% 
  filter(year == max(year)) %>%
  summarize(meanLifeExp = mean(lifeExp))

#mean GDP per capita for the first/earlist year
gapminder_data %>% 
  filter(year == min(year)) %>%
  summarize(meanGDP = mean(gdpPercap))

# > < !=(not equal to) 

#-----What is the mean life expectancy for EACH year-------
#group_by()

gapminder_data %>%   ##showed there are 12 groups but nothing changed
  group_by(year) 

gapminder_data %>%   ##
  group_by(year) %>% 
  summarize(meanLifeExp = mean(lifeExp))

#What is the mean life expectancy for each continent? (here only printed but not saved yet)
gapminder_data %>%   ##
  group_by(continent) %>% 
  summarize(meanLifeExp = mean(lifeExp))

#What is the mean life expectancy AND mean GDP per capita for each continent
#in a single result tibble
gapminder_data %>% 
  group_by(continent) %>% 
  summarize(meanLifeExp = mean(lifeExp), meanGDP = mean(gdpPercap))

#What is the GDP (not per capita)?
#mutate() #making a new column and attach to the dataset

gapminder_data %>% 
  mutate(gdp = gdpPercap * pop )

#Make a new column for population in millions popInMillions
gapminder_data %>% 
  mutate(popInMillions = pop/1000000 )

#Make two in the same dataset
gapminder_data %>% 
  mutate(gdp = gdpPercap * pop, popInMillions = pop/1000000 )


#Overwritten gapminder_data (not preferred)
gapminder_data <- gapminder_data %>% 
  mutate(gdp = gdpPercap * pop, popInMillions = pop/1000000 )
#saved in a new object
gapminder_data_popmil <- gapminder_data %>% 
  mutate(gdp = gdpPercap * pop, popInMillions = pop/1000000 )

#----select(): chooese a subset of columns from a dataset----
gapminder_data %>% 
  select(year, pop)

#select all except a particular column(continent)!
gapminder_data %>% 
  select(-continent)

#Create a tibble with only country, continent, year, lifeExp
gapminder_data %>% 
  select(-pop, -gdpPercap)

#select helper function: starts_with(), ends_with(), contains()----
gapminder_data %>% 
  select(year, starts_with("co")) #select year, everything start with "co"

-----#Vectors using c()---- Detour

my_vec <- c()
my_vec <- c("dog", "cat", "horse")
num_vec <- c(1,2,3,4)

proof <- gapminder_data %>% 
  pull(year)

your_data %>% 
  filter(id= "id_of_interest1" & id= "id_of_interest2")
your_data %>% 
  filter(id %in% c("id1, id2, id3")) #faster way

#----------------Detour---

#pivot_longer() and pivot_wider()

gapminder_data %>% 
  select(country, continent, year, lifeExp) %>% 
  pivot_wider(names_from = year, values_from = lifeExp)

#pivot_wider, but poplate values with gdpPercap
gapminder_data %>% 
  select(country, continent, year, gdpPercap) %>% 
  pivot_wider(names_from = year, values_from = gdpPercap)

#pivot_longer 
  gapminder_data %>% 
    pivot_longer(cols=c(pop, lifeExp, gdpPercap),  #using c() !!
                 names_to = "measurment_type",
                 values_to = "measurement")
  
#Is there a relationship between GDP and CO2 emissions? focsued on 2007

#
  assign the result to gapminder_data_2007
#filter for 2007 & continent Americas
#remove (using select) the year and continent columns

gapminder_data_2007 <- gapminder_data %>% 
  filter (year==2007, continent=="Americas") %>% #both 2007 and Americas
  #filter (year==2007 & continent=="Americas") #both 2007 and Americas
  # filter (year==2007 | continent=="Americas") %>% # this is OR
  select(-year, -continent)

#Read in the CO2 data
co2_emission_dirty<- read_csv("data/co2-un-data.csv", skip = 2, #skip first 2 lines of the file
         col_names = c("region","country", "year", "series",
                       "values", "footnotes", "source")) #correct and making column names
  
co2_emission<- co2_emission_dirty %>% 
  select(country, year, series, values) %>% 
  mutate(series = recode(series, "Emissions (thousand metric tons of carbon dioxide)" = "total_emissions",
                         "Emissions per capita (metric tons of carbon dioxide)" = "per_capita_emission")) %>% 
  pivot_wider(names_from = series, values_from = values) %>% 
  filter(year ==2005) %>% 
  select(-year)

#inner_join
inner_join(gapminder_data_2007, co2_emission, by="country")


