##  ------ Installing and Loading Packages ------------------
# install tidyverse if you haven't yet
# install.packages("tidyverse")

# This is a comment - we use these to document our code.

# load tidyverse
library(tidyverse)

# import csv file into R environment
read_csv("data/measles_us.csv")

# import csv files into R environment, assign to an object
measles_us <- read_csv("data/measles_us.csv")

states <- read_csv(file = "data/states.csv")


##  ------ Exploring Data ------------------

# opens data in spreadsheet view
View(measles_us)

# provides information about each column
summary(measles_us)

# makes columns easy to view
glimpse(measles_us)


# find out the distinct values in the ConditionName variable
distinct(measles_us, ConditionName)

# find out the distinct variables in the Admin1Name variable
distinct(measles_us, Admin1Name)


# find out the number of observations(rows) per distinct value in Admin1Name
count(measles_us, Admin1Name) 

# same as above but written with the pipe %>% 
measles_us %>% 
  count(Admin1Name)

### ----- Challenge ----
# Exploring the `states` tibble in our environment

# 1. Use `glimpse()` to inspect the columns and data types in the dataset.
glimpse(states)

# 2. Use `distinct()` to find out the distinct values in the `region` column.
states %>% distinct(region)

# 3. Using `count()`, find out how many states are in each region. 
states %>% count(region)

# 4. Using `count()`, find out how many states are in each region AND division. HINT: You can add additional column names to `distinct()` and `count()` to look at combinations of columns.
states %>% count(region, division)


##  ------ Subsetting Data ------------------

# create a tibble with just the Admin1Name and CountValue columns
measles_us %>% 
  select(Admin1Name, CountValue)

# create a tibble with all columns from ConditionName through Admin1ISO
measles_us %>% 
  select( ConditionName:Admin1ISO)

# create a tibble with the variables (columns) we want for analysis
measles_select <-
  measles_us %>%
    select(
     Admin1Name,
     PeriodStartDate,
     PeriodEndDate,
     PartOfCumulativeCountSeries,
     CountValue
)


# rename Admin1Name column to state
measles_select <-
  measles_select %>% 
    rename(state = Admin1Name)


# filter tibble to include only rows where Maryland is the state
measles_md <- measles_select %>% 
filter(state == "MARYLAND")


# filter tibble to include rows where state is MD and CountValue is greater than 500
measles_select %>%
  filter(state == "MARYLAND" & CountValue > 500)




# we would rather not have to write verbose code like this
measles_select %>% 
  filter(state == "MARYLAND" & state == "DELAWARE" & state == "Pennsylvania")


# filter to include rows that match the states$name vector of values (i.e. exclude territories)
measles_states_only <-
  measles_select %>% 
  filter(state %in% states$name)


# this is an alternate way of of accomplishing above
# the ! operator tells you to include everything that does not match the vector of values
measles_states_only <- measles_select %>% 
  filter(!state %in% c("PUERTO RICO", "GUAM", "AMERICAN SAMOA", "NORTHERN MARIANA ISLANDS", "VIRGIN ISLANDS, U.S.", "DISTRICT OF COLUMBIA"))


# this keeps only rows where the value is 0 in the PartofCumulativeCountSeries variable.
# this ensures each row corresponds to the counts for a distinct week
measles_non_cumulative <- 
  measles_states_only %>% 
  filter(PartOfCumulativeCountSeries==0)

# Review columns in new tibble
glimpse(measles_non_cumulative)


### ----- Challenge ----
# 1. Use `select()` to create a new tibble with just the `name` and `division` columns from the `states` tibble. Assign this to an object called `us_divisions`.
us_divisions <- 
  states %>% 
  select(name, division)
# 2. Use `filter()` to keep just the rows in the `South Atlantic` division of the `us_divisions` tibble. Assign this to an object called `sa_division`.
sa_division <- 
  us_divisions %>% 
  filter(division == "South Atlantic")

# 3. Use `filter()` to keep just the rows in the `measles_non_cumulative` tibble where the `state` matches one of the states in the `name` column of the `sa_division` tibble and where the `CountValue` is greater than 1000. Assign this to an object called `measles_sa`.
measles_sa <- 
  measles_non_cumulative %>% 
  filter(state %in% sa_division$name & CountValue > 1000)

##  ------ Mutate ------------------

# change PeriodStartDate and PeriodEndDate into Date data types
measles_non_cumulative <- measles_non_cumulative %>% 
  mutate(PeriodStartDate = mdy(PeriodStartDate),
       PeriodEndDate = mdy(PeriodEndDate))


#create a column called Year by extracting the Year of PeriodStartDate
measles_year <- 
  measles_non_cumulative %>% 
  mutate(Year=year(PeriodStartDate))


##  ------ Grouping and Summarizing ------------------

# group measles_year by year
yearly_count_state <-
  measles_year %>%
  group_by(Year)

yearly_count_state


#Get totals for each state each year.
yearly_count <-
  measles_year %>%
  group_by(Year) %>%
  summarise(TotalCount = sum(CountValue))

yearly_count

# group measles_year by Year and state, get counts of rows for each group
yearly_count_state <-
  measles_year %>%
  group_by(Year, state) %>%
  summarise(TotalCount = sum(CountValue))

yearly_count_state

##  ------ Arrange ------------------

# arrange tibble by TotalCount, ascending by default
yearly_count_state %>% 
  arrange(TotalCount)

# arrange tibble by TotalCount, descending with desc() function
yearly_count_state %>% 
  arrange(desc(TotalCount))

##  ------ Transforming Data ------------------

### ----------- Pivoting data ------------------

#import historical population data
hist_pop <-
  read_csv("data/Historical_Population_by_state.csv")

# pivot state name columns to one column called state. Put the values from those columns in a column called pop100
hist_pop_long <- hist_pop %>%
  pivot_longer(ALASKA:WYOMING,
               names_to = "state",
               values_to = "pop1000")


### ----------- Joining tibbles ------------------
joined_df <- yearly_count_state %>% 
  left_join(hist_pop_long, 
            by=join_by(state, 
                       Year == DATE))

joined_df


### ----- Challenge ----

# 1. Use mutate() to calculate the rate of measles per 100,000 persons (remember population is given in 1000s).
measles_yearly_rates <-
  joined_df %>% 
  mutate(rate = (TotalCount / pop1000)* 100)

# 2. Join the states tibble to the measles_yearly rates tibble. What variable do you need to join on? 
states_join <- measles_yearly_rates %>% 
  left_join(states, by = join_by(state == name))

