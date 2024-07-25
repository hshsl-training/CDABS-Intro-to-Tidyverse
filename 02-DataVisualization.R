##  ------ Installing and Loading Packages ------------------
# remove # from lines to install if you haven't yet
# install.packages("tidyverse")
# install.packages("viridis")
# install.packages("usmap")

library(tidyverse)
library(viridis)
library(usmap)

##  ------ Import Data ------------------

yearly_rates_joined <- read_csv("data/yearly_rates_joined.csv")

region_summary <- read_csv("data/region_summary.csv")


##  ------ Basic ggplot2 graph ------------------

region_summary %>%
  ggplot(aes(x=region, y=avg_rate)) +
           geom_bar(stat = "identity")


##  ------ Mapping versus Setting Aesthetics ------------------

### Setting color and fill manually
region_summary %>%
  ggplot(aes(x=region, y=avg_rate)) +
           geom_bar(stat = "identity",
                    fill="blue",
                    color="purple",
                    linewidth=1.5,
                    width = 0.8)


### Mapping fill aesthetic to region variable
region_summary %>%
  ggplot(aes(x=region, y=avg_rate, fill=region)) +
           geom_bar(stat = "identity")



##  ------ Telling a Data Story ------------------

# summarize data
yearly_count <- yearly_rates_joined %>% 
  group_by(Year) %>% 
  summarize(TotalCount = sum(TotalCount))



# Create a line graph

yearly_count_line <- yearly_count %>% 
  ggplot(aes(x=Year, y=TotalCount)) + # main layer, maps x and y axis to variables
  geom_line() + # creates a line layer
  geom_point() + # adds a point layer (so data points are marked with dots)
  scale_x_continuous(breaks = seq(from=1900, to=2000, by=10)) +  # Creates tick marks every 10 years
  geom_vline(xintercept = 1963, # add a red dashed vertical line to graph at the year 1963
             color = "red", 
             inetype= "dashed") +
  annotate(geom = "label", # add a label to the red line created above
           x=1963, 
           y=800000, 
           label="1963: vaccine introduced") +
  labs(title = "Measles Cases Decrease After Vaccine Introduced", # add a title and axis labels
       x = "Year", 
       y = "Total Measles Case Count")



## ----save plot, eval=FALSE, echo=TRUE-------------------------------------------------------------
ggsave("figures/yearly_measles_count.png", plot = yearly_count_line)


##  ------ Working with Three variables ------------------

# summarize data
regional_rates <- 
  yearly_rates_joined %>% 
  group_by(Year, region) %>% 
  summarize(avg_rate = mean(epi_rate, na.rm=TRUE))


# basic grouped line graph
regional_rates %>% 
  ggplot(aes(x=Year, y=avg_rate, group=region, color=region)) + 
  geom_line()
  


# working with the viridis color palette
regional_rates %>% 
  ggplot(aes(x=Year, y=avg_rate, group=region, color=region)) + 
  geom_line(linewidth=1) +
  scale_color_viridis(discrete=TRUE)  +
  scale_x_continuous(breaks = seq(from=1900, to=2000, by=10)) +
  labs(x="", 
       y= "Incidence Rate of measles \n per 100,000 persons", 
       color="Region")


# adjusting the theme

regional_rates %>% 
  ggplot(aes(x=Year, y=avg_rate, group=region, color=region)) + 
  geom_line(linewidth=1) +
  scale_color_viridis(discrete=TRUE)  +
  scale_x_continuous(breaks = seq(from=1900, to=2000, by=10)) +
  labs(title = "Measles Cases in the 20th Century", x="", y= "Average rate\nper 100,000", color="Region") +
  theme_classic() + # changes the overall theme
  theme(axis.title.y = element_text(angle=0,         # adjusts the y axis label to be horizontal and centered
                                    vjust = 0.5, 
                                    hjust = 0.5))


# creating small multiples

regional_rates %>% 
  ggplot(aes(x=Year, y=avg_rate, group=region, color=region)) + 
  geom_line(linewidth=1) +
  scale_color_viridis(discrete=TRUE)  +
  scale_x_continuous(breaks = seq(from=1900, to=2000, by=10)) +
  labs(title = "Measles Cases in the 20th Century", 
       x="", 
       y= "Average rate\nper 100,000", 
       color="Region") +
  facet_wrap(~region, nrow=2) + # breaks each region into a separate graph creates 2 x 2 grid of graphs
  theme_classic() +
  theme(axis.title.y = element_text(angle=0, vjust = 0.5, hjust = 0.5))


# using highlighting and small multiples

tmp <- regional_rates %>%
  mutate(region2=region)

tmp %>%
  ggplot(aes(x=Year, y=avg_rate)) +
  geom_line(data=tmp %>% dplyr::select(-region), aes(group=region2), color="grey", linewidth=0.5, alpha=0.5) +
  geom_line(aes(color=region), color="#69b3a2", linewidth=1.2 ) +
  scale_x_continuous(breaks = seq(from=1900, to=2000, by=10)) +
  theme_minimal() +
  theme(
    legend.position="none",
    plot.title = element_text(size=14),
    panel.grid = element_blank()
  ) +
  ggtitle("A comparison of measles cases by Region") +
  facet_wrap(~region, ncol = 2)


##  ------ Creating Maps ------------------


# usmap package basics
plot_usmap() # creates empty map with state borders

plot_usmap(regions="counties") # creates empty  map with county borders


# create a 1963 data set
measles1963df <- yearly_rates_joined %>% 
  filter(Year==1963)

# plot rates, save map
map_1963 <- 
  plot_usmap(data=measles1963df, values = "epi_rate") +
  scale_fill_viridis(option = "rocket", direction = -1) +
  labs(title = "Incidence Rate of Measles per 100,000 people in 1963")

#save file
ggsave(filename = "figures/map_1963.png", plot = map_1963, bg = "white")

