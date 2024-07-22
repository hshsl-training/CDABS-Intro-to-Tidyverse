##  ------ Installing and Loading Packages ------------------
# install tidyverse if you haven't yet
install.packages("tidyverse")
install.packages("viridis")
install.packages("usmap")

library(viridis)
library(usmap)

#import data

yearly_rates_joined <- read_csv("data/yearly_rates_joined.csv")
region_summary <- read_csv("data/region_summary.csv")


##  ------ ggplot Basics ------------------
region_summary %>% 
  ggplot()


region_summary %>%
  ggplot(aes(x=region, y=avg_rate))



region_summary %>%
  ggplot(aes(x=region, y=avg_rate)) +
           geom_bar(stat = "identity")


##  ------ Mapping vs Setting Aesthetics ------------------
region_summary %>%
  ggplot(aes(x=region, y=avg_rate)) +
           geom_bar(stat = "identity",
                    fill="blue",
                    color="purple",
                    linewidth=1.5,
                    width = 0.8)



region_summary %>%
  ggplot(aes(x=region, y=avg_rate, fill=region)) +
           geom_bar(stat = "identity")



##  ------ Looking at measles case counts over time ------------------

# create a summary tibble
yearly_rates_joined %>% 
  group_by(Year) %>% 
  summarize(TotalCount = sum(TotalCount))


# build the graph
yearly_rates_joined %>% 
  group_by(Year) %>% 
  summarize(TotalCount = sum(TotalCount)) %>% 
  ggplot(aes(x=Year, y=TotalCount))


year_total_line <- yearly_rates_joined %>% 
  group_by(Year) %>% 
  summarize(TotalCount = sum(TotalCount)) %>% 
  ggplot(aes(x=Year, y=TotalCount)) + 
  geom_line()

year_total_line



year_total_line <- yearly_rates_joined %>% 
  group_by(Year) %>% 
  summarize(TotalCount = sum(TotalCount)) %>% 
  ggplot(aes(x=Year, y=TotalCount)) + 
  geom_line() +
  geom_point()

year_total_line



year_total_line <- yearly_rates_joined %>% 
  group_by(Year) %>% 
  summarize(TotalCount = sum(TotalCount)) %>% 
  ggplot(aes(x=Year, y=TotalCount)) + 
  geom_line() +
  geom_point() + 
  scale_x_continuous(breaks = seq(from=1900, to=2000, by=10))

year_total_line



year_total_line <- yearly_rates_joined %>% 
  group_by(Year) %>% 
  summarize(TotalCount = sum(TotalCount)) %>% 
  ggplot(aes(x=Year, y=TotalCount)) + 
  geom_line() +
  geom_point() + 
  scale_x_continuous(breaks = seq(from=1900, to=2000, by=10)) +
  geom_vline(xintercept = 1963, color = "red", linetype= "dashed") +
  annotate(geom = "label", x=1963, y=800000, label="1963: vaccine introduced")

year_total_line



year_total_line <- 
  yearly_rates_joined %>% 
  group_by(Year) %>% 
  summarize(TotalCount = sum(TotalCount)) %>% 
  ggplot(aes(x=Year, y=TotalCount)) + 
  geom_line() +
  geom_point() + 
  scale_x_continuous(breaks = seq(from=1900, to=2000, by=10)) +
  geom_vline(xintercept = 1963, color = "red", linetype= "dashed") +
  annotate(geom = "label", x=1963, y=800000, label="1963: vaccine introduced") +
  labs(title = "Measles Cases Decrease After Vaccine Introduced", x = "Year", y = "Total Measles Case Count")

year_total_line


##  ------ Saving Plots ------------------
## ggsave("figures/yearly_measles_count.png", plot = year_total_line)


##  ------ Working with Three variables ------------------

### ------ Making grouped graphs ------------------
yearly_rates_joined %>% 
  group_by(Year, region) %>% 
  summarize(TotalCount = sum(TotalCount)) %>% 
  ggplot(aes(x=Year, y=TotalCount, group=region, color=region)) + 
  geom_line() +
  scale_x_continuous(breaks = seq(from=1900, to=2000, by=10)) +
  geom_vline(xintercept = 1963, color = "red", linetype= "dashed") +
  annotate(geom = "label", x=1963, y=800000, label="1963: vaccine introduced") +
  labs(title = "Measles Cases Decrease After Vaccine Introduced", x = "Year", y = "Total Measles Case Count")



yearly_rates_joined %>% 
  group_by(Year, region) %>% 
  summarize(TotalCount = sum(TotalCount)) %>% 
  ggplot(aes(x=Year, y=TotalCount, group=region, color=region)) + 
  geom_line(linewidth=1) +
  scale_x_continuous(breaks = seq(from=1900, to=2000, by=10)) +
  geom_vline(xintercept = 1963, color = "red", linetype= "dashed") +
  annotate(geom = "label", x=1963, y=400000, label="1963: vaccine introduced") +
  labs(title = "Measles Cases Decrease After Vaccine Introduced", x = "Year", y = "Total Measles Case Count") +
  scale_color_viridis_d()



yearly_rates_joined %>% 
  group_by(Year, region) %>% 
  summarize(avg_rate = mean(epi_rate)) %>% 
  ggplot(aes(x=Year, y=avg_rate, group=region, color=region)) + 
  geom_line(linewidth=1) +
  scale_x_continuous(breaks = seq(from=1900, to=2000, by=10)) +
  geom_vline(xintercept = 1963, color = "red", linetype= "dashed") +
  annotate(geom = "label", x=1963, y=8000, label="1963: vaccine introduced") +
  labs(title = "Measles Cases Decrease After Vaccine Introduced", x = "Year", y = "Total Measles Case Count") +
  scale_color_viridis_d() +
  theme_classic()

###  ------ Faceting and Small Multiples ------------------
yearly_rates_joined %>% 
  group_by(Year, region) %>% 
  summarize(avg_rate = mean(epi_rate)) %>% 
  ggplot(aes(x=Year, y=avg_rate, group=region, color=region)) + 
  geom_line(linewidth=1) +
  scale_x_continuous(breaks = seq(from=1900, to=2000, by=10)) +
  geom_vline(xintercept = 1963, color = "red", linetype= "dashed") +
  annotate(geom = "label", x=1963, y=8000, label="1963: vaccine introduced") +
  labs(title = "Measles Cases Decrease After Vaccine Introduced", x = "Year", y = "Total Measles Case Count") +
  scale_color_viridis_d() +
  facet_wrap(~region, nrow=2) +
  theme_classic()


###  ------ Highlighting ------------------

regional_rates <- yearly_rates_joined %>% filter(division=="South Atlantic" & between(Year, 1950, 1980))


tmp <- regional_rates %>%
  mutate(State2=State)

tmp %>%
  ggplot(aes(x=Year, y=epi_rate)) +
  geom_line(data=tmp %>% dplyr::select(-State), aes(group=State2), color="grey", linewidth=0.5, alpha=0.5) +
  geom_line(aes(color=state), color="#69b3a2", linewidth=1.2 ) +
  scale_x_continuous(breaks=seq(from=1950, to=1980, by=5)) +
 theme_minimal() +
  theme(
    legend.position="none",
    plot.title = element_text(size=14),
    panel.grid = element_blank()
  ) +
  ggtitle("A comparison of measles cases in the South Atlantic Region") +
  facet_wrap(~State, ncol = 2)

##  ------ Making Maps ------------------
plot_usmap()



plot_usmap(regions="counties")



measles1963df <- yearly_rates_joined %>% 
  filter(Year==1963) %>% 
rename(state=State)



plot_usmap(data=measles1963df, values = "epi_rate")



plot_usmap(data=measles1963df, values = "epi_rate") +
  scale_fill_viridis()



plot_usmap(data=measles1963df, values = "epi_rate") +
  scale_fill_viridis(direction = -1)



plot_usmap(data=measles1963df, values = "epi_rate") +
  scale_fill_viridis(option = "rocket", direction = -1) 



map_1963 <- plot_usmap(data=measles1963df, values = "epi_rate") +
  scale_fill_viridis(option = "rocket", direction = -1) +
  labs(title = "Incidence Rate of Measles per 1000 people in 1963")

ggsave(filename = "figures/map_1963.png", plot = map_1963, bg = "white")

