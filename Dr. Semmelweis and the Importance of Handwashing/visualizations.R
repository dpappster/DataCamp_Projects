library(ggplot2)
library(dplyr)
library(readr)

monthly_deaths <- read_csv("data/monthly_deaths.csv")
yearly_deaths <- read_csv("data/yearly_deaths_by_clinic.csv")

View(yearly_deaths)
View(monthly_deaths)

yearly_deaths <- yearly_deaths %>%
  mutate(proportion_deaths = deaths/births, used_handwashing = clinic == "clinic 2")

ggplot(yearly_deaths, aes(x = year, y = proportion_deaths, color = clinic)) +
    geom_line() +
    labs(title = "Proportion of Deaths by Clinic", x = "Year", 
        y = "Proportion of Deaths") +
    theme(legend.position = "top", legend.title = element_blank())

monthly_deaths <- monthly_deaths %>%
  mutate(proportion_deaths = deaths/births, 
         handwashing_start = date > "1847-05-01",
         color = ifelse(handwashing_start == TRUE, 'After', "Before"))

ggplot(monthly_deaths, aes(x = date, y = proportion_deaths, color = color)) +
  geom_line() +
  labs(title = "Effect of Handwashing on Proportion of Deaths at Clinic 1", 
       x = "Date", y = "Proportion Deaths") + 
  theme(legend.position = "top") + 
  geom_vline(xintercept = as.numeric(monthly_deaths$date[77]), 
             linetype = "dashed", size = 1, color = "dimgrey")  +
  theme(legend.title = element_blank())