# Loading in required libraries
library(tidyverse)
library(readr)
library(ggplot2)

# Start coding here!
nobel <- read_csv("nobel.csv")

#What is the most commonly awarded gender and birth country? 
#Storing the string answers as top_gender and top_country.

nobel_gender <- nobel %>%
  group_by(sex) %>%
  count()

ggplot(nobel_gender, aes(x = sex, y = n)) +
    geom_col() +
    labs(title = "Nobel Wins by Gender", x = "Gender", y = "Prize Count")

ggplot(nobel, aes(x = sex, fill = category)) +
  geom_bar(position = "fill")

top_gender <- "Male"

nobel_country <- nobel %>%
  group_by(birth_country) %>%
  count() %>%
  arrange(desc(n)) %>%
  head()
nobel_country

ggplot(nobel_country, aes(x = reorder(birth_country, n), y = n)) +
    geom_col(stat = "identity") +
    labs(title = "Nobel Prizes by Birth Country", x = "Country", y = "Prize Count") +
    coord_flip()

top_country <- "United States of America"

#What decade had the highest proportion of US-born winners? 
#Store this as an integer called max_decade_usa.

nobel <- nobel %>%
  mutate(decade = year - year %% 10)

nobel_decade_usa <- nobel %>%
  filter(birth_country == "United States of America") %>%
  group_by(decade) %>%
  count() %>%
  arrange(desc(n))

ggplot(nobel_decade_usa, aes(x = decade, y = n)) +
    geom_col() +
    labs(title = "Nobel Prizes Awarded to US Citizens by Decade", x = "Decade", y = "Prize Count") +
    scale_x_continuous(breaks = seq(1900, 2020, by = 10))

max_decade_usa <- 2000

#What decade and category pair had the highest proportion of female laureates? 
#Store this as a list called max_female_list where the decade is the names of 
#the values are decade and category.

female_category_pair <- nobel %>%
  mutate(is_female = sex == "Female") %>%
  group_by(decade, category) %>%
  summarize(proportion_female = mean(is_female))

ggplot(female_category_pair, aes(x = decade, y = proportion_female)) +
    geom_col() +
    facet_wrap(~category) +
    labs(title = "Proportion of Female Nobel Prize Winners by Decade",
         x = "Decade", y = "Proportion Female")

max_female_list <- lst("decade" = 2010, "category" = "Peace")	

#Who was the first woman to receive a Nobel Prize, and in what category? 
#Save your string answers as first_woman_name and first_woman_category.

first_woman <- nobel %>%
  filter(sex == "Female") %>%
  slice_min(year, n = 3)
first_woman

first_woman_name <- "Marie Curie, née Sklodowska"
first_woman_category <- "Physics"

#Which individuals or organizations have won multiple Nobel Prizes throughout 
#the years? Store the full names in a list named repeats.

name_repeats <- nobel %>%
    group_by(full_name) %>%
    count() %>%
    filter(n>1)

repeats <- lst(name_repeats$full_name)

  