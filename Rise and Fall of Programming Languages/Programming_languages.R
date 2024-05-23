# Use this cell to begin your analysis, and add as many as you would like!
library(readr)
library(dplyr)
library(ggplot2)

stack_overflow <- read_csv("datasets/stack_overflow_data.csv")
head(stack_overflow)

#What fraction of the total number of questions asked in 2019 had the R tag? 
#Save your answer as a variable r_percentage in percentage format (e.g. 0.5 becomes 50).

r_percentage <- stack_overflow %>%
  filter(year == "2019", tag == "r") %>%
  summarize(percent_r = number/year_total * 100)

top10_2019 <- stack_overflow %>%
    filter(year == "2019") %>%
    arrange(desc(number)) %>%
    slice_max(number, n = 10)

ggplot(top10_2019, aes(x = reorder(tag, number), y = number)) +
    geom_col(fill = "mediumaquamarine") +
    coord_flip() +
    labs(title = "Top 10 Tags of 2019", y = "Number of Mentions", x = "Tag")
    

#What were the five most asked-about tags in the last 5 years (2015-2020)? 
#Save your answer as a variable highest_tags in the form of character vector.

pop_tags <- stack_overflow %>%
  filter(year %in% as.character(c(2015:2020))) %>%
  group_by(tag) %>%
  summarize(top_tags = sum(number)) %>%
  arrange(desc(top_tags)) %>%
  slice_max(top_tags, n = 10)
pop_tags

highest_tags <- pop_tags$tag[1:5]

ggplot(pop_tags, aes(x = reorder(tag, top_tags), y = top_tags)) +
    geom_col(stat = "identity", fill = "slateblue") +
    labs(title = "Top Tags for the Years 2015-2020", x = "Tag", 
        y = "Number of mentions") +
        scale_y_continuous(n.breaks = 7)
