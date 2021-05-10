rm(list=ls())
library(tidyverse)
library(lubridate)

pop_df <- "./data/cleaned/21.3.6_fl_agg_new_cat_(2000-2021).csv" %>%
    read_csv(col_types = cols()) %>%
    mutate(Age_Group = ifelse(Age_Group == "75-85", "75-84", Age_Group))


agg_pop_df <- pop_df %>%
    rename(Date = Anchor_Date) %>%
    group_by(Date, Age_Group, Sex) %>%
    summarise(Population = sum(Total, na.rm = TRUE), .groups = "drop") %>%
    filter(year(Date) >= 2015)

agg_pop_df %>%
    group_by(Date, Age_Group) %>%
    summarise(Population = sum(Population), .groups = "drop_last") %>%
    mutate(Percent = Population / sum(Population) * 100) %>%
    ggplot(aes(x = Date, y = Percent, color = Age_Group)) +
    geom_line()

agg_pop_df %>%
    group_by(Date, Sex) %>%
    summarise(Population = sum(Population), .groups = "drop_last") %>%
    mutate(Percent = Population / sum(Population) * 100) %>%
    group_by(Sex) %>%
    summarise(max(Percent), min(Percent))

read_csv("./data/cleaned/median_age.csv") %>%
    select(Date = Anchor_Date, Median_Age) %>%
    filter(Date >= ymd("2015-01-01")) %>%
    unique() %>%
    mutate(Name = "") %>%
    ggplot(aes(x = Date, y = Median_Age, color = Name)) +
    geom_line(size = 2) +
    scale_color_bbdiscrete() +
    theme_behindbars() +
    theme(legend.position = "none") +
    ylab("Median Age")
