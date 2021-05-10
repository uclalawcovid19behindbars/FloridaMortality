rm(list=ls())
library(tidyverse)
library(parallel)
library(lubridate)
library(tidyverse)
library(behindbarstools)

death_df <- "./data/cleaned/FL_death_roster_historical_by_FLDHS.csv" %>%
    read_csv() %>%
    mutate(Age_Group = ifelse(Age_Group == "85+", "75-84", Age_Group)) %>%
    group_by(Year, Month, Age_Group) %>%
    summarize(Deaths = sum(Deaths), .groups = "drop") %>%
    filter(Age_Group != "15-19") %>%
    mutate(Date = ymd(str_c(Year, "-", Month, "-1"))) %>%
    select(Date, Age_Group, Deaths) %>%
    filter(year(Date) < 2021)

pop_df <- "./data/cleaned/21.3.6_fl_agg_new_cat_(2000-2021).csv" %>%
    read_csv(col_types = cols()) %>%
    mutate(Age_Group = ifelse(Age_Group == "75-85", "75-84", Age_Group)) %>%
    mutate(Age_Group = ifelse(Age_Group == "85+", "75-84", Age_Group)) %>%
    rename(Date = Anchor_Date) %>%
    group_by(Date, Age_Group) %>%
    summarize(Population = sum(Total, na.rm = TRUE), .groups = "drop") %>%
    filter(year(Date) >= 2015) %>%
    filter(year(Date) < 2021) %>%
    filter(!(Age_Group %in% c("10-14", "15-19")))

sex_df <- "./data/cleaned/21.3.6_fl_agg_new_cat_(2000-2021).csv" %>%
    read_csv(col_types = cols()) %>%
    filter(month(Anchor_Date) == 12) %>%
    mutate(Year = year(Anchor_Date)) %>%
    filter(Year >= 2015 & Year <= 2020) %>%
    group_by(Year, Sex) %>%
    summarise(Population = sum(Total)) %>%
    mutate(Pecent = Population / sum(Population)) %>%
    filter(Sex == "FEMALE")

agg_mx_df <- pop_df %>%
    left_join(death_df, by = c("Date", "Age_Group")) %>%
    mutate(Deaths = ifelse(is.na(Deaths), 0, Deaths)) %>%
    mutate(Age_Group = ifelse(Age_Group == "75-84", "75+", Age_Group))

median_age_df <- read_csv("./data/cleaned/median_age.csv") %>%
    select(Date = Anchor_Date, Median_Age) %>%
    filter(Date >= ymd("2015-01-01")) %>%
    filter(month(Date) == 12) %>%
    unique() %>%
    mutate(Year = year(Date)) %>%
    select(-Date)

agg_mx_df %>%
    mutate(Year = year(Date)) %>%
    group_by(Year) %>%
    summarize(
        Deaths = sum(Deaths),
        `Person-Months` = sum(Population)) %>%
    left_join(median_age_df, by = "Year") %>%
    left_join(sex_df, by = "Year") %>%
    mutate(Crude = Deaths / `Person-Months` * 12 * 100000)
