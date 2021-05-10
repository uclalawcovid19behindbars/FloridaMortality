rm(list=ls())
library(tidyverse)
library(parallel)
library(lubridate)
library(tidyverse)
library(behindbarstools)

age_pop_df <- "./data/cleaned/21.3.11_fl_pop_rec_age_(2000-2021).csv" %>%
    #str_c("21.3.6_fl_aggregate_corr_(1980-2040).csv") %>%
    read_csv() %>%
    group_by(Anchor_Date, Age_Group) %>%
    summarize(Population = sum(Total, na.rm = TRUE), .groups = "drop") %>%
    rename(Date = Anchor_Date) %>%
    filter(Date == ymd("2019-07-01")) %>%
    mutate(Percentage = Population/ sum(Population)*100) %>%
    mutate(Age_Group = ifelse(
        Age_Group == "Under 17", "17 & Under", Age_Group)) %>%
    arrange(Age_Group) %>%
    select(-Date)


age_pop_df %>%
    mutate(
        FLDOC_Pop = c(92, 7771, 28417, 35400, 15628, 8318)) %>%
    mutate(FLDOC_Per = FLDOC_Pop / sum(FLDOC_Pop)*100)
