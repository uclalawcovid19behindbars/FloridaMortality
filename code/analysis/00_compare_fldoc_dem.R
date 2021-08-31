rm(list=ls())
library(tidyverse)
library(parallel)
library(lubridate)
library(tidyverse)
library(behindbarstools)

# load in aggregate data
age_pop_df <- "./data/cleaned/21.3.11_fl_pop_rec_age_(2000-2021).csv" %>%
    read_csv(col_types = cols()) %>%
    # group by the anchor data and age group and add up populations
    group_by(Anchor_Date, Age_Group) %>%
    summarize(Population = sum(Total, na.rm = TRUE), .groups = "drop") %>%
    rename(Date = Anchor_Date) %>%
    # filter only to the date that most closely matches the FLDOC pop report
    filter(Date == ymd("2019-07-01")) %>%
    # calculate percentages
    mutate(Percentage = Population/ sum(Population)*100) %>%
    # change this so it gets sorted nicely
    mutate(Age_Group = ifelse(
        Age_Group == "Under 17", "17 & Under", Age_Group)) %>%
    arrange(Age_Group) %>%
    select(-Date)

# take our population data and compare to FLDOC
# looks like the distributions are close
age_pop_df %>%
    # and compare against the FLDOC numbers that were taken from pg 16 from
    # http://www.dc.state.fl.us/pub/annual/1819/FDC_AR2018-19.pdf
    mutate(
        FLDOC_Pop = c(92, 7771, 28417, 35400, 15628, 8318)) %>%
    mutate(FLDOC_Per = FLDOC_Pop / sum(FLDOC_Pop)*100) %>%
    mutate(FLDOC_Per - Percentage)

# our totals are slightly larger as well
age_pop_df %>%
    select(Population) %>%
    mutate(
        FLDOC_Pop = c(92, 7771, 28417, 35400, 15628, 8318)) %>%
    summarize_all(sum)

# note that this aggregate file we compared here has different aggregated age
# groups than what we use in file 11 in order to more directly compare to the
# FLDOC pop report
