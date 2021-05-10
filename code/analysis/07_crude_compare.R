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

agg_mx_df <- pop_df %>%
    left_join(death_df, by = c("Date", "Age_Group")) %>%
    mutate(Deaths = ifelse(is.na(Deaths), 0, Deaths)) %>%
    mutate(Age_Group = ifelse(Age_Group == "75-84", "75+", Age_Group))

ind_mx_df <- bind_rows(lapply(1:nrow(agg_mx_df), function(i){
    tibble(
        Death = c(
            rep(1, agg_mx_df$Deaths[i]),
            rep(0, agg_mx_df$Population[i] - agg_mx_df$Deaths[i]))) %>%
        bind_cols(select(agg_mx_df, Date, Age_Group)[i,])})) %>%
    mutate(Year = year(Date)) %>%
    select(-Age_Group)

person_years <- nrow(ind_mx_df)

sims <- 10000

set.seed(123)
crude_sim_df <- bind_rows(mclapply(1:sims, function(j){

    ind_mx_df[sample.int(person_years, replace = TRUE),] %>%
        group_by(Year) %>%
        summarize(Deaths = sum(Death), Pop = n()/12, .groups = "drop") %>%
        mutate(Rate = Deaths / Pop, sim = j)

}, mc.cores = 10))

crude_sim_df %>%
    filter(Year >= 2019) %>%
    arrange(sim, Year) %>%
    group_by(sim) %>%
    summarise(delta = last(Rate)/first(Rate), .groups = "drop") %>%
    summarise(
        mu = mean(delta),
        lwr = quantile(delta, probs = .025),
        upr = quantile(delta, probs = .975))
