rm(list=ls())
library(tidyverse)
library(readxl)
library(tidycensus)

fl_2020 <- read_excel("~/Downloads/FLMX2020.xlsx", skip = 1)
fl_2019 <- read_excel("~/Downloads/FLMX2019.xlsx", skip = 1)
pop_df <- get_acs("state", table = "B01001")
vars_df <- load_variables(2019, "acs1") %>%
    filter(str_starts(name, "B01001_"))

trans_df <- vars_df %>%
    filter(!str_ends(label, "\\:")) %>%
    mutate(Age_Group = case_when(
        # str_ends(label, "15 to 17 years") ~ "15-19",
        # str_ends(label, "18 and 19 years") ~ "15-19",
        str_ends(label, "20 years") ~ "20-24",
        str_ends(label, "21 years") ~ "20-24",
        str_ends(label, "22 to 24 years") ~ "20-24",
        str_ends(label, "25 to 29 years") ~ "25-34",
        str_ends(label, "30 to 34 years") ~ "25-34",
        str_ends(label, "35 to 39 years") ~ "35-44",
        str_ends(label, "40 to 44 years") ~ "35-44",
        str_ends(label, "45 to 49 years") ~ "45-54",
        str_ends(label, "50 to 54 years") ~ "45-54",
        str_ends(label, "55 to 59 years") ~ "55-64",
        str_ends(label, "60 and 61 years") ~ "55-64",
        str_ends(label, "62 to 64 years") ~ "55-64",
        str_ends(label, "65 and 66 years") ~ "65-74",
        str_ends(label, "67 to 69 years") ~ "65-74",
        str_ends(label, "70 to 74 years") ~ "65-74",
        str_ends(label, "75 to 79 years") ~ "75+",
        str_ends(label, "80 to 84 years") ~ "75+",
        str_ends(label, "5 years and over") ~ "75+",
        TRUE ~ NA_character_)) %>%
    select(variable = name, Age_Group)


tot_pop_df <- pop_df %>%
    filter(NAME == "Florida") %>%
    left_join(trans_df, by = "variable") %>%
    filter(!is.na(Age_Group)) %>%
    group_by(Age_Group) %>%
    summarize(Pop = sum(estimate), .groups = "drop")

mx_df <- bind_rows(
    fl_2020 %>%
        filter(`...1` == "FLORIDA") %>%
        .[1,] %>%
        .[,3:(ncol(.)-1)] %>%
        mutate(Year = 2020),

    fl_2019 %>%
        filter(`...1` == "FLORIDA") %>%
        .[1,] %>%
        .[,3:(ncol(.)-1)] %>%
        mutate(Year = 2019)) %>%
    pivot_longer(-Year, names_to = "Age_Group", values_to = "Deaths") %>%
    filter(!(Age_Group %in% c("<1", "1-4", "5-9", "10-14", "15-19"))) %>%
    mutate(Age_Group = ifelse(
        Age_Group %in% c("75-84", "85+"), "75+", Age_Group)) %>%
    group_by(Year, Age_Group) %>%
    summarise_all(sum) %>%
    ungroup() %>%
    left_join(tot_pop_df) %>%
    mutate(qx = Deaths/ Pop)

ag <- length(unique(mx_df$Age_Group))

mx_df %>%
    group_by(Year) %>%
    mutate(min_age = as.numeric(str_sub(Age_Group, 1, 2))) %>%
    {suppressWarnings(mutate(., max_age = as.numeric(str_sub(Age_Group, 4, 5))))} %>%
    mutate(n = max_age - min_age + 1) %>%
    mutate(px = (1-qx)^n, lx = cumprod(c(1, px))[1:ag]) %>%
    mutate(nax=ifelse(!is.na(n), n/2, 25)) %>%
    mutate(nqx=ifelse(!is.na(lead(lx)), 1-(lead(lx) / lx), 1)) %>%
    mutate(ndx=ifelse(!is.na(lead(lx)), lx-lead(lx), lx)) %>%
    mutate(nLx=ifelse(!is.na(n), n*lead(lx) + nax*ndx, log(.5)/log(1-qx))) %>%
    mutate(nmx=ndx/nLx) %>%
    mutate(Tx=rev(cumsum(rev(nLx)))) %>%
    mutate(ex=Tx/lx, eplusx=ex+min_age) %>%
    select(Year, Age_Group, min_age, ex, eplusx) %>%
    ungroup() %>%
    filter(min_age == min(min_age))

