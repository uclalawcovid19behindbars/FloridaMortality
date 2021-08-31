rm(list=ls())
library(tidyverse)
library(lubridate)
library(ggpattern)

sa_gen_lx_df <- read_rds("data/results/sa_gen_lx_df.rds") %>%
    mutate(Group = "Florida State")
lx_df <- read_rds("data/results/lx_df.rds") %>%
    mutate(Group = "FLDOC")

# make sure the Death totals look reasonable
lx_df %>%
    group_by(Year) %>%
    summarize(
        Deaths = sum(COVID.Deaths + Other.Deaths),
        COVID.Deaths = sum(COVID.Deaths),
        )

comb_lx_df <- bind_rows(sa_gen_lx_df, lx_df) %>%
    filter(Year >= 2019)

# for age adjustment
pw_df <- lx_df %>%
    filter(Year == 2020) %>%
    mutate(PW = Population / sum(Population)) %>%
    select(Age_Group, PW)

# calculate yearly change in life expectancy for each group
comb_lx_df %>%
    filter(Age_Group == "20-24") %>%
    select(Group, Year, eplusx) %>%
    group_by(Group) %>%
    mutate(deltaex = eplusx - lag(eplusx))

# calculate the yearly change in asmr
comb_lx_df %>%
    select(Group, Year, Age_Group, qx) %>%
    left_join(pw_df, by = "Age_Group") %>%
    group_by(Group, Year) %>%
    summarise(asmr = sum(qx*PW), .groups = "drop_last") %>%
    mutate(asmr = asmr*100000) %>%
    mutate(pdiff = (asmr - lag(asmr)) / lag(asmr))

# ccalculate change in smr
comb_lx_df %>%
    select(Group, Year, Age_Group, qx) %>%
    mutate(qx = qx*100000) %>%
    arrange(Group, Age_Group, Year) %>%
    group_by(Group, Age_Group) %>%
    mutate(pdiff = (qx - lag(qx)) / lag(qx)*100) %>%
    ungroup() %>%
    filter(Year == 2020) %>%
    mutate(qx = round(qx, 1), pdiff = round(pdiff, 1)) %>%
    as.data.frame()

# calculate diff in covid mortality rate
comb_lx_df %>%
    filter(Year == 2020) %>%
    select(Group, Year, Age_Group, COVID.Deaths, Population) %>%
    left_join(pw_df, by = "Age_Group") %>%
    group_by(Group) %>%
    summarise(
        casmr = sum((COVID.Deaths/Population)*PW*100000),
        .groups = "drop_last") %>%
    mutate(RR = lag(casmr) / casmr)

# calculate diff in mortality rate
comb_lx_df %>%
    filter(Year == 2020) %>%
    mutate(Deaths = Other.Deaths + COVID.Deaths) %>%
    select(Group, Year, Age_Group, Deaths, Population) %>%
    left_join(pw_df, by = "Age_Group") %>%
    group_by(Group) %>%
    summarise(
        casmr = sum((Deaths/Population)*PW*100000),
        .groups = "drop_last") %>%
    mutate(RR = lag(casmr) / casmr)


# compare cause effects
ce_df <- bind_rows(
    mutate(
        read_rds("./data/results/cause_effect.rds"),
        Group = "FLDOC"),
    mutate(
        read_rds("./data/results/sa_gen_cause_effect.rds"),
        Group = "Florida State"))

ce_df %>%
    mutate(Age_Group = case_when(
        Age_Group == "75+" ~ "55+",
        Age_Group == "65-74" ~ "55+",
        Age_Group == "55-64" ~ "55+",
        TRUE ~ "20-54")) %>%
    group_by(Group, Age_Group) %>%
    summarize_all(sum)

ce_df %>%
    select(-Age_Group) %>%
    group_by(Group) %>%
    summarize_all(sum)

ce_df %>%
    select(-Total.Effect) %>%
    pivot_longer(ends_with("Effect")) %>%
    mutate(name = str_remove(name, ".Effect")) %>%
    mutate(name = str_replace(name, "COVID", "COVID-19")) %>%
    mutate(Age_Group = case_when(
        Age_Group == "75+" ~ "75+",
        Age_Group == "65-74" ~ "65-74",
        Age_Group == "55-64" ~ "55-64",
        TRUE ~ "20-54")) %>%
    group_by(Group, Age_Group, name) %>%
    summarize(value = -sum(value)) %>%
    ggplot(aes(x = Group, y = value, pattern = name, fill = Age_Group)) +
    geom_col_pattern(
        width = .5,
        color = "black",
        pattern_fill = "black",
        pattern_angle = 45,
        pattern_density = 0.1,
        pattern_spacing = 0.025,
        pattern_key_scale_factor = 0.6
    ) +
    geom_hline(yintercept = 0, color = "black", size = 1.25) +
    scale_pattern_manual(values = c(Other = "stripe", `COVID-19` = "none")) +
    theme_behindbars() +
    guides(pattern = guide_legend(override.aes = list(fill = "white")),
           fill = guide_legend(override.aes = list(pattern = "none"))) +
    scale_fill_bbdiscrete() +
    labs(
        y = "Attributable Decline to \nLife Expectancy (in years)",
        fill = "Age", pattern = "Cause")

ce_df %>%
    select(-Total.Effect) %>%
    pivot_longer(ends_with("Effect")) %>%
    mutate(name = str_remove(name, ".Effect")) %>%
    mutate(name = str_replace(name, "COVID", "COVID-19")) %>%
    mutate(Age_Group = case_when(
        Age_Group == "75+" ~ "75+",
        Age_Group == "65-74" ~ "65-74",
        Age_Group == "55-64" ~ "55-64",
        TRUE ~ "20-54")) %>%
    group_by(Group, Age_Group, name) %>%
    summarize(value = -sum(value)) %>%
    filter(name == "COVID-19") %>%
    ggplot(aes(x = Group, y = value, fill = Age_Group)) +
    geom_col(width = .5) +
    theme_behindbars() +
    scale_fill_bbdiscrete() +
    labs(
        y = "Decline in Life Expectancy\nAttributable to COVID-19 (in years)",
        fill = "Age", pattern = "Cause")

