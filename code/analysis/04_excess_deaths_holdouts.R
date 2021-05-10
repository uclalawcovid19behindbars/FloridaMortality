rm(list=ls())
library(tidyverse)
library(parallel)
library(lubridate)
library(tidyverse)
library(behindbarstools)
library(plotly)
library(glmmTMB)
library(INLA)

agg_mx_df <- read_csv("data/cleaned/agg_mx_df.csv") %>%
    mutate(N = as.numeric(as.factor(as.character(Date))))

agg_mx_df %>%
    select(Date, Population, Deaths) %>%
    group_by(Date) %>%
    summarise_all(sum) %>%
    mutate(Rate = Deaths/Population) %>%
    pull(Rate) %>%
    acf()

agg_mx_df %>%
    mutate(Year = year(Date)) %>%
    select(Year, Deaths, Population) %>%
    group_by(Year) %>%
    summarise_all(sum) %>%
    mutate(Population = Population/12) %>%
    mutate(Rate = (Deaths / Population)*100000)

agg_mx_df %>%
    group_by(Date) %>%
    summarize(Population = sum(Population)) %>%
    mutate(Name = "") %>%
    ggplot(aes(x=Date, y=Population, color = Name)) +
    geom_line(size = 2) +
    #geom_area(aes(fill = Name), alpha = .5) +
    scale_color_bbdiscrete() +
    scale_fill_bbdiscrete() +
    theme_behindbars() +
    theme(legend.position = "none") +
    ylab("Population") +
    scale_y_continuous(label=scales::comma, limits = c(80000, 100000))


pred_df <- bind_rows(lapply(unique(agg_mx_df$Age_Group), function(a){

    agg_mx_df %>%
        filter(Age_Group == a) %>%
        arrange(Date) %>%
        mutate(Deaths = ifelse(year(Date) >= 2019, NA, Deaths), N2= N) %>%
        {inla(
            Deaths ~ 1 + f(N, model = "rw1") + f(N2, model = "iid"),
            data=., family = "binomial", Ntrials = .$Population,
            control.predictor = list(compute = TRUE))} %>%
        .$summary.linear.predictor %>%
        as_tibble() %>%
        select(mu = `0.5quant`, lwr = `0.025quant`, upr = `0.975quant`) %>%
        mutate_all(boot::inv.logit) %>%
        mutate(N=1:n(), Age_Group = a)
}))

ex_df <- pred_df %>%
    pivot_longer(mu:upr) %>%
    left_join(agg_mx_df, by = c("Age_Group", "N")) %>%
    mutate(Expected = value * Population) %>%
    mutate(Excess = Deaths - Expected) %>%
    select(Date, Deaths, Expected, Excess, name) %>%
    group_by(name, Date) %>%
    summarize_all(sum) %>%
    ungroup() %>%
    #filter(Date >= ymd("2020-03-01")) %>%
    left_join(covid_deaths, by = "Date")

ex_df %>%
    mutate(Year = year(Date)) %>%
    filter(Year < 2020) %>%
    group_by(Year, name) %>%
    summarise(Excess = sum(Excess), Expected = sum(Expected)) %>%
    mutate(pex = (Excess/Expected)*100) %>%
    select(Year, name, pex) %>%
    ungroup() %>%
    pivot_wider(names_from = name, values_from = pex)
