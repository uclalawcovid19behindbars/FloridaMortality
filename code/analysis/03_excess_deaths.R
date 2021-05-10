rm(list=ls())
library(tidyverse)
library(parallel)
library(lubridate)
library(tidyverse)
library(behindbarstools)
library(plotly)
library(INLA)

# read in aggregated mortality data
agg_mx_df <- read_csv("data/cleaned/agg_mx_df.csv") %>%
    mutate(N = as.numeric(as.factor(as.character(Date))))

# run through summed auto-correlation function
# there seems to be significant auto-correlation but no seasonal trends
agg_mx_df %>%
    select(Date, Population, Deaths) %>%
    group_by(Date) %>%
    summarise_all(sum) %>%
    mutate(Rate = Deaths/Population) %>%
    pull(Rate) %>%
    acf()

# look at annual death rates
agg_mx_df %>%
    mutate(Year = year(Date)) %>%
    select(Year, Deaths, Population) %>%
    group_by(Year) %>%
    summarise_all(sum) %>%
    mutate(Population = Population/12) %>%
    mutate(Rate = (Deaths / Population)*100000)

# check out monthly population changes
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

# covid death load from behindbars
covid_deaths <- calc_aggregate_counts(
    state = TRUE, all_dates = TRUE, week_grouping = FALSE) %>%
    filter(State == "Florida" & Measure == "Residents.Deaths") %>%
    arrange(Date) %>%
    mutate(UCLA = ifelse(is.na(UCLA), 0, UCLA)) %>%
    mutate(Covid.Deaths = UCLA - lag(UCLA, default = 0)) %>%
    select(Date, Covid.Deaths)

# check out the deaths across months
covid_deaths %>%
    mutate(Name = "") %>%
    filter(Date < ymd("2021-01-01")) %>%
    ggplot(aes(x=Date, y=Covid.Deaths, fill=Name)) +
    geom_col() +
    scale_fill_bbdiscrete() +
    theme_behindbars() +
    labs(y = "COVID Deaths\nin FL Prisons") +
    theme(legend.position = "none")

# run the bayesian hierarchical models by age group
pred_df <- bind_rows(lapply(unique(agg_mx_df$Age_Group), function(a){

    agg_mx_df %>%
        filter(Age_Group == a) %>%
        arrange(Date) %>%
        mutate(Deaths = ifelse(year(Date) >= 2020, NA, Deaths), N2= N) %>%
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


# combine prediction with observed data
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
    filter(name == "mu" & Date >= ymd("2020-03-01")) %>%
    {summary(lm(Excess ~ Covid.Deaths, data = .))}

ex_df %>%
    filter(name == "mu" & Date >= ymd("2020-03-01")) %>%
    {cor(.$Excess, .$Covid.Deaths)}

ex_df %>%
    filter(name == "mu") %>%
    filter(Date >= ymd("2020-03-01")) %>%
    summarise_if(is.numeric, sum)

# run through plots of observed, expected, and covid deaths
bind_rows(
    ex_df %>%
        select(name, Expected, Date) %>%
        pivot_wider(names_from = name, values_from = Expected) %>%
        mutate(Type = "Expected\nDeaths")) %>%
    filter(Date < ymd("2020-01-01")) %>%
    ggplot(aes(x=Date, y=mu, ymin=lwr, ymax=upr)) +
    geom_line(aes(color = Type)) +
    geom_ribbon(aes(fill = Type), alpha = .3) +
    labs(y="Deaths", color = "", fill = "") +
    theme_behindbars() +
    scale_fill_manual(values = "#82CAA4") +
    scale_color_manual(values = "#82CAA4") +
    ylim(c(0,NA))

bind_rows(
    ex_df %>%
        select(name, Expected, Date) %>%
        pivot_wider(names_from = name, values_from = Expected) %>%
        mutate(Type = "Expected\nDeaths"),
    ex_df %>%
        select(Date, mu = Deaths) %>%
        mutate(Type = "All\nDeaths")) %>%
    filter(Date < ymd("2020-01-01")) %>%
    ggplot(aes(x=Date, y=mu, ymin=lwr, ymax=upr)) +
    geom_line(aes(color = Type)) +
    geom_ribbon(aes(fill = Type), alpha = .3) +
    labs(y="Deaths", color = "", fill = "") +
    theme_behindbars() +
    scale_color_bbdiscrete() +
    scale_fill_bbdiscrete() +
    ylim(c(0,NA))

bind_rows(
    ex_df %>%
        select(name, Expected, Date) %>%
        pivot_wider(names_from = name, values_from = Expected) %>%
        mutate(Type = "Expected\nDeaths"),

    ex_df %>%
        select(Date, mu = Deaths) %>%
        mutate(Type = "All\nDeaths")) %>%
    ggplot(aes(x=Date, y=mu, ymin=lwr, ymax=upr)) +
    geom_line(aes(color = Type)) +
    geom_ribbon(aes(fill = Type), alpha = .3) +
    labs(y="Deaths", color = "", fill = "") +
    theme_behindbars() +
    scale_color_bbdiscrete() +
    scale_fill_bbdiscrete() +
    ylim(c(0,NA))

bind_rows(
    ex_df %>%
        select(name, Expected, Date) %>%
        pivot_wider(names_from = name, values_from = Expected) %>%
        mutate(Type = "Expected\nDeaths"),

    ex_df %>%
        select(Date, mu = Deaths) %>%
        mutate(Type = "All\nDeaths"),
    ex_df %>%
       select(Date, mu = Covid.Deaths) %>%
       mutate(Type = "COVID\nDeaths")) %>%
    mutate(Type = factor(
        Type, levels = c("All\nDeaths", "Expected\nDeaths", "COVID\nDeaths"))) %>%
    ggplot(aes(x=Date, y=mu, ymin=lwr, ymax=upr)) +
    geom_line(aes(color = Type)) +
    geom_ribbon(aes(fill = Type), alpha = .3) +
    labs(y="Deaths", color = "", fill = "") +
    theme_behindbars() +
    scale_color_bbdiscrete() +
    scale_fill_bbdiscrete() +
    ylim(c(0,NA))

ex_df %>%
    filter(Date >= ymd("2020-03-01")) %>%
    group_by(name) %>%
    summarise(Excess = sum(Excess), Expected = sum(Expected)) %>%
    mutate(pex = (Excess/Expected)*100)

ex_df %>%
    mutate(Year = year(Date)) %>%
    filter(Year < 2021) %>%
    group_by(Year, name) %>%
    summarise(Excess = sum(Excess), Expected = sum(Expected)) %>%
    mutate(pex = (Excess/Expected)*100) %>%
    select(Year, name, pex) %>%
    ungroup() %>%
    pivot_wider(names_from = name, values_from = pex)
