rm(list=ls())
library(tidyverse)
library(parallel)
library(lubridate)
library(tidyverse)
library(behindbarstools)

# read in death data
death_df <- "./data/cleaned/FL_death_roster_historical_by_FLDHS.csv" %>%
    read_csv() %>%
    mutate(Age_Group = ifelse(Age_Group == "85+", "75-84", Age_Group)) %>%
    group_by(Year, Month, Age_Group) %>%
    summarize(Deaths = sum(Deaths), .groups = "drop") %>%
    filter(Age_Group != "15-19") %>%
    mutate(Date = ymd(str_c(Year, "-", Month, "-1"))) %>%
    select(Date, Age_Group, Deaths) %>%
    filter(year(Date) < 2021)

# read in pop data
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

# combine the data together
agg_mx_df <- pop_df %>%
    left_join(death_df, by = c("Date", "Age_Group")) %>%
    mutate(Deaths = ifelse(is.na(Deaths), 0, Deaths)) %>%
    mutate(Age_Group = ifelse(Age_Group == "75-84", "75+", Age_Group))

write_csv(agg_mx_df, "data/cleaned/agg_mx_df.csv")

# age specific smoothed plot
agg_mx_df %>%
    mutate(Age_Group = ifelse(
        Age_Group %in% c("20-24", "25-34"),
        "20-34",
        Age_Group
    )) %>%
    group_by(Date, Age_Group) %>%
    summarise_all(sum) %>%
    mutate(Rate = Deaths/Population*100000) %>%
    ggplot(aes(x = Date, y = Rate, color = Age_Group, fill = Age_Group)) +
    geom_smooth() +
    scale_y_log10() +
    theme_behindbars() +
    scale_color_bbdiscrete() +
    scale_fill_bbdiscrete() +
    labs(fill = "Age Group", color = "Age Group", y = "Mortality Rate")

# calculate raw rate df
rate_df <- agg_mx_df %>%
    mutate(Year = year(Date)) %>%
    select(Year, Population, Deaths) %>%
    group_by(Year) %>%
    summarise_all(sum) %>%
    mutate(Rate = Deaths/Population*12*100000)

rate_df

# create an individual level data set where person months are
# the rows
ind_mx_df <- bind_rows(lapply(1:nrow(agg_mx_df), function(i){
    tibble(
        Death = c(
            rep(1, agg_mx_df$Deaths[i]),
            rep(0, agg_mx_df$Population[i] - agg_mx_df$Deaths[i]))) %>%
        bind_cols(select(agg_mx_df, Date, Age_Group)[i,])})) %>%
    mutate(Year = year(Date))

n_age <- length(unique(agg_mx_df$Age_Group))
person_years <- nrow(ind_mx_df)

# define the number of simulations you could like to do for the analysis
sims <- 10000

# run teh boot strapped simulation life tables
# this takes a while!!!!!
set.seed(123)
bs_lx_df <- bind_rows(mclapply(1:sims, function(j){

    mx_df <- ind_mx_df[sample.int(person_years, replace = TRUE),] %>%
        group_by(Year, Age_Group) %>%
        summarize(Deaths = sum(Death), Pop = n()/12, .groups = "drop") %>%
        mutate(Rate = Deaths / Pop)

    # do life table calculations
    mx_df %>%
        group_by(Year) %>%
        mutate(min_age = as.numeric(str_sub(Age_Group, 1, 2))) %>%
        #{suppressWarnings(as.numeric(.))}
        {suppressWarnings(mutate(., max_age = as.numeric(str_sub(Age_Group, 4, 5))))} %>%
        mutate(qx = Rate) %>%
        mutate(n = max_age - min_age + 1) %>%
        mutate(px = (1-qx)^n, lx = cumprod(c(1, px))[1:n_age]) %>%
        mutate(nax=ifelse(!is.na(n), n/2, 5)) %>%
        mutate(nqx=ifelse(!is.na(lead(lx)), 1-(lead(lx) / lx), 1)) %>%
        mutate(ndx=ifelse(!is.na(lead(lx)), lx-lead(lx), lx)) %>%
        mutate(nLx=ifelse(!is.na(n), n*lead(lx) + nax*ndx, log(.5)/log(1-qx))) %>%
        mutate(nmx=ndx/nLx) %>%
        mutate(Tx=rev(cumsum(rev(nLx)))) %>%
        mutate(ex=Tx/lx, eplusx=ex+min_age) %>%
        select(Year, Age_Group, min_age, ex, eplusx, qx) %>%
        ungroup() %>%
        mutate(sim = j)
}, mc.cores = 10))


# make all the plots
bs_lx_df %>%
    filter(Age_Group == "20-24") %>%
    group_by(Age_Group, Year) %>%
    summarise(
        mu = mean(eplusx),
        lwr = quantile(eplusx, probs = .025),
        upr = quantile(eplusx, probs = .975), .groups = "drop") %>%
    ggplot(aes(x=Year, y=mu, ymin=lwr, ymax=upr, color = Age_Group)) +
    geom_point(size=2.5) +
    geom_errorbar(size=1.2) +
    theme_behindbars() +
    scale_color_bbdiscrete() +
    theme(legend.position = "none") +
    scale_x_continuous(breaks = 2015:2020) +
    ylab("Life Expectancy")

bs_lx_df %>%
    filter(Year >= 2019) %>%
    #filter(Age_Group %in% c("55-64", "65-74", "75+")) %>%
    arrange(Age_Group, sim, Year) %>%
    group_by(Age_Group, sim) %>%
    summarize(RR = last(qx)/first(qx)) %>%
    summarise(
        mu = mean(RR),
        lwr = quantile(RR, probs = .025),
        upr = quantile(RR, probs = .975), .groups = "drop")

agg_mx_df %>%
    filter(month(Date) == 12) %>%
    mutate(Year = year(Date)) %>%
    group_by(Age_Group, Year) %>%
    summarise(Population = sum(Population)) %>%
    filter(Year >= 2019) %>%
    ungroup() %>%
    pivot_wider(names_from = Year, values_from = Population) %>%
    rename(Age = Age_Group) %>%
    mutate(`Percent Change` =  (`2020`-`2019`)/`2019` * 100)

bs_lx_df %>%
    filter(Age_Group == "20-24") %>%
    arrange(sim, Year) %>%
    group_by(sim) %>%
    mutate(delta = eplusx - lag(eplusx)) %>%
    filter(Year != 2015) %>%
    group_by(Year) %>%
    summarize(
        mu = mean(delta),
        lwr = quantile(delta, probs = .025),
        upr = quantile(delta, probs = .975))
