rm(list=ls())
library(tidyverse)
library(readxl)
library(tidycensus)
library(parallel)

valid_ages <- c(str_c(
    c("15-24", "25-34", "35-44", "45-54", "55-64", "65-74", "75-84"), " years"),
    "85 years and over")
oldages <- c("75-84 years", "85 years and over")


# read_csv("~/Downloads/IHME-GBD_2019_DATA-8ccef3bc-1.csv")
sex_df <- tibble(
    Sex = c("Male", "Female"),
    W = c(.94, .06)
)

# pull COVID death data from CDC
gen_covid_df <- "https://data.cdc.gov/api/views/9bhg-hcku/rows.csv" %>%
    str_c("?accessType=DOWNLOAD") %>%
    read_csv(col_types = cols()) %>%
    filter(State == "Florida" & Year == "2020" & Group == "By Year") %>%
    filter(Sex != "All Sexes") %>%
    filter(`Age Group` %in% valid_ages) %>%
    mutate(Age_Group = ifelse(`Age Group` %in% oldages, "75+", `Age Group`)) %>%
    mutate(Age_Group = str_remove(Age_Group, " years")) %>%
    select(Sex, Age_Group, COVID.Deaths = `COVID-19 Deaths`) %>%
    group_by(Age_Group, Sex) %>%
    summarize(COVID.Deaths = sum(COVID.Deaths)) %>%
    mutate(Age_Group = ifelse(Age_Group == "15-24", "20-24", Age_Group)) %>%
    # change this a bit to make sure it dont really change results basically the
    # issue here is that CDC reports data for the age group of 15-24 but we
    # want 20-24 we can cut the death total in half to estimate it but even
    # leaving it at the full count doesnt really change the result as this age
    # group has so few COVID deaths
    mutate(COVID.Deaths = ifelse(
        Age_Group == "20-24", COVID.Deaths*1., COVID.Deaths)) %>%
    mutate(Year = 2020)

# this number is just shy of teh CDC aggregate total found below which makes
# sense as we have removed deaths for younger ages
# https://www.cdc.gov/nchs/nvss/vsrr/covid19/index.htm
sum(gen_covid_df$COVID.Deaths)

# pull death data from FLDOH
# http://www.flhealthcharts.com/FLQUERY_New/Death/Count
death_df <- "data/raw/Resident Deaths by Residence County by Year.xlsx" %>%
    read_excel(skip = 3) %>%
    rename(Age_Group = Year) %>%
    filter(!str_detect(Sex, "(?i)unknown")) %>%
    filter(!str_detect(Sex, "(?i)total")) %>%
    filter(!str_detect(Age_Group, "(?i)unknown")) %>%
    filter(!str_detect(Age_Group, "(?i)total")) %>%
    rename(`2020` = "2020 (provisional)") %>%
    select(-Total, -`2021 (provisional)`) %>%
    pivot_longer(`2011`:`2020`, names_to = "Year", values_to = "Deaths") %>%
    mutate(Age_Group = case_when(
        Age_Group == "75-84" ~ "75+",
        Age_Group == "85+" ~ "75+",
        TRUE ~ Age_Group
    )) %>%
    group_by(Sex, Age_Group, Year) %>%
    mutate(Deaths = as.numeric(as.integer(str_remove_all(Deaths, ",")))) %>%
    mutate(Year = as.integer(Year)) %>%
    filter(Year >= 2015) %>%
    summarize(Deaths = sum(Deaths), .groups = "drop") %>%
    # data reported by the FLDOH is marked as provisional for 2020 for the FL
    # so lets scale up the death count so that it matches the CDC count number
    # of 243,105 found on the CDC data tracker; select Yearly & Florida
    # https://www.cdc.gov/nchs/nvss/vsrr/covid19/index.htm
    group_by(Year) %>%
    mutate(Deaths = case_when(
        Year < 2020 ~ Deaths,
        Year == 2020 ~ Deaths * (243105/sum(Deaths))
    )) %>%
    ungroup()

# pull population data from Census ACS
pop_df <- bind_rows(lapply(2015:2020, function(y){
    y_ <- ifelse(y == 2020, 2019, y)
    get_acs("state", table = "B01001", year = y_, survey = "acs1") %>%
        mutate(Year = y)
}))

vars_df <- load_variables(2019, "acs1") %>%
    filter(str_starts(name, "B01001_"))

trans_df <- vars_df %>%
    filter(!str_ends(label, "\\:")) %>%
    mutate(Sex = case_when(
        str_detect(label, "Male") ~ "Male",
        str_detect(label, "Female") ~ "Female",
        TRUE ~ NA_character_
    )) %>%
    mutate(Age_Group = case_when(
        str_ends(label, "Under 5 years") ~ "0-4",
        str_ends(label, "5 to 9 years") ~ "5-9",
        str_ends(label, "10 to 14 years") ~ "10-14",
        str_ends(label, "15 to 17 years") ~ "15-19",
        str_ends(label, "18 and 19 years") ~ "15-19",
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
    select(variable = name, Age_Group, Sex)

tot_pop_df <- pop_df %>%
    filter(NAME == "Florida") %>%
    left_join(trans_df, by = "variable") %>%
    filter(!is.na(Age_Group) & !is.na(Sex)) %>%
    group_by(Age_Group, Year, Sex) %>%
    summarize(Population = sum(estimate), .groups = "drop")

tot_pop_df %>%
    filter(Year == 2020) %>%
    pull(Population) %>%
    sum()



mx_df <- death_df %>%
    filter(!(Age_Group %in% c("<1", "1-4", "5-9", "10-14", "15-19"))) %>%
    left_join(tot_pop_df, by = c("Sex", "Age_Group", "Year")) %>%
    mutate(qx = Deaths/ Population) %>%
    left_join(gen_covid_df) %>%
    mutate(COVID.Deaths = ifelse(is.na(COVID.Deaths), 0, COVID.Deaths)) %>%
    mutate(Other.Deaths = Deaths - COVID.Deaths)

n_age <- length(unique(mx_df$Age_Group))

gen_lx_df <- mx_df %>%
    group_by(Year, Sex) %>%
    mutate(min_age = as.numeric(str_sub(Age_Group, 1, 2))) %>%
    {suppressWarnings(mutate(., max_age = as.numeric(str_sub(Age_Group, 4, 5))))} %>%
    mutate(n = max_age - min_age + 1) %>%
    mutate(px = (1-qx)^n, lx = cumprod(c(1, px))[1:n_age]) %>%
    mutate(nax=ifelse(!is.na(n), n/2, 5)) %>%
    mutate(nqx=ifelse(!is.na(lead(lx)), 1-(lead(lx) / lx), 1)) %>%
    mutate(ndx=ifelse(!is.na(lead(lx)), lx-lead(lx), lx)) %>%
    mutate(nLx=ifelse(!is.na(n), n*lead(lx) + nax*ndx, log(.5)/log(1-qx))) %>%
    mutate(nmx=ndx/nLx) %>%
    mutate(Tx=rev(cumsum(rev(nLx)))) %>%
    mutate(ex=Tx/lx, eplusx=ex+min_age) %>%
    select(
        Year, Age_Group, Sex, min_age, ex, eplusx, qx, nmx, lx, nLx,
        COVID.Deaths, Other.Deaths, Population) %>%
    ungroup()

sa_gen_lx_df <- gen_lx_df %>%
    left_join(sex_df, by = "Sex") %>%
    group_by(Year, Age_Group, min_age) %>%
    summarize(
        ex = sum(ex*W),
        eplusx = sum(eplusx*W),
        qx = sum(qx*W),
        nmx = sum(nmx*W),
        lx = sum(lx*W),
        nLx = sum(nLx*W),
        COVID.Deaths = sum(COVID.Deaths*W),
        Other.Deaths = sum(Other.Deaths*W),
        Population = sum(Population*W),
        .groups = "drop")

gen_decomp_age_df <- lapply(c("Male", "Female"), function(g){
    gen_lx_df %>%
        filter(Sex == g) %>%
        filter(Year >= 2019) %>%
        select(Age_Group, Year, lx, nLx, ex) %>%
        pivot_longer(lx:ex) %>%
        mutate(name = str_c(name, Year)) %>%
        select(-Year) %>%
        pivot_wider() %>%
        mutate(DE = lx2019 * ((nLx2020/lx2020) - (nLx2019/lx2019))) %>%
        mutate(IE = (lx2019 *
                         (lead(lx2020, default = 0)/lx2020) - lead(lx2019, default = 0)) *
                   lead(ex2020, default = 0)) %>%
        mutate(TE = DE + IE) %>%
        mutate(PTE = TE/sum(TE)) %>%
        mutate(CP = 1-cumsum(PTE) +PTE) %>%
        select(Age_Group, DE, IE, TE) %>%
        mutate(Sex = g)
    }) %>%
    bind_rows()

sa_gen_decomp_age_df <- gen_decomp_age_df %>%
    left_join(sex_df, by = "Sex") %>%
    group_by(Age_Group) %>%
    summarize(
        DE = sum(DE*W),
        IE = sum(IE*W),
        TE = sum(TE*W))

gen_cause_effect <- lapply(c("Male", "Female"), function(g){
    mx_df %>%
        filter(Year >= 2019, Sex == g) %>%
        mutate(COVID.Deaths = COVID.Deaths/Population) %>%
        mutate(Other.Deaths = Other.Deaths/Population) %>%
        select(Year, Age_Group, COVID.Deaths, Other.Deaths) %>%
        pivot_longer(ends_with("Deaths"), names_to = "Cause") %>%
        pivot_wider(names_from = Year, values_from = value) %>%
        mutate(diff = `2020` - `2019`) %>%
        group_by(Age_Group) %>%
        mutate(pcontr = diff/sum(diff)) %>%
        ungroup() %>%
        select(Age_Group, Cause, pcontr) %>%
        pivot_wider(names_from = Cause, values_from = pcontr) %>%
        mutate(Sex = g)}) %>%
    bind_rows() %>%
    left_join(
        select(gen_decomp_age_df, Age_Group, TE, Sex),
        by = c("Age_Group", "Sex")) %>%
    mutate(COVID.Effect = COVID.Deaths*TE) %>%
    mutate(Other.Effect = Other.Deaths*TE) %>%
    select(-COVID.Deaths, -Other.Deaths) %>%
    rename(Total.Effect = TE)

sa_gen_cause_effect <- gen_cause_effect %>%
    left_join(sex_df) %>%
    group_by(Age_Group) %>%
    summarize(
        Total.Effect = sum(Total.Effect*W),
        COVID.Effect = sum(COVID.Effect*W),
        Other.Effect = sum(Other.Effect*W))

sa_gen_cause_effect

sa_gen_cause_effect %>%
    summarize_if(is.numeric, sum)

write_rds(gen_cause_effect, "data/results/gen_cause_effect.rds")
write_rds(gen_decomp_age_df, "data/results/gen_decomp_age.rds")
write_rds(gen_lx_df, "data/results/gen_lx_df.rds")
write_rds(sa_gen_cause_effect, "data/results/sa_gen_cause_effect.rds")
write_rds(sa_gen_decomp_age_df, "data/results/sa_gen_decomp_age.rds")
write_rds(sa_gen_lx_df, "data/results/sa_gen_lx_df.rds")
