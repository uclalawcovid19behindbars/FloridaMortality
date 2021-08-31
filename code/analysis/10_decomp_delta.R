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

sex_weight_df <- "./data/cleaned/21.3.6_fl_agg_new_cat_(2000-2021).csv" %>%
    read_csv(col_types = cols()) %>%
    mutate(Age_Group = ifelse(Age_Group == "75-85", "75+", Age_Group)) %>%
    mutate(Age_Group = ifelse(Age_Group == "85+", "75+", Age_Group)) %>%
    rename(Date = Anchor_Date) %>%
    mutate(Sex = str_to_title(Sex)) %>%
    group_by(Age_Group, Sex, Date) %>%
    summarize(Population = sum(Total, na.rm = TRUE), .groups = "drop_last") %>%
    filter(year(Date) == 2020) %>%
    filter(Age_Group != "15-19") %>%
    summarize(Population = sum(Population)/12, .groups = "drop") %>%
    mutate(PW = Population / sum(Population)) %>%
    select(-Population)

# read in pop data
pop_df <- "./data/cleaned/21.3.6_fl_agg_new_cat_(2000-2021).csv" %>%
    read_csv(col_types = cols()) %>%
    mutate(Age_Group = ifelse(Age_Group == "75-85", "75-84", Age_Group)) %>%
    mutate(Age_Group = ifelse(Age_Group == "85+", "75-84", Age_Group)) %>%
    rename(Date = Anchor_Date) %>%
    group_by(Date, Age_Group) %>%
    summarize(Population = sum(Total, na.rm = TRUE), .groups = "drop") %>%
    filter(year(Date) >= 2015) %>%
    filter(year(Date) < 2021)


# our pop numbers are a lil different from Vera but only by about 1% and our
# numbers are on average slightly larger which would bias towards a lower
# mortality rate
pop_df %>%
    filter(Date %in% ymd(c("2020-01-01", "2020-07-01", "2020-12-01"))) %>%
    group_by(Date) %>%
    summarize(Population = sum(Population)) %>%
    # compare to Vera population report pg6
    # vera.org/downloads/publications/people-in-jail-and-prison-in-2020.pdf
    mutate(Vera = c(96009, 89382, 83362)) %>%
    mutate(pdiff = (Population-Vera)/Vera*100)

# how much did the FLDOC population change over the study period?
pop_df %>%
    filter(Date == ymd("2019-01-01") | Date == ymd("2020-12-01")) %>%
    group_by(Age_Group) %>%
    summarise(delta = diff(Population))

# read in the medical examiner records
covid_deaths <- "./data/raw/" %>%
    str_c("FL_combined_roster_med_examiner_records_04.21.21.csv") %>%
    read_csv(col_types = cols()) %>%
    # only keep individuals if they have an FLDOC DC number
    filter(!is.na(DC.Number)) %>%
    # only keep COVID deaths
    filter(COVID.Flag_ME == 1) %>%
    # calculate age from the roster data
    mutate(Age_RR = floor(
        decimal_date(Date.of.Death) - decimal_date(Birth.Date))) %>%
    # use age from the medical record data execept when not available then use
    # roster age
    mutate(Age = ifelse(is.na(Age_ME), Age_RR, Age_ME)) %>%
    # divide up into approporaie age groups for analysis
    mutate(Age_Group  = cut(
        Age, c(20, 25, 35, 45, 55, 65, 75, Inf), right = F,
        labels = c("20-24","25-34", "35-44", "45-54", "55-64", "65-74", "75+")
    )) %>%
    # group by age group
    select(Age_Group) %>%
    group_by(Age_Group) %>%
    summarize(COVID.Deaths = n()) %>%
    mutate(Age_Group = as.character(Age_Group)) %>%
    mutate(Year = 2020)

# combine the data together
agg_mx_df <- pop_df  %>%
    filter(!(Age_Group %in% c("10-14", "15-19"))) %>%
    left_join(death_df, by = c("Date", "Age_Group")) %>%
    mutate(Deaths = ifelse(is.na(Deaths), 0, Deaths)) %>%
    mutate(Age_Group = ifelse(Age_Group == "75-84", "75+", Age_Group)) %>%
    mutate(Year = year(Date)) %>%
    group_by(Year, Age_Group) %>%
    summarize(
        Population = mean(Population),
        Deaths = sum(Deaths), .groups = "drop") %>%
    # no covid deaths before 2020
    left_join(covid_deaths) %>%
    mutate(COVID.Deaths = ifelse(is.na(COVID.Deaths), 0, COVID.Deaths)) %>%
    mutate(Other.Deaths = Deaths - COVID.Deaths)

# define the number of age groups
n_age <- length(unique(agg_mx_df$Age_Group))

# calculate life expectancy stats
lx_df <- agg_mx_df %>%
    mutate(qx = Deaths/Population) %>%
    arrange(Year, Age_Group) %>%
    group_by(Year) %>%
    mutate(min_age = as.numeric(str_sub(Age_Group, 1, 2))) %>%
    #{suppressWarnings(as.numeric(.))}
    {suppressWarnings(mutate(., max_age = as.numeric(str_sub(Age_Group, 4, 5))))} %>%
    mutate(n = max_age - min_age + 1) %>%
    mutate(px = (1-qx)^n, lx = cumprod(c(1, px))[1:n_age]) %>%
    mutate(nax=ifelse(!is.na(n), n/2, 5)) %>%
    mutate(nqx=ifelse(!is.na(lead(lx)), 1-(lead(lx) / lx), 1)) %>%
    mutate(ndx=ifelse(!is.na(lead(lx)), lx-lead(lx), lx)) %>%
    mutate(nLx=ifelse(!is.na(n), n*lead(lx) + nax*ndx, log(.5)/log(1-qx))) %>%
    #mutate(nLx=ifelse(!is.na(n), n*lead(lx) + nax*ndx, lag(n)*lx+nax*lag(ndx))) %>%
    #mutate(nLx=ifelse(!is.na(n), n*lead(lx) + nax*ndx, 5)) %>%
    mutate(nmx=ndx/nLx) %>%
    mutate(Tx=rev(cumsum(rev(nLx)))) %>%
    mutate(ex=Tx/lx, eplusx=ex+min_age) %>%
    select(
        Year, Age_Group, min_age, ex, eplusx, qx, nmx, lx, nLx,
        COVID.Deaths, Other.Deaths, Population) %>%
    ungroup()

# check out life expectancy over time
lx_df %>%
    filter(Age_Group == "20-24")

# decompose age effects of change in life expectancy by direct and indirect
# effects
decomp_age_df <- lx_df %>%
    ungroup() %>%
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
    select(Age_Group, DE, IE, TE)

# calculate effects of the causes
cause_effect <- agg_mx_df %>%
    filter(Year >= 2019) %>%
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
    left_join(select(decomp_age_df, Age_Group, TE), by = c("Age_Group")) %>%
    mutate(COVID.Effect = COVID.Deaths*TE) %>%
    mutate(Other.Effect = Other.Deaths*TE) %>%
    select(-COVID.Deaths, -Other.Deaths) %>%
    rename(Total.Effect = TE)

cause_effect %>%
    summarise_if(is.numeric, sum)

write_rds(cause_effect, "data/results/cause_effect.rds")
write_rds(decomp_age_df, "data/results/decomp_age.rds")
write_rds(lx_df, "data/results/lx_df.rds")
write_rds(sex_weight_df, "data/results/age_sex_pw.rds")
