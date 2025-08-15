#### scratch
#### project: afcars 2.0 icwa analysis 
#### Author: Frank Edwards
#### Email:  frank.edwards@rutgers.edu
#### repo: 
#
# log: toy with logit models for icwa preferred placement
#
#----------------------------------------
library(tidyverse)
library(brms)
library(lme4)
library(tidybayes)
library(rstan)
library(future)


options(tigris_use_cache = T)
rstan_options(auto_write = TRUE)
oopts <- options(future.globals.maxSize = 2.0 * 1e9)  ## 1.0 GB
# read imputed afcars and pep NDACAN census special pull
dat <- read_csv("./data/afcars_imputed.csv")

pop<-read_fwf("~/Projects/data/us.1990_2023.singleages.through89.90plus.adjusted.txt",
              fwf_widths(c(4, 2, 2, 3, 2, 1, 1, 1, 2, 8),
                         c("year", "state", "st_fips",
                           "cnty_fips", "reg", "race",
                           "hisp", "sex", "age", "pop")))

pop<-pop%>%
  mutate(pop = as.integer(pop))%>%
  mutate(race_ethn =
           case_when(
             race==1 & hisp ==0 ~ "White",
             race==2 ~ "Black",
             race==3 ~ "AIAN", 
             race==4 ~ "API",
             hisp==1 ~ "Latine")) 

pop_st <- pop %>% 
  filter(age<=18) %>% 
  group_by(state, year, st_fips, race_ethn) %>% 
  summarise(pop = sum(pop)) %>% 
  ungroup() |> 
  rename(race = race_ethn)|> 
  rename(st = state,
         state = st_fips) |> 
  mutate(state = as.numeric(state))

xwalk <- data.frame(st = state.abb,
                    region = state.region)

pop_st <- pop_st |> 
  left_join(xwalk)

dat <- dat |> 
  mutate(icwa_pref = 
           case_when(
             amiakn != 1 ~ F,
             curplset == 2 ~ T,
             rf1amakn == 1 ~ T,
             rf2amakn == 1 ~ T,
             T ~ F)) |> 
  mutate(rural = fipscode==8)

dat_aian <- dat |> 
  filter(amiakn == 1) |> 
  left_join(pop_st |> 
              filter(race=="AIAN") |> 
              ungroup()) |> 
  filter(!(is.na(pop))) 

dat <- dat |>
  mutate(white_nh =
           case_when(
             white == 1 & hisorgin == 0 ~ 1,
             T ~ 0))

# model 1, icwa preferred placement

for(i in 1:max(dat$.imp)){
  temp <- dat_aian |> 
    filter(.imp==i)
  print(i)
  m_temp <- glm(icwa_pref ~ -1 + age +
                  totalrem +
                  log(pop) +
                  factor(state),
                family = "binomial",
                data = temp)
  filename <- paste("./models/m1_", i, ".rds", sep = "")
  saveRDS(m_temp, filename)
}

# model 2, kin placement
#### REWRITE FOR SEPARATE MODELS FOR EACH GROUP

for(i in 1:max(dat$.imp)){
  temp <- dat |> 
    filter(.imp==i) |> 
    mutate(kin_placement = curplset==2) 
  print(i)
  m2_1_1 <- glm(kin_placement ~ -1 + age +
                totalrem +
                factor(state),
              family = "binomial",
              data = temp |> 
                filter(amiakn == 1))
  m2_1_2 <- glm(kin_placement ~ -1 + age +
                  totalrem +
                  factor(state),
                family = "binomial",
                data = temp |> 
                  filter(asian == 1))
  m2_1_3 <- glm(kin_placement ~ -1 + age +
                  totalrem +
                  factor(state),
                family = "binomial",
                data = temp |> 
                  filter(blkafram== 1))
  m2_1_4 <- glm(kin_placement ~ -1 + age +
                  totalrem +
                  factor(state),
                family = "binomial",
                data = temp |> 
                  filter(hawaiipi == 1))
  m2_1_5 <- glm(kin_placement ~ -1 + age +
                  totalrem +
                  factor(state),
                family = "binomial",
                data = temp |> 
                  filter(white_nh == 1))
  m2_1_6 <- glm(kin_placement ~ -1 + age +
                  totalrem +
                  factor(state),
                family = "binomial",
                data = temp |> 
                  filter(hisorgin == 1))
  m_out <- list(m2_1_1, m2_1_2, m2_1_3, m2_1_4, m2_1_5, m2_1_6)
  filename <- paste("./models/m2_", i, ".rds", sep = "")
  saveRDS(m_out, filename)
}



# (+ asian + blkafram +
# hawaiipi + white_nh + hisorgin) *
