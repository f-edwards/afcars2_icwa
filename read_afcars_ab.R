#### read afcars ab for icwa compliance analysis
#### project: 
#### Author: Frank Edwards
#### Email:  frank.edwards@rutgers.edu
#### repo: 
#
# log: create 7/11/25
#
#----------------------------------------

library(tidyverse)
library(mice)

afcars <- list.files("./data",
                     full.names = T) |> 
  map(readRDS) |> 
  map(mice::complete, action = "l") |> 
  bind_rows()
  
pop <- read_csv("~/Projects/data/censusdata_race5.csv",
                col_names = c("year", "state", 
                              "county", "race5",
                              "hisp", "age",
                              "pop")) |> 
  mutate(
    race5 = case_when(
      (race5 == 7) & (hisp == F) ~ "White",
      race5 == 8 ~ "Black",
      race5 == 9 ~ "AIAN",
      race5 == 10 ~ "Asian",
      race5 == 11 ~ "NHPI",
      hisp == 2 ~ "Latine")) |> 
  group_by(year, state, race5) |> 
  summarize(pop = sum(pop))

xwalk <- data.frame(st = state.abb,
                    state = state.name)

pop <- pop |> 
  left_join(xwalk)

afcars <- afcars |> 
  mutate(icwa_pref = 
           case_when(
             amiakn != 1 ~ F,
             curplset == 2 ~ T,
             rf1amakn == 1 ~ T,
             rf2amakn == 1 ~ T,
             T ~ F)) |> 
  mutate(rural = fipscode==8)

write_csv(afcars, "./data/afcars_imputed.csv")
