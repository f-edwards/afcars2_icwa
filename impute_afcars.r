#### impute afcars
#### project: ndacan_processing
#### Author: Frank Edwards
#### Email:  frank.edwards@rutgers.edu
#### repo: 
#
# log: impute single year of afcars with pop data
# 7/9/25 updating for AFCARS 2020 July 25 release
#----------------------------------------

#### impute 2010 - 2021 AFCARS with SEER pop state-level pop comp data

#rm(list=ls())
library(tidyverse)
library(haven)
library(mice)

afcars_path<-"~/Projects/ndacan_data/afcars_ab"
afcars_files<-list.files(afcars_path,
                         full.names = T)

### AFCARS 2.0
for(i in 1:length(afcars_files)){
  # pull, parse, clean
  afcars<-read_dta(afcars_files[i]) |> 
    zap_labels() |> 
    rename_all(tolower) |> 
    rename(year = fy) |>  
    mutate(age = ageatend) |> 
    mutate(age = ifelse(age>=99, NA, age)) |>  
    filter(age<18) |> 
    select(stfcid, state, fipscode,
           year, age, sex:white, hisorgin,
           totalrem, entered, istpr, raceethn,
           curplset, rf1amakn, rf2amakn) |>  
    mutate_at(vars(stfcid, fipscode), as.character) |> 
    mutate(hisorgin = case_when(
      hisorgin == 1 ~ 1,
      hisorgin == 2 ~ 0,
      hisorgin == 3 ~ NA)) |> 
    mutate(across(amiakn:white,
                  ~ ifelse(raceethn==99, NA, .))) |> 
    select(-raceethn)
  
  meth <- make.method(afcars)
  pred <- make.predictorMatrix(afcars)
  pred[c(1, 3:4),]<-0
  pred[,c(1, 3:4)]<-0
  
  imps<-futuremice(afcars,
                   method = meth,
                   predictorMatrix = pred,
                   n.core = 5)

  yr<-afcars$year[1]
  
  saveRDS(imps, file=paste("./data/afcars_ab_imp", yr, ".rds", sep = ""))
}

