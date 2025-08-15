library(maps)
library(usmap)
library(tidyverse)
library(brms)
library(tidybayes)
map <- purrr::map

models_files <- list.files("./models",
                           full.names = T)

m1_files <- models_files[1:5]

m_1 <- m1_files |> 
  map(readRDS)

# xwalk fips codes
xwalk <- read_csv("https://gist.githubusercontent.com/dantonnoriega/bf1acd2290e15b91e6710b6fd3be0a53/raw/11d15233327c8080c9646c7e1f23052659db251d/us-state-ansi-fips.csv") |> 
  mutate(st = as.numeric(st))

sim_dat <- expand_grid(
  age = 0,
  totalrem = 1,
  pop = 1e5,
  state = xwalk$st)

sims_out <- list()
for(i in 1:5){
  preds <- predict(m_1[[i]], 
                   newdata = sim_dat,
                   se.fit = T,
                   type = "response")
  temp <- sim_dat
  temp$.imp <- i
  temp$yhat <- preds$fit
  temp$se.yhat <- preds$se.fit
  sims_out[[i]] <- temp
}

sims_out <- bind_rows(sims_out) |> 
  left_join(xwalk |> 
              rename(state = st)) |> 
  rename(fips = state,
         abbr = stusps)

plot_usmap(regions = "states",
           data = sims_out,
           values = "yhat") + 
  scale_fill_viridis_b(direction = -1, breaks = c(0, 0.15, 0.3, 0.45, 0.6)) + 
  labs(fill = "Probability of\nICWA preferred\nplacement") + 
  theme_void()

 ggsave("./vis/fig_e_icwa.png", height = 8, width = 11)
  
### Fig 3; dot/whisker of P(kin|race, age)

## make a white nh flag

m2_files <- models_files[6:10]

m_2 <- m2_files |> 
  map(readRDS)

sim_dat <- expand_grid(
  age = 0,
  totalrem = 1,
  pop = 1e5,
  state = xwalk$st)
# join xwalk

sims_out <- list()
for(i in 1:5){
  preds <- sim_dat |> 
    mutate(yhat = predict(m_2[[i]][[1]], 
                          newdata = sim_dat,
                          type = "response"),
           race_ethn = "AIAN") |> 
    bind_rows(sim_dat |> 
                mutate(yhat = predict(m_2[[i]][[2]], 
                                      newdata = sim_dat,
                                      type = "response"),
                       race_ethn = "Asian")) |> 
    bind_rows(sim_dat |> 
                mutate(yhat = predict(m_2[[i]][[3]], 
                                      newdata = sim_dat,
                                      type = "response"),
                       race_ethn = "Black")) |> 
    bind_rows(sim_dat |> 
                mutate(yhat = predict(m_2[[i]][[4]], 
                                      newdata = sim_dat,
                                      type = "response"),
                       race_ethn = "HPI")) |> 
    bind_rows(sim_dat |> 
                mutate(yhat = predict(m_2[[i]][[5]], 
                                      newdata = sim_dat,
                                      type = "response"),
                       race_ethn = "White")) |> 
    bind_rows(sim_dat |> 
                mutate(yhat = predict(m_2[[i]][[5]], 
                                      newdata = sim_dat,
                                      type = "response"),
                       race_ethn = "Latine"))
  sims_out[[i]] <- preds
}

sims_out <- sims_out |> 
  bind_rows() |> 
  left_join(xwalk |> 
              rename(state = st,
                     state.abb = stusps))

### rubin's rules 
sims_out <- sims_out |> 
  group_by(state, race_ethn, state.abb) |>
  summarize(yhat = mean(yhat))

sims_out <- sims_out |> 
  ungroup() |> 
  select(yhat, race_ethn, state.abb) |> 
  rename(state = state.abb)
## add region
plot_usmap(regions = "states",
           data = sims_out,
           values = "yhat") + 
  scale_fill_viridis_b(direction = -1, breaks = c(0, 0.15, 0.3, 0.45, 0.6)) + 
  labs(fill = "Probability of\nplacement\nwith kin") + 
  theme_void() + 
  facet_wrap(~race_ethn)

ggsave("./vis/fig_e_kin.png", height = 8, width = 11)
