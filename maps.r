
# ### FIG 1; map of ICWAPref / AIANFC
# st_aian_23 <- dat |> 
#   filter(amiakn == 1) |> 
#   filter(.imp==1) |> 
#   group_by(year, state) |> 
#   summarize(n_aian = n(),
#             n_icwa_pref = sum(icwa_pref)) |> 
#   left_join(pop_st |> 
#               filter(race=="AIAN")) |> 
#   mutate(prop_icwa_pref = n_icwa_pref / n_aian,
#          aian_fc_rt = n_aian / pop * 1e3)
# 
# # st shapes, join to st_aian_23 then map
# st_shp <- states(cb = T) + 
#   st_set_crs("EPSG:5070")
# 
# st_aian_23 <- st_shp |> 
#   left_join(st_aian_23 |> 
#   ungroup() |> 
#   rename(STUSPS = state.abb)) |> 
#   filter(!(is.na(prop_icwa_pref)))
# 
# st_aian48 <- st_aian_23 |> 
#   filter(!(STUSPS %in% c("PR", "AK", "HI")))
# 
# st_aianAK <- st_aian_23 |> 
#   filter(STUSPS == "AK")
# 
# st_aianHI <- st_aian_23 |> 
#   filter(STUSPS == "HI")
# 
# 
#   
#   
# # hack by forcing into grob per https://github.com/r-tmap/tmap/issues/1148
# p_AK <- tmap_grob(tm_shape(st_aianAK, crs = 3338) + 
#   tm_polygons(fill = "prop_icwa_pref",
#               fill.scale = tm_scale(breaks = seq(0, 1, by = 0.2))) + 
#   tm_layout(legend.show = FALSE, frame = FALSE))
#   
# 
# p_HI <- tmap_grob(tm_shape(st_aianHI, crs = 3759) + 
#   tm_polygons(fill = "prop_icwa_pref",
#               fill.scale = tm_scale(breaks = seq(0, 1, by = 0.2)))+ 
#   tm_layout(legend.show = FALSE, frame = FALSE))
# 
# p_48 <- tm_shape(st_aian48) + 
#   tm_polygons(fill = "prop_icwa_pref",
#               fill.scale = tm_scale(breaks = seq(0, 1, by = 0.2))) 
# 
# p_48 + 
#   tm_inset(p_AK) + 
#   tm_inset(p_HI) 

### then add p2 with fc per cap
# fix later with tm_layout or tm_component
