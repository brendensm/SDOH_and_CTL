library(tidyverse)
library(CDCPLACES)
library(httr2)
library(jsonlite)

`%!in%` <- function(x,y)!('%in%' (x,y))



SVI <- read_csv("data-raw/SVI_2020_US_county.csv") |> 
  select(FIPS, RPL_THEME1, RPL_THEME2, RPL_THEME3, RPL_THEME4, RPL_THEMES,
         EP_POV150, EP_UNEMP, EP_HBURD, EP_NOHSDP, EP_UNINSUR, EP_MINRTY) |> 
  rename(
    SES = RPL_THEME1, 
    HC = RPL_THEME2, 
    REMS = RPL_THEME3, 
    HTT = RPL_THEME4,
    TOTAL = RPL_THEMES
  )

# write_csv(SVI, "data/SVI_2020_clean.csv")

teeth <- get_places(measure = c("TEETHLOST", "DENTAL"), geometry = F)

teeth_clean <- teeth |> 
  select(year, locationid, data_value, low_confidence_limit, 
         high_confidence_limit, datavaluetypeid, measure) |> 
  rename(
    FIPS = locationid, 
    low_cl = low_confidence_limit, 
    high_cl = high_confidence_limit, 
    datatype = datavaluetypeid, 
    measure_text = measure
  ) 

# teeth_clean <- teeth |> 
#   select(year, locationid, data_value, low_confidence_limit, 
#          high_confidence_limit, datavaluetypeid, measure, geometry) |> 
#   rename(
#     FIPS = locationid, 
#     low_cl = low_confidence_limit, 
#     high_cl = high_confidence_limit, 
#     datatype = datavaluetypeid, 
#     measure_text = measure
#   ) 

# sf::st_write(teeth_clean, "data/PLACES_teeth.shp")

teeth_nogeo_c <- teeth |> 
  select(year, locationid, data_value, low_confidence_limit, 
         high_confidence_limit, datavaluetypeid, measureid) |> 
  rename(
    FIPS = locationid, 
    low_cl = low_confidence_limit, 
    high_cl = high_confidence_limit, 
    datatype = datavaluetypeid
   # measure_text = measure
  ) 


teethwide <- teeth_nogeo_c |> 
  pivot_wider(id_cols = c(FIPS, datatype), names_from = measureid, values_from = c(data_value, high_cl, low_cl, year))


# teethwide |> 
#   select(data_value_TEETHLOST, data_value_DENTAL) |> 
#   sjPlot::tab_corr()

teeth_svi <- left_join(teethwide, SVI, by = "FIPS")


# teeth_svi <- left_join(teeth_clean, SVI, by = "FIPS") |> 
#   mutate(
#     SES_cat = case_when(
#       SES > .75 ~ "High",
#       SES > .25 ~ "Moderate",
#       SES >= 0 ~ "Low"
#     ),
#     HC_cat = case_when(
#       HC > .75 ~ "High",
#       HC > .25 ~ "Moderate",
#       HC >= 0 ~ "Low"
#     ),
#     REMS_cat = case_when(
#       REMS > .75 ~ "High",
#       REMS > .25 ~ "Moderate",
#       REMS >= 0 ~ "Low"
#     ),
#     HTT_cat = case_when(
#       HTT > .75 ~ "High",
#       HTT > .25 ~ "Moderate",
#       HTT >= 0 ~ "Low"
#     ),
#     TOTAL_cat = case_when(
#       TOTAL > .75 ~ "High",
#       TOTAL > .25 ~ "Moderate",
#       TOTAL >= 0 ~ "Low"
#     )
#   )

#write_csv(teeth_svi, "data/places_teeth_svi.csv")

# Write a shapefile
# sf::st_write(teeth_svi, "data/final_data/places_teeth_svi.shp")

# teeth_svi <- read_csv("data/places_teeth_svi.csv")

#teeth_svi <- sf::st_read("data/final_data/places_teeth_svi.shp")

# 
# teeth_svi |> 
#   filter(datatyp == "AgeAdjPrv") |> 
#   group_by(TOTAL_c) |> 
#   summarise(median_val = median(data_vl)) |> 
#   ggplot(aes(x = median_val, y = reorder(TOTAL_c, -median_val))) +
#   geom_col()
# 
# teeth_svi |> 
#   select(data_value, SES, HC, REMS, HTT, TOTAL) |> 
#   sjPlot::tab_corr()
# 
# teeth_map <- teeth_svi |> 
#   filter(datatyp == "AgeAdjPrv") |>
#   tigris::shift_geometry() |> 
#   ggplot(aes(fill = data_vl)) +
#   geom_sf(lwd = 0) +
#   scale_fill_viridis_c() +
#   theme_void() +
#   labs(title = "", #"Age-adjusted prevalence of complete tooth loss among those 65 or older",
#        fill = "") +
#   theme(legend.position = "bottom")
# 
# 
# 
# teeth_svi$TOTAL_c <- factor(teeth_svi$TOTAL_c, c("Low", "Moderate", "High"))
# 
# teeth_svi <- teeth_svi |> 
#   mutate(
#     msr_cat = case_when(
#       data_vl > 0.16 ~ "High",
#       data_vl > 0.107 ~ "Moderate",
#       data_vl > 0 ~ "Low"
#     ),
#     msr_cat = factor(msr_cat, c("Low", "Moderate", "High"))
#   )
# 
# svi_total_map <- teeth_svi|> 
#   filter(datatyp == "AgeAdjPrv") |>
#   tigris::shift_geometry() |> 
#   ggplot(aes(fill = TOTAL_c)) +
#   geom_sf(lwd = 0) +
#   scale_fill_viridis_d() +
#   theme_void() +
#   labs(title = "SVI", #"Age-adjusted prevalence of complete tooth loss among those 65 or older",
#        fill = "") +
#   theme(legend.position = "bottom")
# 
# 
# teeth_cat_map <- teeth_svi|> 
#   filter(datatyp == "AgeAdjPrv") |>
#   tigris::shift_geometry() |> 
#   ggplot(aes(fill = msr_cat)) +
#   geom_sf(lwd = 0) +
#   scale_fill_viridis_d() +
#   theme_void() +
#   labs(title = "Complete Teeth Loss", #"Age-adjusted prevalence of complete tooth loss among those 65 or older",
#        fill = "") +
#   theme(legend.position = "bottom")
# 
# 
# teeth |> 
#   filter(datavaluetypeid == "AgeAdjPrv") |>
#   select(data_value) |> 
#   summary()


# Fluoridation Data by County ---------------------------------------------

fluor <- request("https://data.cdc.gov/resource/hgyx-uuxz.json?$limit=500000&IndicatorGroupId=IG_1&Type=Fluoridated") |> 
          req_perform() |> 
          resp_body_string() |> 
          fromJSON() |> 
          filter(grepl("County", locationdesc)) #|> 
 # mutate(locationid = as.numeric(locationid))

fluor_clean <- fluor |> 
  select(year, stateabbr, locationabbr,data_value, locationid) |> 
  rename(
         perc_fluor = data_value) |> 
  mutate(count_state = paste0(locationabbr, ", ", stateabbr))


# counties_geo <- tigris::counties(cb = TRUE) |> 
#   select(STATEFP, GEOID, NAME, geometry)

counties_fp <- tigris::fips_codes |> 
  mutate(count_state = paste0(county, ", ", state),
         FIPS = paste0(state_code, county_code)) |> 
  select(count_state, FIPS)

my_fips <- teeth |> filter(datavaluetypeid == "AgeAdjPrv") |> 
  select(stateabbr, locationid, locationname) |>
  distinct() |> 
  mutate(count_state = paste0(locationname, ", ", stateabbr)) |> 
  rename(FIPS = locationid) |> 
  select(FIPS, count_state)


fluor_fips <- left_join(fluor_clean, my_fips, by = "count_state") |> 
  select(FIPS, perc_fluor, year, locationid) |> 
  mutate(perc_fluor = as.numeric(perc_fluor),
         FIPS = case_when(is.na(FIPS) ~ locationid,
                          TRUE~FIPS))

teeth_svi_age <- teeth_svi |> filter(datatype == "AgeAdjPrv")


teeth_svi_fluor <- left_join(fluor_fips, teeth_svi_age, by = "FIPS") |> 
  select(-c(year_DENTAL, year_TEETHLOST)) |> 
  rename(
    TEETHLOST = data_value_TEETHLOST,
    DENTALVISITS = data_value_DENTAL
  ) |> 
  mutate(
    TEETHLOST_pr = percent_rank(TEETHLOST),
    DENTALVISITS_pr = percent_rank(DENTALVISITS),
    FLUORIDE_pr = percent_rank(perc_fluor),
    SES_pr = percent_rank(SES),
    HC_pr = percent_rank(HC),
    REMS_pr = percent_rank(REMS),
    HTT_pr = percent_rank(HTT),
    TOTAL_pr = percent_rank(TOTAL),
    FIPS_num = as.numeric(FIPS),
    POV150_pr  = percent_rank( EP_POV150) ,
    UNEMP_pr  =  percent_rank(EP_UNEMP)   ,
    HBURD_pr  =  percent_rank(EP_HBURD)  ,
    NOHSDP_pr  = percent_rank( EP_NOHSDP) ,
    UNINSU_pr  = percent_rank( EP_UNINSUR) ,
    MINRTY_pr  = percent_rank(EP_MINRTY),
  )

write_csv(teeth_svi_fluor, "data/places_svi_and_fluoride_06112024.csv")

# stateabbr <- zctaCrosswalk::state_names |> 
#   select(usps, fips_character)
# 
# counties_geo_ab <- left_join(counties_geo, stateabbr, by = c("STATEFP" = "fips_character"))
# 
# 
# fluor_geo <- left_join(fluor, counties_geo_ab, by = c("stateabbr" = "usps", "locationabbr" = "NAME"))
# 
# fluor_geo <- sf::st_as_sf(fluor_geo)
# 
# fluor_geo <- fluor_geo |> mutate(data_value = as.numeric(data_value))
# 
# fluor_geo |> 
#   ggplot(aes(fill = data_value)) +
#   geom_sf() 



