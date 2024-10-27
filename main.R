library(tidyverse)
library(CDCPLACES)
library(httr2)
library(jsonlite)
library(gtsummary)
library(gt)


# Import Data -------------------------------------------------------------

# SVI data downloaded from: https://www.atsdr.cdc.gov/placeandhealth/svi/data_documentation_download.html

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

# PLACES data

teeth <- get_places(release = 2023, measure = c("TEETHLOST", "DENTAL"), geometry = F, age_adjust = TRUE)

teeth2 <- teeth |> 
  select(year, locationid, data_value, low_confidence_limit, 
         high_confidence_limit, datavaluetypeid, measureid) |> 
  rename(
    FIPS = locationid, 
    low_cl = low_confidence_limit, 
    high_cl = high_confidence_limit, 
    datatype = datavaluetypeid
    # measure_text = measure
  ) 

teethwide <- teeth2 |> 
  pivot_wider(id_cols = c(FIPS, datatype), names_from = measureid, values_from = c(data_value, high_cl, low_cl, year))


# Merge with SVI

teeth_svi <- left_join(teethwide, SVI, by = "FIPS")

# Fluoridation

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


counties_fp <- tigris::fips_codes |> 
  mutate(count_state = paste0(county, ", ", state),
         FIPS = paste0(state_code, county_code)) |> 
  select(count_state, FIPS)

my_fips <- teeth |> #filter(datavaluetypeid == "AgeAdjPrv") |> 
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

#teeth_svi_age <- teeth_svi |> filter(datatype == "AgeAdjPrv")


teeth_svi_fluor <- left_join(fluor_fips, teeth_svi, by = "FIPS") |> 
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


# Statistical Models ------------------------------------------------------



model <- lm(TEETHLOST_pr ~ DENTALVISITS_pr + FLUORIDE_pr + SES_pr + HC_pr +
              REMS_pr + HTT_pr,
            data = teeth_svi_fluor)

bs_model <- function(x){
  
  tmodel <- broom::tidy(x)
  
  tmodel |>
    mutate(term = case_when(
      term == "DENTALVISITS_pr" ~ "Dental Visits",
      term ==  "FLUORIDE_pr" ~ "Fluoridation",
      term == "SES_pr" ~ "Socioeconomic Status" ,
      term == "HC_pr" ~ "Household Characteristics" ,
      term == "REMS_pr" ~ "Racial/Ethnic Minority Status" ,
      term == "HTT_pr" ~ "Housing Type/Transportation",
      
      
      term == "DENTALVISITS" ~ 'Dental Visits',
      term == "EP_MINRTY" ~ '% non-White',
      term == "EP_UNINSUR"  ~ "% Uninsured" ,
      term == "EP_HBURD" ~  "% Housing Cost Burden" ,
      term == "EP_NOHSDP" ~ "% No HS Diploma" ,
      term == "EP_UNEMP" ~  "% Unemployed",
      term == "EP_POV150" ~ "% Below 150% Poverty",
      
      
      TRUE ~ term
    )) |> 
    filter(term != "(Intercept)") |>
    mutate(low = estimate - std.error,
           high = estimate + std.error,
           group = case_when(estimate > 0 ~ "pos",
                             TRUE ~ "neg")) |>
    ggplot(aes(estimate, reorder(term, -estimate), color = group)) +
    geom_point(size = 2.75) +
    geom_errorbarh(aes(xmin = low, xmax = high, color = group, height = 0)) +
    geom_text(aes(label = paste0(round(estimate, 2),
                                 ifelse(p.value < 0.05, ifelse(p.value < 0.01, "***", "*"), ""))), nudge_y = .25, size = 4.5) +
    theme_minimal(base_size = 22) +
    # xlim(-.75, .75) +
    theme(legend.position = "none",
          plot.title.position = "plot") +
    labs( y = "")+
    scale_color_brewer(palette = "Set1") +
    bmisc::wrap_axis( 15, xaxis = F)
  
}

bs_model(model) 

gtsummary::tbl_regression(model,
                          label = list(
                            DENTALVISITS_pr = 'Dental Visits',
                            FLUORIDE_pr = 'Fluoridation',
                            SES_pr  = "Socioeconomic Status" ,
                            HC_pr =  "Household Characteristics" ,
                            REMS_pr = "Racial/Ethnic Minority Status" ,
                            HTT_pr =  "Housing Type/Transportation"
                          )) |> 
  gtsummary::bold_p() |> 
  add_glance_table(include = c(nobs, adj.r.squared)) |> 
  as_gt() |> 
  gt::tab_options(
    table.font.size = px(28),  # Set the font size in pixels
    data_row.padding = px(10),   # Adjust row padding
    table.width = px(1050)
  ) |> 
  tab_style(
    style = list(
      cell_fill(color = "#E0C5FC"),  # Background color
      cell_text(weight = "bold")       # Bold text
    ),
    locations = cells_body(
      rows = variable %in% c("DENTALVISITS_pr", "SES_pr", "REMS_pr")
    )
  )



m4 <- lm(TEETHLOST ~ DENTALVISITS + EP_POV150 + EP_UNEMP + EP_HBURD + EP_NOHSDP +
           EP_UNINSUR + EP_MINRTY, data = teeth_svi_fluor)


bs_model(m4)

gtsummary::tbl_regression(m4,
                          label = list(
                            DENTALVISITS = 'Dental Visits',
                            EP_MINRTY = '% non-White',
                            EP_UNINSUR  = "% Uninsured" ,
                            EP_HBURD =  "% Housing Cost Burden" ,
                            EP_NOHSDP = "% No HS Diploma" ,
                            EP_UNEMP =  "% Unemployed",
                            EP_POV150 = "% Below 150% Poverty")
) |> 
  gtsummary::bold_p() |> 
  add_glance_table(include = c(nobs, adj.r.squared)) |> 
  as_gt() |> 
  gt::tab_options(
    table.font.size = px(28),  # Set the font size in pixels
    data_row.padding = px(10),   # Adjust row padding
    table.width = px(1050)
  ) |> 
  tab_style(
    style = list(
      cell_fill(color = "#E0C5FC"),  # Background color
      cell_text(weight = "bold")       # Bold text
    ),
    locations = cells_body(
      rows = variable %in% c("DENTALVISITS", "EP_POV150", "EP_UNEMP")
    )
  )
