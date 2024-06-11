library(sjPlot)
library(tidyverse)
library(ggfortify)
library(CDCPLACES)
library(gtsummary)

final <- read_csv("data/places_svi_and_fluoride_06112024.csv")

# teethp <- get_places(measure = c("TEETHLOST", "DENTAL"))
# 
# tidyteeth <- teethp |> filter(data_value_type == "Age-adjusted prevalence") |> 
#   select(data_value, measureid, locationid) |> 
#   pivot_wider( names_from = measureid, values_from = c(data_value))


mod0 <- lm(TEETHLOST ~ DENTALVISITS, data = final)

mod00 <- lm(TEETHLOST ~ DENTALVISITS + perc_fluor, data = final)

final |> 
  ggplot(aes(y = TEETHLOST, x = perc_fluor)) +
  geom_point()

final |> 
  select(TEETHLOST, DENTALVISITS, perc_fluor) |> 
  tbl_summary(label = list(
    TEETHLOST ~ "≥65 with Complete Tooth Loss (%)",
    DENTALVISITS ~ "≥18 Been to the Dentist in the Past Year (%)",
    perc_fluor ~ "Population Receiving Fluoridated Water (%)"
  )) |> 
  modify_header(label = "**Variable**") %>%
  modify_caption("Table 1: Descriptive Statistics") #%>%
 # as_flex_table() #|> 
# flextable::save_as_docx(path = "descrip_table1.docx")

  

final |> 
  select(SES,        
         HC,        
         REMS,    
         HTT, 
         TOTAL,
         DENTALVISITS,
         TEETHLOST,
         perc_fluor) |> 
  tab_corr(triangle = "upper")

wrapper <- function(x, ...) 
{
  paste(strwrap(x, ...), collapse = "\n")
}


final |> 
  ggplot(aes(DENTALVISITS, TEETHLOST)) +
  geom_point(color = "steelblue") +
  theme_minimal(base_size = 14) +
  labs(title = wrapper("Figure 1: Relationship between complete tooth loss in those ≥65 and those that had seen a dentist (percentile ranks)", 64)) +
  theme(plot.title.position = "plot",
        plot.title = element_text(face = "bold"))

final |> 
  ggplot(aes(perc_fluor, TEETHLOST)) +
  geom_point(color = "tomato2") +
  theme_minimal(base_size = 14) +
  labs(title = wrapper("Figure 2: Relationship between complete tooth loss in those ≥65 and population receiving fluoridated water (percentile ranks)", 64)) +
  theme(plot.title.position = "plot",
        plot.title = element_text(face = "bold"))


# Modeling ----------------------------------------------------------------


model <- lm(TEETHLOST_pr ~ DENTALVISITS_pr + FLUORIDE_pr + SES_pr + HC_pr + 
              REMS_pr + HTT_pr, 
              data = final)
summary(model)

tab_model(model)

plot_model(model) +
  theme_minimal(base_size = 14)  +
  labs(title = "Figure 4: Multiple Linear Regression Model Results") +
  theme(plot.title.position = "plot",
        plot.title = element_text(face = "bold"))

autoplot(model)

model2 <- lm(TEETHLOST_pr ~ SES_pr + HC_pr + REMS_pr + HTT_pr, 
            data = final)
summary(model2)

tab_model(model, model2)

model3 <- lm(DENTALVISITS ~ FLUORIDE +SES + HC + REMS + HTT, 
            data = final)

tab_model(model3)




# percentages with svi variables


m4 <- lm(TEETHLOST ~ DENTALVISITS + EP_POV150 + EP_UNEMP + EP_HBURD + EP_NOHSDP +
     EP_UNINSUR + EP_MINRTY, data = final)

summary(m4)
plot_model(m4)
autoplot(m4)

# Maps --------------------------------------------------------------------



county_geo <- tigris::counties(cb = T, year = 2020) |> 
  select(STATEFP, GEOID, NAME, geometry)

