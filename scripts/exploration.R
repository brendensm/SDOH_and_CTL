library(sjPlot)
library(tidyverse)
library(ggfortify)
library(CDCPLACES)
library(gtsummary)

final <- read_csv("data/places_svi_and_fluoride_03262024.csv")

teethp <- get_places(measure = c("TEETHLOST", "DENTAL"))

tidyteeth <- teethp |> filter(data_value_type == "Age-adjusted prevalence") |> 
  select(data_value, measureid, locationid) |> 
  pivot_wider( names_from = measureid, values_from = c(data_value))


mod0 <- lm(TEETHLOST ~ DENTAL, data = tidyteeth)


comb <- left_join(final, tidyteeth, by = c("FIPS" = "locationid"))

mod00 <- lm(TEETHLOST.y ~ DENTAL + perc_fluor, data = comb)

comb |> 
  ggplot(aes(y = TEETHLOST.y, x = perc_fluor)) +
  geom_point()

comb |> 
  select(TEETHLOST.y, DENTAL, perc_fluor) |> 
  tbl_summary(label = list(
    TEETHLOST.y ~ "≥65 with Complete Tooth Loss (%)",
    DENTAL ~ "≥18 Been to the Dentist in the Past Year (%)",
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
         FLUORIDE) |> 
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
  ggplot(aes(FLUORIDE, TEETHLOST)) +
  geom_point(color = "tomato2") +
  theme_minimal(base_size = 14) +
  labs(title = wrapper("Figure 2: Relationship between complete tooth loss in those ≥65 and population receiving fluoridated water (percentile ranks)", 64)) +
  theme(plot.title.position = "plot",
        plot.title = element_text(face = "bold"))


# Modeling ----------------------------------------------------------------


model <- lm(TEETHLOST ~ DENTALVISITS + FLUORIDE +SES + HC + REMS + HTT, 
   data = final)
summary(model)

tab_model(model)

plot_model(model) +
  theme_minimal(base_size = 14)  +
  labs(title = "Figure 4: Multiple Linear Regression Model Results") +
  theme(plot.title.position = "plot",
        plot.title = element_text(face = "bold"))

autoplot(model)

model2 <- lm(TEETHLOST ~ SES + HC + REMS + HTT, 
            data = final)
summary(model2)

tab_model(model, model2)

model3 <- lm(DENTALVISITS ~ FLUORIDE +SES + HC + REMS + HTT, 
            data = final)

tab_model(model3)

# Maps --------------------------------------------------------------------



county_geo <- tigris::counties(cb = T, year = 2020) |> 
  select(STATEFP, GEOID, NAME, geometry)

