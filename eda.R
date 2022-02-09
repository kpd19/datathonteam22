# EDA --------
# load packages...

# load packages
library(tidyverse)
library(tidymodels)
library(splines)

tidymodels_prefer()


# correlation to impute energy_star_rating
energy_no_cat <- energy_train %>% 
  select(-c(facility_type, building_class, State_Factor, site_eui))

cor(energy_no_cat, energy_train$site_eui, use = "na.or.complete")

# plots of variables

# spline function
plot_smoother <- function(deg_free) {
  energy_train %>% 
    filter(year_built >= 1900) %>% 
    ggplot(aes(x = year_built, y = site_eui, color = building_class)) + 
      geom_point(alpha = .2) +
      geom_smooth(
        method = lm,
        formula = y ~ ns(x, df = deg_free),
        color = "lightblue",
        se = FALSE
      ) +
      labs(title = paste(deg_free, "Spline Terms"),
           y = "Site_eui")
}

plot_smoother(10)



# group by states
dif_by_state <-
  energy_train %>% 
    group_by(State_Factor) %>% 
    summarise(
      n(),
      mean_site_eui = mean(site_eui)
    )
