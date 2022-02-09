### New models ------

# load packages...
library(tidyverse)
library(tidymodels)
library(splines)
library(xgboost)

tidymodels_prefer()

# load dataset
all_dat <- read_csv("train.csv")

# internal testing
energy_split <- initial_split(all_dat, prop = 0.80, strata = site_eui)
energy_train <- training(energy_split)
energy_test  <-  testing(energy_split)

simple_energy <- 
  recipe(site_eui ~ energy_star_rating + precipitation_inches + snowfall_inches + 
           heating_degree_days + cooling_degree_days + building_class + facility_type
         + avg_temp + State_Factor, data = energy_train) %>%  
  step_dummy(all_nominal_predictors()) %>% 
  step_impute_linear(energy_star_rating, 
                     impute_with = imp_vars(precipitation_inches, snowfall_inches,
                                            heating_degree_days, cooling_degree_days,
                                            avg_temp,
                                            starts_with("building_class"), 
                                            starts_with("facility_type"))) %>% 
  step_interact(~ starts_with("facility_type"):energy_star_rating + 
                  starts_with("building_class"):energy_star_rating)

# recipe with all imputes

energy_star_recipe <- 
  recipe(site_eui ~ energy_star_rating + facility_type + State_Factor
         + building_class + avg_temp, data = energy_train) %>% 
  step_dummy(all_nominal_predictors()) %>% 
  step_impute_bag(energy_star_rating, impute_with = imp_vars(all_predictors())) %>% 
  step_interact(~ energy_star_rating:starts_with("facility_type")) %>% 
  step_ns(avg_temp, deg_free = 15)

# linear reg models ----
# lin model #1

lm_model <-
  linear_reg() %>% 
  set_engine("lm")

lm_wflow <-
  workflow() %>% 
  add_model(lm_model) %>% 
  add_recipe(energy_star_recipe) 

lm_fit <- fit(lm_wflow, energy_train)

# testing metrics

energy_metrics <- metric_set(rmse, rsq, mae)

# 2
energy_metrics_res <- predict(lm_fit, new_data = energy_test %>% select(-site_eui)) %>% 
  bind_cols(energy_test %>% select(site_eui))

energy_metrics_res

# 3
energy_metrics(energy_metrics_res, truth = site_eui, estimate = .pred)

# lasso model

lasso_model <-
  linear_reg(penalty = .00001, mixture = 1) %>% 
  set_engine("glmnet")

lasso_wflow <-
  workflow() %>% 
  add_model(lasso_model) %>% 
  add_recipe(simple_energy) 

lasso_fit <- fit(lasso_wflow, energy_train)

# testing metrics

# 2
lasso_metrics <- predict(lasso_fit, new_data = energy_test %>% select(-site_eui)) %>% 
  bind_cols(energy_test %>% select(site_eui))

lasso_metrics

# 3
energy_metrics(lasso_metrics, truth = site_eui, estimate = .pred)


# random forest model

rf_model <- rand_forest(mode = "regression", trees = 1000) %>%
  set_engine("ranger")

# define workflow 

rf_wflow <-
  workflow() %>%
  add_model(rf_model) %>% 
  add_recipe(energy_star_recipe)

# fit workflow

rf_fit <- fit(rf_wflow, data = energy_train)

# metrics
rf_metrics <- predict(rf_fit, new_data = energy_test %>% select(-site_eui)) %>% 
  bind_cols(energy_test %>% select(site_eui))

rf_metrics

# 3
energy_metrics(rf_metrics, truth = site_eui, estimate = .pred)


boost_tree_xgboost_spec <-
  boost_tree(learn_rate = .1) %>%
  set_engine('xgboost') %>%
  set_mode('regression')

xgb_wflow <-
  workflow() %>%
  add_model(boost_tree_xgboost_spec) %>% 
  add_recipe(simple_energy)

xgb_fit <- fit(xgb_wflow, data = energy_train)

# metrics
xgb_metrics <- predict(xgb_fit, new_data = energy_test %>% select(-site_eui)) %>% 
  bind_cols(energy_test %>% select(site_eui))

xgb_metrics

energy_metrics(xgb_metrics, truth = site_eui, estimate = .pred)

# getting test values ----
test_dat <- read_csv("test.csv")


predictions <- predict(rf_fit, new_data = test_dat)
test <-
  test_dat %>% 
    select(id) %>% 
    bind_cols(predictions) %>% 
  rename("site_eui" = .pred) %>% 
  mutate(
    site_eui = if_else(site_eui < 0, 0, site_eui)
  )

write_csv(test, file = "predictions.csv")
