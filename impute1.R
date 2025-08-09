rm(list=ls())


# Initial configuration ---------------------------------------------------

library(pacman)

p_load(rio,
       tidyverse,
       skimr,
       visdat,
       corrplot,
       stargazer)

# Loading and inspecting data ---------------------------------------------

df <- import("https://github.com/ignaciomsarmiento/datasets/blob/main/GEIH_sample1.Rds?raw=true")
db <- as_tibble(df)

# Creating imputation -----------------------------------------------------

  # salario -> y_salary_m
  # sex -> sex
  # Educación <- maxLevelEduc

salary_miss <- skim(db) %>%
  select(skim_variable, n_missing) %>% 
  filter(skim_variable == "y_salary_m")

base_sexo_educ <- db %>%
  select(sex, maxEducLevel, y_salary_m) %>% 
  drop_na(maxEducLevel) %>% 
  group_by(sex, maxEducLevel) %>% 
  summarize(promedio = mean(y_salary_m, na.rm = TRUE),
            mediana = median(y_salary_m, na.rm = TRUE))

# Missing values df -------------------------------------------------------

missing_salaries <- db %>% 
  select(sex, maxEducLevel, y_salary_m) %>% 
  filter(is.na(y_salary_m)) %>% 
  drop_na(maxEducLevel) %>% 

# Imputating values -------------------------------------------------------

db <- db %>% 
  mutate(ifelse())

  