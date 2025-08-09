# Include your code here for you second imputation method

# install pacman
if(!require(pacman)) install.packages("pacman") ; require(pacman)

# require/install packages on this session
p_load(rio, # import/export data
       tidyverse, # tidy-data
       skimr, # summary data
       visdat, # visualizing missing data
       corrplot, # Correlation Plots 
       stargazer) # tables/output to TEX. 


## load data
df <- import("https://github.com/ignaciomsarmiento/datasets/blob/main/GEIH_sample1.Rds?raw=true")

db <- as_tibble(df) ## from dataframe to tibble


# calculating the most commun value of maxEducLevel. 
mode_edu <- as.numeric(names(sort(table(db$maxEducLevel), decreasing = TRUE)[1]))

# Imputing the missing value. 
db <- db  %>%
  mutate(maxEducLevel = ifelse(is.na(maxEducLevel) == TRUE, mode_edu , maxEducLevel))

# transform categorical to factor
db$maxEducLevel<- factor(db$maxEducLevel)

dummy_maxEducLevel <- as.data.frame(model.matrix(~ maxEducLevel - 1, data = db)) 
#db<- db  %>% cbind(dummy_maxEducLevel) 
db <- cbind(db, dummy_maxEducLevel)

linear_imput_model <- lm(y_salary_m ~ ingtot + sex   + maxEducLevel3 + maxEducLevel4 + maxEducLevel5 + maxEducLevel6 + maxEducLevel7 , data = db)

db$predicted_y <- predict(linear_imput_model, newdata = db)

db<-  db %>%  mutate(y_salary_m = ifelse(is.na(y_salary_m) == TRUE, predicted_y , y_salary_m))
