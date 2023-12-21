library(tidyverse)
library(tidymodels)
library(vroom)
library(timetk)
library(gridExtra)
setwd("~/Documents/BYU code/Fall23/STAT348/ItemDemand")

train <-vroom("train.csv")
test  <-vroom("test.csv")


recipe <- recipe(sales ~ ., data = train) %>%
  step_date(date, features = "doy") %>%
  step_range(date_doy, min=0, max = 2 * pi) %>%
  step_mutate(sinDOY = sin(date_doy), cosDOY = cos(date_doy))

prepped_recipe <- prep(recipe)

baked_data <- bake(prepped_recipe, new_data = test)

head(baked_data)

boost_model <- boost_tree(tree_depth=tune(),
                          trees=tune(),
                          learn_rate=tune()) %>%
set_engine("lightgbm") %>% #or "xgboost" but lightgbm is faster6
  set_mode("classification")

workflow <- workflow() %>%
  add_model(model) %>%
  add_recipe(recipe)

# Define the lower and upper bounds for mtry and min_n
mtry_range <- c(1, 10)  # For mtry, from 1 to 10
min_n_range <- c(1, 10)  # For min_n, from 1 to 10

# Create the grid
grid <- grid_regular(
  mtry(range = mtry_range),
  min_n(range = min_n_range),
  levels = 10
)

cv_splits <- vfold_cv(train, v = 5)

CV_results <- workflow %>%
  tune_grid(resamples=cv_splits,
            grid=grid)

best_parameters <- select_best(CV_results, "accuracy")




  

for(s in 1:nStores){
  for(i in 1:nItems){
    storeItemTrain <- train %>%
    filter(store==s, item==i)
    storeItemTest <- test %>%
    filter(store==s, item==i)
    
    ## Fit storeItem models here

    ## Predict storeItem sales
    
    ## Save storeItem predictions
    if(s==1 & i==1){
      all_preds <- preds
    } else {
      all_preds <- bind_rows(all_preds, preds)
    }
  }
}

trainSB <- train %>% 
  filter(store == 3, item == 25)

# All data in a plot
trainSB %>%
  plot_time_series(date, sales, .interactive=FALSE)

# capture residual trends in autocorrelation with ACF
trainSB %>%
  pull(sales) %>% 
  forecast::ggAcf(.)

# capture residual trends in autocorrelation with ACF
S3I25 <- trainSB %>%
  pull(sales) %>% 
  forecast::ggAcf(., lag.max = 2*365)+
  ggtitle("Store 3 Item 25")

S5I20 <- train %>% 
  filter(store == 5, item == 20) %>% 
  pull(sales) %>% 
  forecast::ggAcf(., lag.max = 2*365)+
  ggtitle("Store 5 Item 20")

S1I1 <- train %>% 
  filter(store == 1, item == 1) %>% 
  pull(sales) %>% 
  forecast::ggAcf(., lag.max = 2*365)+
  ggtitle("Store 1 Item 1")


S9I49 <- train %>% 
  filter(store == 9, item == 49) %>% 
  pull(sales) %>% 
  forecast::ggAcf(., lag.max = 2*30)+
  ggtitle("Store 9 Item 49")



grid.arrange(S3I25, S5I20, S1I1, S9I49, ncol = 2, nrow = 2)
