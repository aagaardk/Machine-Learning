###########################################################################
#
# Machine Learning examples
#--------------------------------------------------------------------------
#
# Created by K. Aagaard
#
# Modified:  
# 27 January 2021
# By: Kevin Aagaard
#
###########################################################################

# Initialize settings (workspace, directory, etc.) ------------------------

#Clear the workspace
rm(
  list = 
    ls()
)

#Clear the console
cat(
  '\014'
)

#Set seed for random number generation
set.seed(123)

#Include necessary libraries
x = 
  c(
    "AmesHousing",
    "AppliedPredictiveModeling",
    "bookdown",
    "broom",
    "caret",
    "caretEnsemble",
    "cluster",
    "cowplot",
    "DALEX",
    "data.table",
    "doParallel",
    "dplyr",
    "dslabs",
    "e1071",
    "earth",
    "emo",
    "factoextra",
    "foreach",
    "forecast",
    "ggbeeswarm",
    "ggmap",
    "ggplot2",
    "ggplotify",
    "gbm",
    "glmnet",
    "gridExtra",
    "h2o",
    "HDclassif",
    "iml",
    "ipred",
    "kableExtra",
    "keras",
    "kernlab",
    "knitr",
    "lime",
    "markdown",
    "MASS",
    "Matrix",
    "mclust",
    "mlbench",
    "modeldata",
    "NbClust",
    "pBrackets",
    "pcadapt",
    "pdp",
    "plotROC",
    "pls",
    "pROC",
    "purrr",
    "ranger",
    "readr",
    "recipes",
    "reshape2",
    "ROCR",
    "rpart",
    "rpart.plot",
    "rsample",
    "rstudioapi",
    "scales",
    "sparsepca",
    "stringr",
    "SuperLearner",
    "tfruns",
    "tfestimators",
    "tidymodels",
    "tidyr",
    "vip",
    "visdat",
    "xgboost",
    "yardstick"
  )

# # If you don't know if you've installed the packages for some of these 
# # libraries, run this:
# install.packages(x)

# # `emo` not available for R traditionally
# devtools::install_github("hadley/emo")

lapply(
  x, 
  library, 
  character.only = TRUE
)

rm(
  x
)

#Set working directory (should be universal)
setwd(
  dirname(
    rstudioapi::callFun(
      "getActiveDocumentContext"
    )$path
  )
)

getwd()


#
# Generate/Gather data ----------------------------------------------------
h2o.init()

# Ames housing data
ames = AmesHousing::make_ames()

ames.h2o = 
  as.h2o(
    ames
  )

# Job attrition data
data(
  attrition
)

churn = 
  attrition %>% 
  mutate_if(
    is.ordered, 
    .funs = factor, 
    ordered = FALSE
  )

churn.h2o =
  as.h2o(
    churn
  )


#
# Subset data into training and testing data ------------------------------
# Random subset
split_2 = 
  h2o.splitFrame(
    ames.h2o, 
    ratios = 0.7, 
    seed = 123
  )

train_4 = split_2[[1]]
test_4  = split_2[[2]]

# Stratified sampling for churn
split_strat = 
  initial_split(
    churn, 
    prop = 0.7,
    strata = "Attrition"
  )

train_strat = training(split_strat)
test_strat = testing(split_strat)

# Stratified sampling with the rsample package
split = 
  initial_split(
    ames, 
    prop = 0.7, 
    strata = "Sale_Price"
  )

ames_train = training(split)
ames_test = testing(split)


#
# Model fitting -----------------------------------------------------------
# Specify resampling strategy
cv = 
  trainControl(
    method = "repeatedcv", 
    number = 10, 
    repeats = 5
  )

# Create grid of hyperparameter values
hyper_grid = 
  expand.grid(
    k = 
      seq(
        2, 
        25, 
        by = 1
      )
  )

# Tune a knn model using grid search
knn_fit = 
  caret::train(
    Sale_Price ~ ., 
    data = ames_train, 
    method = "knn", 
    trControl = cv, 
    tuneGrid = hyper_grid,
    metric = "RMSE"
  )

ggplot(knn_fit)


#