# Load packages
library(tidymodels)
library(tidyverse)
library(janitor)
library(butcher)
library(bundle)
theme_set(theme_bw())

# The `tidymodels` documentation is a great help:
# https://www.tidymodels.org/start/models/

# Load data. It contains:
# - the atomisation energies of molecules with at most 50 atoms (Eat)
# - the eigenvalues of the Coulomb matrix of these molecules (x0--x49)
# - the pubChem ID of these molecules (id)
EatEigen <- read_csv("Data/CoulombMat_eigenval.csv") |> 
    clean_names() |> 
    select(-id)

# Initial_split of data into train and test sets
data_split <- initial_split(EatEigen, prop = 8/10)
train_data <- training(data_split)
test_data  <- testing(data_split)

# Create a 5-fold cross-validation object
folds <- vfold_cv(train_data, v = 5)

# Create a recipe to preprocess the data, with a PCA step 
eat_rec_PCA <- 
    recipe(eat ~ ., data = train_data) |> 
    step_zv(all_predictors()) |>
    step_normalize(all_predictors()) |> 
    step_pca(all_predictors(), threshold = 0.99)

# Compute the PCA dimension reduction and see the effective reduction
prep(eat_rec_PCA) |> bake(train_data) |> dim()
# => we reduced the number of columns from 50 to 40, it's not really worth it to do the PCA
# => So we make a new recipe without the PCA step
eat_rec <- 
    recipe(eat ~ ., data = train_data) |> 
    step_zv(all_predictors()) |>
    step_normalize(all_predictors())

# Create a random forest model in 'regression' mode
# with the ranger engine, and we want to tune the number of trees
eat_mod <- 
    rand_forest(trees = tune()) |> 
    set_engine("ranger") |> 
    set_mode("regression")

# Create a grid of hyperparameters to tune
tree_grid <- expand_grid(trees = c(100,150,200,300,400,500))

# Create a workflow
eat_wf <- 
  workflow() |>
  add_model(eat_mod) |>
  add_recipe(eat_rec)

# Tune the model (this may take a while)
doParallel::registerDoParallel()
eat_fit_rs <- 
  eat_wf |> 
  tune_grid(resamples = folds,
            grid = tree_grid)

# Plot the resulting R2 as a function of the number of trees
eat_fit_rs |> 
    collect_metrics() |> 
    filter(.metric == "rsq") |>
    ggplot(aes(x = trees, y = mean)) +
    geom_point() +
    geom_line() +
    geom_errorbar(aes(ymin = mean - std_err, 
                      ymax = mean + std_err),
                  width = 0)

# Show the best model
eat_fit_rs |> 
    show_best("rsq")

# Select the best model
best_tree <- eat_fit_rs |>
    select_best("rsq")

# Finalize the workflow
final_wf <- 
    eat_wf |> 
    finalize_workflow(best_tree)

# Fit the final workflow on the training data
final_fit <- 
    final_wf |>
    last_fit(data_split) 

# Show the final model
final_fit |>
    collect_metrics()

# Show the predictions on the test set
final_fit |>
    collect_predictions() |> 
    ggplot(aes(x = eat, y = .pred)) +
    geom_point() +
    geom_abline(color = "red")

# Extract the final model
final_tree <- extract_workflow(final_fit)

# Save Model
final_tree |>
    butcher() |> 
    bundle() |> 
    saveRDS(file="Data/final_tree.RDS")

# # # # # # # # # # # # # # # # # # # # # # # # # # 
# AFTER A RELOAD
# # # # # # # # # # # # # # # # # # # # # # # # # # 

library(tidymodels)
library(tidyverse)
library(janitor)
library(bundle)
theme_set(theme_bw())

EatEigen <- read_csv("Data/CoulombMat_eigenval.csv") |> 
    clean_names() |> 
    select(-id)

# Load the final model
final_tree_reloaded <- readRDS("Data/final_tree.RDS") |> 
    unbundle()

# Make sure it works
final_tree_reloaded |>
    predict(EatEigen[,-1]) |> 
    mutate(eat = EatEigen$eat) |>
    ggplot(aes(x = eat, y = .pred)) +
    geom_point() +
    geom_abline(color = "red")+
    labs(title = "Reloaded model",
         x = "True atomisation energy [Ry]",
         y = "Predicted atomisation energy [Ry]")
