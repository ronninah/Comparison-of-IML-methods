#RScript for for Master Thesis
#Spatial Cross validation
#Nahid Hasan Ronnie
#matriculation 3944438
#IML
#XAI

#checking working directory
getwd()
setwd("E:/Thesis/Program file on RStudio")

#Library

library(dplyr)
library(tidyverse) 
library(countrycode)
library(purrr)

library(ggplot2)
library(RColorBrewer)
#install.packages("rnaturalearthdata")
library(rnaturalearthdata)
library(stars)
library(rnaturalearth)
library(patchwork)
library(cowplot)
library(caret)
library(pdp)
library(vip)
library(rpart)
library(rpart.plot)
library(iml)
library(writexl)

library(ALEPlot)

library(DALEX)
library(DALEXtra)

library(mlr)
library(ggplot2)
library(nnet)

library(viridis)

library(readxl)
library(sf)
library(blockCV)

library(codetools)
#install.packages("raster")

library(raster)

#install.packages("cluster")
#install.packages("factoextra")
#install.packages("spatialsample")
library(cluster)
library(factoextra)
library(spatialsample)

# Import the maize dataset
#maizedata <- read.csv("Globalmaize.csv", header = TRUE, sep = ",", stringsAsFactors = TRUE) %>% na.omit()

# Import the main dataset
cropdata <- read.csv("Data.csv", header = TRUE, sep = ",", stringsAsFactors = TRUE) %>% na.omit()


# Variable types
cropdata$NT_effect <- cropdata$NT_effect %>% fct_rev()




cropdata1 <- cropdata[-which(cropdata$Yield_change > quantile(cropdata$Yield_change, 0.975)),]

cropdata1$continent <- countrycode(
  sourcevar = cropdata1[, "Site.country"],
  origin = "country.name",
  destination = "continent"
)



# Exporting excel
#write_xlsx(cropdata1, path = "E:/Thesis/Program file on RStudio/cropdata1.xlsx")
write.csv(cropdata1, file = "E:/Thesis/Program file on RStudio/cropdata1.csv", row.names = FALSE)


#Only for maize
Globalmaize <- cropdata1%>% filter(Crop=="maize") %>% dplyr::select(-Yield_NT, -NT_effect, -Site.country, -Location, -continent, -Crop)



# cross validation
#5-fold
#spatial
colnames(Globalmaize)


##########
#########

####Saving the plot

# Set the path where you want to save the figures
save_path <- "E:/Thesis/Program file on RStudio/For Thesis/Spatial/08March/"

# Set the width and height for the plots
width <- 10
height <- 7

###### Convert data to an sf object


spatial_maize <- st_as_sf(Globalmaize, coords = c("Longitude", "Latitude"), crs = 4326)

# Perform k-means clustering
set.seed(123)

coords <- Globalmaize[, c("Longitude", "Latitude")]


fig_clusterN <- fviz_nbclust(coords, kmeans, method = "silhouette")

ggsave(paste0(save_path, "fig_clusterN.png"), fig_clusterN, width = width, height = height, dpi = 300)


num_clusters <- 5  

set.seed(123)
clusters <- kmeans(Globalmaize[, c("Longitude", "Latitude")], centers = num_clusters)

# Add cluster information to the data
spatial_maize$cluster <- clusters$cluster

# Divide data into folds
set.seed(123)
spatial_maize <- spatial_maize %>%
  group_by(cluster) %>%
  mutate(fold = sample(1:5, n(), replace = TRUE)) %>%
  ungroup()

# Verify that the data points are distributed across the folds
table(spatial_maize$fold)



# Create 5-fold assignments where each cluster is a fold
for (i in 1:num_clusters) {
  spatial_maize$fold[spatial_maize$cluster == i] <- i
}

# Now, for each fold 'i', cluster 'i' is the testing set, and all other clusters are the training set.
training_maize_s <- list()
testing_maize_s <- list()

for (i in 1:num_clusters) {
  training_indices <- which(spatial_maize$cluster != i)
  testing_indices <- which(spatial_maize$cluster == i)
  
  training_maize_s[[i]] <- spatial_maize[training_indices, ]
  testing_maize_s[[i]] <- spatial_maize[testing_indices, ]
}






# The next steps involve visualizing the distribution of training and testing sets on a map
# Get the world map data
world <- ne_countries(scale = "medium", returnclass = "sf")

# Define a function to plot each fold with distinct training and testing sets
plot_fold_clusters <- function(train_set, test_set, fold_number, world_map) {
  ggplot() +
    geom_sf(data = world_map, fill = "white", color = "black") +
    geom_sf(data = train_set, aes(color = as.factor(cluster)), size = 2, alpha = 0.5, shape = 16, show.legend = TRUE) +
    geom_sf(data = test_set, aes(color = as.factor(cluster)), size = 2, alpha = 0.5, shape = 17, show.legend = TRUE) +
    labs(title = paste("Fold", fold_number, "- Training (circle) and Testing (triangle) Data"),
         color = "Cluster") +
    theme_minimal() +
    theme(panel.background = element_rect(fill = "white"))
}

# Generate and combine plots for each fold
plots <- lapply(1:5, function(i) {
  plot_fold_clusters(training_maize_s[[i]], testing_maize_s[[i]], i, world)
})

# Combine the plots
combined_plot <- do.call(gridExtra::grid.arrange, c(plots, ncol = 3))
print(combined_plot)

# Save the combined plot
ggsave(paste0(save_path, "Combined_Folds_Plot.png"), combined_plot, width = 16, height = 9, dpi = 300)

# Generate and save plots for each fold individually
for (i in 1:5) {
  plot_fold <- plot_fold_clusters(training_maize_s[[i]], testing_maize_s[[i]], i, world)
  ggsave(paste0(save_path, "Fold_", i, "_Plot.png"), plot_fold, width = 16, height = 9, dpi = 300)
}

####saving as excel

for (i in 1:5) {
  # Define file paths for training and testing CSV files
  train_file_path <- paste0(save_path, "training_set_fold_", i, ".csv")
  test_file_path <- paste0(save_path, "testing_set_fold_", i, ".csv")
  
  # Export training set for the current fold to CSV
  write.csv(training_maize_s[[i]], train_file_path, row.names = FALSE)
  
  # Export testing set for the current fold to CSV
  write.csv(testing_maize_s[[i]], test_file_path, row.names = FALSE)
}

###########################
#########
####
###Modeling

# Function to process each fold dataset, excluding 'cluster' and 'fold' from predictors

process_fold_data <- function(data) {
  # Check if the data is an sf object and convert to a data frame if necessary
  if ("sf" %in% class(data)) {
    # Extract Latitude and Longitude from geometry column if present
    if ("geometry" %in% names(data)) {
      coords <- st_coordinates(data)
      data$Longitude <- coords[, "X"]
      data$Latitude <- coords[, "Y"]
      
      # Convert sf object to a regular data frame
      data <- as.data.frame(data)
    }
  }
  
  # Exclude 'cluster' and 'fold' columns from being used as predictors
  predictors <- setdiff(names(data), c("cluster", "fold", "geometry"))
  data_for_modeling <- data[predictors]
  
  return(data_for_modeling)
}


# Apply the processing to each fold for both training and testing sets


for (i in 1:length(training_maize_s)) {
  training_maize_s[[i]] <- process_fold_data(training_maize_s[[i]])
  testing_maize_s[[i]] <- process_fold_data(testing_maize_s[[i]])
}


# Function to run the models for different datasets
run_models <- function(train_data) {
  tc <- trainControl(method = "cv", number = 5)
  cart_control <- rpart.control(minsplit = 5, cp = 0.0001)
  
  # Linear Regression
  set.seed(111)
  model.lm <- caret::train(Yield_change ~ ., data = train_data, method = "glmStepAIC", trControl = tc)
  

    # Classification and Regression Trees (CART)
  #set.seed(111)
  #model.cart <- caret::train(Yield_change ~ ., data = train_data, method = "rpart", trControl = tc, control = cart_control)
  
  
  # Random Forest
  set.seed(111)
  model.rf <- caret::train(Yield_change ~ ., data = train_data, method = "rf", trControl = tc)
  
  
  # Gradient Boosting Machine (GBM)
  set.seed(111)
  model.gbm <- caret::train(Yield_change ~ ., data = train_data, method = "gbm", trControl = tc,
                            tuneGrid = expand.grid(n.trees = (1:5) * 5, interaction.depth = (1:5) * 1,
                                                   shrinkage = 0.5, n.minobsinnode = 5))
  
  
  return(list("Linear Model" = model.lm,
              #"Decision Tree" = model.cart,
              "Random Forests" = model.rf,
              "Gradient Boosting" = model.gbm))
}

# Initialize a list to store model results for each fold
models_maize_folds <- list()

# Loop through each fold's training data and run models
for (i in 1:length(training_maize_s)) {
  # Extract the training data for the current fold
  train_data_fold <- training_maize_s[[i]]
  
  # Run the models for the current fold
  models_maize_folds[[paste("Fold", i)]] <- run_models(train_data_fold)
}

# Check the models for the first fold as an example
print(models_maize_folds[[1]])




######Predictions



# Function to run predictions for each fold and plot results
run_predictions <- function(models, test_data, fold_number) {
  predictions <- list()
  plots <- list()
  
  for (model_name in names(models)) {
    model <- models[[model_name]]
    
    # Make predictions
    preds <- predict(model, newdata = test_data)
    predictions[[model_name]] <- preds
    
    # Create a plot for each model's predictions
    plot <- ggplot(data.frame(Actual = test_data$Yield_change, Predicted = preds), 
                   aes(x = Actual, y = Predicted)) +
      geom_point() +
      geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
      labs(title = paste("Fold", fold_number, "Model:", model_name),
           x = "Actual Yield Change", 
           y = "Predicted Yield Change") +
      theme_bw()
    plots[[model_name]] <- plot
  }
  
  # Return both predictions and plots
  return(list(predictions = predictions, plots = plots))
}

# Initialize a list to store predictions and plots for each fold
predictions_maize_folds <- list()

# Helper function to harmonize factor levels between training and test data
harmonize_factors <- function(train_data, test_data) {
  factors <- names(Filter(is.factor, train_data))
  for (f in factors) {
    levs <- levels(train_data[[f]])
    test_data[[f]] <- factor(test_data[[f]], levels = levs)
  }
  return(test_data)
}



# Loop through each fold, harmonize factor levels, make predictions, and plot results
for (i in 1:length(training_maize_s)) {
  # Get the training and test data for the current fold
  train_data_fold <- training_maize_s[[i]]
  test_data_fold <- testing_maize_s[[i]]
  
  # Ensure that factor levels in the test data match the training data
  test_data_fold <- harmonize_factors(train_data_fold, test_data_fold)
  
  # Get the models for the current fold
  models_for_fold <- models_maize_folds[[paste("Fold", i)]]
  
  # Run predictions for the current fold and generate plots
  results <- run_predictions(models_for_fold, test_data_fold, i)
  
  # Store the predictions and plots
  predictions_maize_folds[[paste("Fold", i)]] <- results
}



# Loop through each fold for saving plots
for (fold in names(predictions_maize_folds)) {
  # Access the plots for the current fold
  fold_plots <- predictions_maize_folds[[fold]][["plots"]]
  
  # Loop through each model within the fold
  for (model_name in names(fold_plots)) {
    # Access the plot for the current model
    plot <- fold_plots[[model_name]]
    
    # Define a unique filename for each plot
    file_name <- paste0("Prediction_Plot_Fold_", fold, "_Model_", model_name, ".png")
    
    # Use ggsave to save the plot
    ggsave(filename = paste0(save_path, file_name), plot = plot, width = width, height = height)
  }
}


####R2

# Function to calculate R-squared for each model and test dataset
calculate_r2 <- function(predictions, test_data) {
  r2_values <- list()
  
  for (i in seq_along(predictions)) {
    preds <- predictions[[i]]
    model_name <- names(predictions)[i]
    
    # Check if the standard deviation is zero
    if (sd(preds) == 0 || sd(test_data$Yield_change) == 0) {
      r2 <- 0  # Assign an R-squared value of 0
    } else {
      # Calculate R-squared
      r2 <- cor(preds, test_data$Yield_change)^2 %>% round(., 4)
    }
    
    # Store R-squared value
    r2_values[[model_name]] <- r2
  }
  
  return(r2_values)
}
# Initialize a list to store R-squared values for each fold
r2_values_maize_folds <- list()

# Loop through each fold
for (i in 1:length(predictions_maize_folds)) {
  # Extract predictions for the current fold
  predictions_fold <- predictions_maize_folds[[paste("Fold", i)]][["predictions"]]
  test_data_fold <- testing_maize_s[[i]]
  
  # Calculate R-squared for the current fold
  r2_values_fold <- calculate_r2(predictions_fold, test_data_fold)
  
  # Store R-squared values
  r2_values_maize_folds[[paste("Fold", i)]] <- r2_values_fold
}


# Function to plot R-squared values for each model and dataset
plot_r_squared <- function(r2_data, title) {
  algorithm_colors <- c("Linear Model" = "#1f77b4",
                        #"Decision Tree" = "#ff7f0e",
                        "Random Forests" = "#2ca02c",
                        "Gradient Boosting" = "#9467bd")
  
  p <- ggplot(r2_data, aes(x = algorithm, y = r2, fill = algorithm)) +
    geom_bar(stat = "identity") +
    ggtitle(title) +
    xlab("IML algorithm") +
    ylab("R-squared") +
    scale_fill_manual(values = algorithm_colors) +
    theme_bw(base_size = 14) +
    theme(legend.position = "none", plot.title = element_text(hjust = 0.5))
  
  return(p)
}


# Example for plotting fold 1:
r2_data_fold1 <- data.frame(r2 = unlist(r2_values_maize_folds[["Fold 1"]]),
                            algorithm = names(r2_values_maize_folds[["Fold 1"]]))

plot_r_squared(r2_data_fold1, "R-squared Values for Fold 1")

# Loop through each fold to plot and save R-squared values for all the folds

for (i in 1:length(r2_values_maize_folds)) {
  # Prepare the data for plotting
  r2_data_fold <- data.frame(
    r2 = unlist(r2_values_maize_folds[[paste("Fold", i)]]),
    algorithm = names(r2_values_maize_folds[[paste("Fold", i)]])
  )
  
  # Generate the plot
  r2_plot_fold <- plot_r_squared(r2_data_fold, paste("R-squared Values for Fold", i))
  
  # Define the file name for saving the plot
  file_name <- sprintf("%s/R2_Plot_Fold_%d.png", save_path, i)
  
  # Save the plot
  ggsave(file_name, plot = r2_plot_fold, width = 10, height = 7)
}


# Calculate mean R-squared for each model across all folds
mean_r2_values <- list()

for (model_name in names(r2_values_maize_folds[[1]])) {
  mean_r2 <- mean(sapply(r2_values_maize_folds, function(fold) fold[[model_name]]))
  mean_r2_values[[model_name]] <- mean_r2
}

# Create a data frame for plotting
mean_r2_data <- data.frame(
  r2 = unlist(mean_r2_values),
  algorithm = names(mean_r2_values)
)

# Plot mean R-squared values
r2_plot_mean_S <- plot_r_squared(mean_r2_data, "Mean R-squared Values Across All Folds")

# Save the plot
ggsave(paste0(save_path, "Fig14.4.png"), r2_plot_mean_S, width = width, height = height)


# Convert the list of R-squared values to a data frame
r2_values_df <- do.call(rbind, lapply(r2_values_maize_folds, function(fold_r2) {
  do.call(rbind, lapply(fold_r2, data.frame, stringsAsFactors = FALSE))
}))

# Add a fold column to identify each fold
r2_values_df$Fold <- rep(names(r2_values_maize_folds), each = length(r2_values_maize_folds[[1]]))

# Add a model column to identify each model (assuming that the models are the same across all folds)
r2_values_df$Model <- rep(names(r2_values_maize_folds[[1]]), times = length(r2_values_maize_folds))

# Reorder the data frame by fold and model
r2_values_df <- r2_values_df[order(r2_values_df$Fold, r2_values_df$Model), ]

# Rename the first column to R2 for clarity
names(r2_values_df)[1] <- "R2"

# View the table
print(r2_values_df)

r2_values_wide <- pivot_wider(r2_values_df, names_from = Model, values_from = R2, id_cols = Fold)
# View the wide format table
print(r2_values_wide)
write.csv(r2_values_wide, "R2_Values_All_Folds.csv", row.names = FALSE)

#####RMSE calculation

# Function to calculate RMSE
calculate_rmse <- function(predictions, test_data) {
  rmse_values <- list()
  
  for (i in seq_along(predictions)) {
    preds <- predictions[[i]]
    model_name <- names(predictions)[i]
    actual <- test_data$Yield_change
    
    rmse <- sqrt(mean((preds - actual)^2))
    rmse_values[[model_name]] <- rmse
  }
  
  return(rmse_values)
}

# Initialize a list to store RMSE values for each fold
rmse_values_maize_folds <- list()

# Loop through each fold and calculate RMSE
for (i in 1:length(predictions_maize_folds)) {
  predictions_fold <- predictions_maize_folds[[paste("Fold", i)]][["predictions"]]
  test_data_fold <- testing_maize_s[[i]]
  
  rmse_values_fold <- calculate_rmse(predictions_fold, test_data_fold)
  rmse_values_maize_folds[[paste("Fold", i)]] <- rmse_values_fold
}


# Function to plot RMSE values
plot_rmse <- function(rmse_data, title) {
  algorithm_colors <- c("Linear Model" = "#1f77b4",
                        #"Decision Tree" = "#ff7f0e",
                        "Random Forests" = "#2ca02c",
                        "Gradient Boosting" = "#9467bd")
  
  p <- ggplot(rmse_data, aes(x = algorithm, y = rmse, fill = algorithm)) +
    geom_bar(stat = "identity") +
    ggtitle(title) +
    xlab("Model") +
    ylab("RMSE") +
    scale_fill_manual(values = algorithm_colors) +
    theme_bw(base_size = 14) +
    theme(legend.position = "none", plot.title = element_text(hjust = 0.5))
  
  return(p)
}

# Loop through each fold to plot and save RMSE values for all the models



for (i in 1:length(r2_values_maize_folds)) {
  # Prepare the data for plotting
  rmse_data_fold <- data.frame(
    rmse = unlist(rmse_values_maize_folds[[paste("Fold", i)]]),
    algorithm = names(rmse_values_maize_folds[[paste("Fold", i)]])
  )
  
  # Generate the plot
  rmse_plot_fold <- plot_rmse(rmse_data_fold, paste("RMSE Values for Fold", i))
  
  # Define the file name for saving the plot
  file_name <- sprintf("%s/RMSE_Plot_Fold_%d.png", save_path, i)
  
  # Save the plot
  ggsave(file_name, plot = rmse_plot_fold, width = width, height = height)
}

# Calculate mean RMSE for each model across all folds
mean_rmse_values <- list()
for (model_name in names(rmse_values_maize_folds[[1]])) {
  mean_rmse <- mean(sapply(rmse_values_maize_folds, function(fold) fold[[model_name]]))
  mean_rmse_values[[model_name]] <- mean_rmse
}

# Create a data frame for plotting mean RMSE values
mean_rmse_data <- data.frame(
  rmse = unlist(mean_rmse_values),
  algorithm = names(mean_rmse_values)
)

# Plot mean RMSE values
rmse_plot_mean_S <- plot_rmse(mean_rmse_data, "Mean RMSE Values Across All Folds")

# Save the mean RMSE plot
ggsave(paste0(save_path, "Mean_RMSE_Plot.png"), rmse_plot_mean_S, width = width, height = height)


# Convert the list of RMSE values to a data frame
rmse_values_df <- do.call(rbind, lapply(rmse_values_maize_folds, function(fold_rmse) {
  t(data.frame(fold_rmse))
}))

# Convert the list of RMSE values to a data frame
rmse_values_df <- do.call(rbind, lapply(rmse_values_maize_folds, function(fold_rmse) {
  # Ensure that each element is a data frame before binding
  data.frame(t(unlist(fold_rmse)))
}))

# Check the structure of rmse_values_df to make sure it is a data frame
str(rmse_values_df)

# Now add the fold column (assuming rmse_values_df is indeed a data frame)
rmse_values_df$Fold <- rep(names(rmse_values_maize_folds), each = nrow(rmse_values_df)/length(rmse_values_maize_folds))

# Reformat the data frame if necessary
rmse_values_df <- as.data.frame(rmse_values_df)



# Interpretable machine learning 

# variable importance (feature importance)

# model agnostic (generic) post-hoc interpretability

# permutation-based feature importance

# Function to calculate permutation-based variable importance


#### For all model and fold

calculate_pvip <- function(model, train_data, crop_name, model_name, fill_color, fold_number) {
  set.seed(123)
  pvip <- vip(model, method = "permute", train = train_data, target = "Yield_change", metric = "rsq",
              pred_wrapper = predict, nsim = 30, geom = "boxplot", 
              aesthetics = list(fill = fill_color, color = "black")) +
    labs(title = paste(model_name, "- Crop:", crop_name, "- Fold:", fold_number)) +
    theme_bw()
  return(pvip)
}



# Initialize a list to store VIP plots for each fold
vip_plots_folds <- list()

# Colors for different models
model_colors <- c("Linear Model" = "#1f77b4", #"Decision Tree" = "#ff7f0e",
                  "Random Forests" = "#2ca02c", "Gradient Boosting" = "#9467bd")

# Loop through each fold
for (i in 1:length(training_maize_s)) {
  # Prepare a list to store VIP plots for current fold
  vip_plots <- list()
  
  # Extract the training data for the current fold
  train_data_fold <- training_maize_s[[i]]
  
  # Get models for the current fold
  models_for_fold <- models_maize_folds[[paste("Fold", i)]]
  
  # Loop through each model within the fold
  for (model_name in names(models_for_fold)) {
    model <- models_for_fold[[model_name]]
    fill_color <- model_colors[model_name]
    fold_number <- i
    
    # Calculate VIP
    pvip <- calculate_pvip(model, train_data_fold, "maize", model_name, fill_color, fold_number)
    
    # Store VIP plot
    vip_plots[[model_name]] <- pvip
  }
  
  # Combine VIP plots for current fold
  vip_plots_folds[[paste("Fold", i)]] <- vip_plots
}

# access and plot VIP for any model within any fold

print(vip_plots_folds[["Fold 1"]][["Random Forests"]])


# combine VIP plots for selected models of a specific fold into one figure
combine_vip_plots_per_fold <- function(vip_plots_fold, fold_number) {
  combined_plot <- vip_plots_fold[["Linear Model"]] + 
    vip_plots_fold[["Random Forests"]] + 
    vip_plots_fold[["Gradient Boosting"]] + 
    plot_layout(guides = 'collect') & 
    plot_annotation(title = paste("Fold", fold_number, "Variable Importance Plots"),
                    theme = theme(plot.title = element_text(hjust = 0.5)))
  return(combined_plot)
}

# Initialize an empty list to store combined figures for each fold
combined_vip_figs <- list()

# Loop through each fold and combine VIP plots
for (i in 1:length(vip_plots_folds)) {
  combined_vip_figs[[paste("Fold", i)]] <- combine_vip_plots_per_fold(vip_plots_folds[[paste("Fold", i)]], i)
}

# print combined VIP plots for each fold
for (fold_name in names(combined_vip_figs)) {
  print(combined_vip_figs[[fold_name]])
}

#save combined VIP plots to files
for (fold_name in names(combined_vip_figs)) {
  ggsave(paste0(save_path, "VIP_", fold_name, "_without_DT.png"), combined_vip_figs[[fold_name]], width = 16, height = 9, dpi = 300)
}

#################

# partial dependence plot
#### PDP


#Single predictor pdps



#maize 1. rf, 2. gbm, 3. lm

###
##For fold 1

# Assuming models_for_fold_1 is the list of models trained on fold 1 data
models_for_fold_1 <- models_maize_folds[["Fold 1"]]
# Assuming train_data_fold_1 is the training dataset for fold 1
train_data_fold_1 <- training_maize_s[[1]]

# For Yield_CT variable in fold 1
pdp_fold1_Yield_CT <- rbind(
  models_for_fold_1[["Random Forests"]] %>% partial(pred.var = "Yield_CT", grid.resolution = 10) %>% cbind(., algorithm = "Random Forests"),
  models_for_fold_1[["Gradient Boosting"]] %>% partial(pred.var = "Yield_CT", grid.resolution = 10) %>% cbind(., algorithm = "Gradient Boosting"),
  models_for_fold_1[["Linear Model"]] %>% partial(pred.var = "Yield_CT", grid.resolution = 10) %>% cbind(., algorithm = "Linear Model")
)

pdp_fold1_Yield_CT$algorithm <- factor(pdp_fold1_Yield_CT$algorithm, levels=c("Random Forests", "Gradient Boosting", "Linear Model"))


# For Tmax variable in fold 1
pdp_fold1_Tmax <- rbind(
  models_for_fold_1[["Random Forests"]] %>% partial(pred.var = "Tmax", approx = TRUE) %>% cbind(., algorithm = "Random Forests"),
  models_for_fold_1[["Gradient Boosting"]] %>% partial(pred.var = "Tmax", approx = TRUE) %>% cbind(., algorithm = "Gradient Boosting"),
  models_for_fold_1[["Linear Model"]] %>% partial(pred.var = "Tmax", approx = TRUE) %>% cbind(., algorithm = "Linear Model")
)

pdp_fold1_Tmax$algorithm <- factor(pdp_fold1_Tmax$algorithm, levels=c("Random Forests", "Gradient Boosting", "Linear Model"))




Fig16.4a <- ggplot(pdp_fold1_Yield_CT, aes(x=Yield_CT, y=yhat, color=algorithm)) +
  geom_line(linewidth=1) +
  scale_color_manual(values = c("Random Forests" = "#2ca02c", "Gradient Boosting" = "#9467bd", "Linear Model" = "#1f77b4")) +
  ylab("Partial Dependence") +
  labs(title = "pdp for Yield_CT variable for maize in fold 1") +
  theme_bw() +
  theme(legend.position = c(0.7, 0.7),
        axis.title.x = element_blank(),
        axis.text.x = element_blank())


Fig16.4b <- ggplot(pdp_fold1_Tmax, aes(x=Tmax, y=yhat, color=algorithm)) +
  geom_line(linewidth=1) +
  scale_color_manual(values = c("Random Forests" = "#2ca02c", "Gradient Boosting" = "#9467bd", "Linear Model" = "#1f77b4")) +
  ylab("Partial Dependence") +
  labs(title = "pdp for Tmax variable for maize in fold 1") +
  theme_bw() +
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        axis.text.x = element_blank())

Fig16.4c <- ggplot(train_data_fold_1, aes(x=Yield_CT)) + geom_histogram(bins=30) + theme_bw( base_size = 14)
Fig16.4d <- ggplot(train_data_fold_1, aes(x=Tmax)) + geom_histogram(bins=30) + theme_bw( base_size = 14)

Fig16.4 <- Fig16.4a + Fig16.4b + Fig16.4c + Fig16.4d +
  plot_annotation(tag_levels = "a") +
  plot_layout(heights=c(9,1))


# Save the plot
ggsave(paste0(save_path, "Fig16.4.png"), Fig16.4, width = width, height = height)


# For Fold 2
models_for_fold_2 <- models_maize_folds[["Fold 2"]]
train_data_fold_2 <- training_maize_s[[2]]

# For Yield_CT variable in Fold 2
pdp_fold2_Yield_CT <- rbind(
  models_for_fold_2[["Random Forests"]] %>% partial(pred.var = "Yield_CT", grid.resolution = 10) %>% cbind(., algorithm = "Random Forests"),
  models_for_fold_2[["Gradient Boosting"]] %>% partial(pred.var = "Yield_CT", grid.resolution = 10) %>% cbind(., algorithm = "Gradient Boosting"),
  models_for_fold_2[["Linear Model"]] %>% partial(pred.var = "Yield_CT", grid.resolution = 10) %>% cbind(., algorithm = "Linear Model")
)
pdp_fold2_Yield_CT$algorithm <- factor(pdp_fold2_Yield_CT$algorithm, levels=c("Random Forests", "Gradient Boosting", "Linear Model"))

# For Tmax variable in Fold 2
pdp_fold2_Tmax <- rbind(
  models_for_fold_2[["Random Forests"]] %>% partial(pred.var = "Tmax", grid.resolution = 10) %>% cbind(., algorithm = "Random Forests"),
  models_for_fold_2[["Gradient Boosting"]] %>% partial(pred.var = "Tmax", grid.resolution = 10) %>% cbind(., algorithm = "Gradient Boosting"),
  models_for_fold_2[["Linear Model"]] %>% partial(pred.var = "Tmax", grid.resolution = 10) %>% cbind(., algorithm = "Linear Model")
)
pdp_fold2_Tmax$algorithm <- factor(pdp_fold2_Tmax$algorithm, levels=c("Random Forests", "Gradient Boosting", "Linear Model"))

Fig16.5a <- ggplot(pdp_fold2_Yield_CT, aes(x=Yield_CT, y=yhat, color=algorithm)) +
  geom_line(linewidth=1) +
  scale_color_manual(values = c("Random Forests" = "#2ca02c", "Gradient Boosting" = "#9467bd", "Linear Model" = "#1f77b4")) +
  ylab("Partial Dependence") +
  labs(title = "pdp for Yield_CT variable for maize in fold 2") +
  theme_bw() +
  theme(legend.position = c(0.7, 0.7),
        axis.title.x = element_blank(),
        axis.text.x = element_blank())


Fig16.5b <- ggplot(pdp_fold2_Tmax, aes(x=Tmax, y=yhat, color=algorithm)) +
  geom_line(linewidth=1) +
  scale_color_manual(values = c("Random Forests" = "#2ca02c", "Gradient Boosting" = "#9467bd", "Linear Model" = "#1f77b4")) +
  ylab("Partial Dependence") +
  labs(title = "pdp for Tmax variable for maize in fold 2") +
  theme_bw() +
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        axis.text.x = element_blank())

Fig16.5c <- ggplot(train_data_fold_2, aes(x=Yield_CT)) + geom_histogram(bins=30) + theme_bw( base_size = 14)
Fig16.5d <- ggplot(train_data_fold_2, aes(x=Tmax)) + geom_histogram(bins=30) + theme_bw( base_size = 14)

Fig16.5 <- Fig16.5a + Fig16.5b + Fig16.5c + Fig16.5d +
  plot_annotation(tag_levels = "a") +
  plot_layout(heights=c(9,1))


# Save the plot
ggsave(paste0(save_path, "Fig16.5.png"), Fig16.5, width = width, height = height)

# For Fold 3
models_for_fold_3 <- models_maize_folds[["Fold 3"]]
train_data_fold_3 <- training_maize_s[[3]]

# For Yield_CT variable in Fold 3
pdp_fold3_Yield_CT <- rbind(
  models_for_fold_3[["Random Forests"]] %>% partial(pred.var = "Yield_CT", grid.resolution = 10) %>% cbind(., algorithm = "Random Forests"),
  models_for_fold_3[["Gradient Boosting"]] %>% partial(pred.var = "Yield_CT", grid.resolution = 10) %>% cbind(., algorithm = "Gradient Boosting"),
  models_for_fold_3[["Linear Model"]] %>% partial(pred.var = "Yield_CT", grid.resolution = 10) %>% cbind(., algorithm = "Linear Model")
)
pdp_fold3_Yield_CT$algorithm <- factor(pdp_fold3_Yield_CT$algorithm, levels=c("Random Forests", "Gradient Boosting", "Linear Model"))

# For Tmax variable in Fold 3
pdp_fold3_Tmax <- rbind(
  models_for_fold_3[["Random Forests"]] %>% partial(pred.var = "Tmax", grid.resolution = 10) %>% cbind(., algorithm = "Random Forests"),
  models_for_fold_3[["Gradient Boosting"]] %>% partial(pred.var = "Tmax", grid.resolution = 10) %>% cbind(., algorithm = "Gradient Boosting"),
  models_for_fold_3[["Linear Model"]] %>% partial(pred.var = "Tmax", grid.resolution = 10) %>% cbind(., algorithm = "Linear Model")
)
pdp_fold3_Tmax$algorithm <- factor(pdp_fold3_Tmax$algorithm, levels=c("Random Forests", "Gradient Boosting", "Linear Model"))

Fig16.6a <- ggplot(pdp_fold3_Yield_CT, aes(x=Yield_CT, y=yhat, color=algorithm)) +
  geom_line(linewidth=1) +
  scale_color_manual(values = c("Random Forests" = "#2ca02c", "Gradient Boosting" = "#9467bd", "Linear Model" = "#1f77b4")) +
  ylab("Partial Dependence") +
  labs(title = "pdp for Yield_CT variable for maize in fold 3") +
  theme_bw() +
  theme(legend.position = c(0.7, 0.7),
        axis.title.x = element_blank(),
        axis.text.x = element_blank())


Fig16.6b <- ggplot(pdp_fold3_Tmax, aes(x=Tmax, y=yhat, color=algorithm)) +
  geom_line(linewidth=1) +
  scale_color_manual(values = c("Random Forests" = "#2ca02c", "Gradient Boosting" = "#9467bd", "Linear Model" = "#1f77b4")) +
  ylab("Partial Dependence") +
  labs(title = "pdp for Tmax variable for maize in fold 3") +
  theme_bw() +
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        axis.text.x = element_blank())

Fig16.6c <- ggplot(train_data_fold_3, aes(x=Yield_CT)) + geom_histogram(bins=30) + theme_bw( base_size = 14)
Fig16.6d <- ggplot(train_data_fold_3, aes(x=Tmax)) + geom_histogram(bins=30) + theme_bw( base_size = 14)

Fig16.6 <- Fig16.6a + Fig16.6b + Fig16.6c + Fig16.6d +
  plot_annotation(tag_levels = "a") +
  plot_layout(heights=c(9,1))


# Save the plot
ggsave(paste0(save_path, "Fig16.6.png"), Fig16.6, width = width, height = height)

#### for fold 4

models_for_fold_4 <- models_maize_folds[["Fold 4"]]
train_data_fold_4 <- training_maize_s[[4]]

# For Yield_CT variable in Fold 4
pdp_fold4_Yield_CT <- rbind(
  models_for_fold_4[["Random Forests"]] %>% partial(pred.var = "Yield_CT", grid.resolution = 10) %>% cbind(., algorithm = "Random Forests"),
  models_for_fold_4[["Gradient Boosting"]] %>% partial(pred.var = "Yield_CT", grid.resolution = 10) %>% cbind(., algorithm = "Gradient Boosting"),
  models_for_fold_4[["Linear Model"]] %>% partial(pred.var = "Yield_CT", grid.resolution = 10) %>% cbind(., algorithm = "Linear Model")
)
pdp_fold4_Yield_CT$algorithm <- factor(pdp_fold4_Yield_CT$algorithm, levels=c("Random Forests", "Gradient Boosting", "Linear Model"))

# For Tmax variable in Fold 4
pdp_fold4_Tmax <- rbind(
  models_for_fold_4[["Random Forests"]] %>% partial(pred.var = "Tmax", grid.resolution = 10) %>% cbind(., algorithm = "Random Forests"),
  models_for_fold_4[["Gradient Boosting"]] %>% partial(pred.var = "Tmax", grid.resolution = 10) %>% cbind(., algorithm = "Gradient Boosting"),
  models_for_fold_4[["Linear Model"]] %>% partial(pred.var = "Tmax", grid.resolution = 10) %>% cbind(., algorithm = "Linear Model")
)
pdp_fold4_Tmax$algorithm <- factor(pdp_fold4_Tmax$algorithm, levels=c("Random Forests", "Gradient Boosting", "Linear Model"))


Fig16.7a <- ggplot(pdp_fold4_Yield_CT, aes(x=Yield_CT, y=yhat, color=algorithm)) +
  geom_line(linewidth=1) +
  scale_color_manual(values = c("Random Forests" = "#2ca02c", "Gradient Boosting" = "#9467bd", "Linear Model" = "#1f77b4")) +
  ylab("Partial Dependence") +
  labs(title = "pdp for Yield_CT variable for maize in fold 4") +
  theme_bw() +
  theme(legend.position = c(0.7, 0.7),
        axis.title.x = element_blank(),
        axis.text.x = element_blank())


Fig16.7b <- ggplot(pdp_fold4_Tmax, aes(x=Tmax, y=yhat, color=algorithm)) +
  geom_line(linewidth=1) +
  scale_color_manual(values = c("Random Forests" = "#2ca02c", "Gradient Boosting" = "#9467bd", "Linear Model" = "#1f77b4")) +
  ylab("Partial Dependence") +
  labs(title = "pdp for Tmax variable for maize in fold 4") +
  theme_bw() +
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        axis.text.x = element_blank())

Fig16.7c <- ggplot(train_data_fold_4, aes(x=Yield_CT)) + geom_histogram(bins=30) + theme_bw( base_size = 14)
Fig16.7d <- ggplot(train_data_fold_4, aes(x=Tmax)) + geom_histogram(bins=30) + theme_bw( base_size = 14)

Fig16.7 <- Fig16.7a + Fig16.7b + Fig16.7c + Fig16.7d +
  plot_annotation(tag_levels = "a") +
  plot_layout(heights=c(9,1))


# Save the plot
ggsave(paste0(save_path, "Fig16.7.png"), Fig16.7, width = width, height = height)


# For Fold 5
models_for_fold_5 <- models_maize_folds[["Fold 5"]]
train_data_fold_5 <- training_maize_s[[5]]

# For Yield_CT variable in Fold 5
pdp_fold5_Yield_CT <- rbind(
  models_for_fold_5[["Random Forests"]] %>% partial(pred.var = "Yield_CT", grid.resolution = 10) %>% cbind(., algorithm = "Random Forests"),
  models_for_fold_5[["Gradient Boosting"]] %>% partial(pred.var = "Yield_CT", grid.resolution = 10) %>% cbind(., algorithm = "Gradient Boosting"),
  models_for_fold_5[["Linear Model"]] %>% partial(pred.var = "Yield_CT", grid.resolution = 10) %>% cbind(., algorithm = "Linear Model")
)
pdp_fold5_Yield_CT$algorithm <- factor(pdp_fold5_Yield_CT$algorithm, levels=c("Random Forests", "Gradient Boosting", "Linear Model"))

# For Tmax variable in Fold 5
pdp_fold5_Tmax <- rbind(
  models_for_fold_5[["Random Forests"]] %>% partial(pred.var = "Tmax", grid.resolution = 10) %>% cbind(., algorithm = "Random Forests"),
  models_for_fold_5[["Gradient Boosting"]] %>% partial(pred.var = "Tmax", grid.resolution = 10) %>% cbind(., algorithm = "Gradient Boosting"),
  models_for_fold_5[["Linear Model"]] %>% partial(pred.var = "Tmax", grid.resolution = 10) %>% cbind(., algorithm = "Linear Model")
)
pdp_fold5_Tmax$algorithm <- factor(pdp_fold5_Tmax$algorithm, levels=c("Random Forests", "Gradient Boosting", "Linear Model"))



Fig16.8a <- ggplot(pdp_fold5_Yield_CT, aes(x=Yield_CT, y=yhat, color=algorithm)) +
  geom_line(linewidth=1) +
  scale_color_manual(values = c("Random Forests" = "#2ca02c", "Gradient Boosting" = "#9467bd", "Linear Model" = "#1f77b4")) +
  ylab("Partial Dependence") +
  labs(title = "pdp for Yield_CT variable for maize in fold 5") +
  theme_bw() +
  theme(legend.position = c(0.7, 0.7),
        axis.title.x = element_blank(),
        axis.text.x = element_blank())


Fig16.8b <- ggplot(pdp_fold5_Tmax, aes(x=Tmax, y=yhat, color=algorithm)) +
  geom_line(linewidth=1) +
  scale_color_manual(values = c("Random Forests" = "#2ca02c", "Gradient Boosting" = "#9467bd", "Linear Model" = "#1f77b4")) +
  ylab("Partial Dependence") +
  labs(title = "pdp for Tmax variable for maize in fold 5") +
  theme_bw() +
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        axis.text.x = element_blank())

Fig16.8c <- ggplot(train_data_fold_5, aes(x=Yield_CT)) + geom_histogram(bins=30) + theme_bw( base_size = 14)
Fig16.8d <- ggplot(train_data_fold_5, aes(x=Tmax)) + geom_histogram(bins=30) + theme_bw( base_size = 14)

Fig16.8 <- Fig16.8a + Fig16.8b + Fig16.8c + Fig16.8d +
  plot_annotation(tag_levels = "a") +
  plot_layout(heights=c(9,1))


# Save the plot
ggsave(paste0(save_path, "Fig16.8.png"), Fig16.8, width = width, height = height)



#####Multiple predictor
# 2-way interactions(PDP)
#Maize 1. rf, 2. gbm, 3. lm

###### For fold 1

pdp_fold1_mrf <- models_for_fold_1[["Random Forests"]] %>%  partial(pred.var=c("Yield_CT","Tmax"), approx=T, grid.resolution = 10) %>% autoplot + 
  labs(title="Random Forests")

pdp_fold1_mgbm <- models_for_fold_1[["Gradient Boosting"]] %>%  partial(pred.var=c("Yield_CT","Tmax"), approx=T, n.trees=500, grid.resolution = 10) %>% autoplot + 
  labs(title="Gradient Boosting")

#pdp_fold1_mlm <- models_for_fold_1[["Linear Model"]] %>%  partial(pred.var=c("Yield_CT","Tmax"), approx=T, grid.resolution = 10) %>% autoplot + 
  #labs(title="Linear Model")


Fig17.4 <-
  pdp_fold1_mrf+ pdp_fold1_mgbm #+ pdp_fold1_mlm


# Save the plot
ggsave(paste0(save_path, "Fig17.4.png"), Fig17.4, width = width, height = height)



###### For fold 2

pdp_fold2_mrf <- models_for_fold_2[["Random Forests"]] %>%  partial(pred.var=c("Yield_CT","Tmax"), approx=T, grid.resolution = 10) %>% autoplot + 
  labs(title="Random Forests")

pdp_fold2_mgbm <- models_for_fold_2[["Gradient Boosting"]] %>%  partial(pred.var=c("Yield_CT","Tmax"), approx=T, n.trees=500, grid.resolution = 10) %>% autoplot + 
  labs(title="Gradient Boosting")

#pdp_fold2_mlm <- models_for_fold_2[["Linear Model"]] %>%  partial(pred.var=c("Yield_CT","Tmax"), approx=T, grid.resolution = 10) %>% autoplot + 
#labs(title="Linear Model")


Fig17.5 <-
  pdp_fold2_mrf+ pdp_fold2_mgbm #+ pdp_fold2_mlm


# Save the plot
ggsave(paste0(save_path, "Fig17.5.png"), Fig17.5, width = width, height = height)

###### For fold 3

pdp_fold3_mrf <- models_for_fold_3[["Random Forests"]] %>%  partial(pred.var=c("Yield_CT","Tmax"), approx=T, grid.resolution = 10) %>% autoplot + 
  labs(title="Random Forests")

pdp_fold3_mgbm <- models_for_fold_3[["Gradient Boosting"]] %>%  partial(pred.var=c("Yield_CT","Tmax"), approx=T, n.trees=500, grid.resolution = 10) %>% autoplot + 
  labs(title="Gradient Boosting")

#pdp_fold3_mlm <- models_for_fold_3[["Linear Model"]] %>%  partial(pred.var=c("Yield_CT","Tmax"), approx=T, grid.resolution = 10) %>% autoplot + 
#labs(title="Linear Model")


Fig17.6 <-
  pdp_fold3_mrf+ pdp_fold3_mgbm #+ pdp_fold3_mlm


# Save the plot
ggsave(paste0(save_path, "Fig17.6.png"), Fig17.6, width = width, height = height)


###### For fold 4

pdp_fold4_mrf <- models_for_fold_4[["Random Forests"]] %>%  partial(pred.var=c("Yield_CT","Tmax"), approx=T, grid.resolution = 10) %>% autoplot + 
  labs(title="Random Forests")

pdp_fold4_mgbm <- models_for_fold_4[["Gradient Boosting"]] %>%  partial(pred.var=c("Yield_CT","Tmax"), approx=T, n.trees=500, grid.resolution = 10) %>% autoplot + 
  labs(title="Gradient Boosting")

#pdp_fold4_mlm <- models_for_fold_4[["Linear Model"]] %>%  partial(pred.var=c("Yield_CT","Tmax"), approx=T, grid.resolution = 10) %>% autoplot + 
#labs(title="Linear Model")


Fig17.7 <-
  pdp_fold4_mrf+ pdp_fold4_mgbm #+ pdp_fold4_mlm


# Save the plot
ggsave(paste0(save_path, "Fig17.7.png"), Fig17.7, width = width, height = height)


###### For fold 5

pdp_fold5_mrf <- models_for_fold_5[["Random Forests"]] %>%  partial(pred.var=c("Yield_CT","Tmax"), approx=T, grid.resolution = 10) %>% autoplot + 
  labs(title="Random Forests")

pdp_fold5_mgbm <- models_for_fold_5[["Gradient Boosting"]] %>%  partial(pred.var=c("Yield_CT","Tmax"), approx=T, n.trees=500, grid.resolution = 10) %>% autoplot + 
  labs(title="Gradient Boosting")

#pdp_fold5_mlm <- models_for_fold_5[["Linear Model"]] %>%  partial(pred.var=c("Yield_CT","Tmax"), approx=T, grid.resolution = 10) %>% autoplot + 
#labs(title="Linear Model")


Fig17.8 <-
  pdp_fold5_mrf+ pdp_fold5_mgbm #+ pdp_fold5_mlm


# Save the plot
ggsave(paste0(save_path, "Fig17.8.png"), Fig17.8, width = width, height = height)



#########


####ALE plots

###All variable

##### For fold 1

# Extract the Random Forest model from models_sorghum
rf_model_fold1 <- models_for_fold_1[["Random Forests"]]
gbm_model_fold1 <- models_for_fold_1[["Gradient Boosting"]]
lm_model_fold1 <- models_for_fold_1[["Linear Model"]]

# Create a Predictor object
X_fold1_M <- train_data_fold_1[, !names(train_data_fold_1) %in% c("yhat")]  # Exclude the target variable
predictor_fold1_rf <- Predictor$new(rf_model_fold1, data = X_fold1_M, y = train_data_fold_1$yhat)
predictor_fold1_gbm <- Predictor$new(gbm_model_fold1, data = X_fold1_M, y = train_data_fold_1$yhat)
predictor_fold1_lm <- Predictor$new(lm_model_fold1, data = X_fold1_M, y = train_data_fold_1$yhat)

Fig18.4A1 <-ale_fold1_rf <-FeatureEffects$new(predictor_fold1_rf)%>%plot()
Fig18.4A2 <-ale_fold1_gbm <-FeatureEffects$new(predictor_fold1_gbm)%>%plot()
Fig18.4A3 <-ale_fold1_lm <-FeatureEffects$new(predictor_fold1_lm)%>%plot()

# Save the plot
ggsave(paste0(save_path, "Fig18.4A1.png"), Fig18.4A1, width = width, height = height, dpi = 300)
ggsave(paste0(save_path, "Fig18.4A2.png"), Fig18.4A2, width = width, height = height, dpi = 300)
ggsave(paste0(save_path, "Fig18.4A3.png"), Fig18.4A3, width = width, height = height, dpi = 300)



### Single variable
### With Yield_CT variable

ale_fold1_rf_Yield_CT = FeatureEffect$new(predictor_fold1_rf, "Yield_CT", method = "ale")$ plot()+
  scale_x_continuous("Yield_CT") +
  ggtitle("Random Forests on Maize")

#Fig18.4b1 <- ale_maize_rf_Yield_CT

ale_fold1_gbm_Yield_CT = FeatureEffect$new(predictor_fold1_gbm, "Yield_CT", method = "ale")$ plot()+
  scale_x_continuous("Yield_CT") +
  ggtitle("Gradient Boosting on Maize")

#Fig18.4b2 <- ale_maize_gbm_Yield_CT

ale_fold1_lm_Yield_CT = FeatureEffect$new(predictor_fold1_lm, "Yield_CT", method = "ale")$ plot()+
  scale_x_continuous("Yield_CT") +
  ggtitle("Linear Model on Maize")

#Fig18.4b3 <- ale_maize_lm_Yield_CT


###Combining

#####
# Extract data from ALE objects
data_Ale_fold1_rf <- ale_fold1_rf_Yield_CT$data
data_Ale_fold1_gbm <- ale_fold1_gbm_Yield_CT$data
data_Ale_fold1_lm <- ale_fold1_lm_Yield_CT$data

# Extract effects from ALE objects
effects_Ale_fold1_rf <- ale_fold1_rf_Yield_CT$effects
effects_Ale_fold1_gbm <- ale_fold1_gbm_Yield_CT$effects
effects_Ale_fold1_lm <- ale_fold1_lm_Yield_CT$effects

# Add Algorithm column to the data
data_Ale_fold1_rf$Algorithm <- "Random Forests"
data_Ale_fold1_gbm$Algorithm <- "Gradient Boosting"
data_Ale_fold1_lm$Algorithm <- "Linear Model"


# Combine data and effects
combined_data_fold1 <- rbind(data_Ale_fold1_rf, data_Ale_fold1_gbm, data_Ale_fold1_lm)



# Create a plot
combined_ale_fold1_Yield_CT <- 
  
  ggplot(combined_data_fold1, aes(x = Yield_CT, y = .value, color = Algorithm)) +
  geom_line() +
  scale_color_manual(values = c("Random Forests" = "#2ca02c", "Gradient Boosting" = "#9467bd", "Linear Model" = "#1f77b4")) +
  labs(title = "ALE Plots in  fold 1 for Maize Yield_CT", y = "ALE Effect")

# Save the plot
ggsave(paste0(save_path, "Fig18.4CCC.png"), combined_ale_fold1_Yield_CT, width = width, height = height)




####With Tmax variable

ale_fold1_rf_Tmax = FeatureEffect$new(predictor_fold1_rf, "Tmax", method = "ale")$ plot()+
  scale_x_continuous("Tmax") +
  ggtitle("Random Forests on Maize")
#Fig18.4c1 <- ale_maize_rf_Tmax

ale_fold1_gbm_Tmax = FeatureEffect$new(predictor_fold1_gbm, "Tmax", method = "ale")$ plot()+
  scale_x_continuous("Tmax") +
  ggtitle("Gradient Boosting on Maize")
#Fig18.4c2 <- ale_maize_gbm_Tmax

ale_fold1_lm_Tmax = FeatureEffect$new(predictor_fold1_lm, "Tmax", method = "ale")$ plot()+
  scale_x_continuous("Tmax") +
  ggtitle("Linear Model on Maize")
#Fig18.4c3 <- ale_maize_lm_Tmax

#plot_combined_18.4c <- plot_grid(Fig18.4c1, Fig18.4c2,Fig18.4c3, ncol = 3)

# Save the plot
#ggsave(paste0(save_path, "Fig18.4c.png"), plot_combined_18.4c, width = width, height = height)


###Combining

#####
# Extract data from ALE objects
data_Ale_fold1_rf_Tmax <- ale_fold1_rf_Tmax$data
data_Ale_fold1_gbm_Tmax <- ale_fold1_gbm_Tmax$data
data_Ale_fold1_lm_Tmax <- ale_fold1_lm_Tmax$data

# Add Algorithm column to the data
data_Ale_fold1_rf_Tmax$Algorithm <- "Random Forests"
data_Ale_fold1_gbm_Tmax$Algorithm <- "Gradient Boosting"
data_Ale_fold1_lm_Tmax$Algorithm <- "Linear Model"


# Combine data and effects
combined_data_fold1_Tmax <- rbind(data_Ale_fold1_rf_Tmax, data_Ale_fold1_gbm_Tmax, data_Ale_fold1_lm_Tmax)

# Plotting
combined_ale_fold1_Tmax <- 
  
  ggplot(combined_data_fold1_Tmax, aes(x = Tmax, y = .value, color = Algorithm)) +
  geom_line() +
  scale_color_manual(values = c("Random Forests" = "#2ca02c", "Gradient Boosting" = "#9467bd", "Linear Model" = "#1f77b4")) +
  labs(title = "ALE Plots in  fold 1 for Maize Tmax", y = "ALE Effect")

# Save the plot
ggsave(paste0(save_path, "Fig18.4Tmax.png"), combined_ale_fold1_Tmax, width = width, height = height)



##### For fold 2

# Extract the Random Forest model from models_sorghum
rf_model_fold2 <- models_for_fold_2[["Random Forests"]]
gbm_model_fold2 <- models_for_fold_2[["Gradient Boosting"]]
lm_model_fold2 <- models_for_fold_2[["Linear Model"]]

# Create a Predictor object
X_fold2_M <- train_data_fold_2[, !names(train_data_fold_2) %in% c("yhat")]  # Exclude the target variable
predictor_fold2_rf <- Predictor$new(rf_model_fold2, data = X_fold2_M, y = train_data_fold_2$yhat)
predictor_fold2_gbm <- Predictor$new(gbm_model_fold2, data = X_fold2_M, y = train_data_fold_2$yhat)
predictor_fold2_lm <- Predictor$new(lm_model_fold2, data = X_fold2_M, y = train_data_fold_2$yhat)

Fig18.5A1 <-ale_fold2_rf <-FeatureEffects$new(predictor_fold2_rf)%>%plot()
Fig18.5A2 <-ale_fold2_gbm <-FeatureEffects$new(predictor_fold2_gbm)%>%plot()
Fig18.5A3 <-ale_fold2_lm <-FeatureEffects$new(predictor_fold2_lm)%>%plot()

# Save the plot
ggsave(paste0(save_path, "Fig18.5A1.png"), Fig18.5A1, width = width, height = height, dpi = 300)
ggsave(paste0(save_path, "Fig18.5A2.png"), Fig18.5A2, width = width, height = height, dpi = 300)
ggsave(paste0(save_path, "Fig18.5A3.png"), Fig18.5A3, width = width, height = height, dpi = 300)


### Single variable
### With Yield_CT variable

ale_fold2_rf_Yield_CT = FeatureEffect$new(predictor_fold2_rf, "Yield_CT", method = "ale")$ plot()+
  scale_x_continuous("Yield_CT") +
  ggtitle("Random Forests on Maize")

#Fig18.5b1 <- ale_maize_rf_Yield_CT

ale_fold2_gbm_Yield_CT = FeatureEffect$new(predictor_fold2_gbm, "Yield_CT", method = "ale")$ plot()+
  scale_x_continuous("Yield_CT") +
  ggtitle("Gradient Boosting on Maize")

#Fig18.5b2 <- ale_maize_gbm_Yield_CT

ale_fold2_lm_Yield_CT = FeatureEffect$new(predictor_fold2_lm, "Yield_CT", method = "ale")$ plot()+
  scale_x_continuous("Yield_CT") +
  ggtitle("Linear Model on Maize")

#Fig18.5b3 <- ale_maize_lm_Yield_CT


###Combining

#####
# Extract data from ALE objects
data_Ale_fold2_rf <- ale_fold2_rf_Yield_CT$data
data_Ale_fold2_gbm <- ale_fold2_gbm_Yield_CT$data
data_Ale_fold2_lm <- ale_fold2_lm_Yield_CT$data

# Extract effects from ALE objects
effects_Ale_fold2_rf <- ale_fold2_rf_Yield_CT$effects
effects_Ale_fold2_gbm <- ale_fold2_gbm_Yield_CT$effects
effects_Ale_fold2_lm <- ale_fold2_lm_Yield_CT$effects

# Add Algorithm column to the data
data_Ale_fold2_rf$Algorithm <- "Random Forests"
data_Ale_fold2_gbm$Algorithm <- "Gradient Boosting"
data_Ale_fold2_lm$Algorithm <- "Linear Model"


# Combine data and effects
combined_data_fold2 <- rbind(data_Ale_fold2_rf, data_Ale_fold2_gbm, data_Ale_fold2_lm)



# Create a plot
combined_ale_fold2_Yield_CT <- 
  
  ggplot(combined_data_fold2, aes(x = Yield_CT, y = .value, color = Algorithm)) +
  geom_line() +
  scale_color_manual(values = c("Random Forests" = "#2ca02c", "Gradient Boosting" = "#9467bd", "Linear Model" = "#1f77b4")) +
  labs(title = "ALE Plots in  fold 2 for Maize Yield_CT", y = "ALE Effect")

# Save the plot
ggsave(paste0(save_path, "Fig18.5CCC.png"), combined_ale_fold2_Yield_CT, width = width, height = height)



####With Tmax variable

ale_fold2_rf_Tmax = FeatureEffect$new(predictor_fold2_rf, "Tmax", method = "ale")$ plot()+
  scale_x_continuous("Tmax") +
  ggtitle("Random Forests on Maize")
#Fig18.5c1 <- ale_maize_rf_Tmax

ale_fold2_gbm_Tmax = FeatureEffect$new(predictor_fold2_gbm, "Tmax", method = "ale")$ plot()+
  scale_x_continuous("Tmax") +
  ggtitle("Gradient Boosting on Maize")
#Fig18.5c2 <- ale_maize_gbm_Tmax

ale_fold2_lm_Tmax = FeatureEffect$new(predictor_fold2_lm, "Tmax", method = "ale")$ plot()+
  scale_x_continuous("Tmax") +
  ggtitle("Linear Model on Maize")
#Fig18.5c3 <- ale_maize_lm_Tmax

#plot_combined_18.5c <- plot_grid(Fig18.5c1, Fig18.5c2,Fig18.5c3, ncol = 3)

# Save the plot
#ggsave(paste0(save_path, "Fig18.5c.png"), plot_combined_18.5c, width = width, height = height)


###Combining

#####
# Extract data from ALE objects
data_Ale_fold2_rf_Tmax <- ale_fold2_rf_Tmax$data
data_Ale_fold2_gbm_Tmax <- ale_fold2_gbm_Tmax$data
data_Ale_fold2_lm_Tmax <- ale_fold2_lm_Tmax$data

# Add Algorithm column to the data
data_Ale_fold2_rf_Tmax$Algorithm <- "Random Forests"
data_Ale_fold2_gbm_Tmax$Algorithm <- "Gradient Boosting"
data_Ale_fold2_lm_Tmax$Algorithm <- "Linear Model"


# Combine data and effects
combined_data_fold2_Tmax <- rbind(data_Ale_fold2_rf_Tmax, data_Ale_fold2_gbm_Tmax, data_Ale_fold2_lm_Tmax)

# Plotting
combined_ale_fold2_Tmax <- 
  
  ggplot(combined_data_fold2_Tmax, aes(x = Tmax, y = .value, color = Algorithm)) +
  geom_line() +
  scale_color_manual(values = c("Random Forests" = "#2ca02c", "Gradient Boosting" = "#9467bd", "Linear Model" = "#1f77b4")) +
  labs(title = "ALE Plots in  fold 2 for Maize Tmax", y = "ALE Effect")

# Save the plot
ggsave(paste0(save_path, "Fig18.5Tmax.png"), combined_ale_fold2_Tmax, width = width, height = height)


##### For fold 3

# Extract the Random Forest model from models_sorghum
rf_model_fold3 <- models_for_fold_3[["Random Forests"]]
gbm_model_fold3 <- models_for_fold_3[["Gradient Boosting"]]
lm_model_fold3 <- models_for_fold_3[["Linear Model"]]

# Create a Predictor object
X_fold3_M <- train_data_fold_3[, !names(train_data_fold_3) %in% c("yhat")]  # Exclude the target variable
predictor_fold3_rf <- Predictor$new(rf_model_fold3, data = X_fold3_M, y = train_data_fold_3$yhat)
predictor_fold3_gbm <- Predictor$new(gbm_model_fold3, data = X_fold3_M, y = train_data_fold_3$yhat)
predictor_fold3_lm <- Predictor$new(lm_model_fold3, data = X_fold3_M, y = train_data_fold_3$yhat)

Fig18.6A1 <-ale_fold3_rf <-FeatureEffects$new(predictor_fold3_rf)%>%plot()
Fig18.6A2 <-ale_fold3_gbm <-FeatureEffects$new(predictor_fold3_gbm)%>%plot()
Fig18.6A3 <-ale_fold3_lm <-FeatureEffects$new(predictor_fold3_lm)%>%plot()

# Save the plot
ggsave(paste0(save_path, "Fig18.6A1.png"), Fig18.6A1, width = width, height = height, dpi = 300)
ggsave(paste0(save_path, "Fig18.6A2.png"), Fig18.6A2, width = width, height = height, dpi = 300)
ggsave(paste0(save_path, "Fig18.6A3.png"), Fig18.6A3, width = width, height = height, dpi = 300)


### Single variable
### With Yield_CT variable

ale_fold3_rf_Yield_CT = FeatureEffect$new(predictor_fold3_rf, "Yield_CT", method = "ale")$ plot()+
  scale_x_continuous("Yield_CT") +
  ggtitle("Random Forests on Maize")

#Fig18.6b1 <- ale_maize_rf_Yield_CT

ale_fold3_gbm_Yield_CT = FeatureEffect$new(predictor_fold3_gbm, "Yield_CT", method = "ale")$ plot()+
  scale_x_continuous("Yield_CT") +
  ggtitle("Gradient Boosting on Maize")

#Fig18.6b2 <- ale_maize_gbm_Yield_CT

ale_fold3_lm_Yield_CT = FeatureEffect$new(predictor_fold3_lm, "Yield_CT", method = "ale")$ plot()+
  scale_x_continuous("Yield_CT") +
  ggtitle("Linear Model on Maize")

#Fig18.6b3 <- ale_maize_lm_Yield_CT


###Combining

#####
# Extract data from ALE objects
data_Ale_fold3_rf <- ale_fold3_rf_Yield_CT$data
data_Ale_fold3_gbm <- ale_fold3_gbm_Yield_CT$data
data_Ale_fold3_lm <- ale_fold3_lm_Yield_CT$data

# Extract effects from ALE objects
effects_Ale_fold3_rf <- ale_fold3_rf_Yield_CT$effects
effects_Ale_fold3_gbm <- ale_fold3_gbm_Yield_CT$effects
effects_Ale_fold3_lm <- ale_fold3_lm_Yield_CT$effects

# Add Algorithm column to the data
data_Ale_fold3_rf$Algorithm <- "Random Forests"
data_Ale_fold3_gbm$Algorithm <- "Gradient Boosting"
data_Ale_fold3_lm$Algorithm <- "Linear Model"


# Combine data and effects
combined_data_fold3 <- rbind(data_Ale_fold3_rf, data_Ale_fold3_gbm, data_Ale_fold3_lm)



#Plotting
combined_ale_fold3_Yield_CT <- 
  
  ggplot(combined_data_fold3, aes(x = Yield_CT, y = .value, color = Algorithm)) +
  geom_line() +
  scale_color_manual(values = c("Random Forests" = "#2ca02c", "Gradient Boosting" = "#9467bd", "Linear Model" = "#1f77b4")) +
  labs(title = "ALE Plots in  fold 3 for Maize Yield_CT", y = "ALE Effect")

# Save the plot
ggsave(paste0(save_path, "Fig18.6CCC.png"), combined_ale_fold3_Yield_CT, width = width, height = height)



####With Tmax variable

ale_fold3_rf_Tmax = FeatureEffect$new(predictor_fold3_rf, "Tmax", method = "ale")$ plot()+
  scale_x_continuous("Tmax") +
  ggtitle("Random Forests on Maize")
#Fig18.6c1 <- ale_maize_rf_Tmax

ale_fold3_gbm_Tmax = FeatureEffect$new(predictor_fold3_gbm, "Tmax", method = "ale")$ plot()+
  scale_x_continuous("Tmax") +
  ggtitle("Gradient Boosting on Maize")
#Fig18.6c2 <- ale_maize_gbm_Tmax

ale_fold3_lm_Tmax = FeatureEffect$new(predictor_fold3_lm, "Tmax", method = "ale")$ plot()+
  scale_x_continuous("Tmax") +
  ggtitle("Linear Model on Maize")
#Fig18.6c3 <- ale_maize_lm_Tmax

#plot_combined_18.6c <- plot_grid(Fig18.6c1, Fig18.6c2,Fig18.6c3, ncol = 3)

# Save the plot
#ggsave(paste0(save_path, "Fig18.6c.png"), plot_combined_18.6c, width = width, height = height)


###Combining

#####
# Extract data from ALE objects
data_Ale_fold3_rf_Tmax <- ale_fold3_rf_Tmax$data
data_Ale_fold3_gbm_Tmax <- ale_fold3_gbm_Tmax$data
data_Ale_fold3_lm_Tmax <- ale_fold3_lm_Tmax$data

# Add Algorithm column to the data
data_Ale_fold3_rf_Tmax$Algorithm <- "Random Forests"
data_Ale_fold3_gbm_Tmax$Algorithm <- "Gradient Boosting"
data_Ale_fold3_lm_Tmax$Algorithm <- "Linear Model"


# Combine data and effects
combined_data_fold3_Tmax <- rbind(data_Ale_fold3_rf_Tmax, data_Ale_fold3_gbm_Tmax, data_Ale_fold3_lm_Tmax)

# Plotting
combined_ale_fold3_Tmax <- 
  
  ggplot(combined_data_fold3_Tmax, aes(x = Tmax, y = .value, color = Algorithm)) +
  geom_line() +
  scale_color_manual(values = c("Random Forests" = "#2ca02c", "Gradient Boosting" = "#9467bd", "Linear Model" = "#1f77b4")) +
  labs(title = "ALE Plots in  fold 3 for Maize Tmax", y = "ALE Effect")

# Save the plot
ggsave(paste0(save_path, "Fig18.6Tmax.png"), combined_ale_fold3_Tmax, width = width, height = height)




##### For fold 4

# Extract the Random Forest model from models_sorghum
rf_model_fold4 <- models_for_fold_4[["Random Forests"]]
gbm_model_fold4 <- models_for_fold_4[["Gradient Boosting"]]
lm_model_fold4 <- models_for_fold_4[["Linear Model"]]

# Create a Predictor object
X_fold4_M <- train_data_fold_4[, !names(train_data_fold_4) %in% c("yhat")]  # Exclude the target variable
predictor_fold4_rf <- Predictor$new(rf_model_fold4, data = X_fold4_M, y = train_data_fold_4$yhat)
predictor_fold4_gbm <- Predictor$new(gbm_model_fold4, data = X_fold4_M, y = train_data_fold_4$yhat)
predictor_fold4_lm <- Predictor$new(lm_model_fold4, data = X_fold4_M, y = train_data_fold_4$yhat)

Fig18.7A1 <-ale_fold4_rf <-FeatureEffects$new(predictor_fold4_rf)%>%plot()
Fig18.7A2 <-ale_fold4_gbm <-FeatureEffects$new(predictor_fold4_gbm)%>%plot()
Fig18.7A3 <-ale_fold4_lm <-FeatureEffects$new(predictor_fold4_lm)%>%plot()

# Save the plot
ggsave(paste0(save_path, "Fig18.7A1.png"), Fig18.7A1, width = width, height = height, dpi = 300)
ggsave(paste0(save_path, "Fig18.7A2.png"), Fig18.7A2, width = width, height = height, dpi = 300)
ggsave(paste0(save_path, "Fig18.7A3.png"), Fig18.7A3, width = width, height = height, dpi = 300)


### Single variable
### With Yield_CT variable

ale_fold4_rf_Yield_CT = FeatureEffect$new(predictor_fold4_rf, "Yield_CT", method = "ale")$ plot()+
  scale_x_continuous("Yield_CT") +
  ggtitle("Random Forests on Maize")

#Fig18.7b1 <- ale_maize_rf_Yield_CT

ale_fold4_gbm_Yield_CT = FeatureEffect$new(predictor_fold4_gbm, "Yield_CT", method = "ale")$ plot()+
  scale_x_continuous("Yield_CT") +
  ggtitle("Gradient Boosting on Maize")

#Fig18.7b2 <- ale_maize_gbm_Yield_CT

ale_fold4_lm_Yield_CT = FeatureEffect$new(predictor_fold4_lm, "Yield_CT", method = "ale")$ plot()+
  scale_x_continuous("Yield_CT") +
  ggtitle("Linear Model on Maize")

#Fig18.7b3 <- ale_maize_lm_Yield_CT


###Combining

#####
# Extract data from ALE objects
data_Ale_fold4_rf <- ale_fold4_rf_Yield_CT$data
data_Ale_fold4_gbm <- ale_fold4_gbm_Yield_CT$data
data_Ale_fold4_lm <- ale_fold4_lm_Yield_CT$data

# Extract effects from ALE objects
effects_Ale_fold4_rf <- ale_fold4_rf_Yield_CT$effects
effects_Ale_fold4_gbm <- ale_fold4_gbm_Yield_CT$effects
effects_Ale_fold4_lm <- ale_fold4_lm_Yield_CT$effects

# Add Algorithm column to the data
data_Ale_fold4_rf$Algorithm <- "Random Forests"
data_Ale_fold4_gbm$Algorithm <- "Gradient Boosting"
data_Ale_fold4_lm$Algorithm <- "Linear Model"


# Combine data and effects
combined_data_fold4 <- rbind(data_Ale_fold4_rf, data_Ale_fold4_gbm, data_Ale_fold4_lm)



# Create a plot
combined_ale_fold4_Yield_CT <- 
  
  ggplot(combined_data_fold4, aes(x = Yield_CT, y = .value, color = Algorithm)) +
  geom_line() +
  scale_color_manual(values = c("Random Forests" = "#2ca02c", "Gradient Boosting" = "#9467bd", "Linear Model" = "#1f77b4")) +
  labs(title = "ALE Plots in  fold 4 for Maize Yield_CT", y = "ALE Effect")

# Save the plot
ggsave(paste0(save_path, "Fig18.7CCC.png"), combined_ale_fold4_Yield_CT, width = width, height = height)



####With Tmax variable

ale_fold4_rf_Tmax = FeatureEffect$new(predictor_fold4_rf, "Tmax", method = "ale")$ plot()+
  scale_x_continuous("Tmax") +
  ggtitle("Random Forests on Maize")
#Fig18.7c1 <- ale_maize_rf_Tmax

ale_fold4_gbm_Tmax = FeatureEffect$new(predictor_fold4_gbm, "Tmax", method = "ale")$ plot()+
  scale_x_continuous("Tmax") +
  ggtitle("Gradient Boosting on Maize")
#Fig18.7c2 <- ale_maize_gbm_Tmax

ale_fold4_lm_Tmax = FeatureEffect$new(predictor_fold4_lm, "Tmax", method = "ale")$ plot()+
  scale_x_continuous("Tmax") +
  ggtitle("Linear Model on Maize")
#Fig18.7c3 <- ale_maize_lm_Tmax

#plot_combined_18.7c <- plot_grid(Fig18.7c1, Fig18.7c2,Fig18.7c3, ncol = 3)

# Save the plot
#ggsave(paste0(save_path, "Fig18.7c.png"), plot_combined_18.7c, width = width, height = height)


###Combining

#####
# Extract data from ALE objects
data_Ale_fold4_rf_Tmax <- ale_fold4_rf_Tmax$data
data_Ale_fold4_gbm_Tmax <- ale_fold4_gbm_Tmax$data
data_Ale_fold4_lm_Tmax <- ale_fold4_lm_Tmax$data

# Add Algorithm column to the data
data_Ale_fold4_rf_Tmax$Algorithm <- "Random Forests"
data_Ale_fold4_gbm_Tmax$Algorithm <- "Gradient Boosting"
data_Ale_fold4_lm_Tmax$Algorithm <- "Linear Model"


# Combine data and effects
combined_data_fold4_Tmax <- rbind(data_Ale_fold4_rf_Tmax, data_Ale_fold4_gbm_Tmax, data_Ale_fold4_lm_Tmax)

# Plotting
combined_ale_fold4_Tmax <- 
  
  ggplot(combined_data_fold4_Tmax, aes(x = Tmax, y = .value, color = Algorithm)) +
  geom_line() +
  scale_color_manual(values = c("Random Forests" = "#2ca02c", "Gradient Boosting" = "#9467bd", "Linear Model" = "#1f77b4")) +
  labs(title = "ALE Plots in  fold 4 for Maize Tmax", y = "ALE Effect")

# Save the plot
ggsave(paste0(save_path, "Fig18.7Tmax.png"), combined_ale_fold4_Tmax, width = width, height = height)


##### For fold 5

# Extract the Random Forest model from models_sorghum
rf_model_fold5 <- models_for_fold_5[["Random Forests"]]
gbm_model_fold5 <- models_for_fold_5[["Gradient Boosting"]]
lm_model_fold5 <- models_for_fold_5[["Linear Model"]]

# Create a Predictor object
X_fold5_M <- train_data_fold_5[, !names(train_data_fold_5) %in% c("yhat")]  # Exclude the target variable
predictor_fold5_rf <- Predictor$new(rf_model_fold5, data = X_fold5_M, y = train_data_fold_5$yhat)
predictor_fold5_gbm <- Predictor$new(gbm_model_fold5, data = X_fold5_M, y = train_data_fold_5$yhat)
predictor_fold5_lm <- Predictor$new(lm_model_fold5, data = X_fold5_M, y = train_data_fold_5$yhat)

Fig18.8A1 <-ale_fold5_rf <-FeatureEffects$new(predictor_fold5_rf)%>%plot()
Fig18.8A2 <-ale_fold5_gbm <-FeatureEffects$new(predictor_fold5_gbm)%>%plot()
Fig18.8A3 <-ale_fold5_lm <-FeatureEffects$new(predictor_fold5_lm)%>%plot()

# Save the plot
ggsave(paste0(save_path, "Fig18.8A1.png"), Fig18.8A1, width = width, height = height, dpi = 300)
ggsave(paste0(save_path, "Fig18.8A2.png"), Fig18.8A2, width = width, height = height, dpi = 300)
ggsave(paste0(save_path, "Fig18.8A3.png"), Fig18.8A3, width = width, height = height, dpi = 300)


### Single variable
### With Yield_CT variable

ale_fold5_rf_Yield_CT = FeatureEffect$new(predictor_fold5_rf, "Yield_CT", method = "ale")$ plot()+
  scale_x_continuous("Yield_CT") +
  ggtitle("Random Forests on Maize")

#Fig18.8b1 <- ale_maize_rf_Yield_CT

ale_fold5_gbm_Yield_CT = FeatureEffect$new(predictor_fold5_gbm, "Yield_CT", method = "ale")$ plot()+
  scale_x_continuous("Yield_CT") +
  ggtitle("Gradient Boosting on Maize")

#Fig18.8b2 <- ale_maize_gbm_Yield_CT

ale_fold5_lm_Yield_CT = FeatureEffect$new(predictor_fold5_lm, "Yield_CT", method = "ale")$ plot()+
  scale_x_continuous("Yield_CT") +
  ggtitle("Linear Model on Maize")

#Fig18.8b3 <- ale_maize_lm_Yield_CT


###Combining

#####
# Extract data from ALE objects
data_Ale_fold5_rf <- ale_fold5_rf_Yield_CT$data
data_Ale_fold5_gbm <- ale_fold5_gbm_Yield_CT$data
data_Ale_fold5_lm <- ale_fold5_lm_Yield_CT$data

# Extract effects from ALE objects
effects_Ale_fold5_rf <- ale_fold5_rf_Yield_CT$effects
effects_Ale_fold5_gbm <- ale_fold5_gbm_Yield_CT$effects
effects_Ale_fold5_lm <- ale_fold5_lm_Yield_CT$effects

# Add Algorithm column to the data
data_Ale_fold5_rf$Algorithm <- "Random Forests"
data_Ale_fold5_gbm$Algorithm <- "Gradient Boosting"
data_Ale_fold5_lm$Algorithm <- "Linear Model"


# Combine data and effects
combined_data_fold5 <- rbind(data_Ale_fold5_rf, data_Ale_fold5_gbm, data_Ale_fold5_lm)



# Create a plot
combined_ale_fold5_Yield_CT <- 
  
  ggplot(combined_data_fold5, aes(x = Yield_CT, y = .value, color = Algorithm)) +
  geom_line() +
  scale_color_manual(values = c("Random Forests" = "#2ca02c", "Gradient Boosting" = "#9467bd", "Linear Model" = "#1f77b4")) +
  labs(title = "ALE Plots in  fold 5 for Maize Yield_CT", y = "ALE Effect")

# Save the plot
ggsave(paste0(save_path, "Fig18.8CCC.png"), combined_ale_fold5_Yield_CT, width = width, height = height)



####With Tmax variable

ale_fold5_rf_Tmax = FeatureEffect$new(predictor_fold5_rf, "Tmax", method = "ale")$ plot()+
  scale_x_continuous("Tmax") +
  ggtitle("Random Forests on Maize")
#Fig18.8c1 <- ale_maize_rf_Tmax

ale_fold5_gbm_Tmax = FeatureEffect$new(predictor_fold5_gbm, "Tmax", method = "ale")$ plot()+
  scale_x_continuous("Tmax") +
  ggtitle("Gradient Boosting on Maize")
#Fig18.8c2 <- ale_maize_gbm_Tmax

ale_fold5_lm_Tmax = FeatureEffect$new(predictor_fold5_lm, "Tmax", method = "ale")$ plot()+
  scale_x_continuous("Tmax") +
  ggtitle("Linear Model on Maize")
#Fig18.8c3 <- ale_maize_lm_Tmax

#plot_combined_18.8c <- plot_grid(Fig18.8c1, Fig18.8c2,Fig18.8c3, ncol = 3)

# Save the plot
#ggsave(paste0(save_path, "Fig18.8c.png"), plot_combined_18.8c, width = width, height = height)


###Combining

#####
# Extract data from ALE objects
data_Ale_fold5_rf_Tmax <- ale_fold5_rf_Tmax$data
data_Ale_fold5_gbm_Tmax <- ale_fold5_gbm_Tmax$data
data_Ale_fold5_lm_Tmax <- ale_fold5_lm_Tmax$data

# Add Algorithm column to the data
data_Ale_fold5_rf_Tmax$Algorithm <- "Random Forests"
data_Ale_fold5_gbm_Tmax$Algorithm <- "Gradient Boosting"
data_Ale_fold5_lm_Tmax$Algorithm <- "Linear Model"


# Combine data and effects
combined_data_fold5_Tmax <- rbind(data_Ale_fold5_rf_Tmax, data_Ale_fold5_gbm_Tmax, data_Ale_fold5_lm_Tmax)

# Plotting
combined_ale_fold5_Tmax <- 
  
  ggplot(combined_data_fold5_Tmax, aes(x = Tmax, y = .value, color = Algorithm)) +
  geom_line() +
  scale_color_manual(values = c("Random Forests" = "#2ca02c", "Gradient Boosting" = "#9467bd", "Linear Model" = "#1f77b4")) +
  labs(title = "ALE Plots in  fold 5 for Maize Tmax", y = "ALE Effect")

# Save the plot
ggsave(paste0(save_path, "Fig18.8Tmax.png"), combined_ale_fold5_Tmax, width = width, height = height)



############
########
#####2D ###with multiple variable

######for fold 1

# Random Forests
ale_fold1_rf_Yield_CT_Tmax_2D <- FeatureEffect$new(predictor_fold1_rf, feature = c("Yield_CT", "Tmax"), method = "ale", grid.size = 40)$plot() +   
  labs(title = "Random Forest") +
  scale_fill_gradient("ALE", low = "red", high = "yellow") + 
  scale_x_continuous("Yield_CT") + 
  scale_y_continuous("Maximum Temperature") +
  scale_fill_viridis(option = "D")

fig19.4a <- ale_fold1_rf_Yield_CT_Tmax_2D

# Gradient Boosting
ale_fold1_gbm_Yield_CT_Tmax_2D <- FeatureEffect$new(predictor_fold1_gbm, feature = c("Yield_CT", "Tmax"), method = "ale", grid.size = 40)$plot() +   
  labs(title = "Gradient Boosting") +
  scale_fill_gradient("ALE", low = "red", high = "yellow") + 
  scale_x_continuous("Yield_CT") + 
  scale_y_continuous("Maximum Temperature") +
  scale_fill_viridis(option = "D")

fig19.4b <- ale_fold1_gbm_Yield_CT_Tmax_2D

# Linear Model
'ale_fold1_lm_Yield_CT_Tmax_2D <- FeatureEffect$new(predictor_fold1_lm, feature = c("Yield_CT", "Tmax"), method = "ale", grid.size = 40)$plot() + 
  labs(title = "Linear Model") +
  scale_fill_gradient("ALE", low = "red", high = "yellow") + 
  scale_x_continuous("Yield_CT") + 
  scale_y_continuous("Maximum Temperature") +
  scale_fill_viridis(option = "D")

fig19.4c <- ale_fold1_lm_Yield_CT_Tmax_2D'




plot_combined_19.4 <- plot_grid(fig19.4a, fig19.4b, ncol = 2)

# Save the plot
ggsave(paste0(save_path, "Fig19.4.png"), plot_combined_19.4, width = width, height = height)


######for fold 2

# Random Forests
ale_fold2_rf_Yield_CT_Tmax_2D <- FeatureEffect$new(predictor_fold2_rf, feature = c("Yield_CT", "Tmax"), method = "ale", grid.size = 40)$plot() +   
  labs(title = "Random Forest") +
  scale_fill_gradient("ALE", low = "red", high = "yellow") + 
  scale_x_continuous("Yield_CT") + 
  scale_y_continuous("Maximum Temperature") +
  scale_fill_viridis(option = "D")

fig19.5a <- ale_fold2_rf_Yield_CT_Tmax_2D

# Gradient Boosting
ale_fold2_gbm_Yield_CT_Tmax_2D <- FeatureEffect$new(predictor_fold2_gbm, feature = c("Yield_CT", "Tmax"), method = "ale")$plot() +   
  labs(title = "Gradient Boosting") +
  scale_fill_gradient("ALE", low = "red", high = "yellow") + 
  scale_x_continuous("Yield_CT") + 
  scale_y_continuous("Maximum Temperature") +
  scale_fill_viridis(option = "D")

fig19.5b <- ale_fold2_gbm_Yield_CT_Tmax_2D




plot_combined_19.5 <- plot_grid(fig19.5a, fig19.5b, ncol = 2)

# Save the plot
ggsave(paste0(save_path, "Fig19.5.png"), plot_combined_19.5, width = width, height = height)


######for fold 3

# Random Forests
ale_fold3_rf_Yield_CT_Tmax_2D <- FeatureEffect$new(predictor_fold3_rf, feature = c("Yield_CT", "Tmax"), method = "ale")$plot() +   
  labs(title = "Random Forest") +
  scale_fill_gradient("ALE", low = "red", high = "yellow") + 
  scale_x_continuous("Yield_CT") + 
  scale_y_continuous("Maximum Temperature") +
  scale_fill_viridis(option = "D")

fig19.6a <- ale_fold3_rf_Yield_CT_Tmax_2D

# Gradient Boosting
ale_fold3_gbm_Yield_CT_Tmax_2D <- FeatureEffect$new(predictor_fold3_gbm, feature = c("Yield_CT", "Tmax"), method = "ale", grid.size = 40)$plot() +   
  labs(title = "Gradient Boosting") +
  scale_fill_gradient("ALE", low = "red", high = "yellow") + 
  scale_x_continuous("Yield_CT") + 
  scale_y_continuous("Maximum Temperature") +
  scale_fill_viridis(option = "D")

fig19.6b <- ale_fold3_gbm_Yield_CT_Tmax_2D




plot_combined_19.6 <- plot_grid(fig19.6a, fig19.6b, ncol = 2)

# Save the plot
ggsave(paste0(save_path, "Fig19.6.png"), plot_combined_19.6, width = width, height = height)

######for fold 4

# Random Forests
ale_fold4_rf_Yield_CT_Tmax_2D <- FeatureEffect$new(predictor_fold4_rf, feature = c("Yield_CT", "Tmax"), method = "ale", grid.size = 40)$plot() +   
  labs(title = "Random Forest") +
  scale_fill_gradient("ALE", low = "red", high = "yellow") + 
  scale_x_continuous("Yield_CT") + 
  scale_y_continuous("Maximum Temperature") +
  scale_fill_viridis(option = "D")

fig19.7a <- ale_fold4_rf_Yield_CT_Tmax_2D

# Gradient Boosting
ale_fold4_gbm_Yield_CT_Tmax_2D <- FeatureEffect$new(predictor_fold4_gbm, feature = c("Yield_CT", "Tmax"), method = "ale", grid.size = 40)$plot() +   
  labs(title = "Gradient Boosting") +
  scale_fill_gradient("ALE", low = "red", high = "yellow") + 
  scale_x_continuous("Yield_CT") + 
  scale_y_continuous("Maximum Temperature") +
  scale_fill_viridis(option = "D")

fig19.7b <- ale_fold4_gbm_Yield_CT_Tmax_2D




plot_combined_19.7 <- plot_grid(fig19.7a, fig19.7b, ncol = 2)

# Save the plot
ggsave(paste0(save_path, "Fig19.7.png"), plot_combined_19.7, width = width, height = height)


######for fold 5

# Random Forests
ale_fold5_rf_Yield_CT_Tmax_2D <- FeatureEffect$new(predictor_fold5_rf, feature = c("Yield_CT", "Tmax"), method = "ale", grid.size = 40)$plot() +   
  labs(title = "Random Forest") +
  scale_fill_gradient("ALE", low = "red", high = "yellow") + 
  scale_x_continuous("Yield_CT") + 
  scale_y_continuous("Maximum Temperature") +
  scale_fill_viridis(option = "D")

fig19.8a <- ale_fold5_rf_Yield_CT_Tmax_2D

# Gradient Boosting
ale_fold5_gbm_Yield_CT_Tmax_2D <- FeatureEffect$new(predictor_fold5_gbm, feature = c("Yield_CT", "Tmax"), method = "ale", grid.size = 40)$plot() +   
  labs(title = "Gradient Boosting") +
  scale_fill_gradient("ALE", low = "red", high = "yellow") + 
  scale_x_continuous("Yield_CT") + 
  scale_y_continuous("Maximum Temperature") +
  scale_fill_viridis(option = "D")

fig19.8b <- ale_fold5_gbm_Yield_CT_Tmax_2D




plot_combined_19.8 <- plot_grid(fig19.8a, fig19.8b, ncol = 2)

# Save the plot
ggsave(paste0(save_path, "Fig19.8.png"), plot_combined_19.8, width = width, height = height)


######Conditional pdp

####for fold 1
# Generate partial dependence data
pdp_interaction_fold1 <- rbind(
  partial(rf_model_fold1, pred.var = c("Yield_CT", "Tmax"), chull = TRUE, grid.resolution = 10) %>% cbind(., algorithm = "Random Forests"),
  partial(gbm_model_fold1, pred.var = c("Yield_CT", "Tmax"), chull = TRUE, grid.resolution = 10, n.trees = 500) %>% cbind(., algorithm = "Gradient Boosting")
)

# Convert to a factor with specified levels for plotting
pdp_interaction_fold1$algorithm <- factor(pdp_interaction_fold1$algorithm, levels = c("Random Forests", "Gradient Boosting"))

# Categorize `Tmax` into 'high' and 'low'

pdp_interaction_fold1 <-
  
  pdp_interaction_fold1 %>% 
  mutate(Tmax_class = case_when(
    Tmax > 33 ~ "Tmax_high",
    Tmax < 33 ~ "Tmax_low"
  ))



# Customizing the plot as per your instruction
Fig20.4 <- ggplot(pdp_interaction_fold1, aes(x = Yield_CT, y = yhat, 
                                       color = algorithm,
                                       linetype = Tmax_class)) +
  geom_line(stat = "summary", fun = mean, linewidth = 1) +
  facet_wrap(~algorithm) +
  ylab("Partial Dependence") +
  theme_bw() +
  scale_color_manual(values = c("Random Forests" = "#2ca02c", 
                                "Gradient Boosting" = "#9467bd")) +
  scale_linetype_manual(values = c("Tmax_high" = "solid", "Tmax_low" = "dashed")) +
  theme(legend.position = "right")

# Save the plot
ggsave(paste0(save_path, "Fig20.4.png"), Fig20.4, width = width, height = height)


####For all folds

# Placeholder list for pdp interaction data for each fold
pdp_interaction_folds <- list()

# Loop through each fold
for (fold_number in 1:5) {
  # Generate partial dependence data for the current fold
  # Assuming you have `rf_model_foldX` and `gbm_model_foldX` for each fold
  pdp_interaction_current <- rbind(
    partial(get(paste0("rf_model_fold", fold_number)), pred.var = c("Yield_CT", "Tmax"), chull = TRUE, grid.resolution = 10) %>% cbind(., algorithm = "Random Forests", fold = paste("Fold", fold_number)),
    partial(get(paste0("gbm_model_fold", fold_number)), pred.var = c("Yield_CT", "Tmax"), chull = TRUE, grid.resolution = 10, n.trees = 500) %>% cbind(., algorithm = "Gradient Boosting", fold = paste("Fold", fold_number))
  )
  
  # Categorize `Tmax` into 'high' and 'low'
  pdp_interaction_current <- pdp_interaction_current %>% 
    mutate(Tmax_class = case_when(
      Tmax > 33 ~ "Tmax_high",
      Tmax <= 33 ~ "Tmax_low"
    ))
  
  # Store the pdp data for the current fold
  pdp_interaction_folds[[fold_number]] <- pdp_interaction_current
}

# Combine all folds into a single dataframe
pdp_interaction_all_folds <- bind_rows(pdp_interaction_folds)

# Convert algorithm to a factor with specified levels for plotting
pdp_interaction_all_folds$algorithm <- factor(pdp_interaction_all_folds$algorithm, levels = c("Random Forests", "Gradient Boosting"))

# Plotting
Fig20.4_all_folds <- ggplot(pdp_interaction_all_folds, aes(x = Yield_CT, y = yhat, 
                                                           color = algorithm, linetype = Tmax_class)) +
  geom_line(stat = "summary", fun = mean, linewidth = 1) +
  facet_grid(fold ~ algorithm) +
  ylab("Partial Dependence") +
  theme_bw() +
  scale_color_manual(values = c("Random Forests" = "#2ca02c", "Gradient Boosting" = "#9467bd")) +
  scale_linetype_manual(values = c("Tmax_high" = "solid", "Tmax_low" = "dashed")) +
  theme(legend.position = "right")

# Display the plot
print(Fig20.4_all_folds)
# Save the plot
ggsave(paste0(save_path, "Fig20.4_all_folds.png"), Fig20.4_all_folds, width = width, height = height)





#####PDP for random forests in all folds

####for Yield_CT


# Initialize an empty list to store PDP data for each fold
all_folds_pdp_data_rf <- list()

# Loop through each fold to generate PDP data for Random Forests model
for(fold_number in 1:5) {
  # Retrieve the Random Forest model for the current fold
  rf_model <- models_maize_folds[[paste("Fold", fold_number)]]$`Random Forests`
  
  # Generate PDP data for Yield_CT variable
  pdp_data <- partial(rf_model, pred.var = "Yield_CT", grid.resolution = 10, train = training_maize_s[[fold_number]])
  
  # Add a column to indicate the fold number
  pdp_data$Fold <- paste("Fold", fold_number)
  
  # Store the PDP data in the list
  all_folds_pdp_data_rf[[fold_number]] <- pdp_data
}

# Combine PDP data from all folds into one data frame
combined_pdp_data_rf <- bind_rows(all_folds_pdp_data_rf)

# Convert Fold to a factor for plotting
combined_pdp_data_rf$Fold <- factor(combined_pdp_data_rf$Fold, levels = paste("Fold", 1:5))

# Plot combined PDP data for all folds with customized colors
pdp_rf_allfolds <- ggplot(combined_pdp_data_rf, aes(x = Yield_CT, y = yhat, color = Fold)) +
  geom_line() +
  labs(title = "Partial Dependence Plot for Yield_CT across all folds in Random Forests", y = "Partial Dependence", x = "Yield_CT") +
  scale_color_manual(values = c("Fold 1" = "#1f77b4", "Fold 2" = "#ff7f0e", "Fold 3" = "#2ca02c", "Fold 4" = "#d62728", "Fold 5" = "#9467bd")) +
  theme_minimal() +
  theme(legend.title = element_text(size = 12), legend.text = element_text(size = 10),
        panel.background = element_rect(fill = "white", colour = "white"),
        plot.background = element_rect(fill = "white", colour = "white"))

# Save the plot
ggsave(paste0(save_path, "Fig21.4_yieldct_all_folds.png"), pdp_rf_allfolds, width = width, height = height)

####for Tmax

# Initialize an empty list to store PDP data for each fold
all_folds_pdp_data_rf_Tmax <- list()

# Loop through each fold to generate PDP data for Random Forests model
for(fold_number in 1:5) {
  # Retrieve the Random Forest model for the current fold
  rf_Tmax_model <- models_maize_folds[[paste("Fold", fold_number)]]$`Random Forests`
  
  # Generate PDP data for Tmax variable
  pdp_data <- partial(rf_Tmax_model, pred.var = "Tmax", grid.resolution = 10, train = training_maize_s[[fold_number]])
  
  # Add a column to indicate the fold number
  pdp_data$Fold <- paste("Fold", fold_number)
  
  # Store the PDP data in the list
  all_folds_pdp_data_rf_Tmax[[fold_number]] <- pdp_data
}

# Combine PDP data from all folds into one data frame
combined_pdp_data_rf_Tmax <- bind_rows(all_folds_pdp_data_rf_Tmax)

# Convert Fold to a factor for plotting
combined_pdp_data_rf_Tmax$Fold <- factor(combined_pdp_data_rf_Tmax$Fold, levels = paste("Fold", 1:5))

# Plot combined PDP data for all folds with customized colors
pdp_rf_Tmax_allfolds <- ggplot(combined_pdp_data_rf_Tmax, aes(x = Tmax, y = yhat, color = Fold)) +
  geom_line() +
  labs(title = "Partial Dependence Plot for Tmax across all folds in Random Forests", y = "Partial Dependence", x = "Tmax") +
  scale_color_manual(values = c("Fold 1" = "#1f77b4", "Fold 2" = "#ff7f0e", "Fold 3" = "#2ca02c", "Fold 4" = "#d62728", "Fold 5" = "#9467bd")) +
  theme_minimal() +
  theme(legend.title = element_text(size = 12), legend.text = element_text(size = 10),
        panel.background = element_rect(fill = "white", colour = "white"),
        plot.background = element_rect(fill = "white", colour = "white"))

# Save the plot
ggsave(paste0(save_path, "Fig21.4_Tmax_all_folds.png"), pdp_rf_Tmax_allfolds, width = width, height = height)