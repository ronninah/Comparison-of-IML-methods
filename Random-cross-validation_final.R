#RScript for for Master Thesis
#
#Nahid Hasan Ronnie
#matriculation 3944438
#IML
#XAI

#checking working directory
getwd()
setwd("E:/Thesis/Program file on RStudio")


#Library

#install.packages("iml")
library(dplyr)
library(tidyverse) 
library(countrycode)
library(purrr)

library(ggplot2)
library(RColorBrewer)
library(rnaturalearth)

library(stars)

library(patchwork)
library(cowplot)
library(caret)
library(pdp)
library(vip)
library(rpart)
library(rpart.plot)
library(iml)
library(writexl)
#install.packages("ALEPlot")
library(ALEPlot)

library(DALEX)
library(DALEXtra)

library(mlr)
library(ggplot2)
library(nnet)

library(viridis)
library(maps)

###########################
###


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
write_xlsx(cropdata1, path = "E:/Thesis/Program file on RStudio/cropdata1.xlsx")

cropdata_cleaned <- cropdata1 %>% dplyr::select(-Yield_NT, -NT_effect, -Site.country, -Location, -continent)

# Function to create crop datasets
create_crop_datasets <- function(data, crop_names) {
  datasets <- list()
  
  for (crop_name in crop_names) {
    dataset <- filter(data, Crop == crop_name) %>% select(-Crop)
    datasets[[crop_name]] <- dataset
  }
  
  return(datasets)
}

# Crop names
crop_names <- c("cotton", "rice", "soybean", "sorghum", "sunflower", "wheat.winter", "wheat.spring", "barley.winter", "barley.spring", "maize")

# crop datasets
crop_datasets <- create_crop_datasets(cropdata_cleaned, crop_names)

# Access the individual crop datasets
GlobalCotton <- crop_datasets$cotton
Globalrice <- crop_datasets$rice
GlobalSoybean <- crop_datasets$soybean
Globalsorghum <- crop_datasets$sorghum
Globalsunflower <- crop_datasets$sunflower
Globalwheat.winter <- crop_datasets$wheat.winter
Globalwheat.spring <- crop_datasets$wheat.spring
Globalbarley.winter <- crop_datasets$barley.winter
Globalbarley.spring <- crop_datasets$barley.spring
Globalmaize <- crop_datasets$maize


# Exporting excel

write.csv(Globalmaize, file = "E:/Thesis/Program file on RStudio/Globalmaize.csv", row.names = FALSE)


# world map with number of factors
world <- ne_countries(scale="small", returnclass = "sf") 
map_global_star <- world$Cumulative %>% st_as_stars()


# Function to create world maps
create_world_map <- function(data, crop, color) {
  ggplot() +
    geom_sf(data = world) +
    geom_point(data = data, aes(x = Longitude, y = Latitude), size = 1, color = color) +
    ggtitle(bquote(bold("World map distribution of" ~ .(crop)))) +
    theme_bw(base_size = 14) +
    theme(plot.title = element_text(size = 12, hjust = 0.5))
}

# Create world maps for all crops
world_maps <- list(
  "Cotton" = create_world_map(GlobalCotton, "Cotton", color = "darkolivegreen4"),
  "Rice" = create_world_map(Globalrice, "Rice", color = "blue"),
  "Soybean" = create_world_map(GlobalSoybean, "Soybean", color = "orange"),
  "Maize" = create_world_map(Globalmaize, "Maize", color = "yellow"),
  "Sorghum" = create_world_map(Globalsorghum, "Sorghum", color = "green"),
  "Sunflower" = create_world_map(Globalsunflower, "Sunflower", color = "brown"),
  "Wheat (Spring)" = create_world_map(Globalwheat.spring, "Wheat (Spring)", color = "purple"),
  "Wheat (Winter)" = create_world_map(Globalwheat.winter, "Wheat (Winter)", color = "pink2"),
  "Barley (Spring)" = create_world_map(Globalbarley.spring, "Barley (Spring)", color = "cyan2"),
  "Barley (Winter)" = create_world_map(Globalbarley.winter, "Barley (Winter)", color = "red")
)

# for only Maize
maize_world_map <- create_world_map(Globalmaize, "Maize", color = "yellow")


# Combine world maps
Fig01 <- cowplot::plot_grid(plotlist = world_maps, ncol = 2, nrow = 5)
# Display the plots
Fig01



# Function to create histograms
create_histogram <- function(data, crop) {
  ggplot(data = data, aes(y = Yield_change)) +
    geom_histogram(bins = 30) +
    coord_flip() +
    ylab("Relative change in crop yield") +
    ggtitle(paste("Histogram for", crop)) +
    theme_bw(base_size = 14)
}

# Create histograms for all crops
histograms <- list(
  "Cotton" = create_histogram(GlobalCotton, "Cotton"),
  "Rice" = create_histogram(Globalrice, "Rice"),
  "Soybean" = create_histogram(GlobalSoybean, "Soybean"),
  "Maize" = create_histogram(Globalmaize, "Maize"),
  "Sorghum" = create_histogram(Globalsorghum, "Sorghum"),
  "Sunflower" = create_histogram(Globalsunflower, "Sunflower"),
  "Wheat (Spring)" = create_histogram(Globalwheat.spring, "Wheat (Spring)"),
  "Wheat (Winter)" = create_histogram(Globalwheat.winter, "Wheat (Winter)"),
  "Barley (Spring)" = create_histogram(Globalbarley.spring, "Barley (Spring)"),
  "Barley (Winter)" = create_histogram(Globalbarley.winter, "Barley (Winter)")
)

# Create histogram for Maize
maize_histogram <- create_histogram(Globalmaize, "Maize")

# Calculate the mean of the Yield_change column
mean_yield_change <- mean(maize_histogram[["data"]][["Yield_change"]])

# Calculate the standard deviation of the Yield_change column
sd_yield_change <- sd(maize_histogram[["data"]][["Yield_change"]])


# Combine histograms
Fig02 <- cowplot::plot_grid(plotlist = histograms, ncol = 2, nrow = 5)

# Display the plots
Fig02


#for maize only
Fig01.2 <- (maize_world_map / maize_histogram) + 
  plot_layout(ncol=2, widths = c(2,1), heights = c(1,1)) + 
  plot_annotation(tag_levels = 'a')

Fig01.2

####Saving the plot

# Set the path where you want to save the figures
save_path <- "E:/Thesis/Program file on RStudio/For Thesis/Random New/28Feb/"

# Set the width and height
width <- 10
height <- 7

ggsave(paste0(save_path, "Fig01.2.png"), Fig01.2, width = width, height = height)
ggsave(paste0(save_path, "maize_histogram.png"), maize_histogram, width = width, height = height)


#cross validation
#5-fold
#random
#train : 80%, test : 20%



# Function to split test and train datasets
split_train_test <- function(data, train_ratio = 0.8, seed = 111) {
  set.seed(seed)
  train_indices <- sample(1:nrow(data), size = floor(train_ratio * nrow(data)), replace = FALSE)
  train_data <- data[train_indices, ]
  test_data <- data[-train_indices, ]
  
  return(list(train_data = train_data, test_data = test_data))
}



# Split test and train datasets for each crop
split_datasets <- function(crop_datasets, train_ratio = 0.8, seed = 111) {
  split_crop_datasets <- list()
  
  for (crop_name in names(crop_datasets)) {
    crop_data <- crop_datasets[[crop_name]]
    split_data <- split_train_test(crop_data, train_ratio, seed)
    split_crop_datasets[[crop_name]] <- split_data
  }
  
  return(split_crop_datasets)
}

# Split test and train datasets for each crop
split_crop_datasets <- split_datasets(crop_datasets, train_ratio = 0.8, seed = 111)


train_maize <- split_crop_datasets$maize$train_data
test_maize <- split_crop_datasets$maize$test_data


####pointing on global map
###for maize only



# Create a combined dataset with an identifier for train/test
train_maize$dataset <- 'Train'
test_maize$dataset <- 'Test'
combined_maize <- rbind(train_maize, test_maize)

# Convert combined_maize to an sf object, assuming columns Latitude and Longitude exist
combined_maize_sf <- st_as_sf(combined_maize, coords = c("Longitude", "Latitude"), crs = 4326, agr = "constant")

# Plot
Fig10.1 <- ggplot() +
  geom_sf(data = world, fill = "white", color = "black") + # World map
  geom_sf(data = combined_maize_sf, aes(color = dataset), size = 2, alpha = 0.7) + # Maize data points
  scale_color_manual(values = c("Train" = "blue", "Test" = "red")) + # Custom colors
  labs(title = "Train and Test Maize Data Points on Global Map",
       color = "Dataset") + # Labels
  theme_minimal()+
  theme(text = element_text(color = "black"), # Black text color
        panel.background = element_rect(fill = "white", color = "black"), # White panel background with black border
        plot.background = element_rect(fill = "white", color = "black")) # White plot background with black border

ggsave(paste0(save_path, "Fig10.1.png"), Fig10.1, width = width, height = height)



# Machine learning algorithm implementation -------------------------------

# Function to run the models for different datasets
run_models <- function(train_data) {
  tc <- trainControl(method = "cv", number = 5)
  cart_control <- rpart.control(minsplit = 5, cp = 0.0001)
  
  # Linear Regression
  set.seed(111)
  model.lm <- caret::train(Yield_change ~ ., data = train_data, method = "glmStepAIC", trControl = tc)
  
  
  '# Classification and Regression Trees (CART)
  set.seed(111)
  model.cart <- caret::train(Yield_change ~ ., data = train_data, method = "rpart", trControl = tc, control = cart_control)'
  
  
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

# Run models for each dataset

models_maize <- run_models(train_maize)


# performance evaluation

#with test dataset

run_predictions <- function(models, test_data) {
  predictions <- list()
  
  for (i in seq_along(models)) {
    model <- models[[i]]
    model_name <- names(models)[i]
    
    # Make predictions
    preds <- predict(model, newdata = test_data)
    
    # Store predictions
    predictions[[model_name]] <- preds
    
    #Plotting
    # Plotting
    if (grepl("Decision Tree", model_name)) {
      rpart.plot(model$finalModel, type = 0, extra = 1)
      cat("Model: ", model_name, "\n")
    } else {
      df <- data.frame(Actual = test_data$Yield_change, Predicted = preds)
      p <- ggplot(df, aes(x = Actual, y = Predicted)) +
        geom_point() +
        ggtitle(paste("Crop:", unique(test_data$Crop), ", Model:", model_name)) +
        theme_bw(base_size = 14)
      print(p)
    }
  }
  
  return(predictions)
  
}

# Run predictions for each model and test dataset

predictions_maize <- run_predictions(models_maize, test_maize)


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

# Calculate R-squared values for each model and dataset

r2_values_maize <- calculate_r2(predictions_maize, test_maize)

# Create data frames for R-squared values

r2_maize <- data.frame(r2 = unlist(r2_values_maize), algorithm = names(r2_values_maize))


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

# Plot R-squared values for each dataset using the function

Fig03.4 <- plot_r_squared(r2_maize, "R-Squared values for maize")



#save the plot
ggsave(paste0(save_path, "Fig03.4.png"), Fig03.4, width = width, height = height)


# Function to calculate RMSE for each model and test dataset

calculate_rmse <- function(predictions, test_data) {
  rmse_values <- list()
  
  for (i in seq_along(predictions)) {
    preds <- predictions[[i]]
    model_name <- names(predictions)[i]
    
    # Calculate RMSE
    rmse <- sqrt(mean((preds - test_data$Yield_change)^2)) %>% round(., 4)
    
    # Store RMSE value
    rmse_values[[model_name]] <- rmse
  }
  
  return(rmse_values)
}

# Calculation function for RMSE values for each model and data set


rmse_values_maize <- calculate_rmse(predictions_maize, test_maize)



# Create data frames for RMSE values

rmse_maize <- data.frame(rmse = unlist(rmse_values_maize), algorithm = names(rmse_values_maize))


#Plotting function for RMSE

plot_rmse <- function(rmse_data, title) {
  algorithm_colors <- c("Linear Model" = "#1f77b4",
                        "Decision Tree" = "#ff7f0e",
                        "Random Forests" = "#2ca02c",
                        "Gradient Boosting" = "#9467bd")
  
  p <- ggplot(rmse_data, aes(x = algorithm, y = rmse, fill = algorithm)) +
    geom_bar(stat = "identity") +
    ggtitle(title) +
    xlab("IML algorithm") +
    ylab("RMSE") +
    scale_fill_manual(values = algorithm_colors) +
    theme_bw(base_size = 14) +
    theme(legend.position = "none", plot.title = element_text(hjust = 0.5))
  
  return(p)
}

# Plot RMSE values for each dataset using the function

Fig04.4 <- plot_rmse(rmse_maize, "RMSE values for maize")



#save the plot
ggsave(paste0(save_path, "Fig04.4.png"), Fig04.4, width = width, height = height)

#######
#######
#######



# Interpretable machine learning 

# variable importance (feature importance)

# model agnostic (generic) post-hoc interpretability

# permutation-based feature importance



# Function to calculate permutation-based variable importance for Linear Model and a specific crop
calculate_pvip_lm <- function(model, train_data, crop_name) {
  set.seed(123)
  pvip <- vip(model, method="permute", train=train_data, target="Yield_change", metric="rsq", 
              pred_wrapper=predict, nsim=30, geom="boxplot", aesthetics = list(fill = "#1f77b4", color="black")) +
    labs(title = paste("Linear Model ")) +
    theme_bw()
  return(pvip)
}

'# Function to calculate permutation-based variable importance for Decision Tree and a specific crop
calculate_pvip_cart <- function(model, train_data, crop_name) {
  set.seed(123)
  pvip <- vip(model, method="permute", train=train_data, target="Yield_change", metric="rsq", 
              pred_wrapper=predict, nsim=30, geom="boxplot", aesthetics = list(fill = "#ff7f0e", color="black")) +
    labs(title = paste("Decision Tree ")) +
    theme_bw()
  return(pvip)
}'

# Function to calculate permutation-based variable importance for Random Forest and a specific crop
calculate_pvip_rf <- function(model, train_data, crop_name) {
  set.seed(123)
  pvip <- vip(model, method="permute", train=train_data, target="Yield_change", metric="rsq", 
              pred_wrapper=predict, nsim=30, geom="boxplot", aesthetics = list(fill = "#2ca02c", color="black")) +
    labs(title = paste("Random Forest ")) +
    theme_bw()
  return(pvip)
}

# Function to calculate permutation-based variable importance for Gradient Boosting and a specific crop
calculate_pvip_gbm <- function(model, train_data, crop_name) {
  set.seed(123)
  pvip <- vip(model, method="permute", train=train_data, target="Yield_change", metric="rsq", 
              pred_wrapper=predict, nsim=30, geom="boxplot", aesthetics = list(fill = "#9467bd", color="black")) +
    labs(title = paste("Gradient Boosting ")) +
    theme_bw()
  return(pvip)
}



# For maize:
pvip_lm_maize <- calculate_pvip_lm(models_maize[["Linear Model"]], train_maize, "maize")
#pvip_cart_maize <- calculate_pvip_cart(models_maize[["Decision Tree"]], train_maize, "maize")
pvip_rf_maize <- calculate_pvip_rf(models_maize[["Random Forests"]], train_maize, "maize")
pvip_gbm_maize <- calculate_pvip_gbm(models_maize[["Gradient Boosting"]], train_maize, "maize")



####

####Saving the plot

#for maize

plot_pvip_maize <-
  pvip_lm_maize  + pvip_rf_maize + pvip_gbm_maize

Fig05.4 <-
  plot_pvip_maize + 
  plot_annotation(tag_levels = 'a')

#save the plot
ggsave(paste0(save_path, "Fig05.4.png"), Fig05.4, width = width, height = height)




###########


# partial dependence plot

#Single predictor pdps

################################


#maize 1. rf, 2. gbm, 3. lm

# For  Yield_CT variable
pdp_maize_Yield_CT <- rbind(
  models_maize[["Random Forests"]] %>% partial(pred.var = "Yield_CT", approx = TRUE) %>% cbind(., algorithm = "Random Forests"),
  models_maize[["Gradient Boosting"]] %>%partial(pred.var = "Yield_CT", approx = TRUE) %>% cbind(., algorithm = "Gradient Boosting"),
  models_maize[["Linear Model"]] %>%partial(pred.var = "Yield_CT", approx = TRUE) %>% cbind(., algorithm = "Linear Model")
)

pdp_maize_Yield_CT$algorithm <- factor(pdp_maize_Yield_CT$algorithm, levels=c("Random Forests", "Gradient Boosting", "Linear Model"))

# For Tmax variable
pdp_maize_Tmax <- rbind(
  models_maize[["Random Forests"]] %>% partial(pred.var = "Tmax", approx = TRUE) %>% cbind(., algorithm = "Random Forests"),
  models_maize[["Gradient Boosting"]] %>% partial(pred.var = "Tmax", approx = TRUE) %>% cbind(., algorithm = "Gradient Boosting"),
  models_maize[["Linear Model"]] %>% partial(pred.var = "Tmax", approx = TRUE) %>% cbind(., algorithm = "Linear Model")
)

pdp_maize_Tmax$algorithm <- factor(pdp_maize_Tmax$algorithm, levels=c("Random Forests", "Gradient Boosting", "Linear Model"))


Fig06.4a <- ggplot(pdp_maize_Yield_CT, aes(x=Yield_CT, y=yhat, color=algorithm)) +
  geom_line(linewidth=1) +
  scale_color_manual(values = c("Random Forests" = "#2ca02c", "Gradient Boosting" = "#9467bd", "Linear Model" = "#1f77b4")) +
  ylab("Partial Dependence") +
  labs(title = "pdp for maize") +
  theme_bw() +
  theme(legend.position = c(0.7, 0.7),
        axis.title.x = element_blank(),
        axis.text.x = element_blank())


Fig06.4b <- ggplot(pdp_maize_Tmax, aes(x=Tmax, y=yhat, color=algorithm)) +
  geom_line(linewidth=1) +
  scale_color_manual(values = c("Random Forests" = "#2ca02c", "Gradient Boosting" = "#9467bd", "Linear Model" = "#1f77b4")) +
  ylab("Partial Dependence") +
  labs(title = "pdp for maize") +
  theme_bw() +
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        axis.text.x = element_blank())

Fig06.4c <- ggplot(train_maize, aes(x=Yield_CT)) + geom_histogram(bins=30) + theme_bw( base_size = 14)
Fig06.4d <- ggplot(train_maize, aes(x=Tmax)) + geom_histogram(bins=30) + theme_bw( base_size = 14)

Fig06.4 <- Fig06.4a + Fig06.4b + Fig06.4c + Fig06.4d +
  plot_annotation(tag_levels = "a") +
  plot_layout(heights=c(9,1))

# Save the plot
ggsave(paste0(save_path, "Fig06.4.png"), Fig06.4, width = width, height = height)

Fig06.4


#####Multiple predictor

####
# 2-way interactions(PDP)
#Maize 1. rf, 2. gbm, #3. lm

pdp_maize_rf <- models_maize[["Random Forests"]] %>%  partial(pred.var=c("Yield_CT","Tmax"), approx=T) %>% autoplot + 
  labs(title="Random Forests")
pdp_maize_gbm <- models_maize[["Gradient Boosting"]] %>%  partial(pred.var=c("Yield_CT","Tmax"), approx=T, n.trees=500) %>% autoplot + 
  labs(title="Gradient Boosting")
'pdp_maize_lm <- models_maize[["Linear Model"]] %>%  partial(pred.var=c("Yield_CT","Tmax"), approx=T) %>% autoplot + 
  labs(title="Linear Model")'


Fig07.4 <-
  pdp_maize_rf+ pdp_maize_gbm #+ pdp_maize_lm


# Save the plot
ggsave(paste0(save_path, "Fig07.4.png"), Fig07.4, width = width, height = height)





#########Accumilated Local Effect plots

####ALE plots

######

#Maize 1. rf, 2. gbm, 3. lm


########for all variable 


# Extract the Random Forest model from models_sorghum
rf_model_maize <- models_maize[["Random Forests"]]
gbm_model_maize <- models_maize[["Gradient Boosting"]]
lm_model_maize <- models_maize[["Linear Model"]]

# Create a Predictor object
X_maize <- train_maize[, !names(train_maize) %in% c("yhat")]  # Exclude the target variable
predictor_maize_rf <- Predictor$new(rf_model_maize, data = X_maize, y = train_maize$yhat)
predictor_maize_gbm <- Predictor$new(gbm_model_maize, data = X_maize, y = train_maize$yhat)
predictor_maize_lm <- Predictor$new(lm_model_maize, data = X_maize, y = train_maize$yhat)

Fig08.4A1 <-ale_maize_rf <-FeatureEffects$new(predictor_maize_rf)%>%plot()
Fig08.4A2 <-ale_maize_gbm <-FeatureEffects$new(predictor_maize_gbm)%>%plot()
Fig08.4A3 <-ale_maize_lm <-FeatureEffects$new(predictor_maize_lm)%>%plot()

# Save the plot
ggsave(paste0(save_path, "Fig08.4A1.png"), Fig08.4A1, width = width, height = height)
ggsave(paste0(save_path, "Fig08.4A2.png"), Fig08.4A2, width = width, height = height)
ggsave(paste0(save_path, "Fig08.4A3.png"), Fig08.4A3, width = width, height = height)


####for single variable


###With Yield_CT variable
ale_maize_rf_Yield_CT = FeatureEffect$new(predictor_maize_rf, "Yield_CT", method = "ale")$ plot()+
  scale_x_continuous("Yield_CT") +
  ggtitle("Random Forests on Maize")
Fig08.4b1 <- ale_maize_rf_Yield_CT

ale_maize_gbm_Yield_CT = FeatureEffect$new(predictor_maize_gbm, "Yield_CT", method = "ale")$ plot()+
  scale_x_continuous("Yield_CT") +
  ggtitle("Gradient Boosting on Maize")
Fig08.4b2 <- ale_maize_gbm_Yield_CT

ale_maize_lm_Yield_CT = FeatureEffect$new(predictor_maize_lm, "Yield_CT", method = "ale")$ plot()+
  scale_x_continuous("Yield_CT") +
  ggtitle("Linear Model on Maize")
Fig08.4b3 <- ale_maize_lm_Yield_CT

plot_combined_08.4b <- plot_grid(Fig08.4b1, Fig08.4b2,Fig08.4b3, ncol = 3)

# Save the plot
ggsave(paste0(save_path, "Fig08.4b.png"), plot_combined_08.4b, width = width, height = height)

#####
# Extract data from ALE objects
data_maize_rf <- ale_maize_rf_Yield_CT$data
data_maize_gbm <- ale_maize_gbm_Yield_CT$data
data_maize_lm <- ale_maize_lm_Yield_CT$data

# Extract effects from ALE objects
effects_maize_rf <- ale_maize_rf_Yield_CT$effects
effects_maize_gbm <- ale_maize_gbm_Yield_CT$effects
effects_maize_lm <- ale_maize_lm_Yield_CT$effects

# Add Algorithm column to the data
data_maize_rf$Algorithm <- "Random Forests"
data_maize_gbm$Algorithm <- "Gradient Boosting"
data_maize_lm$Algorithm <- "Linear Model"

# Combine data and effects
combined_data_maize <- rbind(data_maize_rf, data_maize_gbm, data_maize_lm)
combined_effects_maize <- cbind(effects_maize_rf, effects_maize_gbm, effects_maize_lm)

# Create a ggplot
combined_ale_maize_Yield_CT <- ggplot(combined_data_maize, aes(x = Yield_CT, y = .value, color = Algorithm)) +
  geom_line() +
  scale_color_manual(values = c("Random Forests" = "#2ca02c", "Gradient Boosting" = "#9467bd", "Linear Model" = "#1f77b4")) +
  labs(title = "ALE Plots for Maize Yield_CT", y = "ALE")

# Save the plot
ggsave(paste0(save_path, "Fig08.4CC.png"), combined_ale_maize_Yield_CT, width = width, height = height)

#####
####With Tmax variable

ale_maize_rf_Tmax = FeatureEffect$new(predictor_maize_rf, "Tmax", method = "ale")$ plot()+
  scale_x_continuous("Tmax") +
  ggtitle("Random Forests on Maize")
Fig08.4c1 <- ale_maize_rf_Tmax

ale_maize_gbm_Tmax = FeatureEffect$new(predictor_maize_gbm, "Tmax", method = "ale")$ plot()+
  scale_x_continuous("Tmax") +
  ggtitle("Gradient Boosting on Maize")
Fig08.4c2 <- ale_maize_gbm_Tmax

ale_maize_lm_Tmax = FeatureEffect$new(predictor_maize_lm, "Tmax", method = "ale")$ plot()+
  scale_x_continuous("Tmax") +
  ggtitle("Linear Model on Maize")
Fig08.4c3 <- ale_maize_lm_Tmax

plot_combined_08.4c <- plot_grid(Fig08.4c1, Fig08.4c2,Fig08.4c3, ncol = 3)

# Save the plot
ggsave(paste0(save_path, "Fig08.4c.png"), plot_combined_08.4c, width = width, height = height)


#####
# Extract data from ALE objects
data_maize_rf_Tmax <- ale_maize_rf_Tmax$data
data_maize_gbm_Tmax <- ale_maize_gbm_Tmax$data
data_maize_lm_Tmax <- ale_maize_lm_Tmax$data

# Extract effects from ALE objects
effects_maize_rf_Tmax <- ale_maize_rf_Tmax$effects
effects_maize_gbm_Tmax <- ale_maize_gbm_Tmax$effects
effects_maize_lm_Tmax <- ale_maize_lm_Tmax$effects

# Add Algorithm column to the data
data_maize_rf_Tmax$Algorithm <- "Random Forests"
data_maize_gbm_Tmax$Algorithm <- "Gradient Boosting"
data_maize_lm_Tmax$Algorithm <- "Linear Model"

# Combine data and effects
combined_data_maize_Tmax <- rbind(data_maize_rf_Tmax, data_maize_gbm_Tmax, data_maize_lm_Tmax)
combined_effects_maize_Tmax <- cbind(effects_maize_rf_Tmax, effects_maize_gbm_Tmax, effects_maize_lm_Tmax)

# Create a ggplot
combined_ale_maize_Tmax <- ggplot(combined_data_maize_Tmax, aes(x = Tmax, y = .value, color = Algorithm)) +
  geom_line() +
  scale_color_manual(values = c("Random Forests" = "#2ca02c", "Gradient Boosting" = "#9467bd", "Linear Model" = "#1f77b4")) +
  labs(title = "ALE Plots for Maize Tmax", y = "ALE")

# Save the plot
ggsave(paste0(save_path, "Fig08.4DD.png"), combined_ale_maize_Tmax, width = width, height = height)


############
########
#####2D ###with multiple variable


#########Maize crop


# Random Forests
ale_maize_rf_Yield_CT_Tmax_2D <- FeatureEffect$new(predictor_maize_rf, feature = c("Yield_CT", "Tmax"), method = "ale", grid.size = 40)$plot() +   
  labs(title = "Random Forest") +
  scale_fill_gradient("ALE", low = "red", high = "yellow") + 
  scale_x_continuous("Yield_CT") + 
  scale_y_continuous("Maximum Temperature") +
  scale_fill_viridis(option = "D")

fig09.4a <- ale_maize_rf_Yield_CT_Tmax_2D

# Gradient Boosting
ale_maize_gbm_Yield_CT_Tmax_2D <- FeatureEffect$new(predictor_maize_gbm, feature = c("Yield_CT", "Tmax"), method = "ale", grid.size = 40)$plot() +   
  labs(title = "Gradient Boosting") +
  scale_fill_gradient("ALE", low = "red", high = "yellow") + 
  scale_x_continuous("Yield_CT") + 
  scale_y_continuous("Maximum Temperature") +
  scale_fill_viridis(option = "D")

fig09.4b <- ale_maize_gbm_Yield_CT_Tmax_2D

# Linear Model
'ale_maize_lm_Yield_CT_Tmax_2D <- FeatureEffect$new(predictor_maize_lm, feature = c("Yield_CT", "Tmax"), method = "ale", grid.size = 40)$plot() + 
  labs(title = "Linear Model") +
  scale_fill_gradient("ALE", low = "red", high = "yellow") + 
  scale_x_continuous("Yield_CT") + 
  scale_y_continuous("Maximum Temperature") +
  scale_fill_viridis(option = "D")

fig09.4c <- ale_maize_lm_Yield_CT_Tmax_2D'




plot_combined_09.4 <- plot_grid(fig09.4a, fig09.4b, ncol = 2)

# Save the plot
ggsave(paste0(save_path, "Fig09.4.png"), plot_combined_09.4, width = width, height = height)


#####################

######Conditional pdp

####maize


# Generate partial dependence data
pdp_interaction_maize <- rbind(
  partial(rf_model_maize, pred.var = c("Yield_CT", "Tmax"), chull = TRUE, grid.resolution = 10) %>% cbind(., algorithm = "Random Forests"),
  partial(gbm_model_maize, pred.var = c("Yield_CT", "Tmax"), chull = TRUE, grid.resolution = 10, n.trees = 500) %>% cbind(., algorithm = "Gradient Boosting")
)

# Convert to a factor with specified levels for plotting
pdp_interaction_maize$algorithm <- factor(pdp_interaction_maize$algorithm, levels = c("Random Forests", "Gradient Boosting"))

# Categorize `Tmax` into 'high' and 'low'

pdp_interaction_maize <-
  
  pdp_interaction_maize %>% 
  mutate(Tmax_class = case_when(
    Tmax > 33 ~ "Tmax_high",
    Tmax < 33 ~ "Tmax_low"
  ))



# Customizing the plot as per your instruction
Fig10.4 <- ggplot(pdp_interaction_maize, aes(x = Yield_CT, y = yhat, 
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
ggsave(paste0(save_path, "Fig10.4.png"), Fig10.4, width = width, height = height)





