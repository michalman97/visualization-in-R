# Libraries
install.packages("PerformanceAnalytics")
library(PerformanceAnalytics)
library(ggplot2)
library(GGally)
install.packages("ggcorrplot")
library(ggcorrplot)
install.packages("ggbiplot")
library(ggbiplot)
install.packages("corrplot")
library(corrplot)
install.packages("ggfortify")
library(ggfortify)
library(scales)    # For formatting scales
library(reshape2)  # For reshaping data

# Load the CSV file
data <- read.csv("find_corr_for_R.csv")

# visual inspection of assumptions (linear relation, low correlation, normal distribution)
# This is an informative plot, but not a nice one..
relevant_data <- data[, 5:22]
ggpairs(relevant_data) + theme_bw()

##################################################FOR ROSEARCH PROPOSAL#############################
relevant_data = data[, c('GSW', 'LAI', 'LWP', 'Temperature', 'NDVI.plant', 'Yield', 'Genotype')]
plot <- ggpairs(relevant_data, ggplot2::aes(color = Genotype), upper = list(
  continuous = wrap("cor", size = 6)), diag = list(continuous = wrap('densityDiag', alpha = 0.5)))
# Increase the text size using ggplot2::theme()
plot <- plot + theme(
  strip.text = element_text(size = 14),  # Facet labels
  axis.title = element_text(size = 12),  # Axis titles
  axis.text = element_text(size = 12),   # Axis labels
  legend.text = element_text(size = 10), # Legend text
  legend.title = element_text(size = 12) # Legend title
)

# Print the plot
print(plot)
###################################################################################################

# focus on linear relations - zoom to the problematic columns
non_linear <- relevant_data[, c("gsw", "SR.plot", "SR.plant", 'yield')]
plot <- ggpairs(non_linear) + theme_bw()
plot
# Save the plot to a file 
ggsave("non_linear.png", plot, width = 10, height = 8)

# gsw has an outliar, let's handle it
# Find and replace with mean the index of the largest value in the "gsw" column
max_index <- which.max(data$gsw)
data$gsw[max_index] <- NaN
mean_value <- mean(data$gsw, na.rm = TRUE) # Calculate the mean of the column
data$gsw[is.nan(data$gsw)] <- mean_value # Replace NaN with the mean value

#log transform the "SR" columns to turn data to linear
data$SR.plant <- log(data$SR.plant)
data$SR.plot <- log(data$SR.plot)

#plot again
relevant_data <- data[, 5:22]
plot <- ggpairs(relevant_data) + theme_bw()
plot
ggsave("linear_data.png", plot, width = 12, height = 10)


# we can see now that linear relation and normal distribution assumptions are beeing met.

# Now check the third assumption - low correlation
# Although we can see the correlations in the previus plot,
#we can visualize them better using corrplot
corr_matrix <- cor(data[, 5:21])
corrplot(corr_matrix, method = 'circle', type = 'lower', insig = 'blank',
                 order = 'AOE', diag = FALSE, addCoef.col = "black", tl.cex = 0.7, number.cex = 0.5)
png("corr_plot.png", width = 800, height = 600, units = "px", res = 300)

# we can see a problem of too high correlations.
# we will pick the traits that are least correlated with other traits

# Find the absolute correlation coefficients for each variable
abs_corr <- apply(abs(corr_matrix), 2, sum)

# Sort the variables based on their absolute correlation coefficients
sorted_vars <- names(sort(abs_corr))

# Select the 8 least correlated variables
least_correlated <- sorted_vars[1:8]

# look at the correlation matrix again to see if its better
least_correlated_data <- data[, least_correlated]
corr_matrix <- cor(least_correlated_data)
corrplot(corr_matrix, method = 'circle', type = 'lower', insig = 'blank',
         order = 'AOE', diag = FALSE, addCoef.col = "black")

# and we can see in the PCA how the traits that we chose are representing the variance vary well
# Names of the variables you want to display loadings for
variables_of_interest <- c("WI.plot", "WI.plant", "LWP", "gsw", "transpiration", "Tleaf", "plant.height", "DFTA")

pca_res <- prcomp(relevant_data, scale. = TRUE)

col_indices <- match(least_correlated_data, colnames(pca_res$rotation))
loadings_subset <- pca_res$rotation[, col_indices] # Filter the loadings for least_correlated_data


ggbiplot(pca_res, obs.scale = 1, var.scale = 1, 
         groups = data$type, ellipse = TRUE, loadings = loadings_subset)

# So we have 8 variables that fit the 3 assumptions of linear regression,
# we can now apply the regression and find the best combination of traits.

# define yield as target variable, explanatory variables will be the 8 least correlated that we found
target_variable <- data[, "yield"]
# Perform stepwise variable selection based on AIC
selected_model <- step(lm(target_variable ~ ., data = least_correlated_data), direction = "both", k = 2)

# Print the selected model
summary(selected_model)


# Predicted vs. Observed Plot
# Extract adjusted R-squared from the model summary
rsquared <- summary(selected_model)$adj.r.squared
# Make predictions using the selected model
predicted <- predict(selected_model)
# Create the plot with larger title and axis labels
plot(target_variable, predicted, xlab = "Yield observed [kg]", ylab = "Yield predicted [kg]", main = "Predicted vs. Observed", cex.main = 1.5, cex.lab = 1.2)
# Add a diagonal line of "perfect fit" for reference
abline(a = 0, b = 1, col = "red")

# Calculate RMSE
rmse <- sqrt(mean((predicted - target_variable)^2))

# Add R-squared and RMSE text to the plot
text(x = min(target_variable), y = max(predicted), labels = paste("R-squared =", round(rsquared, 3)), pos = 4, cex = 1.5)
text(x = min(target_variable), y = max(predicted) * 0.95, labels = paste("RMSE =", round(rmse, 3)),pos = 4, cex = 1.5)








############ final model assumptions ###########

# Normal Q-Q Plot
plot(selected_model, which = 1) #residuals are evenly spread
plot(selected_model, which = 2) #normal distribution check




