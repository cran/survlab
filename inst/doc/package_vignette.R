## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  warning = FALSE,
  message = FALSE,
  fig.width = 7,
  fig.height = 5
)

## ----eval=FALSE---------------------------------------------------------------
# # Install from GitHub (once published)
# # devtools::install_github("yourusername/survlab")
# 
# # Load the package
# library(survlab)

## ----setup--------------------------------------------------------------------
library(survlab)
library(data.table)
library(ggplot2)

## -----------------------------------------------------------------------------
# Load example data
data(multi_censored_data)

# Explore the dataset
multi_censored_data[, .(
  total_samples = .N,
  non_detects = sum(censored == 0),
  detects = sum(censored == 1),
  min_value = min(value),
  max_value = max(value)
)]

## -----------------------------------------------------------------------------
# View the different detection limit levels
detection_limits <- multi_censored_data[censored == 0, unique(value)]
cat("Detection limit levels:", paste(sort(detection_limits), collapse = ", "))

## -----------------------------------------------------------------------------
# Set seed for reproducibility
set.seed(123)

# Perform imputation with parameter validation
result <- impute_nondetect(
  dt = multi_censored_data,
  value_col = "value", 
  cens_col = "censored",
  parameter_col = "parameter",
  unit_col = "unit"
)

## -----------------------------------------------------------------------------
# Validate the imputation
validate_imputation(result)

## -----------------------------------------------------------------------------
# Look at the first 10 non-detect observations
result[censored == 0, .(
  original_detection_limit = value,
  imputed_value = round(value_imputed, 4),
  final_value = round(value_final, 4)
)][1:10]

## ----fig.width=7, fig.height=5------------------------------------------------
# Prepare data for plotting
plot_data <- rbind(
  result[censored == 1, .(value = value, type = "Detected")],
  result[censored == 0, .(value = value_imputed, type = "Imputed")]
)

# Create histogram
ggplot(plot_data, aes(x = value, fill = type)) +
  geom_histogram(alpha = 0.7, bins = 30, position = "identity") +
  geom_vline(xintercept = attr(result, "max_detection_limit"), 
             linetype = "dashed", color = "red", linewidth = 1) +
  labs(title = "Distribution Comparison: Detected vs Imputed Values",
       subtitle = paste("Red line shows maximum detection limit =", 
                        round(attr(result, "max_detection_limit"), 3)),
       x = "Value", y = "Count", fill = "Type") +
  theme_minimal() +
  scale_fill_manual(values = c("Detected" = "blue", "Imputed" = "orange"))

## ----fig.width=7, fig.height=5------------------------------------------------
# Q-Q plot to check distribution fit
ggplot(result[censored == 0], aes(sample = value_imputed)) +
  stat_qq() + 
  stat_qq_line() +
  labs(title = "Q-Q Plot of Imputed Values",
       subtitle = paste("Expected distribution:", 
                        attr(result, "best_distribution"))) +
  theme_minimal()

## -----------------------------------------------------------------------------
# Test only specific distributions with custom validation
result_custom <- impute_nondetect(
  dt = multi_censored_data,
  dist = c("gaussian", "lognormal", "weibull"),
  min_observations = 50,
  max_censored_pct = 50
)

## -----------------------------------------------------------------------------
# Extract model information
cat("Best distribution:", attr(result, "best_distribution"), "\n")
cat("Model AIC:", round(attr(result, "aic"), 2), "\n")
cat("Parameter:", attr(result, "parameter"), "\n")
cat("Unit:", attr(result, "unit"), "\n")
cat("Sample size:", attr(result, "sample_size"), "\n")
cat("Censoring percentage:", attr(result, "censored_pct"), "%\n")
cat("Detection limits found:", paste(attr(result, "detection_limits"), collapse = ", "), "\n")
cat("Maximum detection limit:", attr(result, "max_detection_limit"), "\n")

# Access the fitted model
model <- attr(result, "best_model")
summary(model)

