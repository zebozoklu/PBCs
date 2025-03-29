# ----------------------------------------
# Script 04 - Log, ADF Test, Conditional Diff + Plot (Final Stable Version)
# ----------------------------------------

library(readr)
library(dplyr)
library(zoo)
library(tseries)
library(ggplot2)
library(stringr)
library(scales)

# Load and prepare data
df <- read_csv("data/processed/EVDS_real.csv")
df$quarter <- as.yearqtr(df$quarter)
df <- df %>% arrange(quarter)

# Define election quarters
elections <- as.yearqtr(c("2002 Q4", "2007 Q3", "2011 Q2", 
                          "2015 Q2", "2015 Q4", "2018 Q2", "2023 Q2"))


# Identify *_real variables (excluding known problematic ones)
real_vars <- names(df) %>%
  str_subset("_real$") %>%
  setdiff(c("cpi", "balance_real", "pri_balance_real"))

# ✅ Remove rows where ALL real fiscal variables are NA
df <- df %>% filter(if_any(all_of(real_vars), ~ !is.na(.)))

# Initialize stationarity result table
results <- data.frame(
  variable = character(),
  p_value_log = numeric(),
  stationary_log = character(),
  p_value_dlog = numeric(),
  stationary_dlog = character(),
  stringsAsFactors = FALSE
)


# Loop through each real fiscal variable
for (var in real_vars) {
  log_var <- paste0("log_", var)
  dlog_var <- paste0("dlog_", var)
  
  # Log-transform and clean invalid values
  df[[log_var]] <- tryCatch(log(df[[var]]), error = function(e) rep(NA, nrow(df)))
  df[[log_var]][!is.finite(df[[log_var]])] <- NA  # remove -Inf, Inf, NaN
  
  # Prepare clean data for ADF
  x_clean <- na.omit(df[[log_var]])
  
  # Run ADF test only if enough clean data
  if (length(x_clean) >= 25 && sd(x_clean) > 0) {
    pval <- tryCatch(adf.test(x_clean)$p.value, error = function(e) NA)
  } else {
    pval <- NA
  }
  
  # Determine stationarity
  stationary <- ifelse(!is.na(pval) && pval < 0.05, "Yes", "No")
  
  # If non-stationary in log, difference and re-test
  if (stationary == "No") {
    df[[dlog_var]] <- tryCatch(c(NA, diff(df[[log_var]])), error = function(e) rep(NA, nrow(df)))
    
    x_diff_clean <- na.omit(df[[dlog_var]])
    if (length(x_diff_clean) >= 25 && sd(x_diff_clean) > 0) {
      pval_dlog <- tryCatch(adf.test(x_diff_clean)$p.value, error = function(e) NA)
    } else {
      pval_dlog <- NA
    }
    
    stationary_dlog <- ifelse(!is.na(pval_dlog) && pval_dlog < 0.05, "Yes", "No")
    
    # Save dlog plot as before
    p_dlog <- ggplot(df, aes(x = quarter, y = .data[[dlog_var]])) +
      geom_line(color = "steelblue") +
      geom_vline(xintercept = elections, linetype = "dashed", color = "red") +
      labs(
        title = paste("Log-Diff of", str_to_title(gsub("_real", "", var))),
        y = "Growth Rate (log-diff)",
        x = "Quarter"
      ) +
      theme_minimal()
    
    ggsave(paste0("output/plots/", dlog_var, ".png"), p_dlog, width = 8, height = 5, dpi = 300)
    
  } else {
    pval_dlog <- NA
    stationary_dlog <- NA
  }
  
  # Store test result
  results <- rbind(results, data.frame(
    variable = log_var,
    p_value_log = round(pval, 4),
    stationary_log = stationary,
    p_value_dlog = round(pval_dlog, 4),
    stationary_dlog = stationary_dlog
  ))
  
  
  # Plot log-level variable
  p_log <- ggplot(df, aes(x = quarter, y = .data[[log_var]])) +
    geom_line(color = "darkgreen") +
    geom_vline(xintercept = elections, linetype = "dashed", color = "red") +
    labs(
      title = paste("Log of", str_to_title(gsub("_real", "", var))),
      y = "Log(Million TRY)", x = "Quarter"
    ) +
    theme_minimal()
  
  ggsave(paste0("output/plots/", log_var, ".png"), p_log, width = 8, height = 5, dpi = 300)
  
  # If not stationary → create log-diff and plot
  if (stationary == "No") {
    df[[dlog_var]] <- tryCatch(c(NA, diff(df[[log_var]])), error = function(e) rep(NA, nrow(df)))
    
    p_dlog <- ggplot(df, aes(x = quarter, y = .data[[dlog_var]])) +
      geom_line(color = "steelblue") +
      geom_vline(xintercept = elections, linetype = "dashed", color = "red") +
      labs(
        title = paste("Log-Diff of", str_to_title(gsub("_real", "", var))),
        y = "Growth Rate (log-diff)", x = "Quarter"
      ) +
      theme_minimal()
    
    ggsave(paste0("output/plots/", dlog_var, ".png"), p_dlog, width = 8, height = 5, dpi = 300)
  }
}

# Save updated dataset and stationarity results
write_csv(df, "data/processed/EVDS_real_transformed.csv")
write_csv(results, "output/tables/stationarity_summary.csv")


