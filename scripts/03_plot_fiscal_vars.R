library(readr)
library(dplyr)
library(zoo)
library(ggplot2)
library(scales)

# Load EVDS_real.csv (already includes nominal + real values)
df <- read_csv("data/processed/EVDS_real.csv")

# Convert quarter to yearqtr format
df$quarter <- as.yearqtr(df$quarter)

# Define election quarters
elections <- as.yearqtr(c("2002 Q4", "2007 Q3", "2011 Q2", 
                          "2015 Q2", "2015 Q4", "2018 Q2", "2023 Q2"))

dir.create("output/plots", recursive = TRUE)



# Save CPI plot
p_cpi <- ggplot(df, aes(x = quarter, y = cpi)) +
  geom_line(color = "purple") +
  geom_vline(xintercept = elections, linetype = "dashed", color = "red", linewidth = 0.7) +
  labs(
    title = "Consumer Price Index (2003=100)",
    x = "Quarter",
    y = "Index"
  ) +
  theme_minimal()

ggsave("output/plots/cpi_plot.png", plot = p_cpi, width = 8, height = 5, dpi = 300)


#Function to plot nominal and real fiscal vars

plot_nominal_real <- function(df, nom_var, real_var, label) {
  p1 <- ggplot(df, aes(x = quarter, y = .data[[nom_var]])) +
    geom_line(color = "orange") +
    geom_vline(xintercept = elections, linetype = "dashed", color = "red", linewidth = 0.7) +
    labs(title = paste("Nominal", label), y = "Million TRY", x = "") +
    scale_y_continuous(labels = label_comma()) +
    theme_minimal()
  
  p2 <- ggplot(df, aes(x = quarter, y = .data[[real_var]])) +
    geom_line(color = "steelblue") +
    geom_vline(xintercept = elections, linetype = "dashed", color = "red", linewidth = 0.7) +
    labs(title = paste("Real", label, "(2003=100)"), y = "Million TRY (Real)", x = "Quarter") +
    scale_y_continuous(labels = label_comma()) +
    theme_minimal()
  
  return(list(p1 = p1, p2 = p2))
}

library(gridExtra)

var_pairs <- list(
  c("exp_total", "exp_total_real", "Total Expenditures"),
  c("exp_primary", "exp_primary_real", "Primary Expenditures"),
  c("exp_personnel", "exp_personnel_real", "Personnel Expenditures"),
  c("exp_transfers", "exp_transfers_real", "Current Transfers")
)

# Loop through each pair and display plots
for (pair in var_pairs) {
  plots <- plot_nominal_real(df, pair[1], pair[2], pair[3])
  
  # Save both plots in one PNG (nominal + real stacked)
  ggsave(
    filename = paste0("output/plots/", gsub(" ", "_", tolower(pair[3])), "_plots.png"),
    plot = grid.arrange(plots$p1, plots$p2, ncol = 1),
    width = 8,
    height = 9,
    dpi = 300
  )
}



