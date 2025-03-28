library(readxl)
library(dplyr)
library(zoo)

# Load data
df <- read_excel("data/raw/EVDS.xlsx", sheet = "EVDS")

# Clean column names (remove spaces for easier handling)
names(df) <- gsub(" ", "_", names(df))

# Rename and select variables
df_clean <- df %>%
  mutate(
    quarter = as.yearqtr(Date, format = "Q%q-%Y")  # Convert to year-quarter format
  ) %>%
  select(
    quarter,
    exp_total     = TP_KB_GID001,  # Total government expenditures (A+B)
    exp_primary   = TP_KB_GID002,  # Expenditures excluding interest (Primary)
    exp_personnel = TP_KB_GID003,  # Compensation of employees (wages/salaries)
    exp_socialsec = TP_KB_GID008,  # Social security contributions
    exp_transfers = TP_KB_GID026   # Current transfers (classic OPTE proxy)
  )

# Preview the cleaned dataset
head(df_clean)

# Save the cleaned data as CSV
write.csv(df_clean, "data/processed/EVDS_clean.csv", row.names = FALSE)


