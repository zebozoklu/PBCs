# Load packages
library(readxl)
library(dplyr)
library(zoo)

# -----------------------------
# 1. Load fiscal data (nominal)
# -----------------------------
df_fiscal <- read.csv("data/processed/EVDS_clean.csv")
df_fiscal$quarter <- as.yearqtr(df_fiscal$quarter)

# --------------------------
# 2. Load CPI (2003 = 100)
# --------------------------
cpi_df <- read_excel("data/raw/EVDS_CPI.xlsx", sheet = "EVDS")
names(cpi_df) <- gsub(" ", "_", names(cpi_df))  # clean column names

# Use TP_FG_J0 for general CPI
df_cpi <- cpi_df %>%
  rename(cpi = TP_FG_J0) %>%
  mutate(quarter = as.yearqtr(Date, format = "Q%q-%Y")) %>%
  select(quarter, cpi)

# --------------------------
# 3. Merge & Deflate (with conversion to million TRY)
# --------------------------
df_fiscal <- df_fiscal %>% distinct(quarter, .keep_all = TRUE)
df_cpi    <- df_cpi %>% distinct(quarter, .keep_all = TRUE)

df_real <- df_fiscal %>%
  left_join(df_cpi, by = "quarter") %>%
  mutate(across(c(exp_total, exp_primary, exp_personnel, exp_socialsec, exp_transfers, cpi), as.numeric)) %>%
  
  # Convert nominal values to Million TRY
  mutate(
    exp_total     = exp_total / 1000,
    exp_primary   = exp_primary / 1000,
    exp_personnel = exp_personnel / 1000,
    exp_socialsec = exp_socialsec / 1000,
    exp_transfers = exp_transfers / 1000
  ) %>%
  
  # Then compute real values (also in Million TRY)
  mutate(
    exp_total_real     = 100 * (exp_total / cpi),
    exp_primary_real   = 100 * (exp_primary / cpi),
    exp_personnel_real = 100 * (exp_personnel / cpi),
    exp_socialsec_real = 100 * (exp_socialsec / cpi),
    exp_transfers_real = 100 * (exp_transfers / cpi)
  )

# --------------------------
# 4. Save the Real Series
# --------------------------

write.csv(df_real, "data/processed/EVDS_real.csv", row.names = FALSE)


