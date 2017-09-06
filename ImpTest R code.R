#!/usr/bin/env Rscript

# Setup work with the input -----------------------------------------
## Insted of reading the STDIN (which is not so trivial in R), I used command line arguments to extract the filename
args <- commandArgs(trailingOnly = TRUE)
if(is.na(args[1]) | length(args) != 1) {stop("Wrong number of arguments. Please specify a single log file")}

# Load / install packages -------------------------------------------------
install_if_needed <- function(packages) {
  for(package in packages) {
    ## check if the package is installed, if not - install
    if (length(grep(pattern = paste0('^', package), installed.packages())) == 0) {
      install.packages(package)
    }
  }
}

install_if_needed(c('tidyverse', 'lubridate'))
require(tidyverse)
require(lubridate)
#require(knitr)

# Read Data ---------------------------------------------------------------
df <- read_csv(file = args[1], progress = FALSE)

# Calculate features ------------------------------------------------------
df_users <- df %>%
  ## pre-calculating some features required for later calculations
  mutate(
    ## extract date from the timestamp
    imp_date = as.Date(ts),
    ## Extract day-of-week
    imp_weekday = wday(ts),
    ## Extract hour
    imp_hour = hour(ts),
    ## define "weekday business hour" criteria fora single impression
    imp_weekday_biz = if_else(
      imp_weekday > 1 & imp_weekday < 7 & imp_hour >= 8 & imp_hour <=17, 1, 0)
  ) %>%
  left_join(
    ## Aggregate impressions by IP and count impressions / distinct users
    df %>%
      group_by(hashed_ip) %>%
      summarise(
        IP_imp_count = n(),
        IP_user_count = n_distinct(uuid)
      ),
    by = c('hashed_ip' = 'hashed_ip')
  ) %>%
  ## define the criteria for a shared IP (>1 uuid)
  mutate(shared_IP = IP_user_count > 1) %>%
  ## Aggregate impressions by uuid
  group_by(uuid) %>%
  summarize(
    imp_count = n(),
    active_days = n_distinct(imp_date),
    weekday_biz_count = sum(imp_weekday_biz),
    shared_IP_count = sum(shared_IP)
  ) %>%
  ## Calcuate thresholds as set by previous analysis
  mutate(
    highly_active = imp_count > 10,
    multiple_days = active_days > 1,
    weekday_biz_prop = weekday_biz_count / imp_count,
    weekday_biz = weekday_biz_prop > 0.5,
    shared_IP_prop = shared_IP_count / imp_count,
    shared_IP_user = shared_IP_prop > 0.5
  ) %>%
  # Cleaup
  select(-imp_count, -active_days, -weekday_biz_count, -shared_IP_count, -weekday_biz_prop, -shared_IP_prop)


# Prepare output for STDOUT -----------------------------------------------
#print(df_users)
col_output = c('uuid', 'highly_active', 'multiple_days', 'weekday_biz', 'shared_IP_user')
write(paste(col_output, collapse = ','), file = stdout())

df_users %>%
  unite_(col = 'output', col_output, sep = ',') %>% 
  select(output) %>%
  unlist %>%
  unname() %>%
  write(file = stdout())
