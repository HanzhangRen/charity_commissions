# Set up.
library(rstudioapi)
library(jsonlite)
library(tidyverse)
getSourceEditorContext()$path %>%
  dirname() %>%
  setwd()
a <- fromJSON("a.json")
b <- fromJSON("b.json")

# For each charity in the charity.json file, find the registered charity
# number. Use the number to get corresponding annual report data.
charity <- fromJSON("charity.json") %>%
  full_join(a, "registered_charity_number", keep = T) %>%
  full_join(b,
    c("registered_charity_number.x" = "registered_charity_number",
      "ar_cycle_reference"
    ),
    keep = T
  ) %>%
  select(-registered_charity_number) %>%
  rename(registered_charity_number = "registered_charity_number.x",
    ar_cycle_reference = "ar_cycle_reference.x"
  ) %>%
  # Prioritize income and expenditure data from b.json to avoid donations <
  # total income. Then prioritize total_gross_income in 2020 over latest
  # available income. 2021 data are not fully in yet.
  mutate(
    income = case_when(!is.na(income_total_income_and_endowments) ~
        income_total_income_and_endowments,
      !is.na(total_gross_income) ~ total_gross_income,
      T ~ latest_income
    ),
    expenditure = case_when(!is.na(expenditure_total) ~ expenditure_total,
      !is.na(total_gross_expenditure) ~ total_gross_expenditure,
      T ~ latest_expenditure
    )
  )

# Problem: each charity number corresponds to multiple charity names. Which one
# should I use.
# Use the one with the greatest non-missing latest_income.
# Find top 1000 for donations
charity %>%
  filter(ar_cycle_reference == "AR20") %>%
  arrange(desc(latest_income)) %>%
  arrange(desc(income_donations_and_legacies)) %>%
  distinct(registered_charity_number, .keep_all = T) %>%
  select(registered_charity_number, charity_name,
    income_donations_and_legacies, income, expenditure
  ) %>%
  head(1000) %>%
  write_csv("charity20.csv")
