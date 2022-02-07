# https://www.census.gov/topics/population/genealogy/data/2010_surnames.html

library(readxl)
library(dplyr)
library(purrr)
library(stringr)

#url <- "https://www2.census.gov/topics/genealogy/2010surnames/names.zip"
#destfile <- "data-raw/surnames.zip"
#exdir <- "data-raw/surnames"
#unzip(destfile, exdir = exdir)

url <- "https://www2.census.gov/topics/genealogy/2010surnames/Names_2010Census_Top1000.xlsx"
destfile <- file.path("data-raw", basename(url))

download.file(url, destfile)

# (S) means value surpressed for confidentiality
names_last <- read_excel(destfile, na = c("", "(S)"), skip = 2) %>%
  mutate(across(c("RANK", "FREQUENCY (COUNT)", "CUMULATIVE PROPORTION"), as.integer))

colnames(names_last) <- c(
  "surname", "rank", "n", "n_per_100k", "cumulative_n_per_100k",
  "pct_white", "pct_black", "pct_hawaiian", "pct_american_indian",
  "pct_multiple", "pct_hispanic")

names_last <- names_last %>%
  filter(!is.na(rank)) %>%
  mutate(
    pct = n_per_100k/sum(n_per_100k),
    surname = str_to_title(surname),
    year = 2010
  ) %>%
  mutate(across(starts_with("pct_"), function(x) if_else(is.na(x), 0, x))) %>%
  select(year, everything())

# save data
usethis::use_data(names_last, overwrite = TRUE)

# clean up
unlink(destfile)
