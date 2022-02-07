library(readr)
library(dplyr)
library(purrr)
library(stringr)

url <- "https://www.ssa.gov/oact/babynames/names.zip"
destfile <- "data-raw/names.zip"
exdir <- "data-raw/names"

download.file(url, destfile)
unzip(destfile, exdir = exdir)

files <- list.files(exdir, pattern = "\\.txt$", full.names = TRUE)

names_first <- map_dfr(files, function(path) {
  # read file
  x <- read_csv(
    path,
    col_names = c("name", "sex", "n"),
    col_types = "cci")

  # extract year from filename
  year <- str_match(basename(path), "[0-9]{4}")[,1]

  # add stat columns
  x %>%
    group_by(sex) %>%
    mutate(
      year = as.integer(year),
      pct = n/sum(n)) %>%
    ungroup() %>%
    select(year, everything())
})

# save data
usethis::use_data(names_first, overwrite = TRUE)

# clean up
unlink(destfile)
unlink(exdir, recursive = TRUE)

