#' Generate Names
#' @param n number of samples
#' @param sex which sex
#' @param year year of birth
#' @export
generate_name <- function(n = 10L, sex = c(NA_character_, "f", "m"), year = NULL) {
  sex <- match.arg(sex)
  first_name <- generate_name_first(n, sex, year)
  last_name <- generate_name_last(n)
  paste(first_name, last_name, sep = " ")
}

#' @rdname generate_name
#' @export
generate_name_first <- function(n = 10L, sex = c(NA_character_, "f", "m"), year = NULL) {
  sex <- match.arg(sex)
  df <- randomnames::names_first

  min_year <- min(df$year)
  max_year <- max(df$year)
  if (year < min_year || year > max_year) {
    stop(sprintf("Year must be between %i and %i. Year provided was %s.", min_year, max_year, year))
  }

  # filter by sex
  if (!is.na(sex)) {
    df <- df[df$sex == toupper(sex),]
  }

  # filter by year
  if (is.numeric(year)) {
    df <- df[df$year == year,]
  }

  # sample results
  idx <- sample.int(nrow(df), size = n, replace = TRUE, prob = df$pct)
  df$name[idx]
}

#' @rdname generate_name
#' @export
generate_name_last <- function(n = 10L) {
  df <- randomnames::names_last
  idx <- sample.int(nrow(df), size = n, replace = TRUE, prob = df$pct)
  df$surname[idx]
}
