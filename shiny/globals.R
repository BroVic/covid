# globals.R
# -----------------------------

library(dplyr)
library(ggplot2)
library(curl)
library(httr)


## Important vectors
# -----------------------------
nm <- c(".cache", "fig")
today <- Sys.Date()
prefix <- "covid_data_"




## Functions
# -----------------------------


# Checks for missing packages and installs them if necessary
# Then it attaches them to the search path
attach_packages <- function() {
  message("Attaching packages... ", appendLF = FALSE)
  pkgs <- c("here", "httr", "ggplot2", "dplyr", "curl")
  miss <- pkgs[!pkgs %in% .packages(all.available = TRUE)]
  if (!identical(miss, character())) {
    install.packages(miss,
                     dependencies = TRUE,
                     repos = 'https://cran.rstudio.com')
  }
  invisible(sapply(pkgs, function(x)
    suppressPackageStartupMessages(library(x, character.only = TRUE))))
  message("Done")
}











# Reads previously stored data, selecting the most recent
readData <- function(dir, prefix) {
  files <- find_files(dir, prefix)
  files <- sort(files, decreasing = TRUE)
  readRDS(files[1])
}







# Finds the data files in the directory, identifiable by the prefix
find_files <- function(dir, prefix) {
  rgx <- paste0("^", prefix, '.+\\.rds$')
  list.files(dir, rgx, full.names = TRUE)
}









# Transforms the data a bit to ease dealing with dates and 
# focuses on the chosen country or countries
transformData <- function(data, country) {
  stopifnot(is.character(country))
  suppressWarnings({
    data %>%
      filter(countriesAndTerritories == country) %>% 
      mutate(date = as.Date(dateRep, format = "%d/%m/%Y")) %>% 
      arrange(date) %>% 
      mutate(cum.cases = cumsum(cases)) %>% 
      mutate(cum.deaths = cumsum(deaths)) %>% 
      rename(Country = countriesAndTerritories)
  })
}







# Checks if there are stored data or not
dataOnDisk <- function(dir, prefix) {
  dlist <- find_files(dir, prefix)
  !identical(dlist, character(0))
}








# Gets an appropriate placeholder value for the main title
countryTitle <- function(c) {
  stopifnot(is.character(c))
  num <- length(c)
  if (num > 1)
    return(paste(num, "countries"))
  chartr("_", " ", c)
}







## Invalidates cache - TODO: implement
invalidateCache <- function(dir, new) {
  if (!dataOnDisk())
    return(FALSE)
  fs <- list.files(dir, '.\\.rds$', full.names = TRUE)
  old <- readData()
  if (old$meta$created >= new$meta$created)
    return(FALSE)
  file.remove(fs)
  TRUE
}





 
get_eu_data <- function(dir, prefix) {
  tryCatch({
    if (interactive() && !has_internet()) {
      response <- NULL
      stop("No internet connection", call. = TRUE)
    }
    message("Fetching data from the server... ", appendLF = FALSE)
    response <- GET(
      url <-
        "https://opendata.ecdc.europa.eu/covid19/casedistribution/csv",
      authenticate(":", ":", type = "ntlm"),
      write_disk(tf <- tempfile(fileext = ".csv"))
    )
    message("Done")
    dt <- read.csv(tf, stringsAsFactors = FALSE)
    save_eu_covid_rds(dir, dt, prefix, url)
  },
  error = function(e) {
    msg <- NULL
    if (!is.null(response)) {
      if (response$status_code == 200)
        stop(e)
      msg <-
        sprintf("Failed (Status: %s). ", response$status_code)
    }
    msg <-
      paste0(msg, "Using data on disk (if available)", collapse = '\n')
    message(msg)
    if (!dataOnDisk())
      stop("No data were found on disk", call. = FALSE)
    covdata <<- readData()
    dt <<- covdata$data
  })
}





save_eu_covid_rds <- function(dir, data, prefix, url) {
  stopifnot(dir.exists(dir), is.data.frame(data))
  today <- Sys.Date()
  covdata <- structure(list(
    data = data,
    meta = list(created = today, source = url)
  ), class = 'COVIDdata')
  fpath <-
    file.path(dir, paste0(prefix, as.character(today), ".rds"))
  saveRDS(covdata, fpath)
}










underscore_compd_names <- function(str) {
  str %>%
    {
      if (grepl('\\,', .)) {
        strsplit(., ',') %>%
          unlist(recursive = FALSE) %>%
          gsub("^\\s|\\s$", "", .)
      } else
        .
    } %>%
    {
      if (any(grepl('\\s', .))) {
        gsub("\\s", "_", .)
      } else
        .
    }
}

















create_ggplot <- function(covdata, loc) {
  theme_set(theme_minimal())
  df <- covdata$data
  df <- transformData(df, loc)
  title <-
    paste("COVID-19 Trend for", countryTitle(loc))
  latest <- with(df, max(date))
  subtitle <- paste("Updated", format(latest, "%A, %d %B %Y"))
  caption <- covdata$meta$source %>%
    parse_url() %>%
    `[[`("hostname") %>%
    paste("Source:", .)
  
  gg <- ggplot(df, aes(x = date))
  # if (length(loc) == 1L) {
  #   gg <- gg  +
  #     geom_point(col = "blue", size = 2)
  # }
  size <- 1
  gg +
    geom_line(aes(y = cases, color = 'cases'), size = size) +
    geom_line(aes(y = deaths, color = 'deaths'), size = size) +
    scale_color_brewer("Variable", labels = c('cases', 'deaths'), palette = "Set1") +
    labs(title = title,
         subtitle = subtitle,
         caption = caption) +
    xlab("Date") +
    ylab("No. of Cases/Deaths") +
    theme(
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5),
      axis.text.x = element_text(face = "bold"),
      legend.title = element_text(face = 'bold')
    )
}
