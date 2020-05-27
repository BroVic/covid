# globals.R
# -----------------------------
# 
# NB: Some of the functions defined here are not called

library(tools)

## Important vectors
# -----------------------------
nm <- c(".cache", "www", "fig")
dirs <- structure(as.list(nm), names = nm)
today <- Sys.Date()
prefix <- "covid_data_"

# Inputs
cntryInputId <- 'country'
varInputId <- 'variable'
cntryInputLabel <- toTitleCase(cntryInputId)
varInputLabel <- toTitleCase(varInputId)
# selectorLabel <- sprintf("%s (one or more):", cntryInputLabel)


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











# Current data are collected and if not available, downloaded from source.
# If unable to access source, old data on disk are used. If all fails,
# execution is stopped entirely.
decide_and_execute_data_sourcing <- function(dir, pref, day) {
    fetch <- TRUE
    if (dataOnDisk(dir, pref)) {
      obj <- readCovidObj(dir, pref)
      if (obj$meta$created == day)
        fetch <- FALSE
    }
    
    if (fetch)
      obj <- get_eu_data(dir, pref)
    obj
}










# Reads previously stored data, selecting the most recent
readCovidObj <- function(dir, prefix) {
  files <- find_files(dir, prefix)
  files <- sort(files, decreasing = TRUE)
  readRDS(files[1])
}







# Finds the data files in the directory, identifiable by the prefix
find_files <- function(dir, prefix) {
  rgx <- paste0("^", prefix, '.+\\.rds$')
  list.files(dir, rgx, full.names = TRUE)
}







# Checks if there are stored data or not
dataOnDisk <- function(dir, prefix) {
  dlist <- find_files(dir, prefix)
  !identical(dlist, character(0))
}








get_eu_data <- function(dir, prefix) {
  require(httr, quietly = TRUE)
  suppressPackageStartupMessages(require(curl, quietly = TRUE))
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
    if (!dataOnDisk(dir, prefix))
      stop("No data were found on disk", call. = FALSE)
  })
  readCovidObj(dir, prefix)
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
clearCache <- function(dir, new, pref) {
  stopifnot(dir.exists(dir), inherits(new, 'COVIDdata'), is.character(pref))
  if (!dataOnDisk(dir, pref))
    return(FALSE)
  fs <- list.files(dir, '.\\.rds$', full.names = TRUE)
  old <- readCovidObj(dir, pref)
  if (old$meta$created >= new$meta$created)
    return(FALSE)
  file.remove(fs)
}





 




save_eu_covid_rds <- function(dir, data, prefix, url) {
  stopifnot(dir.exists(dir), is.data.frame(data))
  today <- Sys.Date()
  covdata <- structure(list(
    data = data,
    meta = list(created = today, source = url)
  ), class = 'COVIDdata')
  if (!clearCache(dir, covdata, prefix))
    return()
  fpath <-
    file.path(dir, paste0(prefix, as.character(today), ".rds"))
  saveRDS(covdata, fpath)
}










underscore_compd_names <- function(str) {
  require(magrittr, quietly = TRUE)
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

















create_ggplot <- function(covdata, loc, var) {
  require(magrittr, quietly = TRUE)
  require(ggplot2, quietly = TRUE)
  theme_set(theme_minimal())
  df <- covdata$data %>% 
    transformData(loc)
  title <-
    paste("COVID-19 Trend for", countryTitle(loc))
  latest <- with(df, max(date))
  subtitle <- paste("Updated", format(latest, "%A, %d %B %Y"))
  caption <- covdata$meta$source %>%
    httr::parse_url() %>%
    `[[`("hostname") %>%
    paste("Source:", .)
  
  szline <- 1.1
  center <- 0.5
  bold <- "bold"
  # browser()
  
  
  gg <- ggplot(df, aes(x = date))
  
  opts <- c('cases', 'deaths')
  if (var == 'both') {
    gg <- gg +
      geom_line(aes(y = cases, color = opts[[1]]), size = szline) +
      geom_line(aes(y = deaths, color = opts[[2]]), size = szline) +
      scale_color_brewer(labels = opts, palette = 'Set1')
  }
  else {
    opts <- var
    gg <- gg +
      geom_line(aes_string(y = var, color = 'Country'), size = szline) +
      scale_color_brewer(cntryInputLabel, palette = 'Set1')
  }
 
  gg +
    labs(title = title,
         subtitle = subtitle,
         caption = caption) +
    ylab(sprintf("No. of %s", paste0(toTitleCase(opts), collapse = "/"))) +
    theme(
      plot.title = element_text(hjust = center, face = bold),
      plot.subtitle = element_text(hjust = center),
      panel.border = element_rect(colour = 'darkgray', size = szline, fill = NA),
      axis.text = element_text(face = bold, size = 12),
      axis.title.x = element_blank(),
      axis.title.y = element_text(face = bold),
      legend.title = element_blank()
    )
}









# Transforms the data a bit to ease dealing with dates and 
# focuses on the chosen country or countries
transformData <- function(data, country) {
  suppressPackageStartupMessages(require(dplyr, quietly = TRUE))
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










# Provides the names to be used for selecting input
get_country_names <- function(obj) {
  stopifnot(inherits(obj, "COVIDdata"))
  data <- obj$data
  unique(data$countriesAndTerritories)
}