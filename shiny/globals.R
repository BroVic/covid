# globals.R
# -----------------------------
# 
# NB: Some of the functions defined here are not called

library(tools)

## Important vectors
# -----------------------------
nm <- c(".cache", "www", "fig")
dirs <- structure(as.list(nm), names = nm)
prefix <- "covid_data_"

# Inputs
cntryInputId <- 'country'
varInputId <- 'variable'
cntryInputLabel <- toTitleCase(cntryInputId)
varInputLabel <- toTitleCase(varInputId)


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
source_data <- function(dir, pref) {
    fetch <- TRUE
    if (dataOnDisk(dir, pref)) {
      obj <- readCovidObj(dir, pref)
      if (obj$meta$created == Sys.Date())
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
  any(file.remove(fs))
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

















create_ggplot <- function(covdata, loc, var, plottype) {
  invisible(lapply(c('magrittr', 'ggplot2', 'rlang'),
                   require, character.only = TRUE, quietly = TRUE))
  df <- select_countrydata(covdata, loc)
  if (is.null(df))
    return()
 
  szline <- 1.1
  center <- 0.5
  bold <- "bold"
  opts <- c('cases', 'deaths')
  
  gg <- ggplot(df, aes(x = date))
  if (plottype == 'tsplot') {
    var1 <- expr(cases)
    var2 <- expr(deaths)
    opts.lab <- opts
  }
  else if (plottype == 'cumplot') {
    var1 <- expr(cum.cases)
    var2 <- expr(cum.deaths)
    opts.lab <- make_cum(opts)
  }
  
  if (length(var) == 2L) {
    gg <- gg +
      geom_line(aes(y = !!var1, color = opts.lab[[1]]), size = szline) +
      geom_line(aes(y = !!var2, color = opts.lab[[2]]), size = szline) +
      scale_color_brewer(labels = opts.lab, palette = 'Set1')
  }
  else {
    gg <- gg +
      geom_line(aes_string(y = var, color = 'Country'), size = szline) +
      scale_color_brewer(cntryInputLabel, palette = 'Set1')
  }
  
  annot <- set_annotations(covdata, loc)
  gg +
    labs(title = annot$title,
         subtitle = annot$subtitle,
         caption = annot$caption) +
    ylab(sprintf("No. of %s", paste0(toTitleCase(opts.lab), collapse = "/"))) +
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









# Filter the data and returns a data frame with only selected countries' data
select_countrydata <- function(covobj, country) {
  stopifnot(inherits(covobj, "COVIDdata"), is.character(country))
  dplyr::filter(covobj$data, Country == country)
}








set_annotations <- function(covobj, country) {
  stopifnot(inherits(covobj, "COVIDdata"), is.character(country))
  require(magrittr)
  df <- select_countrydata(covobj, country)
  title <-
    paste("COVID-19 Trend for", countryTitle(country))
  latest <- with(df, max(date))
  subtitle <- paste("Updated", format(latest, "%A, %d %B %Y"))
  caption <- covobj$meta$source %>%
    httr::parse_url() %>%
    `[[`("hostname") %>%
    paste("Source:", .)
  list(title = title, subtitle = subtitle, caption = caption)
}








# Transforms the data a bit to ease dealing with dates and 
# focuses on the chosen country or countries. The following
# additional columns are created:
#  - cum.cases: The cumuluative sum of cases
#  - cum.deaths: The cumulative sum of deaths
#  - date: The date in the POSIX format i.e. YYYY-MM-DD
#
# The following column is renamed:
#  - countriesAndTerritories => Country
#
transformData <- function(data) {
  suppressPackageStartupMessages(require(dplyr, quietly = TRUE))
  suppressWarnings({
    data %>% 
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
  cntry.names <- unique(obj$data$Country)
  names(cntry.names) <- chartr('_', " ", cntry.names)
  cntry.names
}







draw_summary_table <- function(covidObj, location, var) {
  
}







# Prefixes a character vector with 'cum.'
make_cum <- function(x) {
  stopifnot(is.character(x))
  paste0('cum.', x)
}




# Creates an informational panel
my_infobox_panel <- function() {
  require(shiny)
  inputPanel(
    span(
      "This is a minimalist application created add some utility to",
      
      a('this script',
        href = "https://gist.github.com/BroVic/32eca9d3ae3334bfe573e4aecb35c522",
        target = '_blank'),
      ", which contains the initial work carried out to examine global COVID-19 cases and/or deaths",
      .noWS = "outside"
    ),
    
    span(
      "If you would like us to add a feature or you find a bug, kindly post a message ",
      a('here',
        href = 'https://github.com/BroVic/covid/issues/new',
        target = "_blank")
    ),
    
    span("For the source code, visit",
         strong(
           a("this page",
             href = 'https://github.com/BroVic/covid/tree/master/shiny',
             target = "_blank")
         )),
    
    br(),
    
    span(actionLink("closeFdbk", "Close"), id = "closing-link")
  )
}