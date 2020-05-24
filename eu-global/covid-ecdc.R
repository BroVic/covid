# Inspired by https://www.ecdc.europa.eu/en/publications-data/download-todays-data-geographic-distribution-covid-19-cases-worldwide

# System requirements: Recent version of R e.g. R-3.6.x (www.r-project.org)
#
# Running the script:
# 1. From the command line: Navigate to directory where script is saved and
#                           there run this command:
#                               Rscript covid-ecdc.R
#
# 2. From the R console:    Call the `source()` function using the relative or
#                           absolute path to the script as argument i.e.
#                               source("path/to/dir/covid-ecdc.R")
#
# 3. Using R Studio:        Simply open the source file and in the source
#                           editor toolbar, click on "Source".

location <- commandArgs(trailingOnly = TRUE)

local({
  
  isInteractive <- interactive()
  
  if (!requireNamespace('here', quietly = TRUE))
    install.packages('here')
  source(here::here("shiny/globals.R"))
  
  # dirs <- lapply(nm, function(x) {
  #   dir.create(here(x), showWarnings = FALSE)
  #   x
  # }) %>%
  #   setNames(nm)
  
  covdata <- decide_and_execute_data_sourcing(dirs$.cache, prefix, today)
  # Collect user input.
  # Multiple countries' names are to be separated with a comma
  prompt <-
    paste(c("Enter one or more countries, separated by a ','",
            "('q' to exit): "),
          collapse = " ")
  
  if (identical(location, character(0))) {
    repeat {
      location <- if (isInteractive)
        readline(prompt)
      else {
        cat(prompt)
        readLines('stdin', n = 1L)
      }
      if (location != "")
        break
    }
  }
  
  if (tolower(location) == "q")
    return(NULL)
  
  # Words of countries with coumpound names are separated with underscores
  # Handling depends on how data is provided
  location <- underscore_compd_names(location)
  
  # Prepare data and draw plot
  gg <- create_ggplot(covdata, location)
  
  if (isInteractive)
    print(gg)
  
  locsaved <- paste(tolower(location), collapse = "+")
  fname <- sprintf("%s_covid_%s.png", as.character(today), locsaved)
  ggplot2::ggsave(file.path(dirs$fig, fname))
  
})
