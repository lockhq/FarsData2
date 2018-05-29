#Coursera Building R Packages
#Developing Data Documentation Assignment

#' @title fars_read
#'
#' @description
#' {fars_read} Loads a CSV file specifically data from the Fatality Analysis Reporting System
#' run by the US National Highway Traffice Safety Administration. If the file path is incorrect
#' the function returns a "does not exist" error.
#'
#' @param filename character string with file name of the CSV file to be read.
#'
#' @return Returns a data.frame based on the CSV file.
#' If there is not file, a "does not exist" error is read
#'
#' @examples
#' \dontrun{
#' fars_read("accident_2014.csv.bz2")
#' }
#'
#'
#' @importFrom readr read_csv
#' @importFrom dplyr tbl_df
#'
#' @export
#'
fars_read <- function(filename) {
  if(!file.exists(filename))
    stop("file '", filename, "' does not exist")
  data <- suppressMessages({
    readr::read_csv(filename, progress = FALSE)
  })
  dplyr::tbl_df(data)
}

#' @title make_filename
#'
#' @description
#' Creates a file name for the uploaded CSV file.
#' This is based on the year of the accidents, documented in the CSV file
#'
#' @param year Number representing the year of the accidents in the data set.
#'
#' @return Returns a file name based on the year provided.
#'#'
#' @examples
#' \dontrun{
#' makefilename(2014)
#' }
#'
#' @export
#'
make_filename <- function(year) {
  year <- as.integer(year)
  sprintf("accident_%d.csv.bz2", year)
}

#' @title: fars_read_years
#'
#' @description
#' The function accepts a vector or list of years and returns a list of data
#' frames with MONTH and year columns based on data in "accident_<year>.csv.bz2
#' files. The files need to be located in the working directory.
#'
#' @description
#' Function takes in a list of years, and returns a list of data frames with
#' two columns, MONTH and year. '
#'
#' @param years A list of years in numeric format.
#'
#' @return If the file exists, returns data frame with MONTH and year of
#' each of the entries in that data. Otherwise, returns NULL (i.e. error)
#'
#' @examples
#' \dontrun{
#' fars_read_years(2013:2015)
#' }
#'
#' @importFrom dplyr mutate
#' @importFrom dplyr %>%
#' @importFrom dplyr select
#'
#' @export
#'
fars_read_years <- function(years) {
  lapply(years, function(year) {
    file <- make_filename(year)
    tryCatch({
      dat <- fars_read(file)
      dplyr::mutate_(dat, year = ~year) %>%
        dplyr::select_("MONTH", "year")
    }, error = function(e) {
      warning("invalid year: ", year)
      return(NULL)
    })
  })
}

#' @title: fars_summarise_years
#'
#' @description Tabulates the number of accidents in the month-year combination
#' e.g. how many accidents happened in Apr 2014.'
#'
#' @param years A list of years (i.e. the function will then show the number
#' of accidents in each month for the given years)
#'
#' @return Data frame with months in rows and years in columns. Each value shows
#' the number oaf ccidents in that given month-year.
#'
#' @examples
#' \dontrun{
#' fars_summarize_years(2012:2014)
#' }
#'
#' @importFrom dplyr bind_rows
#' @importFrom dplyr %>%
#' @importFrom dplyr group_by
#' @importFrom dplyr summarize
#' @importFrom tidyr spread
#'
#' @export
#'
fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>%
    dplyr::group_by_("year", "MONTH") %>%
    dplyr::summarize_(n = ~n()) %>%
    tidyr::spread_("year", "n")
}

#' @title: fars_map_state
#'
#' @description This function plots, on a map, all the accidents that happened
#' in a given US state for the given year.
#'
#' @param state.num The number of a state in the US as given in the FARS data
#' sets. (This represents the state that the user wishes to plot)
#' @param year The year of analysis
#'
#' @return Returns a map plot of all the traffic accidents that happened in the
#' given state and year. Returns an error "invalid STATE number" if the state
#' number isn't in the FARS data set. Returns "no accidents to plot" if there
#' are no accidents in the state for that given year (This might happen if
#' the user selects a year that is outside of the FARS dataset
#'
#' @examples
#' \dontrun{
#' fars_map_state(2,2014)
#' }
#'
#' @importFrom dplyr filter
#' @importFrom maps map
#' @importFrom graphics points
#'
#' @export
#'
fars_map_state <- function(state.num, year) {
  filename <- make_filename(year)
  data <- fars_read(filename)
  state.num <- as.integer(state.num)

  if(!(state.num %in% unique(data$STATE)))
    stop("invalid STATE number: ", state.num)
  data.sub <- dplyr::filter_(data, ~STATE == state.num)
  if(nrow(data.sub) == 0L) {
    message("no accidents to plot")
    return(invisible(NULL))
  }
  is.na(data.sub$LONGITUD) <- data.sub$LONGITUD > 900
  is.na(data.sub$LATITUDE) <- data.sub$LATITUDE > 90
  with(data.sub, {
    maps::map("state", ylim = range(LATITUDE, na.rm = TRUE),
              xlim = range(LONGITUD, na.rm = TRUE))
    graphics::points(LONGITUD, LATITUDE, pch = 46)
  })
}
