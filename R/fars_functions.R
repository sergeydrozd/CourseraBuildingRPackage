
#' Read file with FARS data
#'
#' This function reads data from .csv file, stored on disk, from the \strong{US
#' National Highway Traffic Safety Administration's} \emph{Fatality Analysis
#' Reporting System} (FARS), which is a nationwide census, providing the
#' American public YEARly data, regarding fatal injuries suffered in motor
#' vehicle traffic crashes.
#' @importFrom readr read_csv
#' @importFrom dplyr tbl_df
#'
#' @param filename A character string with the name of the file to read, see
#'   notes.
#'
#' @return A data frame with data readed from the csv file, or an error if the
#'   file does not exists.
#'
#' @examples
#' \dontrun{
#' library(dplyr)
#' library(readr)
#' yr <- 2015
#' data <- yr %>%
#'   make_filename %>%
#'   fars_read
#' View(data)
#' }
#' @note To generate file name use: \code{\link{make_filename}}
#' @seealso \link{make_filename}
#' @export
fars_read <- function(filename) {
  if(!file.exists(filename))
    stop("file '", filename, "' does not exist")
  data <- suppressMessages({
    readr::read_csv(filename, progress = FALSE)
  })
  dplyr::tbl_df(data)
}


#' Make data file name
#'
#' Make .csv data file name related to the given \code{YEAR}
#' The function does not check if the file is available.
#'
#' @param YEAR A string or an integer with the input \code{YEAR}
#'
#' @return This function returns a string with the data file name for a given
#'   YEAR
#'
#' @examples
#' make_filename(2013)
#' # "accident_2013.csv.bz2"
#' @seealso \link{fars_read}
#' @export

make_filename <- function(YEAR) {
  YEAR <- as.integer(YEAR)
  system.file("extdata",
              sprintf("accident_%d.csv.bz2", YEAR),
              package = "CourseraBuildingRPackage",
              mustWork = TRUE)
}



#' Read FARS YEARs
#'
#' Ancillary function for \code{fars_summarize_years}
#' @param YEARs A vector with a list of YEARs
#'
#' @importFrom dplyr mutate_
#' @importFrom dplyr select_
#' @importFrom magrittr "%>%"
#
#' @return A data.frame including entries in data by MONTH, or NULL if the
#'  \code{YEAR} is not valid
#'
#' @seealso \link{fars_read}
#' @seealso \link{make_filename}
#' @seealso \link{fars_summarize_years}
#' @examples
#' \dontrun{
#' fars_read_years(2013)
#' }
#' @export
fars_read_years <- function(years) {
  lapply(years, function(YEAR) {
    file <- make_filename(YEAR)
    tryCatch({
      dat <- fars_read(file)
      dplyr::mutate_(dat,  YEAR = "YEAR") %>%
        dplyr::select_("MONTH", "YEAR")
    }, error = function(e) {
      warning("invalid YEAR: ", YEAR)
      return(NULL)
    })
  })
}

#' Summarize FARS data by YEARs
#'
#' This function summarizes YEARly accidents data by MONTH
#' @param YEARs A vector with a list of YEARs to summarize by.
#'
#' @return A data.frame with number of accidents by YEARs summarized by MONTH
#' @importFrom dplyr bind_rows
#' @importFrom dplyr group_by_
#' @importFrom dplyr summarize_
#' @importFrom tidyr spread_
#' @importFrom magrittr "%>%"
#' @seealso \link{fars_read_years}
#' @examples
#' \dontrun{
#' plot(fars_summarize_years(2015))
#' fars_summarize_years(c(2015, 2014))
#' }
#' @export
fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>%
    dplyr::group_by_("YEAR", "MONTH") %>%
    dplyr::summarize_(n = "n()") %>%
    tidyr::spread_("YEAR", "n")
}

#' Display accidents map by state and YEAR
#'
#' Creates a plot with a state map including the accidents location by YEAR
#' If the \code{state_num} is invalid the function shows an error
#' @param state_num An Integer with the state Code
#' @param YEAR A string, or an integer, with the input \code{YEAR}
#'
#' @importFrom maps map
#' @importFrom dplyr filter_
#' @importFrom graphics points
#' @return None
#' @seealso \link{fars_read}
#' @seealso \link{make_filename}
#' @export
fars_map_state<- function(state_num, YEAR) {
  filename <- make_filename(YEAR)
  data <- fars_read(filename)
  state_num <- as.integer(state_num)
  if(!(state_num %in% unique(data$STATE))) {
    stop("invalid state number: ", state_num)
  }
  data.sub <- dplyr::filter_(data, .dots = paste0("STATE==", state_num))
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
