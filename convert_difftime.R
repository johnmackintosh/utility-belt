#' convert_difftime
#' converts the output of difftime to numeric
#'
#' @param later_time the later of the two  dates (and / or times)
#' @param earlier_time the earlier of the two times (and / or times)
#' @param units how the difference should be  expressed. Defaults to mins.
#' Possible options are "auto", "secs", "mins", "hours", "days", or  "weeks"
#' 
#' @return time difference converted to numeric
#' @export
#'
#' @examples
#'\donttest{
#'convert_difftime(data$discharge_date_time,data$arrival_date_time)
#'}

convert_difftime <- function(later_time, earlier_time, units = "mins") {
  as.numeric(difftime(later_time,earlier_time, units = units))
}
