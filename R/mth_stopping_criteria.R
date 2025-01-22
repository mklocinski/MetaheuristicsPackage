#' @title Check elapsed time
#' @description
#' Checks elapsed time against stopping criteria.
#'
#' @details
#' Pending.
#'
#' @param sc_list A list of algorithm output that can be evaluated by stopping functions.
#' @param stop_crit The value to be used when determining if an algorithm should stop.
#'
#' @returns a Boolean value.
#' @export
elapsed_time <- function(sc_list, stop_crit) {
  s <- difftime(Sys.time(), sc_list[1], units = "mins") < stop_crit

  return(s)
}


#' @title Check number of rounds
#' @description
#' Checks number of rounds against stopping criteria.
#'
#' @details
#' Pending.
#'
#' @param sc_list A list of algorithm output that can be evaluated by stopping functions.
#' @param stop_crit The value to be used when determining if an algorithm should stop.
#'
#' @returns a Boolean value.
#' @export
number_of_rounds <- function(sc_list, stop_crit) {
  s <- sc_list[2] < stop_crit

  return(s)
}

#' @title Check relative improvement
#' @description
#' Checks improvement between rounds against stopping criteria.
#'
#' @details
#' Pending.
#'
#' @param sc_list A list of algorithm output that can be evaluated by stopping functions.
#' @param stop_crit The value to be used when determining if an algorithm should stop.
#'
#' @returns a Boolean value.
#' @export
relative_improvement <- function(sc_list, stop_crit) {
  s <- sc_list[3] > stop_crit

  return(s)
}
