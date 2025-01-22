#' @title Choose maximum objective function output value
#' @description
#' Identify the solution that yields the max value from a list of solutions.
#'
#' @details
#' Pending.
#'
#' @param xs A list of objective function solutions (x values) in its raw form (a list of values).
#' @param ys A list of objective function outputs (y values).
#'
#' @returns a list containing the nest solution  or the yielding the maximum objective function output in its
#' raw form (a list of values), a cleaned version of the best solution, the solution's value, and the
#' position of the best solution in the solution list.
#' @export
max_val <- function(xs, ys) {
  value <- max(ys)
  sol_pos <- match(value, ys)
  sol_raw <- xs$x_raw[sol_pos][[1]]
  sol_clean <- xs$clean[sol_pos][[1]]

  return(list(
    sol_raw = sol_raw,
    clean = sol_clean,
    value = value,
    position = sol_pos
  ))
}


#' @title Choose minimum objective function output value
#' @description
#' Identify the solution that yields the min value from a list of solutions.
#'
#' @details
#' Pending.
#'
#' @param xs A list of objective function solutions (x values) in its raw form (a list of values).
#' @param ys A list of objective function outputs (y values).
#'
#' @returns a list containing the nest solution  or the yielding the minimum objective function output in its
#' raw form (a list of values), a cleaned version of the best solution, the solution's value, and the
#' position of the best solution in the solution list.
#' @export
min_val <- function(xs, ys) {
  value <- min(ys)
  sol_pos <- match(value, ys)
  sol_raw <- xs$x_raw[sol_pos][[1]]
  sol_clean <- xs$clean[sol_pos][[1]]

  return(list(
    sol_raw = sol_raw,
    clean = sol_clean,
    value = value,
    position = sol_pos
  ))
}
