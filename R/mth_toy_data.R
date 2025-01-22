# Set Covering
#' @title Set Covering Data
#' @description
#' Data to use in set covering problems.
#'
#'
#' @source OR 670 - Lecture 2 Excel examples.
#'
#' @format a matrix.
#' @export
data_set_covering <- rbind(
  c(0, 10, 20, 30, 30, 20),
  c(10, 0, 25, 35, 20, 10),
  c(20, 25, 0, 15, 30, 20),
  c(30, 35, 15, 0, 15, 25),
  c(30, 20, 30, 15, 0, 14),
  c(20, 10, 20, 25, 14, 0)
)


# Job Shop (one machine, n jobs)
#' @title Set Covering Data
#' @description
#' Data to use in one machine, n jobs job shop problems.
#'
#'
#' @source OR 670 - Lecture 2 Excel examples.
#'
#' @format a matrix.
#' @export
data_1_mach_n_jobs <- rbind(
  c(1, 10, 4, 14),
  c(2, 10, 2, 12),
  c(3, 13, 1, 1),
  c(4, 4, 12, 12)
)

#' @title Set Covering Data
#' @description
#' Data to use in two machines, n jobs job shop problems.
#'
#'
#' @source OR 670 - Lecture 2 Excel examples.
#'
#' @format a matrix.
#' @export
data_2_mach_n_jobs <- rbind(
  c(1, 9, 10, 14),
  c(2, 9, 8, 12),
  c(3, 12, 5, 1),
  c(4, 3, 28, 12)
)

#' @title Traveling Salesman
#' @description
#' Data in traveling salesman problems.
#'
#'
#' @source OR 670 - Lecture 2 Excel examples.
#'
#' @format a matrix.
#' @export
data_tsp <- rbind(
  c(0, 13, 16, 8),
  c(13, 0, 14, 15),
  c(16, 14, 0, 9),
  c(8, 15, 9, 0)
)
