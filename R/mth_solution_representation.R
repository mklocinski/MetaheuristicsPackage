#' @title Update Binary List
#' @description
#' Flips a Boolean value in a binary list.
#'
#' @details
#' Pending.
#'
#' @param binary_sol A binary list, or list of Boolean values.
#' @param position A number or vector of numbers specifying the position(s) in the binary list to be flipped.
#'
#' @returns a binary list.
#' @export
update_binary <- function(binary_sol, position) {
  binary_sol[position] <- !binary_sol[position]

  return(binary_sol)
}


#' @title Binary Round
#' @description
#' Generates round of binary strings.
#'
#' @details
#' Pending.
#'
#' @param init_sol A string representing the initial solution.
#' @param neighbors A number specifying the number of neighbors to iterate through.
#' @param tabu (Optional) A tabu list.
#'
#' @returns a list containing a list of the updated binary lists, a list of the
#' updated binary lists converted to strings, a list of the updated binary strings
#' converted to numbers, and a list of the positions that were flipped.
#' @export
binary_round <- function(init_sol, neighbors, tabu = list()) {
  s <- string_to_bool(init_sol)
  fps <- flip_positions(length(s), neighbors)
  legal <- fps[!(fps %in% tabu)]
  iters <- sapply(legal, function(x) update_binary(s, x))
  vals <- sapply(asplit(iters, 2), function(x) binary_to_number(x))
  clean_sols <- sapply(asplit(iters, 2), function(x) binary_to_string(x))
  return(list(
    x_raw = asplit(iters, 2),
    clean = clean_sols,
    x_values = clean_sols,
    positions = fps
  ))
}

# --------------------------------- Discrete Vector ------------------------------------ #


# -------------------------------- Continuous Vector ----------------------------------- #


# ------------------------------- String Permutation ----------------------------------- #
#' @title Permutation Round
#' @description
#' Generates round of permuted strings.
#'
#' @details
#' Pending.
#'
#' @param init_sol A string representing the initial solution.
#' @param neighbors A number specifying the number of neighbors to iterate through.
#' @param tabu (Optional) A tabu list.
#'
#' @returns a list containing a list of the updated string lists, a list of the
#' updated lists converted to strings, a list of the updated strings
#' converted to function input, and a list of the positions that were flipped.
#' @export
permutation_round <- function(init_sol, neighbors, tabu = list()) {
  s <- string_to_list(init_sol)
  fps <- permute_positions(length(s), neighbors)
  legal <- fps[!(fps %in% tabu)]
  iters <- sapply(1:(length(legal)), function(x) exchange_values(s, legal[[x]][1], legal[[x]][2]))
  vals <- sapply(asplit(iters, 2), function(x) paste0(x, collapse = ""))

  return(list(
    x_raw = asplit(iters, 2),
    clean = vals,
    x_values = vals,
    positions = legal
  ))
}

#' @title K-Swap Round
#' @description
#' Generates round of permuted strings.
#'
#' @details
#' Pending.
#'
#' @param init_sol A string representing the initial solution.
#' @param neighbors A number specifying the number of neighbors to iterate through.
#' @param tabu (Optional) A tabu list.
#'
#' @returns a list containing a list of the updated string lists, a list of the
#' updated lists converted to strings, a list of the updated strings
#' converted to function input, and a list of the positions that were flipped.
#'
#' @export
k_swap_round <- function(init_sol, neighbors, tabu = list()) {
  s <- string_to_list(init_sol)
  tabu <- c(tabu, lapply(tabu, function(x) rev(x)))
  swaps <- mhHelper::array_generator(neighbors, s)
  possible_swaps <- swaps[swaps$uniqueness == T, c("Var1", "Var2")]
  pairs <- lapply(1:nrow(possible_swaps), function(x) c(possible_swaps[x, "Var1"], possible_swaps[x, "Var2"]))
  if (length(tabu) > 0) {
    legal <- pairs[sapply(1:length(pairs), function(x) length(which(sapply(tabu, identical, pairs[[x]]))) == 0)]
  } else {
    legal <- pairs
  }
  iters <- sapply(1:(length(legal)), function(x) exchange_values(s, legal[[x]][1], legal[[x]][2]))
  vals <- sapply(asplit(iters, 2), function(x) paste0(x, collapse = ""))

  return(list(
    x_raw = asplit(iters, 2),
    clean = vals,
    x_values = vals,
    positions = legal
  ))
}
