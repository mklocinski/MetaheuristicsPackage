#' @title Convert string to list
#' @description
#' Translates a solution string to list.
#'
#' @details
#' Solution strings represent sequence information.
#'
#' @param str_sol a string of characters.
#'
#' @returns a list of characters.
#' @export
#' @examples
#' string_to_list("2143")
string_to_list <- function(str_sol) {
  to_list <- strsplit(str_sol, "")[[1]]

  return(to_list)
}

#' @title Convert string to Boolean
#' @description
#' Translates a solution string to a binary list.
#'
#' @details
#' Solution strings can represent sequence information or a binary string.
#' Binary lists are lists of Boolean values that represent a binary string. For example,
#' the binary list list(F, T, F, T, T) represents the binary vector 01011.
#'
#' @param str_sol a string of characters.
#'
#' @returns a list of Boolean values corresponding to a binary string.
#'
#' @export
#' @examples
#' string_to_bool("2143")
#'
string_to_bool <- function(str_sol) {
  to_list <- string_to_list(str_sol)

  return(sapply(to_list, function(x) ifelse(x == 1, T, F)))
}

#' @title Convert binary string to number
#' @description
#' Translates a binary list to a number.
#'
#' @details
#' Binary lists are lists of Boolean values that represent a binary string. For example,
#' the binary list list(F, T, F, T, T) represents the binary vector 01011.
#'
#' Binary strings are translated to binary lists using 'string_to_binary()'.
#'
#' @param binary_sol a list of Boolean values corresponding to a binary string.
#'
#' @returns a number.
#'
#' @export
#'
binary_to_number <- function(binary_sol) {
  b <- rev(sapply(0:(length(binary_sol) - 1), function(x) 2^x))

  return(sum(b[binary_sol]))
}

#' @title Convert binary list to string
#' @description
#' Translate binary list to string.
#'
#' @details
#' Binary lists are lists of Boolean values that represent a binary string. For example,
#' the binary list list(F, T, F, T, T) represents the binary vector 01011.
#'
#' This function is used to format algorithm output.
#'
#' @param binary_sol a list of Boolean values corresponding to a binary vector.
#'
#' @returns a string.
#'
#' @export
#'
binary_to_string <- function(binary_sol) {
  return(paste(sapply(binary_sol, function(x) ifelse(x == T, 1, 0)), collapse = ""))
}

#' @title Convert list of strings to list of Boolean
#' @description
#' Translate list of 0s and 1s, stored as characters, to a list of Boolean values.
#'
#' @details
#' Binary lists  are initially entered as strings and need to be translated to a
#' list of Boolean values for use by this package. The function 'string_to_list()'
#' can convert a string to a list of characters, which can then be converted to a
#' list of Boolean values by this function.
#'
#' @param str_list a list of characters representing a binary string.
#'
#' @returns a list of Boolean values corresponding to a binary string.
#'
#' @export
#' @examples
#' string_list_to_bool(list("1", "1", "0", "0", "1"))
#'
string_list_to_bool <- function(str_list) {
  bool_list <- sapply(str_list, function(x) ifelse(x == 0, F, T))

  return(bool_list)
}


#' @title Calculate maximum feasible value of binary
#' @description
#' Get maximum possible value for based on the length of a binary.
#'
#' @details
#' A 2-bit binary vector will have a maximum value of 3. A 3-bit binary vector
#' will have a maximum value of 7.
#'
#' @param binary_sol a list of Boolean values corresponding to a binary string.
#'
#' @returns a number.
#'
#' @export
#' @examples
#' binary_max(list("1", "1", "0", "0", "1"))
#'
binary_max <- function(binary_sol) {
  b <- rev(sapply(0:(length(binary_sol) - 1), function(x) 2^x))

  return(sum(b))
}

#' @title Apply adaptive memory
#' @description
#' Apply an adaptive memory to a list of prior solutions.
#'
#' @details
#' Certain heuristics, like tabu search, have an adaptive memory that maintains a
#' search history of a specified length. This function prunes a list of prior solutions
#' to a specified length.
#'
#' @param search_history a list of prior solutions.
#' @param memory the maximum length of search_history
#'
#' @returns a list of prior solutions.
#'
#' @export
#' @examples
#' adaptive_memory(list("010", "210", "100", "120"), 3)
#'
adaptive_memory <- function(search_history, memory) {
  if (length(search_history) > memory) {
    return(search_history[(length(search_history) - memory + 1):length(search_history)])
  } else {
    return(search_history)
  }
}


#' @title Matrix setup
#' @description
#' Creates a named, valid transition matrix from input.
#'
#' @details
#' Used to set up matrices.
#'
#' @param data a list of rbind-ed rows.
#' @param state_names (Optional) a list of row and column names list(list(row_names), list(column_names)). If not provided, uses alphabetical labels.
#' @param row_sum_check (Optional) Boolean; should the rows sum to one? Default is True.
#' @returns a list of prior solutions.
#' @export
#'
matrix_setup <- function(data, state_names = list(), row_sum_check = T) {
  dims <- dim(data)
  square <- dims[1] == dims[2]
  stopifnot(square)

  if (length(state_names) == 0) {
    states <- LETTERS[seq(1, dims[1])]
  } else {
    states <- state_names
  }

  m <- matrix(data, ncol = dims[1], nrow = dims[2], dimnames = list(states, states))
  if (row_sum_check == T) {
    rows_sum_to_one <- all(sapply(rowSums(data), function(x) ifelse(x == 1, T, F)))
    stopifnot(rows_sum_to_one)
  }


  return(m)
}


#' @title Count off items in a list into queues
#' @description
#' Sorts items into queues based on item position in item list.
#'
#' @details
#' Works the same dealing n cards to m people, except that it allows for
#' remainders to be included (so the mth player can get (n/m) + 1 cards).
#'
#' To sort items into queues based on queue length, use 'next_in_line' function.
#'
#' @param items a list of items to sort.
#' @param queues a list of queues to sort items into.
#'
#' @returns a list containing item order per queue.
#' @export
#'
countoff <- function(items, queues) {
  countoff_list <- list()
  seq <- 1

  for (i in 1:length(items)) {
    if (seq < length(queues)) {
      countoff_list[[length(countoff_list) + 1]] <- seq
      seq <- seq + 1
    } else {
      countoff_list[[length(countoff_list) + 1]] <- seq
      seq <- 1
    }
  }

  return(unlist(countoff_list))
}
countoff(items = list(4, 50, 6, 3, 1, 9), queues = list(1, 2))

#' @title Sort items into queues based on queue status
#' @description
#' Next in line (used for sorting items to queues with evaluation).
#'
#' @details
#' Pending.
#'
#' @param items a list of items to sort.
#' @param queues a list of queues to sort items into.
#' @param min_max (Optional) Default value 1 instructs function to evaluate based on maximum value, -1 instructs function to evaluate based on minimum value.
#'
#'
#' @returns a list containing item order per queue.
#'
#' @export
#' @examples
#' next_in_line(items = list(4, 50, 6, 3, 1, 9), queues = list(1, 2))
#'
next_in_line <- function(items, queues, min_max = 1) {
  queue_order <- rep(vector(mode = "list", length = 1), length(queues))

  for (q in queues) {
    queue_order[[q]][as.character(items[[q]])] <- items[[q]]
  }


  for (i in items[-(0:length(queues))]) {
    last_items <- sapply(queue_order, function(x) length(x))
    last_item_vals <- sapply(queue_order, function(x) sum(x) * min_max)
    next_queue <- which(last_item_vals %in% min(last_item_vals))[1]
    queue_order[[next_queue]][as.character(i)] <- i
  }
  return(queue_order)
}


#' @title Check if node connection exists
#' @description
#' Checks for connection between two nodes.
#'
#' @details
#' Compares row, column information of two nodes to determine connection.
#'
#' @param pair1 the c(row, column) of a matrix cell.
#' @param pair2 the c(row, column) of a matrix cell.
#'
#' @returns a Boolean value.
#'
#' @export
#' @examples
#' node_connection_exists(c(1, 3), c(3, 1))
#'
node_connection_exists <- function(pair1, pair2) {
  if (pair1[2] == pair2[1] | pair2[2] == pair1[1]) {
    return(T)
  } else {
    return(F)
  }
}


#' @title Generate feasible paths through nodes
#' @description
#' Returns matrix of connected edges.
#'
#' @details
#' Pending.
#'
#' @param pairs a list of pairs.
#'
#' @export
#' @returns a matrix.
#'
node_paths <- function(pairs) {
  labs <- lapply(pairs, function(x) paste0(x[1], "--", x[2]))

  connections <- sapply(pairs, function(x) sapply(pairs, function(y) node_connection_exists(x, y)))
  connect_mat <- matrix(connections, ncol = length(labs), nrow = length(labs), dimnames = list(labs, labs))

  return(connect_mat)
}

#' @title Flip values in binary list
#' @description
#' Generate list of positions to flip.
#'
#' @details
#' Pending.
#'
#' @param sol_length the length of a solution list.
#' @param neighbors the number of positions to flip in the solution list.
#'
#' @returns a list of position pairs to flip.
#' @importFrom utils combn
#' @export
#' @examples
#' flip_positions(5, 2)
flip_positions <- function(sol_length, neighbors) {
  # generate list of positions to flip
  f <- asplit(matrix(combn(1:sol_length, neighbors), nrow = neighbors), 2)

  return(f)
}

#' @title Permute values in list
#' @description
#' Generate list of positions to permute.
#'
#' @details
#' Pending.
#'
#' @param sol_length the length of a solution list.
#' @param neighbors the number of positions to flip in the solution list.
#'
#' @returns a list of position pairs to flip.
#'
#' @export
#' @examples
#' permute_positions(5, 2)
permute_positions <- function(sol_length, neighbors) {
  neighbors <- neighbors + 1
  # generate list of permutated positions
  f <- asplit(matrix(combn(1:sol_length, neighbors), nrow = neighbors), 2)

  return(f)
}


#' @title Exchange values in a list
#' @description
#' Swap values in solution list.
#'
#' @details
#' Pending.
#'
#'
#' @param sol_list a solution list.
#' @param one the first position in the solution list that will be exchanged.
#' @param two the second position in the solution list that will be exchanged.
#'
#' @export
#' @returns a solution list.
#'
exchange_values <- function(sol_list, one, two) {
  new <- sol_list
  new[c(one, two)] <- new[c(two, one)]

  return(new)
}
