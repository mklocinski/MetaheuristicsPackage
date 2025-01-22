#' @title Local Search
#' @description
#' Executes the local search algorithm and returns the results of the best round.
#'
#' @details
#' Pending.
#'
#' @param obj_func The objective function.
#' @param problem_params A list of all parameters required to run the objective function.
#' @param sol_represent A function that outputs the required solution representation.
#' @param eval_func A function that evaluates the algorithm's output.
#' @param stop_func A function that evaluates the heuristic's metadata to determine if the search should end.
#' @param initial_solution A string representing the initial solution.
#' @param neighbors A number representing the number of neighbors to iterate through.
#' @param stop_crit A number used by the stop_func function to determine if the search should stop.
#'
#' @returns a list of heuristic output including time to execute the round, round ID, the current
#' round's solution, the current round's function output, the previous round's function output, and
#' the improvement over the last round.
#' @export

local_search <- function(obj_func, problem_params, sol_represent,
                         eval_func, stop_func,
                         initial_solution, neighbors, stop_crit) {
  continue <- T

  last_sol <- ""
  last_val <- 0

  curr_sol <- initial_solution
  curr_val <- 0

  # Stopping criteria information
  start_time <- Sys.time()
  round <- 1
  improvement <- 1

  while (continue == T) {
    # Last Round:
    # - Best solution
    last_sol <- curr_sol
    # - Best value
    last_val <- curr_val

    # New Round:
    # - generation
    xs <- sol_represent(curr_sol, neighbors)
    # - objective function output list
    ys <- sapply(xs$x_values, function(x) obj_func(c(x, problem_params)))
    # - best objective function output
    best <- eval_func(xs, ys)

    # Current:
    # - clean representation of solution
    curr_sol <- best$clean
    # - solution value
    curr_val <- best$value

    # Stopping Criteria:
    round <- round + 1
    improvement <- ifelse(last_val == 0, 1, (curr_val - last_val) / last_val)
    scs <- list(start_time, round, curr_val, last_val)

    # Should algorithm stop?
    continue <- stop_func(scs, stop_crit)
  }

  return(list(
    execution_time = Sys.time() - start_time,
    rounds = round,
    current_solution = curr_sol,
    current_value = curr_val,
    previous_value = last_val,
    improvement = ifelse(last_val == 0, 1, ((curr_val - last_val) / last_val))
  ))
}


#' @title Local Search (Storage)
#' @description
#' Executes the local search algorithm and returns the results of all rounds.
#'
#' @details
#' Pending.
#'
#' @param obj_func The objective function.
#' @param problem_params A list of all parameters required to run the objective function.
#' @param sol_represent A function that outputs the required solution representation.
#' @param eval_func A function that evaluates the algorithm's output.
#' @param stop_func A function that evaluates the heuristic's metadata to determine if the search should end.
#' @param initial_solution A string representing the initial solution.
#' @param neighbors A number representing the number of neighbors to iterate through.
#' @param stop_crit A number used by the stop_func function to determine if the search should stop.
#'
#' @returns a table of heuristic output including time to execute each round, round ID, each
#' round's solution, each round's function output, each previous round's function output, and
#' the improvement between each round.
#' @export
local_search_storage <- function(obj_func, problem_params,
                                 sol_represent, eval_func, stop_func,
                                 initial_solution, neighbors, stop_crit) {
  continue <- T

  last_sol <- ""
  last_val <- 0

  curr_sol <- initial_solution
  curr_val <- 0

  # Stopping criteria information
  start_time <- Sys.time()
  round <- 0
  improvement <- 1

  # Set up memory storage:
  storage <- list()

  while (continue == T) {
    # Last Round:
    # - Best solution
    last_sol <- curr_sol
    # - Best value
    last_val <- curr_val

    # New Round:
    # - generation
    xs <- sol_represent(curr_sol, neighbors)
    # - objective function output list
    ys <- sapply(xs$x_values, function(x) obj_func(c(x, problem_params)))
    # - best objective function output
    best <- eval_func(xs, ys)

    # Current:
    # - clean representation of solution
    curr_sol <- best$clean
    # - solution value
    curr_val <- best$value

    # Stopping Criteria:
    round <- round + 1
    improvement <- ifelse(last_val == 0, 1, (curr_val - last_val) / last_val)
    scs <- list(start_time, round, curr_val, last_val)

    # Should algorithm stop?
    continue <- stop_func(scs, stop_crit)

    # Update storage:
    storage[[length(storage) + 1]] <- list(
      execution_time = Sys.time() - start_time,
      rounds = round,
      current_solution = curr_sol,
      current_value = curr_val,
      previous_value = last_val,
      improvement = ifelse(last_val == 0, 1, ((curr_val - last_val) / last_val))
    )
  }

  storage <- do.call("rbind", storage)

  return(storage)
}


#' @title Tabu Search
#' @description
#' Executes the tabu search algorithm and returns the results of all rounds.
#'
#' @details
#' Pending.
#'
#' @param obj_func The objective function.
#' @param problem_params A list of all parameters required to run the objective function.
#' @param sol_represent A function that outputs the required solution representation.
#' @param eval_func A function that evaluates the algorithm's output.
#' @param stop_func A function that evaluates the heuristic's metadata to determine if the search should end.
#' @param initial_solution A string representing the initial solution.
#' @param neighbors A number representing the number of neighbors to iterate through.
#' @param stop_crit A number used by the stop_func function to determine if the search should stop.
#' @param memory_length A number specifying the length of the tabu list.

#' @returns a table of heuristic output including time to execute each round, round ID, the round's tabu list,
#' each round's solution, each round's function output, each previous round's function output, and
#' the improvement between each round.
#' @export
tabu_search <- function(obj_func, problem_params,
                        sol_represent, eval_func, stop_func,
                        initial_solution, neighbors, stop_crit,
                        memory_length) {
  continue <- T

  # Initialize solution information
  last_sol <- ""
  last_val <- 0

  curr_sol <- initial_solution
  curr_val <- 0

  ilgl_move <- 0

  # Set stopping criteria information
  start_time <- Sys.time()
  round <- 1
  improvement <- 1

  # Search:
  while (continue == T) {
    # Last Round:
    # - Best solution
    last_sol <- curr_sol
    # - Best value
    last_val <- curr_val

    # New Round:
    # - generation
    xs <- sol_represent(curr_sol, neighbors, tabu)
    # - objective function output list
    ys <- sapply(xs$x_values, function(x) obj_func(c(x, problem_params)))
    # - best objective function output
    best <- eval_func(xs, ys)

    # Current:
    # - string representation of solution
    curr_sol <- best$clean
    # - solution value
    curr_val <- best$value
    # - illegal move
    ilgl_move <- best$position

    # Stopping Criteria:
    round <- round + 1
    improvement <- ifelse(last_val == 0, 1, (curr_val - last_val) / last_val)
    scs <- list(start_time, round, curr_val, last_val)

    # Should algorithm stop?
    continue <- stop_func(scs, stop_crit)

    # Update tabu list
    tabu[[length(tabu) + 1]] <- ilgl_move
    tabu <- adaptive_memory(tabu, memory_length)
  }

  return(list(
    execution_time = Sys.time() - start_time,
    rounds = round,
    tabu = tabu,
    current_solution = curr_sol,
    current_value = curr_val,
    previous_value = last_val,
    improvement = ifelse(last_val == 0, 1, ((curr_val - last_val) / last_val))
  ))
}

#' @title Tabu Search (Storage)
#' @description
#' Executes the tabu search algorithm and returns the results of all rounds.
#'
#' @details
#' Pending.
#'
#' @param obj_func The objective function.
#' @param problem_params A list of all parameters required to run the objective function.
#' @param sol_represent A function that outputs the required solution representation.
#' @param eval_func A function that evaluates the algorithm's output.
#' @param stop_func A function that evaluates the heuristic's metadata to determine if the search should end.
#' @param initial_solution A string representing the initial solution.
#' @param neighbors A number representing the number of neighbors to iterate through.
#' @param stop_crit A number used by the stop_func function to determine if the search should stop.
#' @param memory_length A number specifying the length of the tabu list.

#' @returns a table of heuristic output including time to execute each round, round ID, the round's
#' tabu list each round's solution, each round's function output, each previous round's function output, and
#' the improvement between each round.
#' @export
tabu_search_storage <- function(obj_func, problem_params,
                                sol_represent, eval_func, stop_func,
                                initial_solution, neighbors, stop_crit,
                                memory_length) {
  continue <- T

  # Initialize solution information
  last_sol <- ""
  last_val <- 0

  curr_sol <- initial_solution
  curr_val <- 0

  tabu <- list()
  ilgl_move <- 0

  # Set stopping criteria information
  start_time <- Sys.time()
  round <- 0
  improvement <- 1

  # Set up memory storage:
  storage <- list()

  # Search:
  while (continue == T) {
    # Last Round:
    # - Best solution
    last_sol <- curr_sol
    # - Best value
    last_val <- curr_val

    # New Round:
    # - generation
    xs <- sol_represent(curr_sol, neighbors, tabu)
    # - objective function output list
    ys <- sapply(xs$x_values, function(x) obj_func(c(x, problem_params)))
    # - best objective function output
    best <- eval_func(xs, ys)

    # Current:
    # - Clean representation of solution
    curr_sol <- best$clean
    # - solution value
    curr_val <- best$value
    # - illegal move
    ilgl_move <- xs$positions[best$position]

    # Stopping Criteria:
    round <- round + 1
    improvement <- ifelse(last_val == 0, 1, (curr_val - last_val) / last_val)
    scs <- list(start_time, round, improvement, last_val)

    # Should algorithm stop?
    continue <- stop_func(scs, stop_crit)

    # Update tabu list
    tabu[[length(tabu) + 1]] <- ilgl_move
    tabu <- adaptive_memory(tabu, memory_length)
    tabu_list <- paste(unlist(tabu), collapse = ", ")

    # Update storage
    storage[[length(storage) + 1]] <- list(
      execution_time = Sys.time() - start_time,
      rounds = round,
      tabu = tabu_list,
      current_solution = curr_sol,
      current_value = curr_val,
      previous_value = last_val,
      improvement = ifelse(last_val == 0, 1, ((curr_val - last_val) / last_val))
    )
  }

  storage <- do.call("rbind", storage)
  return(storage)
}


# --------------------------------- Simulated Annealing -------------------------------- #




# --------------------------------------- GRASP ---------------------------------------- #



# -------------------------------- Genetic Algorithm ----------------------------------- #
#' @title Genetic Algorithm
#' @description
#' Executes the genetic algorithm.
#'
#' @details
#' Pending.
#'
#' @export
genetic_algorithm <- function(genes, crossover_probability, crossover_points,
                              mutation_rate, elite_parents = 0,
                              initial_gen,
                              fitness_function,
                              fitness_function_params = NULL,
                              selection_function,
                              crossover_function,
                              mutation_function,
                              stop_func, stop_crit,
                              permutation = F) {
  is_binary <- ifelse(length(setdiff(unique(genes), c(0, 1))) > 0, "not_binary", "binary")
  pop_size <- nrow(initial_gen)

  # Evaluate fitness of first generation
  initial_gen$fitness <- apply(initial_gen, 1, function(x) fitness_function(x, params = fitness_function_params))
  initial_gen <- initial_gen[order(initial_gen$fitness, decreasing = T), ]

  # Set up variables
  continue <- T

  last_sol <- ""
  last_val <- 0
  last_avg_fitness <- 0

  curr_gen <- initial_gen
  curr_sol <- initial_gen[initial_gen$fitness == max(initial_gen$fitness), ][1, ]
  curr_val <- initial_gen[initial_gen$fitness == max(initial_gen$fitness), "fitness"][1]
  curr_avg_fitness <- mean(initial_gen$fitness)

  # Stopping criteria information
  start_time <- Sys.time()
  round <- 1
  improvement <- 1

  # set up storage
  storage <- list()
  # dfs <- list()
  while (continue == T) {
    # Last Generation:
    last_gen <- curr_gen
    # - Best solution
    last_sol <- curr_sol
    # - Best value
    last_val <- curr_val
    # - Overall Fitness
    last_avg_fitness <- curr_avg_fitness

    # Select crossover-eligible population
    next_gen <- last_gen[1:ceiling(nrow(last_gen) * crossover_probability), ]

    # Create parents
    parents <- do.call("rbind", lapply(1:(pop_size / 2), function(x) selection_function(next_gen)))

    # Conduct crossovers
    crossed <- do.call("rbind", crossover_function(parents, crossover_points, permutation))

    # Mutate
    curr_gen <- mutation_function(crossed, mutation_rate, genes, permutation)

    # Evaluate new generation's fitness
    curr_gen$fitness <- apply(curr_gen, 1, function(x) fitness_function(x, params = fitness_function_params))

    # Current:
    # - clean representation of solution
    curr_sol <- paste(curr_gen[curr_gen$fitness == max(curr_gen$fitness), 1:(ncol(curr_gen) - 1)][1, ], collapse = "")
    # - solution value
    curr_val <- curr_gen[curr_gen$fitness == max(curr_gen$fitness), "fitness"][1]
    # - Overall Fitness
    curr_avg_fitness <- mean(curr_gen$fitness)

    # Stopping Criteria:
    round <- round + 1
    improvement <- ifelse(last_val == 0, 1, (curr_val - last_val) / last_val)
    scs <- list(start_time, round, curr_val, last_val)

    # Should algorithm stop?
    continue <- stop_func(scs, stop_crit)


    # Update storage:
    storage[[length(storage) + 1]] <- list(
      execution_time = Sys.time() - start_time,
      rounds = round,
      current_solution = curr_sol,
      current_value = curr_val,
      current_overall_fitness = curr_avg_fitness,
      previous_value = last_val,
      previous_overall_fitness = last_avg_fitness,
      improvement = ifelse(last_val == 0, 1, ((curr_val - last_val) / last_val))
    )
  }

  storage <- do.call("rbind", storage)


  return(storage)
}
