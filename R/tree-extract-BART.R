#' Get posterior tree draws into tibble format from BART model
#'
#' Tibble grouped by iteration (`iter`) and tree id (`tree_id`). All information
#'   calculated by method is included in output.
#'
#' @param model BART model.
#' @param label_digits Rounding for labels.
#'
#' @return A tibble with columns to \describe{
#'   \item{iter}{Integer describing unique MCMC iteration.}
#'   \item{tree_id}{Integer. Unique tree id with each `iter`.}
#'   \item{node}{Integer describing node in tree. Unique to each `tree`-`iter`.}
#'   \item{parent}{Integer describing parent node in tree.}
#'   \item{label}{Label for the node.}
#'   \item{tier}{Position in tree hierarchy.}
#'   \item{var}{Variable for split.}
#'   \item{cut}{Numeric. Value of decision rule for `var`.}
#'   \item{is_leaf}{Logical. `TRUE` if leaf, `FALSE` if stem.}
#'   \item{leaf_value}{}
#'   \item{child_left}{Integer. Left child of node.}
#'   \item{child_right}{Integer. Right child of node.}
#'   }
#'
posterior_trees_BART <- function(model, label_digits = 2) {
  var_names <- names(model$treedraws$cutpoints)

  cut_points_tb <- purrr::map_df(
    .x = model$treedraws$cutpoints,
    .f = ~ dplyr::tibble(cut = ., cut_id = 1:length(.)),
    .id = "var"
  )

  out <- list()

  # first line contains mcmc draws
  fline <- strsplit(
    readr::read_lines(
      file = model$treedraws$trees,
      n_max = 1
    ),
    " "
  )[[1]]
  out$n_mcmc <- as.integer(fline[1])
  out$n_tree <- as.integer(fline[2])
  out$n_var <- as.integer(fline[3])

  out$trees <- suppressWarnings(
    readr::read_table(
      file = model$treedraws$trees,
      col_names = c("node", "var", "cut", "leaf"),
      col_types =
        readr::cols(
          node = readr::col_integer(),
          var = readr::col_integer(),
          cut = readr::col_integer(),
          leaf = readr::col_double()
        ),
      skip = 1,
      na = c(""),
      progress = F
    )
  )

  # indexing and tier
  out$trees <- dplyr::mutate(
    out$trees,
    tier = as.integer(floor(log2(.data$node))),
    cut_id = .data$cut + 1L, # R indexing at 1
    var = var_names[.data$var + 1L] # R indexing at 1
  )

  # define tree id and mcmc iteration number
  out$trees <- dplyr::mutate(
    out$trees,
    unique_tree_id = cumsum(is.na(.data$var) & is.na(.data$cut) & is.na(.data$leaf)),
    iter = (.data$unique_tree_id - 1L) %/% out$n_tree + 1L,
    tree_id = (.data$unique_tree_id - 1L) %% out$n_tree + 1L,
    unique_tree_id = NULL
  )

  # remove information about tree groups (was stored as missing lines)
  out$trees <- dplyr::filter(out$trees, stats::complete.cases(out$trees))

  # add cut information
  out$trees <- dplyr::left_join(
    dplyr::select(out$trees, -cut),
    cut_points_tb,
    by = c("var", "cut_id")
  )

  # add children information
  out$trees <- dplyr::group_by(out$trees, .data$iter, .data$tree_id)
  out$trees <- dplyr::mutate(
    out$trees,
    child_left = child_left(.data$node),
    child_right = child_right(.data$node)
  )

  # remove leaf info if no children
  out$trees <- dplyr::mutate(
    dplyr::ungroup(out$trees),
    is_leaf = is.na(child_left) & is.na(child_right),
    leaf_value = ifelse(.data$is_leaf, .data$leaf, NA_real_),
    cut = ifelse(.data$is_leaf, NA_real_, .data$cut), # is leaf, then no cut for stem
    var = ifelse(.data$is_leaf, NA_character_, .data$var), # is leaf, then no var for cut
    label = ifelse(
      .data$is_leaf,
      as.character(round(.data$leaf_value, digits = label_digits)),
      paste(.data$var, "<", round(.data$cut, digits = label_digits))
    ),
    parent = parent(.data$node)
  )

  # regroup
  out$trees <- dplyr::select(
    dplyr::group_by(out$trees, .data$iter, .data$tree_id),
    .data$iter,
    .data$tree_id,
    .data$node,
    .data$parent,
    .data$label,
    .data$tier,
    .data$var,
    .data$cut,
    .data$is_leaf,
    .data$leaf_value,
    .data$child_left,
    .data$child_right
  )

  return(out)
}

child_left <- function(nodes) {

  # must be grouped by iter and tree to apply
  pot_child <- nodes * 2L
  pot_child[!pot_child %in% nodes] <- NA_integer_

  return(pot_child)
}

child_right <- function(nodes) {

  # must be grouped by iter and tree to apply
  pot_child <- nodes * 2L + 1L
  pot_child[!pot_child %in% nodes] <- NA_integer_

  return(pot_child)
}

parent <- function(nodes) {
  parents <- nodes %/% 2L
  parents[parents == 0L] <- NA_integer_

  return(parents)
}
