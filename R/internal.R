#' Stop Function Execution Without Displaying the Call
#'
#' @inheritParams base::stop
#' @noRd
stop_ <- function(...) {
  stop(..., call. = FALSE)
}

#' Display a Warning Message Displaying the Call
#'
#' @inheritParams base::warning
#' @noRd
warning_ <- function(...) {
  warning(..., call. = FALSE)
}

#' Create a Sequence from 2 to `n` If `n` Is At Least 2
#'
#' @param n An `integer` specifying the last value of the sequence.
#' @noRd
seq_min2 <- function(n) {
  if (n < 2L) {
    integer(0L)
  } else {
    seq.int(2L, n)
  }
}

#' Convert a Vector of Unique Integers to a Set
#'
#' @param vec An `integer` vector.
#' @param n An `integer`, the number of unique elements.
#' @noRd
to_dec <- function(vec, n) {
  sum(2L^seq.int(0L, n - 1L)[vec])
}

#' Convert a Set of Integers to a Vector of Unique Integers
#'
#' @param dec An `integer`, representing a set of integers.
#' @param n An `integer`, the number of unique elements.
#' @noRd
to_vec <- function(dec, n) {
  b <- integer(n)
  for (i in seq_len(n)) {
    b[n - i + 1L] <- (dec %% 2L) * 1L
    dec <- (dec %/% 2L)
  }
  rev(b)
}

#' Create a Comma-separated Character String
#'
#' @param x A `character` vector.
#' @noRd
cs <- function(x) {
  paste0(x, collapse = ", ")
}

#' Check that a Directed Graph is Acyclic via Topological Sort
#'
#' @param lhs A `character` vector giving the left-hand side
#'   variables of the edges.
#' @param rhs A `character` vector giving the right-hand side
#'   variables of the edges.
#' @noRd
is_acyclic <- function(lhs, rhs) {
  s <- setdiff(lhs, rhs)
  while (length(s) > 0L) {
    v <- s[1L]
    s <- s[-1L]
    e <- which(lhs == v)
    e_rhs <- rhs[e]
    for (i in seq_along(e)) {
      w <- e_rhs[i]
      lhs[e[i]] <- ""
      rhs[e[i]] <- ""
      if (!w %in% rhs) {
        s <- c(s, w)
      }
    }
  }
  lhs <- lhs[nzchar(lhs)]
  if (identical(length(lhs), 0L)) {
    TRUE
  } else {
    FALSE
  }
}

#' Wrapper for `requireNamespace` for Mocking
#'
#' @inheritParams requireNamespace
#' @noRd
require_namespace <- function(package, ..., quietly = FALSE) {
  requireNamespace(package, ..., quietly = quietly)
}

#' Add New Variables to a `dovalidate` Call
#'
#' @param args A `list` of arguments to `initialize_dovalidate`.
#' @param new_vars A `character` vector of variable names to add.
#' @noRd
add_new_vars <- function(args, new_vars) {
  if (length(new_vars) > 0L) {
    args$n <- args$n + length(new_vars)
    args$vars <- c(args$vars, new_vars)
    args$nums <- seq_len(args$n)
    names(args$vars) <- args$nums
    names(args$nums) <- args$vars
  }
  args
}

#' Check Validity of Data Generating Mechanisms
#'
#' @param spec_name A `character` vector of length one naming the mechanism.
#' @param spec `transportability`, `selection_bias` or `missing_data` argument
#'   of `dovalidate`.
#' @noRd
validate_special <- function(spec_name, spec) {
  if (!is.null(spec) && (!is.character(spec) || length(spec) > 1L)) {
    stop_("Argument `", spec_name, "` must be a character vector of length 1.")
  }
}

#' Parse Input Distributions for Internal Processing
#'
#' @inheritParams dovalidate
#' @noRd
parse_data <- function(data) {
  if (is.character(data)) {
    if (length(data) > 1L) {
      stop_("Argument `data` must be of length 1 when of type `character`.")
    }
    data
  } else if (is.list(data)) {
    paste0(vapply(data, parse_distribution, character(1L)), collapse = "\n")
  } else if (is.numeric(data)) {
    parse_distribution(data)
  } else {
    stop_("Argument `data` is of an unsupported type.")
  }
}

#' Parse the Target Distribution for Internal Processing
#'
#' @inheritParams dovalidate
#' @noRd
parse_query <- function(query) {
  if (is.character(query)) {
    if (length(query) > 1L) {
      stop_("Argument `query` must be of length 1 when of type `character`.")
    }
    query
  } else if (is.numeric(query)) {
    parse_distribution(query)
  } else {
    stop_("Argument `query` is of an unsupported type.")
  }
}

#' Parse A Single Distribution for Internal Processing
#'
#' @param d A `numeric` or a `character` vector.
#' @noRd
parse_distribution <- function(d) {
  if (is.character(d)) {
    d
  } else if (is.numeric(d)) {
    v <- names(d)
    if (is.null(v)) {
      stop_(
        "Invalid distribution format ", deparse1(d), ": ",
        "role values must be given as a named vector."
      )
    }
    if (any(is.na(d) | !is.finite(d))) {
      stop_(
        "Invalid distribution format ", deparse1(d), ": ",
        "all role values must be non-missing and finite."
      )
    }
    if (any(!d %in% 0:2)) {
      stop_(
        "Invalid variable roles in distribution format ", deparse1(d), ": ",
        "all role values must be either 0, 1 or 2."
      )
    }
    if (all(d > 0)) {
      stop_(
        "Invalid variable roles in distribution format ", deparse1(d), ": ",
        "at least one variable must have role value 0."
      )
    }
    d <- as.integer(d)
    a_set <- v[which(d == 0L)]
    b_set <- v[which(d == 1L)]
    c_set <- v[which(d == 2L)]
    a_val_set <- rep("", length(a_set))
    b_val_set <- rep("", length(b_set))
    c_val_set <- rep("", length(c_set))
    a_str <- paste(a_set, a_val_set, sep = "", collapse = ",")
    b_str <- paste(b_set, b_val_set, sep = "", collapse = ",")
    c_str <- paste(c_set, c_val_set, sep = "", collapse = ",")
    n_b <- nchar(b_str)
    n_c <- nchar(c_str)
    paste(
      "p(",
      a_str,
      ifelse(n_b > 0 | n_c > 0, "|", ""),
      ifelse(n_b > 0, "do(", ""),
      b_str,
      ifelse(n_b > 0, ")", ""),
      ifelse(n_b > 0 & n_c > 0, ",", ""),
      c_str,
      ")",
      sep = ""
    )
  } else {
    stop_("Unable to parse distribution format ", deparse1(d), ".")
  }
}

#' Parse the Graph for Internal Processing
#'
#' @inheritParams dovalidate
#' @noRd
parse_graph <- function(graph) {
  if (inherits(graph, "igraph")) {
    if (require_namespace("igraph", quietly = TRUE)) {
      e <- igraph::E(graph)
      v <- igraph::vertex_attr(graph, "name")
      if (is.null(v)) {
        n <- length(igraph::V(graph))
        pow <- ceiling(log(n) / log(26))
        d <- apply(
          expand.grid(rep(list(letters), pow)),
          1,
          paste0,
          collapse = ""
        )
        v <- d[seq_len(n)]
        message(
          "Argument `graph` is not named, node names have been assigned: ",
          cs(v)
        )
      }
      g_obs <- ""
      g_unobs <- ""
      e_attr <- igraph::edge_attr(graph, "description", index = e)
      obs_edges <- e
      unobs_edges <- NULL
      if (!is.null(e_attr)) {
        obs_edges <- e[is.na(e_attr) | e_attr != "U"]
        unobs_edges <- e[!is.na(e_attr) & e_attr == "U"]
      }
      if (length(obs_edges) > 0L) {
        obs_ind <- igraph::get.edges(graph, obs_edges)
        g_obs <- paste(
          v[obs_ind[, 1L]],
          "->",
          v[obs_ind[, 2L]],
          collapse = "\n"
        )
      }
      if (length(unobs_edges) > 0L) {
        unobs_ind <- igraph::get.edges(graph, unobs_edges)
        unobs_ind <- unobs_ind[
          unobs_ind[, 1L] < unobs_ind[, 2L], ,
          drop = FALSE
        ]
        g_unobs <- paste(
          v[unobs_ind[, 1L]],
          "<->",
          v[unobs_ind[, 2L]],
          collapse = "\n"
        )
      }
      paste0(c(g_obs, g_unobs), collapse = "\n")
    } else {
      stop_("The `igraph` package is not available.")
    }
  } else if (inherits(graph, "dagitty")) {
    if (require_namespace("dagitty", quietly = TRUE)) {
      if (!identical(dagitty::graphType(graph), "dag")) {
        stop_("Attempting to use `dagitty`, but the graph type is not `dag`.")
      }
      e <- dagitty::edges(graph)
      paste(e[, 1L], e[, 3L], e[, 2L], collapse = "\n")
    } else {
      stop_("The `dagitty` package is not available.")
    }
  } else if (is.character(graph)) {
    if (length(graph) > 1L) {
      stop_("Argument `graph` must be of length 1 when of `character` type.")
    }
    graph
  } else {
    stop_("Argument `graph` is of an unsupported type.")
  }
}

#' Check True Graph Size
#'
#' @param args n The number of nodes.
#' @noRd
check_graph_size <- function(n) {
  if (n > 30) {
    stop_("The inputs imply a graph with more than 30 nodes.")
  }
}

#' Set Default Values for Control Arguments
#'
#' @inheritParams dovalidate
#' @noRd
control_defaults <- function(control) {
  rules <- as.integer(control$rules)
  con_vars <- as.character(control$con_vars)
  control$rules <- integer(0L)
  control$con_vars <- character(0L)
  control_lengths <- lengths(control)
  if (any(control_lengths > 1L)) {
    stop(
      "All elements of argument `control` ",
      "must be of length 1 (except `rules` and `con_vars`).\n",
      "The following elements have length > 1: ",
      cs(names(control)[control_lengths > 1L])
    )
  }
  control$rules <- rules
  control$con_vars <- con_vars
  default <- list(
    benchmark = FALSE,
    benchmark_rules = FALSE,
    cache = TRUE,
    draw_all = FALSE,
    draw_derivation = FALSE,
    empty = FALSE,
    formula = TRUE,
    heuristic = FALSE,
    improve = TRUE,
    md_sym = "1",
    rules = integer(0L),
    time_limit = -1.0,
    verbose = FALSE,
    warn = TRUE,
    con_vars = character(0L),
    validate_run = FALSE
  )
  default_names <- names(default)
  control_names <- names(control)
  if (any(!control_names %in% default_names)) {
    stop_(
      "Unknown control arguments: ",
      control_names[!control_names %in% default_names]
    )
  }
  given_args <- match(control_names, default_names)
  control_full <- default
  control_full[given_args] <- control
  control <- control_full
  default_types <- vapply(default, typeof, character(1L))
  control_types <- vapply(control, typeof, character(1L))
  invalid_types <- default_types != control_types
  if (any(invalid_types)) {
    stop_(
      "Some elements of argument `control` have an invalid type.\n",
      "Invalid arguments: ",
      paste0(names(control)[invalid_types], collapse = ", "),
      "\nProvided types: ",
      paste0(control_types[invalid_types], collapse = ", "),
      "\nExpected types: ",
      paste0(default_types[invalid_types], collapse = ", ")
    )
  }
  control
}

#' Construct an Empty `dovalidate` Object
#'
#' @param cl A `list` containing the original `dovalidate` call arguments.
#' @noRd
empty_output <- function(cl) {
  structure(
    list(
      identifiable = FALSE,
      formula = "",
      call = cl
    ),
    class = "dovalidate"
  )
}

#' Is the Argument a `dovalidate` Object?
#'
#' @param x An \R object.
#' @noRd
is_dovalidate <- function(x) {
  inherits(x, "dovalidate")
}

.onUnload <- function(libpath) {
  library.dynam.unload("dovalidate", libpath)
}

# Parses distributions from latex formula
parse_distributions <- function(latex_formula) {
  distributions <- regmatches(latex_formula, gregexpr("[p][^)]*\\)", latex_formula))[[1]]
  distributions <- paste(distributions, collapse = "\n")
  print("internal")
  distributions
}

# Orders variables in identification formula based on reference formula.
reorder_formula_like <- function(ref, target) {
  
  trim <- function(x) gsub("^\\s+|\\s+$", "", x)
  no_ws <- function(x) gsub("\\s+", "", x)
  
  split_vars <- function(s) {
    s <- trim(s)
    if (identical(s, "") || is.na(s)) return(character(0))
    trim(strsplit(s, ",", fixed = TRUE)[[1]])
  }
  
  join_vars <- function(x) paste(x, collapse = ",")
  
  find_matching <- function(s, start, open, close) {
    chars <- strsplit(s, "", fixed = TRUE)[[1]]
    depth <- 0L
    for (i in seq.int(start, length(chars))) {
      if (chars[i] == open) depth <- depth + 1L
      if (chars[i] == close) {
        depth <- depth - 1L
        if (depth == 0L) return(i)
      }
    }
    stop("Sulkevaa merkkiä ei löytynyt.")
  }
  
  parse_expr <- function(s, pos = 1L, stop_char = NULL) {
    factors <- list()
    n <- nchar(s)
    
    while (pos <= n) {
      ch <- substr(s, pos, pos)
      
      if (!is.null(stop_char) && ch == stop_char) break
      
      if (substr(s, pos, pos + 5L) == "\\sum_{") {
        start_brace <- pos + 5L
        end_brace <- find_matching(s, start_brace, "{", "}")
        vars_txt <- substr(s, start_brace + 1L, end_brace - 1L)
        vars <- split_vars(vars_txt)
        
        if (substr(s, end_brace + 1L, end_brace + 1L) != "[") {
          stop("Odotettiin '[' summan jälkeen.")
        }
        start_bracket <- end_brace + 1L
        end_bracket <- find_matching(s, start_bracket, "[", "]")
        inner_txt <- substr(s, start_bracket + 1L, end_bracket - 1L)
        inner <- parse_expr(inner_txt, 1L, NULL)$node
        
        factors[[length(factors) + 1L]] <- list(
          type = "sum",
          vars = vars,
          expr = inner
        )
        pos <- end_bracket + 1L
        
      } else if (substr(s, pos, pos + 1L) == "p(") {
        start_par <- pos + 1L
        end_par <- find_matching(s, start_par, "(", ")")
        inside <- substr(s, start_par + 1L, end_par - 1L)
        
        pipe_pos <- regexpr("|", inside, fixed = TRUE)[1]
        if (pipe_pos < 0) {
          lhs <- split_vars(inside)
          rhs <- character(0)
        } else {
          lhs <- split_vars(substr(inside, 1L, pipe_pos - 1L))
          rhs <- split_vars(substr(inside, pipe_pos + 1L, nchar(inside)))
        }
        
        factors[[length(factors) + 1L]] <- list(
          type = "dist",
          lhs = lhs,
          rhs = rhs
        )
        pos <- end_par + 1L
        
      } else {
        stop(sprintf("Tuntematon rakenne kohdassa %d: '%s'", pos, substr(s, pos, pos)))
      }
    }
    
    node <- if (length(factors) == 1L) factors[[1L]] else list(type = "prod", factors = factors)
    list(node = node, pos = pos)
  }
  
  node_signature <- function(node) {
    if (node$type == "dist") {
      paste0(
        "p(",
        paste(sort(node$lhs), collapse = ","),
        "|",
        paste(sort(node$rhs), collapse = ","),
        ")"
      )
    } else if (node$type == "sum") {
      paste0(
        "sum{",
        paste(sort(node$vars), collapse = ","),
        "}[",
        node_signature(node$expr),
        "]"
      )
    } else if (node$type == "prod") {
      sigs <- vapply(node$factors, node_signature, character(1))
      paste0("prod[", paste(sort(sigs), collapse = ";"), "]")
    } else {
      stop("Tuntematon node-tyyppi.")
    }
  }
  
  reorder_by_ref <- function(ref_vars, target_vars) {
    idx <- match(target_vars, ref_vars)
    if (any(is.na(idx))) {
      stop("Targetissa on muuttujia, joita ei löytynyt referenssistä.")
    }
    target_vars[order(idx)]
  }
  
  align_node <- function(ref_node, target_node) {
    if (ref_node$type != target_node$type) {
      stop("Rakenteet eivät vastaa toisiaan.")
    }
    
    if (ref_node$type == "dist") {
      list(
        type = "dist",
        lhs = reorder_by_ref(ref_node$lhs, target_node$lhs),
        rhs = reorder_by_ref(ref_node$rhs, target_node$rhs)
      )
      
    } else if (ref_node$type == "sum") {
      list(
        type = "sum",
        vars = reorder_by_ref(ref_node$vars, target_node$vars),
        expr = align_node(ref_node$expr, target_node$expr)
      )
      
    } else if (ref_node$type == "prod") {
      ref_factors <- ref_node$factors
      tgt_factors <- target_node$factors
      
      used <- rep(FALSE, length(tgt_factors))
      out <- vector("list", length(ref_factors))
      
      ref_sigs <- vapply(ref_factors, node_signature, character(1))
      tgt_sigs <- vapply(tgt_factors, node_signature, character(1))
      
      for (i in seq_along(ref_factors)) {
        hits <- which(!used & tgt_sigs == ref_sigs[i])
        if (length(hits) == 0L) {
          stop("Sopivaa tekijää ei löytynyt targetista.")
        }
        j <- hits[1L]
        used[j] <- TRUE
        out[[i]] <- align_node(ref_factors[[i]], tgt_factors[[j]])
      }
      
      if (length(out) == 1L) out[[1L]] else list(type = "prod", factors = out)
      
    } else {
      stop("Tuntematon node-tyyppi.")
    }
  }
  
  render_node <- function(node) {
    if (node$type == "dist") {
      if (length(node$rhs) == 0L) {
        paste0("p(", join_vars(node$lhs), ")")
      } else {
        paste0("p(", join_vars(node$lhs), "|", join_vars(node$rhs), ")")
      }
    } else if (node$type == "sum") {
      paste0("\\sum_{", join_vars(node$vars), "}[", render_node(node$expr), "]")
    } else if (node$type == "prod") {
      paste(vapply(node$factors, render_node, character(1)), collapse = "")
    } else {
      stop("Tuntematon node-tyyppi.")
    }
  }
  
  ref_ast <- parse_expr(no_ws(ref))$node
  target_ast <- parse_expr(no_ws(target))$node
  
  aligned <- align_node(ref_ast, target_ast)
  render_node(aligned)
}
