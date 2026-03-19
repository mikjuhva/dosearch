# Useful functions from the private repository related to the article
# Clustering and Structural Robustness in Causal Diagrams (Tikka et al., 2023)
# and R-package causaleffect (Tikka and Karvanen, 2017)
# https://github.com/santikka/causaleffect 

library(igraph)

children <- function(x, g, v = igraph::V(g)) {
  ch_ind <- unlist(igraph::neighborhood(g, order = 1, nodes = x, mode = "out"))
  v[ch_ind]$name
}

parents <- function(x, g, v = igraph::V(g)) {
  pa_ind <- unlist(igraph::neighborhood(g, order = 1, nodes = x, mode = "in"))
  v[pa_ind]$name
}

descendants <- function(x, g, v = igraph::V(g)) {
  de_ind <- unlist(igraph::neighborhood(g, order = length(v), nodes = x, mode = "out"))
  v[de_ind]$name
}

ancestors <- function(x, g, v = igraph::V(g)) {
  an_ind <- unlist(igraph::neighborhood(g, order = length(v), nodes = x, mode = "in"))
  v[an_ind]$name
}

neighbors_ <- function(x, g, v = igraph::V(g)) {
  ne_ind <- unlist(igraph::neighborhood(g, order = 1, nodes = x, mode = "all"))
  v[ne_ind]$name
}

connected <- function(x, g, v = igraph::V(g)) {
  co_ind <- unlist(igraph::neighborhood(g, order = length(v), nodes = x, mode = "all"))
  v[co_ind]$name
}

uu <- function(x) {
  if (length(x)) unique(unlist(x))
  else character(0)
}

edge_subgraph <- function(g, incoming, outgoing) {
  # Setting from and to to NULL to satisfy CRAN if we end up making a package
  # R thinks these are global bindings, but they are igraph-operators for edges
  .to <- .from <- NULL
  e <- igraph::E(g)
  e_inc <- e[.to(incoming)]
  e_out <- e[.from(outgoing)]
  igraph::subgraph.edges(g, e[setdiff(e, union(e_inc, e_out))], delete.vertices = FALSE)
}

# Convert an igraph graph using causaleffect syntax into a dag
# with explicit latent variables
to_dag <- function(g) {
  out <- g
  unobs_edges <- which(igraph::edge.attributes(g)$description == "U")
  if (length(unobs_edges)) {
    e <- igraph::get.edges(g, unobs_edges)
    e <- e[e[ ,1] > e[ ,2], , drop = FALSE]
    e_len <- nrow(e)
    new_nodes <- paste0("U[", 1:e_len, "]")
    g <- igraph::set.vertex.attribute(g, name = "description", value = "")
    g <- g + igraph::vertices(new_nodes, description = rep("U", e_len))
    v <- igraph::get.vertex.attribute(g, "name")
    g <- g + igraph::edges(c(rbind(new_nodes, v[e[ ,1]]), rbind(new_nodes, v[e[ ,2]])))
    obs_edges <- setdiff(igraph::E(g), igraph::E(g)[unobs_edges])
    out <- igraph::subgraph.edges(g, igraph::E(g)[obs_edges], delete.vertices = FALSE)
  }
  out
}

# Convert a dag with explicit latent variables into an 
# acyclic directed mixed graph with causaleffect igraph syntax 
to_admg <- function(g) {
  out <- g
  unobs_vars <- which(igraph::vertex.attributes(g)$description == "U")
  obs_vars <- setdiff(1:length(igraph::V(g)), unobs_vars)
  u <- length(unobs_vars)
  if (u) {
    e <- igraph::E(g)
    g_obs <- igraph::induced_subgraph(g, obs_vars)
    for (i in 1:u) {
      unobs_edges <- igraph::get.edges(g, e[.from(unobs_vars[i])])
      if (nrow(unobs_edges) == 2) {
        g_obs <- g_obs + igraph::edges(c(unobs_edges[1:2,2], unobs_edges[2:1,2]), description = "U")
      }
    }
    out <- g_obs
  }
  out
}

ancestors_unsrt <- function(node, G) {
  an.ind <- unique(unlist(igraph::neighborhood(G, order = igraph::vcount(G), nodes = node, mode = "in")))
  an <- igraph::V(G)[an.ind]$name
  return(an)
}

parents_unsrt <- function(node, G.obs) {
  pa.ind <- unique(unlist(igraph::neighborhood(G.obs, order = 1, nodes = node, mode = "in")))
  pa <- igraph::V(G.obs)[pa.ind]$name
  return(pa)
}

# Implements relevant path separation (rp-separation) for testing d-separation. For details, see:
#
# Relevant Path Separation: A Faster Method for Testing Independencies in Bayesian Networks
# Cory J. Butz, Andre E. dos Santos, Jhonatan S. Oliveira;
# Proceedings of the Eighth International Conference on Probabilistic Graphical Models,
# PMLR 52:74-85, 2016.
#
# Note that the roles of Y and Z have been reversed from the paper, meaning that
# we are testing whether X is separated from Y given Z in G.
dSep <- function(G, x, y, z = NULL) {
  an_z <- ancestors_unsrt(z, G)
  an_xyz <- ancestors_unsrt(union(union(x, y), z), G)
  n <- length(igraph::V(G))
  v <- igraph::V(G)$name
  direction <- NA
  traverse_up <- logical(n)
  visited_up <- logical(n)
  traverse_down <- logical(n)
  visited_down <- logical(n)
  names(traverse_up) <- v
  names(visited_up) <- v
  names(traverse_down) <- v
  names(visited_down) <- v
  traverse_up[x] <- TRUE
  visit <- FALSE
  el_name <- NULL
  while (any(traverse_up) || any(traverse_down)) {
    visit <- FALSE
    for (j in 1:n) {
      if (traverse_up[j]) {
        traverse_up[j] <- FALSE
        if (!visited_up[j]) {
          visit <- TRUE
          direction <- TRUE
          el_name <- v[j]
          break
        }
      }
      if (traverse_down[j]) {
        traverse_down[j] <- FALSE
        if (!visited_down[j]) {
          visit <- TRUE
          direction <- FALSE
          el_name <- v[j]
          break
        }
      }
    }
    if (visit) {
      if (el_name %in% y) return(FALSE)
      if (direction) {
        visited_up[el_name] <- TRUE
      } else {
        visited_down[el_name] <- TRUE
      }
      if (direction && !(el_name %in% z)) {
        visitable_parents <- intersect(setdiff(parents_unsrt(el_name, G), el_name), an_xyz)
        visitable_children <- intersect(setdiff(children_unsrt(el_name, G), el_name), an_xyz)
        traverse_up[visitable_parents] <- TRUE
        traverse_down[visitable_children] <- TRUE
      } else if (!direction) {
        if (!(el_name %in% z)) {
          visitable_children <- intersect(setdiff(children_unsrt(el_name, G), el_name), an_xyz)
          traverse_down[visitable_children] <- TRUE
        }
        if (el_name %in% an_z) {
          visitable_parents <- intersect(setdiff(parents_unsrt(el_name, G), el_name), an_xyz)
          traverse_up[visitable_parents] <- TRUE
        }
      }
    }
  }
  return(TRUE)
}

children_unsrt <- function(node, G) {
  ch.ind <- unique(unlist(igraph::neighborhood(G, order = 1, nodes = node, mode = "out")))
  ch <- igraph::V(G)[ch.ind]$name
  return(ch)
}

random_dag <- function(n) {
  
}

random_graph_with_path_x_to_y <- function(n) {
  prob_dir <- runif(1, 0.1, 0.6)
  prob_bi_dir <- runif(1, 0.1, 0.6)
  vars <- paste0("z", 1:n)
  var_indexes <- 1:n
  y_index <- sample(var_indexes, 1)
  vars[y_index] <- "y"
  x_index <- sample(var_indexes[-y_index], 1)
  vars[x_index] <- "x"
  
  # init the edges list
  edges <- matrix(character(0), nrow = 0, ncol = 2)
  
  # Add path from x to y
  path_length <- sample(0:(n - 2), 1)
  path_vars_indexes <- sample((1:n)[!(vars %in% c("x", "y"))], path_length)
  path_vars_indexes <- c(x_index, path_vars_indexes, y_index)
  for(i in 1:(path_length + 1)) {
    edges <- rbind(edges, cbind(vars[path_vars_indexes[i]], vars[path_vars_indexes[i + 1]]))
  }
  
  # Add directed edges
  for (i in var_indexes) {
    for(j in var_indexes[-i]) {
      u <- runif(1)
      if (u < prob_dir) {
        if(any(edges[,1] == vars[i] & edges[,2] == vars[j])) next
        edges <- rbind(edges, cbind(vars[i], vars[j]))
        temp_graph <- graph_from_edgelist(edges, directed = TRUE)
        if(!is_dag(temp_graph)) edges <- edges[-nrow(edges),]
      }
    }
  } 
  dir_edge_n <- nrow(edges)
  
  # Add bidirected edges
  for (i in head(var_indexes, -1)) {
    for(j in var_indexes[(i + 1):length(var_indexes)]) {
      u <- runif(1)
      if (u < prob_bi_dir) {
        edges <- rbind(edges, cbind(vars[i], vars[j]))
        edges <- rbind(edges, cbind(vars[j], vars[i]))
      }
    }
  }
  edges_rows_n <- nrow(edges)
  graph_igraph <- graph_from_edgelist(edges, directed = TRUE)
  if(edges_rows_n > dir_edge_n) graph_igraph <- set.edge.attribute(graph = graph_igraph, name = "description", index = (dir_edge_n+1):edges_rows_n, value = "U")
  
  vars_with_no_edge <- setdiff(vars, unique(c(edges)))
  graph_igraph <- add_vertices(graph_igraph, length(vars_with_no_edge), attr = list(name = vars_with_no_edge)) 
  graph_igraph
  plot(graph_igraph)
  return(graph_igraph)
} 


# Totally random dag generator
totally_random_graph <- function(n) {
  
  prob_dir <- runif(1, 0.1, 0.6)
  prob_bi_dir <- runif(1, 0.1, 0.6)
  vars <- paste0("z", 1:n)
  var_indexes <- 1:n
  y_index <- sample(var_indexes, 1)
  vars[y_index] <- "y"
  x_ind <- sample(var_indexes[-y_index], 1)
  vars[x_ind] <- "x"
  
  # Add directed edges
  edges <- matrix(character(0), nrow = 0, ncol = 2)
  for (i in var_indexes) {
    for(j in var_indexes[-i]) {
      u <- runif(1)
      if (u < prob_dir) {
        edges <- rbind(edges, cbind(vars[i], vars[j]))
        temp_graph <- graph_from_edgelist(edges, directed = TRUE)
        if(!is_dag(temp_graph)) edges <- edges[-nrow(edges),]
      }
    }
  } 
  dir_edge_n <- nrow(edges)
  
  # Add bidirected edges
  for (i in head(var_indexes, -1)) {
    for(j in var_indexes[(i + 1):length(var_indexes)]) {
      u <- runif(1)
      if (u < prob_bi_dir) {
        edges <- rbind(edges, cbind(vars[i], vars[j]))
        edges <- rbind(edges, cbind(vars[j], vars[i]))
      }
    }
  }
  edges_rows_n <- nrow(edges)
  graph_igraph <- graph_from_edgelist(edges, directed = TRUE)
  if(edges_rows_n > dir_edge_n) graph_igraph <- set.edge.attribute(graph = graph_igraph, name = "description", index = (dir_edge_n+1):edges_rows_n, value = "U")
  
  # graph_char <- ""
  # if (dir_edge_n > 0) {
  #   for (i in 1:dir_edge_n) {
  #     graph_char <- paste0(graph_char, edges[i,1], " -> ", edges[i,2], "\n ")
  #   }
  # }
  # 
  # 
  # if(edges_rows_n > dir_edge_n) {
  #   for (i in seq(dir_edge_n + 1, edges_rows_n, by = 2)) {
  #     graph_char <- paste0(graph_char, edges[i,1], " <-> ", edges[i,2], "\n ")
  #   }
  # }
  vars_with_no_edge <- setdiff(vars, unique(c(edges)))
  graph_igraph <- add_vertices(graph_igraph, length(vars_with_no_edge), attr = list(name = vars_with_no_edge)) 
  return(graph_igraph)
}



random_dag_with_vars <- function(vars, prob1 = 0.5, prob2 = 0.35) {
  n <- length(vars)
  x_ind <- which(vars == "x")
  # Initialize edge vectors (v = from, w = to)
  v <- c()
  w <- c()
  
  # Phase 1: Generate edges from x_ind to n with probability prob1
  # Working backwards from y (position n) down to x (position x_ind)
  for (i in (n - 1):x_ind) {
    j <- n
    while(j > i) {
      u <- runif(1)
      if (u < prob1) {
        # Add edge from vars[i] to vars[j]
        v <- c(v, vars[i])
        w <- c(w, vars[j])
      }
      j <- j - 1
    }
  } 
  
  # Create temporary graph to check if x is ancestor of y
  if (length(v) > 0) {
    temp_edges <- cbind(v, w)
    temp_dag <- graph_from_data_frame(data.frame(from = v, to = w), 
                                      directed = TRUE, 
                                      vertices = data.frame(name = vars))
  } else {
    # If no edges yet, create empty graph with all vertices
    temp_dag <- make_empty_graph(n = n, directed = TRUE)
    V(temp_dag)$name <- vars
  }
  
  # Ensure x is an ancestor of y (there's a causal path from x to y)
  if ("y" %in% V(temp_dag)$name && "x" %in% V(temp_dag)$name) {
    if (!"x" %in% ancestors("y", temp_dag)) {
      # If x is not an ancestor of y, add direct edge x -> y
      v <- c(v, "x")
      w <- c(w, "y")
    }
  } else {
    # If y or x doesn't exist in graph yet, add the edge x -> y
    v <- c(v, "x")
    w <- c(w, "y")
  }
  
  # Phase 2: Generate edges from position 1 to x_ind-1 with probability prob2
  # These are variables that come before x in topological order
  for (i in (x_ind - 1):1) {
    j <- n
    while(j > i) {
      u <- runif(1)
      if (u < prob2) {
        # Add edge from vars[i] to vars[j]
        v <- c(v, vars[i])
        w <- c(w, vars[j])
      }
      j <- j - 1
    }
  } 
  
  # Ensure all variables are connected to the graph
  # For any isolated vertex, connect it to the next vertex in order
  for (i in 1:n) {
    if ((!vars[i] %in% v) & (!vars[i] %in% w)) {
      v <- c(v, vars[i])
      w <- c(w, vars[i + 1])
    } 
  }
  
  # Create the DAG from collected edges
  edges <- cbind(v, w)
  dag <- graph_from_edgelist(edges, directed = TRUE)
  
  # Ensure all variables are ancestors of y (connected to outcome)
  y_anc <- ancestors("y", dag)
  for (i in 1:(n - 1)) {
    if (!vars[i] %in% y_anc) {
      # If vars[i] is not an ancestor of y, connect it to a random ancestor of y
      v <- c(v, vars[i])
      poss_w <- vars[(i + 1):n]  # Only connect to later variables (maintain topological order)
      poss_w <- poss_w[poss_w %in% y_anc]  # Only connect to variables that are ancestors of y
      w <- c(w, sample(poss_w, 1))
    }
  }
  
  # Rebuild final DAG with all edges
  edges <- cbind(v, w)
  dag <- graph_from_edgelist(edges, directed = TRUE)
  
  return(dag)
}

parse_distributions <- function(latex_formula) {
  distributions <- regmatches(latex_formula, gregexpr("[Pp][^)]*\\)", latex_formula))[[1]]
  distributions <- paste(distributions, collapse = "\n")
  print("utils")
  distributions
}

convert_formula_for_validation <- function(x) {
  if (!grepl("\\\\sum", x)) {
    return(x)
  }
  
  x <- gsub("\\}", "}[", x)
  x <- gsub("\\\\right\\)", "]", x)
  x <- gsub("\\\\left\\(", "", x)
  x <- paste0(x, "]")
  return(x)
}
