source("tests/causal-graph-simulation/01_utils.R")
library("causaleffect")
devtools::load_all() 

dag <- random_dag()
# graph <- graphobject$graph
# graph <- generate_mixed_graph_string(n = 3, p_dir = 0.5, p_bi = 0.0, names = c("z","x","y"))
# graph_for_id <- string_to_igraph(graph)
# plot(graph_for_id)
dag
ident_form_id <- causal.effect(y = "y", x = "x", G = dag)
ident_form_id
formul <- convert_formula_for_validation(ident_form_id)
validate_formula(formul, query, graph)

dosear
k <- "dd"
library(igraph)

generate_mixed_graph_string <- function(
    n,
    p_dir = 0.2,
    p_bi  = 0.1,
    names = NULL,
    seed = NULL,
    indent = "  "
) {
  if (!is.null(seed)) set.seed(seed)
  
  if (is.null(names)) {
    names <- paste0("x", seq_len(n))
  } else {
    stopifnot(length(names) == n)
  }
  
  # 1) Directed edges: only from earlier to later to guarantee DAG
  dir_lines <- character(0)
  for (i in seq_len(n - 1)) {
    for (j in (i + 1):n) {
      if (runif(1) < p_dir) {
        dir_lines <- c(dir_lines, paste0(names[i], " -> ", names[j]))
      }
    }
  }
  
  # 2) Bidirected edges: between any unordered pair (i<j)
  bi_lines <- character(0)
  for (i in seq_len(n - 1)) {
    for (j in (i + 1):n) {
      if (runif(1) < p_bi) {
        bi_lines <- c(bi_lines, paste0(names[i], " <-> ", names[j]))
      }
    }
  }
  
  # Combine; (optional) shuffle for nicer variety
  lines <- c(dir_lines, bi_lines)
  if (length(lines) == 0) lines <- character(0)  # allow empty
  if (length(lines) > 1) lines <- sample(lines)
  
  # Format as a quoted multi-line string like your example
  body <- paste0(indent, lines, collapse = "\n")
  paste0('\n', body, if (length(lines) > 0) "\n" else "")
}

string_to_igraph <- function(text) {
  
  lines <- unlist(strsplit(text, "\n"))
  lines <- trimws(lines)
  lines <- lines[lines != ""]
  
  parse_line <- function(line) {
    if (grepl("<->", line, fixed = TRUE)) {
      parts <- strsplit(line, "<->", fixed = TRUE)[[1]]
      return(list(from = trimws(parts[1]), 
                  to   = trimws(parts[2]), 
                  type = "bi"))
    }
    if (grepl("->", line, fixed = TRUE)) {
      parts <- strsplit(line, "->", fixed = TRUE)[[1]]
      return(list(from = trimws(parts[1]), 
                  to   = trimws(parts[2]), 
                  type = "dir"))
    }
    stop("Tuntematon särmä: ", line)
  }
  
  parsed <- lapply(lines, parse_line)
  
  nodes <- unique(unlist(lapply(parsed, \(e) c(e$from, e$to))))
  
  g <- make_empty_graph(directed = TRUE)
  g <- add_vertices(g, length(nodes), name = nodes)
  
  edge_types <- c()
  
  for (e in parsed) {
    
    if (e$type == "dir") {
      g <- add_edges(g, c(e$from, e$to))
      edge_types <- c(edge_types, "dir")
    }
    
    if (e$type == "bi") {
      # lisää molemmat suunnat
      g <- add_edges(g, c(e$from, e$to,
                          e$to, e$from))
      edge_types <- c(edge_types, "bi", "bi")
    }
  }
  
  E(g)$type <- edge_types
  
  return(g)
}

graph2 <- generate_mixed_graph_string(n = 6, p_dir = 0.5, p_bi = 0.3)
print(graph2)
plot(string_to_igraph(graph2))
graph_from_literal( A -+ B,  C ++ D)
# A simple undirected graph
g <- graph_from_literal(
  Alice - Bob - Cecil - Alice,
  Daniel - Cecil - Eugene,
  Cecil - Gordon
)

g <- graph.formula(x -+ y, z -+ x, z +-+ y , x +-+ z, simplify = FALSE)
plot(g)
# Another undirected graph, ":" notation
g2 <- graph_from_literal(Alice - Bob:Cecil:Daniel, Cecil:Daniel - Eugene:Gordon)
g2
# simplify = FALSE to allow multiple edges
g <- graph.formula(x -+ y, z -+ x, z -+ y , x -+ z, z -+x, simplify = FALSE)

# Here the bidirected edge between X and Z is set to be unobserved in graph g
# This is denoted by giving them a description attribute with the value "U"
# The edges in question are the fourth and the fifth edge
g <- set.edge.attribute(graph = g, name = "description", index = c(4,5), value = "U")
plot(g)
is_dag(g)
causal.effect("y", "x", G = g)

#library(igraph)

# Luodaan directed-graafi
g <- make_empty_graph(directed = TRUE)

# Lisätään solmut
g <- add_vertices(g, 3, name = c("x", "z", "y"))

# Lisätään kaaret:
# x -> z
# z -> y
# x <-> y  (eli x->y ja y->x)
g <- add_edges(g, c(
  "x","z",
  "z","y",
  "x","y",
  "y","x"
))

# Merkitään bidirected-kaaret (viimeiset kaksi)
E(g)$description <- NA
E(g)$description[3:4] <- "U"

# Tunnistetaan bidirected-kaaret
isU <- !is.na(E(g)$description) & E(g)$description == "U"

# Piirretään
plot(
  g,
  edge.arrow.mode = ifelse(isU, 3, 2),  # 3 = nuolet molemmissa päissä
  edge.lty        = ifelse(isU, 2, 1),  # katkoviiva latentille
  edge.color      = ifelse(isU, "red", "black"),
  edge.curved     = ifelse(isU, 0.2, 0),
  vertex.size     = 30,
  vertex.color    = "lightblue",
  vertex.label.cex = 1.2
)