source("tests/causal-graph-simulation/01_utils.R")
library("causaleffect")
devtools::load_all() 

# Simulation
query <- "P(y|do(x))"
graph <- totally_random_graph(8)
graph <- random_graph_with_path_x_to_y(6)
ident_form_id <- causal.effect(y = "y", x = "x", G = graph)
formul <- convert_formula_for_validation(ident_form_id)
validate_formula(formul, query, graph)
plot(graph)
cat(ident_form_id)
formul <- "\\sum_{z2,z6}[\\sum_{x}[p(x)p(y|z2,x,z3,z6)]\\sum_{z3}[p(z2|x,z3,z6)p(z3)p(z6|x,z3)]]"
# Save interesting case
save(graph, query, formul, file = "tests/inputs/work_of_impact_of_product_order_and_sum_scopes2.RData")

# Switch for original dosearch
pkgload::unload("dosearch")
library(dosearch)

data1 <- parse_distributions(formul)
query1 <- query
graph1 <- graph
res <- dosearch(data1, query1, graph1, control = list(verbose = TRUE, draw_derivation = TRUE, draw_all = FALSE))
plot(res)
edge_attr(graph)

# Special cases
load("tests/inputs/not_work_front_door.RData")
load("tests/inputs/work_front_door.RData")
load("tests/inputs/not_work_sum_brackets_wrong_from_id_alg.RData")
load("tests/inputs/work_sum_brackets_wrong_from_id_alg.RData")
load("tests/inputs/test_of_impact_of_product_order_and_sum_scopes.RData")
load("tests/inputs/not_work_of_impact_of_product_order_and_sum_scopes2.RData")
load("tests/inputs/work_of_impact_of_product_order_and_sum_scopes2.RData")

ident_form_id <- causal.effect(y = "y", x = "x", G = graph)
ident_form_id
plot(graph)
formul <- convert_formula_for_validation(ident_form_id)
formul
validate_formula(formul, query, graph)
res
graph <- delete_vertices(graph, 4)
V(graph)
formul <- "\\sum_{z2,z6}[\\sum_{x}[p(x)p(y|z2,x,z3,z6)]\\sum_{z3}[p(z2|x,z3,z6)p(z3)p(z6|x,z3)]]"
