source("tests/causal-graph-simulation/01_utils.R")
library("causaleffect")
devtools::load_all() 

graphobject <- random_dag(n_min = 4, n_max = 4)
graph <- graphobject$graph
graph
query <- "p(y|do(x))"
ident_form_id <- causal.effect(y = "y", x = "x", G = graph)
formul <- convert_formula_for_validation(ident_form_id)
validate_formula(formul, query, graph)


"\\sum_{z4,z2,z5}[P(y|x,z4,z2,z5)P(z5|x,z4,z2)P(z2|x,z4)P(z4)]"
formula <- "\\sum_{z4,z1,z6}[P(y|z4,z3,z1,x,z6)\\sum_{z4}[P(z6|z4,z3,z1,x)]P(z1|z4)P(z4)]"
ident_form_dosearch <- dosearch(data = distributions, query = "P(y|do(x))", graph = dag, control = list(draw_derivation = TRUE))

cat(ident_form_id)
cat(ident_form_dosearch$formula)
plot(ident_form_dosearch)
parse_path_rules(formula)


#gsub("\\\\left\\(|\\\\right\\)", "", ident_form_dosearch)[2]
#ident_form_id

distributions_dosearch <- get_distributions(ident_form_dosearch$formula, return_type = "vector")
distributions_id <- get_distributions(ident_form_id, return_type = "vector")
distributions_dosearch
ident_form_dosearch
plot(graphobject$graph)


graph <- random_dag_with_vars(graphobject$order)
modify_graph(dag)
str(dag[1])
plot(graph)
ident_form_dosearch <- dosearch(data = distributions, query = "P(y|do(x))", graph = graph)
ident_form_dosearch

graph_from_edgelist(edges)
"\\sum_{z3,z4}[p(y|z3,x,z4)p(z4|z3,x)p(z3)]"
find("union")


library("igraph")
graphobject <- random_dag()
dag <- graphobject$graph
ident_form_id <- causal.effect(y = "y", x = "x", G = dag)

distributions <- get_distributions(ident_form_id)
ident_form_dosearch <- dosearch(data = distributions, query = "P(y|do(x))", graph = dag)
ident_form_dosearch
plot(dag)
for (edge_id in 1:ecount(dag)) {
  edge <- ends(dag, edge_id)
  new_dag <- delete_edges(dag, edge_id)
  new_dag <- add_edges(new_dag, c(edge[2], edge[1]))
  print(new_dag)
  tryCatch(
    {
      ident_form_dosearch <- dosearch(data = distributions, query = "P(y|do(x))", graph = new_dag)
      print(ident_form_dosearch$formula)
    },
    error = function(e) {
      message("Tapahtui virhe: ", e$message)
    })
}
summary(ident_form_dosearch)
summary(1)
 

