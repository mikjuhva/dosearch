# devtools::load_all() 
# data1 <- "P(x,y,z)"
# query1 <- "P(y|do(x))"
# graph1 <- "
#   x -> y
#   z -> x
#   z -> y
# "
# data2  <- "p(y,d,z)"
# query2 <- "p(y | do(d))"
# graph2 <- "
# z -> d
# z -> y
# d -> y
# "

# cat("\n=== TESTI 1 ===\n")
# res1 <- dosearch(data1, query1, graph1, control = list(draw_derivation = TRUE, draw))
#res2 <- dosearch(data1, query1, graph1, control = list(draw_derivation = TRUE))
# print(res2$formula)
# res1$formula
# # cat("\n=== TESTI 2 ===\n")
# # res2 <- dosearch(data2, query2, graph2)
# # print(res2)
# plot(res1)
# 
# plot(res2)
# write(res1$derivation, file="t.txt")
# 
# 
# dataFull <- "P(Y,W,Z,X)"
# query1 <- "P(Y|do(X))"
# graph1 <- "
#       X -> Y
#       Y <-> W
#       W -> Z
#       Z -> X
#       U -> X
#       U -> W
#       "
# res1 <- dosearch(dataFull, query1, graph1, control = list(draw_derivation = TRUE))
# res1$formula
# # cat("\n=== TESTI 2 ===\n")
# # res2 <- dosearch(data2, query2, graph2)
# # print(res2)
# plot(res1)
# 
# dataPartAll <- "P(Y|W,Z,X)
#              P(X|W,Z)
#              P(W)
#              P(X)"
# res2 <- dosearch(dataPartAll, query1, graph1, control = list(draw_derivation = TRUE, draw_all=TRUE))
# res2$formula
# plot(res2)

devtools::load_all() 
query <- "p(y|do(x))"
graph <- "
  z -> y
  x -> y
  z -> x
  w -> y
"
formula <- "\\sum_{z}[p(y|x,z)\\sum_{w}[p(z,w)]]"

validate_formula(formula, query, graph)
