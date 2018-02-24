library("partitions")

# cost per sheet incl. labor and stock
A = 0.30  

# overhead cost to run form incl. plates, makeready
B = 180 

# number up
n = 2

# versions
V = c(1000, 500, 1100)

# number of versions
m = length(V)

# set of all possible forms
F = compositions(n, m)
N = ncol(F)

zero = function(x) as.numeric((x > 0)) * (x + B)
cost = function(x) rowSums(zero(A*x))

qty = function(x) apply(F, 1, "%*%", t(x))

is.solution = function(x) apply(qty(x) >= V, 1, all)
  
s = seq(0, max(V), 100)

X = do.call(expand.grid,rep(list(s), N))

S = ifelse(is.solution(X),cost(X),Inf)



