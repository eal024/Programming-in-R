


# Reduce ------------------------------------------------------------------

# Basic
set.seed(123)

l <- map(c(1:4), function(x) { sample(1:10,10, replace = T )})

l



# Which number does accour in both l[[1]] and the l[[x]]?
out <- l[[1]]
out <- intersect(out, l[[2]])
out <- intersect(out, l[[3]])
out <- intersect(out, l[[4]])
out

# Alternativ

reduce(l, intersect)


# _group_split ------------------------------------------------------------


