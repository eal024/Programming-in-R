
library(tidyverse)
# 9.5 Reduce family

reduce(1:5, sum)
  
# Find the values that occur in every elements

l <- map(1:4, ~ sample(c(1:10), replace = T))

intersect(l[[1]], l[[2]])
intersect(l[[1]], l[[3]])
intersect(l[[1]], l[[4]])

# 
reduce(l, intersect)

# All the element that at least appear in one entry
union(c(1,4), c(5:10))
reduce(l, union)

df <- tibble(a = rep(c(1:3), times = 3), en_til_fire = sample(size = 9, c(1:4), replace = T))

## accumulate and reduce
# An alternate way to think about reduce and accumulate
df %>%
  arrange( a,  en_til_fire) %>% 
  group_by(  a) %>% 
  mutate( se = reduce(en_til_fire, sum), se2 = cumsum(en_til_fire),
          se3 = accumulate(en_til_fire,`+`),
          se4 = reduce( en_til_fire, `+`)
          )

## init -> if .x is emty
reduce( integer(), `+` )

reduce( integer(), `+` , .init = 1)

# This also ensures that reduce check that 1 is a valid for the function
reduce("a", `+`, .init = 0)


# 9.6 predicate functionals ----------------------------------------------
# F that return True or False 

# Basic
# some: return if any matches
# every: if every matches
# none: if no elements match

some(c(1:10), ~ .x > 5)

some( df, ~ .x < 0)

df$letters <- letters[1:nrow(df)]

some( df, ~.x < 0)
every( keep(df, is.numeric), ~.x > 0)
none( df, ~.x < 0)

some( map( df, ~.x ), ~is.character(.x) )

# detect and keep

streng <- list( 
  list( en = c("en", "to", "tre", "fire", "Fem"),
        to = "to") 
  )

keep( df, is.character )
keep( df, is.numeric )
discard(df, is.numeric)


# map variants ------------------------------------------------------------

map_if( df, is.numeric, mean)
modify_if( df, is.numeric, mean)
map(keep(df, is.numeric), mean)



# Base functional --------------------------------------------------------

a2d <- matrix(1:20, nrow = 5)
a2d

apply(a2d, 1, mean)

mean(a2d[1,1])



































