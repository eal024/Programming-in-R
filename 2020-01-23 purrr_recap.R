

library(tidyverse)

eks_list <- list( a = seq(1:3), b = letters[c(1:3)], df = tibble( a= c(1:3), b =  c(4:6)) )

list <- list( a = c(1:3), b = seq(1:100), a = c(1,3,3))

list_w_list <- list( list(-1, a = 2, x = c(1,2,3)),
                     list(-2, a = 4, x = c(3:6)),
                     list(-4, a = 5, x = c(9:10))
                     )



#map

  # by name
map(list_w_list, "a")

map_dbl(list_w_list, "a")

  # by name and position
map_dbl(list_w_list, list("x", 1))

x <- seq(from = 0, to = 0, length.out = 10)

plus <- function(x,y) {x + y}

map2_dbl( x ,runif(1) ,function(x,y) { plus(x,y)} )

map_dbl(x, plus, runif(1))
map_dbl(x, ~plus(.x, runif(1)))

# Lambda
map( list, ~mean(.x))



# as_mapper (can use Lambda)
as_mapper(~mean(.x, na.rm = T))



# partial
partial( as_mapper(~mean(.x, na.rm = T)))



# ## parsing map(list, f., .....) -> the .... part ------------------------

mult_10 <- as_mapper( function(x,y) {x*10*y})

partial(mult_10, y = 3)

map_dbl(c(1,2,3) , function(x){mult_10(x, y = 3)} )

map_dbl(c(1,2,3) , partial(mult_10, y = 3 ))






# # keep() & discard() ----------------------------------------------------

keep(eks_list, ~is.numeric(.x))

modify( keep(eks_list, ~is.numeric(.x)), function(x) {x^2} )
modify( keep(eks_list, ~is.data.frame(.x)), function(x) {x^2})


discard( list, function(x) {sum(x) > 10})

some(list, function(x) {mean(x) > 2}) 

every(list, function(x) {mean(x) > 2} ) 



# Functional operators - return function modifyed
safely() & posibly()
partial()



# compose()

compose( round, mean)

map_dbl(list( a = seq(from = 0, to = 10, length.out = 21),
          b = seq(from = 0, to = 100, length.out = 30),
          c = seq(from = 0, to = 9, length.out = 90)
          ), compose( ~round(.x, digits = 0), mean)
    )







negate()


# cleaner code
round_mean <- compose( partial(round, digits = 1),
                       partial(mean, trim = 2, na.rm = T)
                       )


map( 
  list(airquality, mtcars),
  function(x) {map_dbl(x, round_mean)}
  )





df <- tibble( x = c(1:3), y = x*2, letters = letters[1:length(x)])


modify_at( df, vars(x, y), function(x) {x^2})

modify_if( df,  function(p) {is.numeric(p)}  , function(x) {x^2}) 

modify_if(df, is.numeric, ~.x^2)


















