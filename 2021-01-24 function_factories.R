

# Functional factories, chapter 10 (Advanved R)
args(mean)

#
power1 <- function(exp) {
  function(x) {
    x^exp
  }
}

square <- power1(2)
cube <- power1(3)
square(2)
square(4)
cube(2)


# Environments
square
cube

# Can see that they are bonded to exp (and have the same parent env.)
# and have different environment
rlang::env_print(square)
rlang::env_print(cube)
rlang::fn_env(square )$exp
rlang::fn_env(cube )$exp

# Forcing evaluation
# Problem:
x <- 2
square <- power1(x)
square(2)
x <- 3
square <- power1(x)
square(2)
# This bug appear because x is evaluated lazily when square() is runned

# Fixing the bug:
power2 <- function(exp) {
  force(exp)
  function(x) { x^exp}
}

x <- 2
square <- power2(x)
x <- 3
square(2)


# 10.2.4 stateful functions -----------------------------------------------

new_counter <- function( ) { 
  i <- 0
  
  function( ) { 
    i <<- i + 1
    i
  }
}

one <- new_counter()
two <- new_counter()

one()
one()
one()

two()

# When the manufactured function is run i <<- i + 1 will modify i in its enclosing environment. Because manufactured functions have 
# independent enclosing environments, they have independent count



# manufactured functions need to explicitly unbind large temp object with rm()



# Graphical factories -----------------------------------------------------
library(tidyverse)
y <- c(123, 124, 125)

# Return a function
scales::comma_format()(y)

# In other words, the primary interface is a function factory. 
scales::comma_format(scale = 10 , suffix = " K")(y)

df <- tibble( x = 1, y = y)

graf <- df %>% ggplot( aes( x = factor(x), y = y)) + 
  geom_point( breaks = 1, labels = NULL ) +
  labs( x = NULL, y = NULL)


graf + scale_y_continuous( labels = scales::comma_format( scale = 10, suffix = " K"))

graf + scale_y_continuous( labels = scales::scientific_format())

## Histogram bins
# binwidth argument can be a function. 

sd <- c(1,5,15)
n <- 100

df <- tibble( x = rnorm(3*n, sd = sd), sd = rep(sd, n))

ggplot( df, aes( x )) +
  geom_histogram( binwidth = 2) +
  facet_wrap( ~sd, scales = "free_x") +
  labs(x = NULL)
# Each facet has the same number of observations, but the variability is very different. 

# To get the same number of observation in each bin
bindwidth_bins <- function(n) {
  force(n)
  
  function(x) { 
    max(x)-min(x)/n
    }
}

ggplot( df, aes(x)) + geom_histogram( binwidth =  bindwidth_bins(20)) +
  facet_wrap( ~sd, scales = "free_x") + labs( x =NULL)


## ggsave()
# Example function factory used internally by ggplot2

require(stats)
centre <- function(x, t) {
  switch(t ,
         mean = mean(x),
         median = median(x), 
         stop("Ikke kjent type"))
}
x <- c(1,1,2,1:10)
centre(x, "median")




# rep ---------------------------------------------------------------------

power1 <- function( exp ) {
  function(x) { x^exp}
}

linear <- power1(1)
square <- power1(2)

map2( 1:10, rep(2, times = 10), function(x,y) { test <- power1(x);test(y)} )













































