

library(tidyverse)

x <- seq(from = 1, to = 10,  length.out = 10)

cut(x, breaks = 2)


df <- tibble( x = x, x_cut = cut(x, breaks = 2))

df


# cutting by quatile

cut_by_quantile <- function( x, n, na.rm, labels, interval_type) {
  
  probs <- seq(0,1, length.out = n+1);
  
  quantiles <- quantile(x, probs, na.rm = na.rm, names = F);
  
  right <- switch( interval_type, "(lo, hi]" = T, "[lo,hi)" = F);
  
  cut(x, quantiles, labels = labels, right = right, include.lowest = T)
  
}

cut_by_quantile(10, n = 10, na.rm = T, interval_type = T, labels = NULL)

