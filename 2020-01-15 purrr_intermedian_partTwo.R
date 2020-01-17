# New chapter - Why cleaner code? -----------------------------------------

library(tidyverse);library(broom)

# Example, can end with repeatly change thing in the formula
iris %>% 
  lm( Sepal.Length ~ Species, data = .) %>% tidy() %>% filter(p.value < 0.05)


## cleaner version:
tidy_iris_lm <- compose(as_mapper(~ filter(.x, p.value < 0.05)),
                        tidy,
                        partial(lm, data = iris, na.action = na.fail))

# Implements:
list(
  Petal.Length ~ Petal.Width,
  Petal.Width ~ Sepal.Width
) %>% map(tidy_iris_lm)


# compose: used when you have two or more function you want to excecute:

round_and_mean <- compose(round, mean)

round(mean(seq(1:100)))
round(mean(seq(1:1000)))
round(median(seq(1:100)))
round(median(seq(1:1000)))

round_and_median <- compose(round, median)

round_and_mean( seq(from = 1, to = 205, by = 0.28))
round_and_median(seq(1:100))


# Example -----------------------------------------------------------------
library(purrr);library(httr)

get_thinkr <- GET("http://thinkr.fr")

status_code(get_thinkr)

# First GET, than status_code -> the compse need to be at the opposite order
get_status_code <-  compose(status_code, GET)

get_status_code("http://datacamp.com")
get_status_code("http://ht.no")

url <- c("http://thinkr.fr", "http://datacamp.com")

map_dbl(url, get_status_code)


# Compose and negate ------------------------------------------------------

# compose -> can include as many as you like:
# It goes in the opposite order!!!!

lm( Sepal.Length ~ Sepal.Width , data = iris) %>% anova() %>% tidy 

clean_anova <- compose(as_mapper(~pull(.x, meansq) ) , tidy, anova, lm)

clean_anova( Sepal.Length ~ Sepal.Width , data = iris)


# The negates function
under_hundred <- as_mapper( ~mean(.x) < 100)

not_under_hundred <- negate(under_hundred)


map_lgl( (60:290), function(x) {under_hundred(x)})
# Change to the opposite
map_lgl( (60:290), function(x) {not_under_hundred(x)})


a <- 1
tall <- c(1,2,3)

a %in% tall

tall %in% a

tall %not_in% a


# Example -----------------------------------------------------------------

# Negate the %in% function 
"%not_in%" <- negate(`%in%`)

# Compose a status extractor 
extract_status <- compose(status_code, GET)

# Complete the function definition
strict_code <- function(url) {
  # Extract the status of the URL
  code <- extract_status(url)
  # If code is not in the acceptable range ...
  if (code %not_in% 200:203) {
    # then return NA
    return(NA)
  }
  code
}


# Map the strict_code function on the urls vector
res <- map_dbl(urls, strict_code)

# Set the names of the results using the urls vector
res_named <- set_names(res, urls)

# Negate the is.na function
is_not_na <- negate(is.na)

# Run is_not_na on the results
is_not_na(res_named)






