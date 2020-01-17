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



# New verb -- partial -----------------------------------------------------------------

lm_iris <- partial(lm, data = iris)

lm_iris(Sepal.Length ~Sepal.Width)


mean_na_rm <- partial( mean, na.rm = T)

mean_na_rm( c(1,2,3,NA))


# compose and partial

rounded_mean <- compose(
  partial( round, digits = 2),
  partial( mean, na.rm =T)
)


rounded_mean( c(1.2,2.02,3.3,NA))



# purrr - compose and partail with the rrvest -----------------------------
urls <- c("https://thinkr.fr", "https://datacamp.com")
library(rvest)

# Prefill html_nodes() with the css param set to h2
get_h2 <- partial(html_nodes, css = "h2")

# Combine the html_text, get_h2 and read_html functions
get_content <- compose(html_text, get_h2, read_html)

# Map get_content to the urls list
res <- map(urls, get_content) %>%
  set_names(urls)

# Print the results to the console
print(res)

### get all the links:

# Create a partial version of html_nodes(), with the css param set to "a"
get_a <- partial(html_nodes, css = "a")

# Create href(), a partial version of html_attr()
href <- partial(html_attr, name = "href")

# Combine href(), get_a(), and read_html()
get_links <- compose(href, get_a, read_html)

# Map get_links() to the urls list
res <- map(urls, get_links) %>%
  set_names(urls)

# See the result
res


# nested dataframe --------------------------------------------------------

seq_letters <- 
  compose( as_mapper(~letters[.x] ), 
           as_mapper(~seq(.x)) )

seq_letters(seq(1:3))

df_letters <- tibble(
  classic = seq_letters(1:3),
  list = list(seq_letters(1:3),
              seq_letters(1:6),
              seq_letters(1:9))
)


df_letters


a_node <- partial( html_node, css = "a")
href <- partial(html_attr, name = "href")
get_links <- compose(href, get_a, read_html)


urls_df <- tibble( 
  urls = c("https://thinkr.fr", "https://datacamp.com")
  )

# nested dataframe (tibble)
urls_df %>% 
  mutate( links = purrr::map(urls, function(x) {get_links(x)})) %>% 
  unnest( links) %>% filter( urls == str_detect(urls, "thinkr"))


iris %>% 
  group_by(Species) %>% 
  nest( ) %>% 
  mutate( descri = purrr::map(data$Sepal.Length , function(x) {summarise( mean = mean(x, na.rm = T ))}))



  
  





