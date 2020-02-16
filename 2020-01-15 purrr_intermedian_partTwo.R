# Chapter three - Why cleaner code? -----------------------------------------

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
  mutate( descri = purrr::map(data , function( x ) {x %>% summarise( sum = sum(x$Sepal.Length, na.rm = T),
                                                                     mean = mean(x$Sepal.Length, na.rm = T),
                                                                     sd = sd(x$Sepal.Length, na.rm =T))  }  )
          ) %>% 
  unnest(descri)



# lm summary compose  -----------------------------------------------------

summary_lm <- compose( summary, lm)


iris %>% group_by(Species) %>% 
  nest() %>% 
  mutate( data = map(data, ~summary_lm(Sepal.Length ~ Sepal.Width,
                                                    data = .x)),
          data = map(data, "r.squared")) %>% 
  unnest()

nested_model_iris <-
  iris %>% 
  group_by(Species) %>% 
  nest() %>% 
  mutate( model = map(data, function(x) { summary_lm(Sepal.Length ~ Sepal.Width,
                                       data = x) } ),
          rsq = map(model, "r.squared")
          ) 


nested_model_iris %>% 
  unnest(rsq) %>%
  select(Species, rsq)



# nest and unnest ---------------------------------------------------------

# Load dplyr, tidyr, and purrr
library(dplyr)
library(tidyr);library(purrr)

df <- tibble( urls =  c("https://thinkr.fr", "https://datacamp.com") )

# Create a "links" columns, by mapping get_links() on urls
df2 <- df %>%
  mutate(links = map(urls, get_links)) 

# Print df2 to see what it looks like
df2

# unnest() df2 to have a tidy dataframe
df2 %>%
  unnest()






# "Discovering the dataset - Part 4 ---------------------------------------

# read_data:RDS

rstudioconf <-  readRDS("#RStudioConf.RDS")

as.list(rstudioconf)


rstudioconf %>% View()

length(rstudioconf[[1]])

vec_depth(rstudioconf)

# predicate - verbs

x <- seq(1:10)
letters <- letters[seq(1:4)]
# map_*()
map_lgl(x , function(x) {ifelse(x > 3, T, F)})
# discard()
discard(x, function(x) {x > 3})

discard( letters, ~.=="a")

# keep()
keep(x , ~.>3)

## test_list
test_list <- list( a = list( is_retweet = c(T,F,T,F) ,tekst=c( "is_retweet", "is_not_is_retweet", "is_retweet", "is_not_is_retweet"), b = c(1:4) ),  b = list(1,2,3))

discard( test_list ,"is_retweet")

#
# Keep the RT, extract the user_id, remove the duplicate
rt <- keep(rstudioconf, "is_retweet") %>%
  map("user_id") %>% 
  unique()

# Remove the RT, extract the user id, remove the duplicate
non_rt <- discard(rstudioconf, "is_retweet") %>%
  map("user_id") %>% 
  unique()

# Determine the total number of users
union(rt, non_rt) %>% length()

# Determine the number of users who has just retweeted
setdiff(rt, non_rt) %>% length()


# ch 4. part 2 - extract info from dataset --------------------------------

sum_no_na <- partial( sum, na.rm = T)

map_dbl( airquality, sum_no_na)

sum_n_na <- partial( as_mapper(sum(x, na.rm = T)))

map_dbl( airquality, sum_no_na)


# compse, create a function from 2 or more function

test <- seq(from = 1, to = 50, by =.1)

test_list <- list(test, test)

round(sum(seq(from = 1, to = 50, by = 0.1)), digits = 2)

round_sum <- compose(round, sum_no_na )

map_dbl( test_list, round_sum)

map_dbl( airquality, round_sum)




# Extract info from he dataset --------------------------------------------

airquality <- airquality %>% as_tibble()

# x <- 

# partial - 

sum_no_na <- partial( sum, na.rm = T )

airquality %>% 
  map_dbl( sum_no_na)


map_dbl(airquality, function(x) {sum(x, na.rm = T)} )

# compose: create a new function from two or more function

rounded_mean <-  compose( round, sum_no_na)

map_dbl( airquality, rounded_mean)


# Other from purrr

# compact: Removing NULL, 
# flattern: Removing one level

eks <- list( NULL, 1, 2, 3, NULL)

compact( eks)


eks2 <- list( a = list(1,2,3), b = list(4,5,6))

eks2
flatten(eks2)


## Eks
# Prefill mean() with na.rm, and round() with digits = 1
mean_na_rm <- partial(mean, na.rm = TRUE)
round_one <- partial(round, digits = 1)

# Compose a rounded_mean function
rounded_mean <- compose(round_one, mean_na_rm)

# Extract the non retweet  
non_rt <- discard(rstudioconf, "is_retweet")

# Extract "favorite_count", and pass it to rounded_mean()
non_rt %>%
  map_dbl("favorite_count") %>%
  rounded_mean()




## 
# Combine as_vector(), compact(), and flatten()
flatten_to_vector <- compose(as_vector, compact, flatten)

# Complete the fonction
extractor <- function(list, what = "mentions_screen_name"){
  map( list , what ) %>%
    flatten_to_vector()
}

# Create six_most, with tail(), sort(), and table()
six_most <- compose(tail, sort, table)

# Run extractor() on rstudioconf
extractor(rstudioconf) %>% 
  six_most()



## Manipulation URLs


mult <- as_mapper( function(x) {x^2})

print_smooth <- as_mapper( function(x) {head(x) })

mult(2)  

mult_print_smooth <- compose( print_smooth , mult )
  

map( list(airquality, mtcars),  mult)


map( list(airquality, mtcars), mult_print_smooth )



# Manipulating urls -------------------------------------------------------



# Extract the "urls_url" elements, and flatten() the result
urls_clean <- map(rstudioconf, "urls_url") %>%
  flatten()

# Remove the NULL
compact_urls <- compact(urls_clean)

# Create a mapper that detects the patten "github"
has_github <- as_mapper(~ str_detect(.x, "github"))

# Look for the "github" pattern, and sum the result
map_lgl( compact_urls, has_github ) %>%
  sum()


##
# From previous step
str_prop_detected <- function(vec, pattern) {
  vec %>%
    str_detect(pattern) %>%
    mean()
} 
flatten_and_compact <- compose(compact, flatten)

rstudioconf %>%
  # From each element, extract "urls_url"
  map("urls_url") %>%
  # Flatten and compact
  flatten_and_compact() %>% 
  # Get proportion of URLs containing "github"
  str_prop_detected("github")



# map_at ------------------------------------------------------------------

# map_at if you want to a task on a spes. place in list:

my_list <- list( a =1:10, b = 1:100)

map_at( my_list , .at = "b", .f = sum)




# negate: invert a predicted inversion

not_char <- negate( is.character )


list_w_chr_and_number <- list( a = 1, b = "B", c = "c")

map(list_w_chr_and_number, function(x) {if(not_char(x)) { print(x) }} ) %>% 
  flatten()


## Splitting the dataset
# In a previous exercise, 
#you have determined that the mean number of retweets by tweet is 3.3.
# In this exercise, we'll have a look at how many tweets are above this mean, 
#and how many are below.
# 
# To do so, we will first create a mapper that tests if .x 
#is above 3.3. We'll then prefill map_at(), with .at being 
#"retweet_count", and .f being first the mapper we've created, and in a second time the negation of this mapper.
# 
# Note that since this course was created, purrr behavior changed, and in order to avoid an argument clash between .f 
# in partial() and .f in map_at(), you must use the quasi-quotation equals operator, :=, (sometimes known as the "walrus operator").
# For the purpose of this exercise, all you need to know is that := works like =, but lets partial() know that the argument should be 
# passed to map_at() rather than being kept for itself.
# 
# Once these tools are created, we will use them on the non_rt object, which is an extraction of the "original tweets" from the rstudioconf dataset.

# Create mean_above, a mapper that tests if .x is over 3.3
mean_above <- as_mapper(~ .x > 3.3)

# Prefil map_at() with "retweet_count", mean_above for above, 
# and mean_above negation for below
above <- partial(map_at , .at = "retweet_count", .f := mean_above )
below <- partial(map_at, .at = "retweet_count", .f := negate(mean_above) )

# Map above() and below() on non_rt, keep the "retweet_count"
ab <- map(non_rt, above) %>% keep("retweet_count")
bl <- map(non_rt, below) %>% keep("retweet_count")

# Compare the size of both elements
length(ab)
length(bl)




# Last example ------------------------------------------------------------

# Get the max() of "retweet_count" 
max_rt <- map_int(non_rt, "retweet_count") %>% 
  max()

# Prefill map_at() with a mapper testing if .x equal max_rt
max_rt_calc <- partial(map_at, .at = "retweet_count", .f := ~ .x == max_rt)

res <- non_rt %>%
  # Call max_rt_calc() on each element
  map(max_rt_calc) %>% 
  # Keep elements where retweet_count is non-zero
  keep("retweet_count") %>% 
  # Flatten it
  flatten()

# Print the "screen_name" and "text" of the result
res$screen_name
res$text  



















