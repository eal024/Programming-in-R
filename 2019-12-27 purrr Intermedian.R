
# 1) purrr basics - a refresher

library(tidyverse)

# Repcap - intro  ---------------------------------------------------------

# purrr: map( .x, .f, ...) / for each element of .x do .f

#visit2015 <- read_csv2( "dataStMalo.csv")

list <- list( a = rnorm(10,1,1), b = rnorm(100,10,2))


map(list, sum)

map( list , function(x) {sum(x)})

# dbl
map_dbl( list , sum)


## With two arguments
map2(list, list, function(x,y) {sum(x,y)})


## More than two elements ->

flere_lister <- list( list, list, list)


pmap(flere_lister, function(...) {sum(...)})

# A sum fra alle listene:
pmap_dbl(flere_lister, function(...) {sum(...)})



# Exercise  ---------------------------------------------------------------

# Vektorer:
visit_a <-  c(1,5,6,8,3,1,4)
visit_b <-  c(12,51,26,48,13,21,54)

to_day <- function(x) {x*24}

all_visit <- list(visit_a, visit_b)

# map
all_visit_converted <- map(all_visit, to_day)

map_dbl(all_visit_converted, mean)

#### Three days, converted
visit_c <- c(100:106)

all_tests <- list(visit_a,visit_b,visit_c)
#
map(all_tests, function(x) {x*24*rnorm(length(x),1,1)}) %>% 
  map(function(.x) {barplot(.x)})

# Without print on the screen
map(all_tests, function(x) {x*24*rnorm(length(x),1,1)}) %>% 
  walk(function(.x) {barplot(.x)})

## The sum of each element: 
all_test_day <- map(all_tests , to_day)
pmap( all_test_day, function(...){sum(...)})

# Eqvivalent to:
all_test_day[[1]][1] + all_test_day[[2]][1] + all_test_day[[3]][1]





# Intro to mappers --------------------------------------------------------

# fucus on .f: Can be 3: function, "n" or number

#1) Anonymous function;

map_dbl( visit_a, ~ round(mean(.x)*10))

map2_dbl( visit_a, visit_b, ~ .x + .y)

map2_dbl( visit_a, visit_b, ~ ..1 + ..2)


# as_mapper

round_mean <- as_mapper(~round(mean(.x)))

round_mean( c(1.9,5.222, 9.9,5.9))

# In map:
map_dbl(visit_a, as_mapper(~.x*24))



# Set_names to list -------------------------------------------------------

mnd_visit <- function( per_mnd = 1,  min = 10, max = 40) {
  visit_a <- list()
  
  for(i in 1:12) {
    visit_a[[i]] <- runif(per_mnd, min = min, max = max);
  }  
  
  return(visit_a)
}

visit_2015 <- mnd_visit(per_mnd = 10, min = 5, max = 40)

visit_2016 <- mnd_visit(per_mnd = 15, min = 5, max = 40)


visit_2015

mean_mnd_2015 <- map(visit_2015, function(x) { round(mean(x), 0) })


# Set name:
  # Null names:
names(mean_mnd_2015)


length(mean_mnd_2015)

# first arg. need to be the same length as the secound.
mean_mnd_2015 <- set_names(mean_mnd_2015, month.abb)

names(mean_mnd_2015)


# Set names in a map:

mean_mnd_2016 <- mnd_visit(per_mnd = 30)
mean_mnd_2017 <- mnd_visit(per_mnd = 40)


names(mean_mnd_2016)


all_mean_visit <- list(mean_mnd_2015, mean_mnd_2016, mean_mnd_2017)

all_mean_visit <-
  # all year visit as list input.
  map(all_mean_visit, function(x) {
    # Extrapt each list from x
    map(x, function(x) {
      # mean all element in each list
      mean(x)
    })
  })


# Look at the new list:
all_mean_visit[[2]]


# give name to each mean_mnd/list


all_mean_visit <-map(all_mean_visit, function(x) {set_names(x, month.abb)})


for(i in 1:length(all_mean_visit) ){
  print(str_c("Element ", i, " med navnene på elementene:"))
  print(names(all_mean_visit[[i]]))
}


## When need to extract only elements that satisfy a condtion, use keep

# ex. which month has over 25 visits?

visit_2015 <- set_names(visit_2015, month.abb)

over_22 <- keep( visit_2015, function(x) {mean(x) > 22 })

names(over_22)

# As mapper:

limits <- as_mapper(~mean(.x) > 22)

visit_2016 <- set_names(visit_2016, month.abb)

over_22 <- keep(visit_2016, limits)

under_22 <- discard(visit_2016, limits)

names(over_22)
names(under_22)

## Keep, discard with map:

head(iris);head(airquality)

df_list <- list(iris, airquality)

#
map( df_list, function(x) {head(x, n = 12)} )


map( df_list, function(x) { head(keep(x, is.factor))} )

map( df_list, ~head(keep(.x, is.factor)))


## Example three:
all_visits <- list( c(100,22, 102, 103, 120,200, 100), 
                    c(100,22, 20, 103, 90,240, 100),
                    c(100,22, 2, 30, 110,101, 110))


# Create a mapper that test if .x is more than 100 
is_more_than_hundred <- as_mapper(~ .x > 100)


# Use this mapper with keep() on the all_visits object 
map(all_visits, ~ keep(.x, is_more_than_hundred))

# Use the  day vector to set names to all_list
day <- c("mon", "tue", "wed", "thu", "fri", "sat", "sun")
full_visits_named <- map(all_visits, ~ set_names(.x, day))

# Use this mapper with keep() 
map(full_visits_named, ~ keep(.x, is_more_than_hundred))



# example 4: Set_name, test if x > 100, and than filter group unde --------
# Set the name of each subvector
day <- c("mon", "tue", "wed", "thu", "fri", "sat", "sun")
all_visits_named <- map(all_visits, ~ set_names(.x, day))

# Create a mapper that will test if .x is over 100 
threshold <- as_mapper( ~.x >100)

# Run this mapper on the all_visits_named object: group_over
group_over <- map(all_visits_named, ~ keep(.x, threshold))

# Run this mapper on the all_visits_named object: group_under
group_under <-  map(all_visits_named, ~ discard(.x, threshold))




# Predicates --------------------------------------------------------------

# predicate return TRUE or FALSE -> test of condiction

# example: keep and discards -> return true and false
# other:
  # every -> test every elements, if they meet a condtion

visit_a
every( visit_a, function(x) { x > 2})

every( visit_a, ~.x > 0)

  # some -> test if some elements meet conditon
some( visit_a , ~.x == 0)

some( visit_a , ~.x != 0)

# Create a threshold variable, set it to 160
threshold <- 160

# Create a mapper that tests if .x is over the defined threshold
over_threshold <- as_mapper(~ .x > threshold)

# Are all elements in every all_visits vectors over the defined threshold? 
map(all_visits, ~ every(.x, over_threshold))

# Are some elements in every all_visits vectors over the defined threshold? 
map(all_visits, ~ some(.x, over_threshold))



# Functional programming in R ---------------------------------------------

#impure function
Sys.time()

# pure
nrow()


# side effect
plot(iris)

# adverbs 
possibly()

# if fail, return result and error
safely()

# Example:

map(list(2,"a"), log )

# use saftly -> tell result or error
map(list(2, "a"), safely(log))

# More: print only error or 
map(list(2, "a"), safely(log)) %>% map("result")

map(list(2, "a"), safely(log)) %>% map("error")


urls <- c("eirik", "trond", "christoffer")

# Create a safe version of read_lines()
safe_read <- safely(function(x) {read_lines(x)})

# Map it on the urls vector
res <- map(urls, safe_read)

# Set the name of the results to `urls`
named_res <- set_names(res,urls)

# Extract only the "error" part of each sublist
named_res %>% map("error")

named_res %>% map("result")




# safely, discard and map as function -------------------------------------

a <- list(1, 2, -1, 2, 0, -3, 5)

a <- a %>% set_names(letters[1:length(a)])

map_df( discard(a, function(x) {x < 0}), function(x){print(x)} ) 


# implement safely
safe_read <- safely( function(x) read_lines(x))

# discard
safe_read_disc <- function(read_list) {
  safe_read(read_list) %>% 
    discard(is.null)
}


til_lesing <- c("en", "to", NULL, "tre")

safe_read_disc(til_lesing) 




# Possibly : as safely but not reported with "error" or "result--------------------

# A first example

try_sum <- possibly( function(x) {sum(x)}, otherwise = "Not logical to sum")

test <-  c(1,2, 3, "En")
test2 <- seq(1:10)

try_sum(test)
try_sum(test2)

# Example 2
# Create a possibly() version of read_lines()
possible_read <- possibly(read_lines, otherwise = 404)

# Map this function on urls, pipe it into set_names()
res <- map(urls, possible_read) %>% set_names(urls)

# Paste each element of the list 
res_pasted <- map(res, paste, collapse = " ")

# Keep only the elements which are equal to 404
keep(res_pasted, function(x){ x == 404})

## Example Three
url_tester <- function(url_list){
  url_list %>%
    # Map a version of read_lines() that otherwise returns 404
    map( possibly(read_lines, otherwise = 404) ) %>%
    # Set the names of the result
    set_names( urls ) %>% 
    # paste() and collapse each element
    map(paste, collapse = " ") %>%
    # Remove the 404 
    discard(function(x) {x == 404}) %>%
    names() # Will return the names of the good ones
}

# Try this function on the urls object
url_tester(urls)




# Handling adverb results -------------------------------------------------

# cleaing safely results

l <- list("a", 2, 3, 4)

# gives 4
map(l, function(x){safely(function(y) {log(y)} ) } ) %>% length()


# gives 2
safe_log <- safely(x, function(x) {log(x)})

# Put errror and result into each sublist
map(l, safe_log) %>% transpose() %>% length()


sublist <- map(l, safe_log) %>% transpose()

sublist[2]

## use compact for remove NULL: Clean safely 

list_w_null <-list(1, NULL, 3, NULL)

list_w_null %>% compact()


# Examples: ---------------------------------------------------------------



se <- function(x, type = c("en", "enig", "toig")) {
  
  valgt <- match.arg(type)
  ganger <-switch(valgt, 
         en = 1,
         enig = 2,
         toig = 3)
  
  x*ganger
  
}

# dont work
se(1, type = "e")

# work:
se(1, type = "en")
se(1, type = "eni")

## Example

urls_l <- list("http://thinr.fr", "http://datacamp.com", "http://ht.no")

safe_read <- safely(function(x) {read_lines(x)})

url_tester <- function(url_list, type = c("result", "error")) {
  type <- match.arg(type)
  url_list %>%
    # Apply safe_read to each URL
    map(safe_read) %>%
    # Set the names to the URLs
    set_names( url_list ) %>%
    # Transpose into a list of $result and $error
    transpose()  %>%
    # Pluck the type element
    pluck(type)  
  }

# Try this function on the urls object
url_tester(urls_l, type = "result") 

## Example cont.:
url_tester <- function(url_list){
  url_list %>%
    # Map a version of GET() that would otherwise return NULL 
    map( safely(GET, function(x) {read_lines(x)})) %>%
    # Set the names of the result
    set_names( urls ) %>%
    # Remove the NULL
    discard(is.null) %>%
    # Extract all the "status_code" elements
    map("status_code" )
}

# Try this function on the urls object
url_tester(urls)


# Great! We have seen in this chapter how to write
# custom functions which can help you when doing data 
# analysis: for example, 
# it's crucial when you are doing web scraping,
# to ensure that the urls you want to scrape are reachable.
# Now you now how to do this ;)














