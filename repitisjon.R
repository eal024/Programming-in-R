

x <- seq( from = 0, to = 3, by = 0.01)

# The normal way
mean((round(x, digits = 1))^2)




# compose way
round_and_then_mean <-  compose( mean,
                                 as_mapper(~.x^2),
                                 as_mapper(~round(.x, digits = 1)) 
                                 )


mean((round(x, digits = 1))^2)
round_and_mean(x)

test_liste <- list( x = x, y = seq(1:1000), z = seq(from = 1.1, to =20212.31, by = 0.1) )


map_dbl( test_liste, function(x) {round_and_mean(x)/100}) 



mtcars <- as_tibble(mtcars)


iris %>% 
  group_by(Species) %>% 
  nest( ) %>% 
  mutate( descri = purrr::map(data , function( x ) {x %>% summarise( sum = sum(x$Sepal.Length, na.rm = T),
                                                                     mean = mean(x$Sepal.Length, na.rm = T),
                                                                     sd = sd(x$Sepal.Length, na.rm =T))  }  )
  ) %>% 
  unnest(descri)