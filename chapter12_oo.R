

# Sail the sea. Filling the missing peases for Base R
library(sloop)

otype(seq(1:3))
otype(mtcars)

# Theres a differens betw. base object and OO
is.object(1:10)
is.object(mtcars)

# The difference is that oo has a class
attr(1:10, "class")
attr(mtcars, "class")

# class Returns False and class
# use: s3_class

sloop::s3_class( matrix(1:4, nrow = 2))

# Every object has a base type
typeof( mtcars)
typeof( tibble(mtcars))


# S3 ----------------------------------------------------------------------

  
