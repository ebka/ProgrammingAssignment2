##makeCacheMatrix
##This function creates a special "vector" object that can cache its inverse.
##It is a list containing a function to
##1 set the value of the vector
##2 get the value of the vector
##3 set the value of the mean
##4 get the value of the mean

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL    		   # initialise inv to NULL
  set <- function(y) { # function to save y
    x <<- y            #copies y into var x 
    inv <<- NULL			 # reinitialise inv to NULL - to erase previous output
  }
  
  get <- function() x			# returns the input matrix
  set_inverse <- function(inverse) inv <<- inverse  # this is to set the inversed matrix
  get_inverse <- function() inv		      # get_inverse returns the inversed matrix
  list(
    set=set, 				            # to set the matrix
    get=get, 				            # to get the matrix
    set_inverse=set_inverse, 		# to set the inversed matrix
    get_inverse=get_inverse			# to get the inversed matrix
  )
}



##cacheSolve
##This function calculates the mean of the special "vector" created in makeCacheMatrix function.
##It checks if the mean is calculated
##If mean is calculated, get mean from cache and skip computation
##If mean is not calculated, function to calculate the mean and
## sets the value of mean in cache in this function

cacheSolve <- function(x, ...) {
  inv <- x$get_inverse()		# get the inversed matrix from object x
  if(!is.null(inv)) {			  # is NULL if not calculated
    message("retrieve cached data.") # returns message if output is there
    return(inv)			      # output calculated inversion
  }
  data <- x$get()			    # if not calculated, then x$get to get the matrix object
  inv <- solve(data)			# calculate mean
  x$set_inverse(inv)			# sets the value of mean
  inv					            # output the result
}

#test run
## > x = rbind(c(1, -1/4), c(-1/4, 1))
## > m = makeCacheMatrix(x)
## > m$get()
##       [,1]  [,2]
## [1,]  1.00 -0.25
## [2,] -0.25  1.00
## No cache in the first run
## > cacheSolve(m)
##           [,1]      [,2]
## [1,] 1.0666667 0.2666667
## [2,] 0.2666667 1.0666667

## Retrieving from the cache in the second run
## > cacheSolve(m)
## retrieve cached data.
##           [,1]      [,2]
## [1,] 1.0666667 0.2666667
## [2,] 0.2666667 1.0666667

