## The function gets a matrix and the output is an invertion matrix through the solve() function
## 

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL             #set a "bucket" variable initially empty
  set <- function(y) {
    x <<- y             # "y" just a variable for the argument of the function above
    m <<- NULL          # the <<- operator assign a new value to "m" (that is set to NULL)  
  }
  get <- function() x    # use to see the actual value of the variable
  setInv <- function(solve) m <<- solve  # calculates the invertion matrix of the input throught the solve() function
  getInv <- function() m #return the invertion matrix
  list(set = set, get = get, setInv = setInv, getInv = getInv) #set the list of parameters in the makeCacheMatrix() function
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getInv() # assign to "m" the value of the getInv vector
  if(!is.null(m)) { # in this conditional statement, if the value of M is different from NULL (!is.null means different from TRUE)
    message("getting cached data") # a message of "reading the cache" is returned
    return(m) # the actual value of m is returned
  }
  data <- x$get() #if the value of m is NULL, then the invertion matrix is calculated "de novo"
  m <- solve(data, ...)
  x$setInv(m)
  m # the m value is returned
}
