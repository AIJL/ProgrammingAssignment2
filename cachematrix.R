## These functions have been written to complete the  R - Programming Week 3 Assignment.

# The first function, makeVector creates a special "vector" 
## which is really a list containing a function to:
## *set the value of the vector
## *get the value of the vector
## *set the value of the mean
## *get the value of the mean

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y                             
    inv <<- NULL                        
  }
  get <- function() x         
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, 
       get = get, 
       setinverse = setinverse, 
       getinverse = getinverse) 
}

# The following function calculates the inverse of the special "vector" created with the above function 

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}