
## My first function will create a matrix that gets stored in the cache and can store it's inversed matrix if possible.
## The second function will use the matrix stored in the cache of the first function and then use it to inverse it and show it.


makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL   ##initializing my variable as null
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function(){x}      ##Function to get the matrix
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv  ##Function to obtain the inverse
  list(set = set, get = get,     ##Generate a list of the variables ready to get use in the cache
       setInverse = setInverse, 
       getInverse = getInverse)
}


## This function will be used to obtain the cache data

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()    ##grab the data in the cache
  if(!is.null(inv)){         ##validation of null data
    message("getting cached data")
    return(inv)            ## gives the inverse value
  }
  data <- x$get()
  inv <- solve(data,...)
  x$setInverse(inv)
  inv   ##return the inverse matrix calculated
}
