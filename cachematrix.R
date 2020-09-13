##makeCacheMatrix first create a matrix that can cache its inverse and with CacheSolve function we can retrieve its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse<- function(inverse) m <<- inverse	
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}

## We ise <<- to assign values to an object different from our current environtment

cacheSolve <- function(x, ...) {

  
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  ##If inverse has already calculate we get it from cached and It's not necessary to calculate 
  
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
 
}

##If not, If not, It calculates the inverse and sets the value by setinv function
## Return a matrix that is the inverse of matrix on makeCachematrix