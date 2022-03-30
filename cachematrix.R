##    The functions use scoping rules of R to create a special object that
##      stores a matrix and caches its matrix

##    The first function creates a special matrix to set and get the elements of 
##    a matrix and sets and gets the elements of the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


##    The second function calculates the inverse of the special matrix above
##    by first checking if the inverse has been calculated,if so, it gets the 
##    inverse from the cache and skips the computation. Else, it calculates the
##    the inverse of the matrix and sets it in the cache.

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  matrix_to_invert <- x$get()
  inv <- solve(matrix_to_invert, ...)
  x$setinverse(inv)
  inv        
  
}
