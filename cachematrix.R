## a pair of functions that cache the inverse of a matrix.

## This function creates a special "matrix" object that 
## can cache its inverse.

makeCacheMatrix = function(x = matrix()) {
  Inver = NULL
  set = function(y){
    x <<- y
    Inver <<- NULL
  }
  get = function() x
  setSolve = function(Inver2) Inver <<- Inver2
  getSolve = function() Inver
  return(list(set = set, get = get,
              setSolve = setSolve, 
              getSolve = getSolve))
}


## computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been 
## calculated (and the matrix has not changed), then the cachesolve 
## should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  Inver = x$getSolve()
  if (!is.null(Inver)){
    message('getting cached data')
    return(Inver)
  }
  matrix = x$get()
  Inver = solve(matrix,...)
  x$setSolve(Inver)
  Inver
}
