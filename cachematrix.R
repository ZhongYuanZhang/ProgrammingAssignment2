## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

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


## Write a short comment describing this function

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
