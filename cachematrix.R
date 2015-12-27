## Put comments here that give an overall description of what your
## functions do
## There are two functions in this script:
## makeCahceMatrix() will cache the inverse of a matrix using R solve function.
## cacheSolve() will retrieve the cache of the inverse of a matrix. If the cache
##   does not exist, it will calculate and cache the inverse.
## example:
## t <- matrix(c(1, 2, 3, 0, 4, 5, 1, 0, 6), nrow=3, ncol=3, byrow=TRUE)
## t1 <- makeCacheMatrix(t)
## t_inv <- cacheSolve(t1)
## t_inv
##           [,1]        [,2]        [,3]
##[1,]  1.0909091 -0.54545455 -0.09090909
##[2,]  0.2272727  0.13636364 -0.22727273
##[3,] -0.1818182  0.09090909  0.18181818

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      set <- function(y) {
          x <<- y
          m <<- NULL
      }
      get <- function() x
      setsolve <- function(solve)  m <<- solve
      getsolve <- function() m
      list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}
