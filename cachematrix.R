## These two functions take the inverse of a matrix, but allow the user to
## store the inverse in a cache, so that if the inverse has already been calculated for a
## specific matrix, the function brings up that inverse from the cache, so that it does not
## have to calculate it again.
 
## This function stores the matrix in a cache, and eventually saves its inverse.
 
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}
 
 
## This function takes the cached matrix from the first function and calculates its inverse
## storing it back in the cache via the first function. If the inverse/matrix have already
## been stored and calculated this function simply retrieves the inverse from the cache and
## displays it.
 
cacheSolve <- function(x, ...) {
          m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
