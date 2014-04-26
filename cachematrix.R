## These two functions work together to inverse a matrix and cache it's value

## The first function expects a matrix as an argument and creates a special matrix 
## The function can set and get the matrix (x), and set and get its inverse (inv)

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInverse <- function(solve) inv <<- solve
        getInverse <- function() inv
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## The 2nd function checks if the inverse has already been calcualted
## and then returns the cached value.  If the inverse is not cached, 
## the solve function is used to compute the inverse of the matrix and the 
## value is cached.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setInverse(inv)
        inv
}
