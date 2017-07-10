
    ## These helper functions can compute and record the inverse of a given matrix and cache it in its memory.
    
    ## The makeCacheMatrix function creates an environment that can cache a matrix's inverse.
    
    makeCacheMatrix <- function(x = matrix()) {
                inv <- NULL
                set <- function(y) {
                        x <<- y
                        inv <<- NULL
                    }
                get <- function() x
                setInverse <- function(inverse) inv <<- inverse
                getInverse <- function() inv
                list(set = set,
                               get = get,
                               setInverse = setInverse,
                               getInverse = getInverse)
    }
    
        ## The cacheSove function retrieves the inverse of a given matrix from cache if it was already created.
        
        cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
                    inv <- x$getInverse()
                    if (!is.null(inv)) {
                            message("getting cached data")
                            return(inv)
                        }
                    matrix <- x$get()
                    inv <- solve(matrix, ...)
                    x$setInverse(inv)
                    inv
        }