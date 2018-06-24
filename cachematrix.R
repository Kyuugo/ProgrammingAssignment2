## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## Creates a matrix object with a cache for its inverse
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(){
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) m <<- inverse
        getInverse <- function() m
        list(set = set, get = get,
                setInverse = setInverse,
                getInverse = getInverse)
}


## Write a short comment describing this function
## Returns the inverse of a matrix, and, if it's already calculated, then return the cached value
## Using the vector function as basis, the only thing to change is the function names and how they work
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInverse()
        if(!is.null(m)){
                return(m)
        }

        data <- x$get()
        m <- solve(data)
        x$setInverse(m)
        m
}

