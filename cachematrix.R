## Taken together, these functions will cache the 
## inverse of a matrix so that it doesn't have to be
## calculated multiple times (e.g., in a loop).

## The makeCacheMatrix function creates a list of four
## functions which will be used in the second function.

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

## The cacheSolve function first checks to see if the
## inverse of the matrix has already been calculated. If
## if has, it will display the inverse. If it has not,
## it will calculate the inverse using the solve function,
## then display the inverse.

cacheSolve <- function(x,...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)                
        }
        data <- x$get()
        m <- solve(data)
        x$setinverse(m)
        m
}