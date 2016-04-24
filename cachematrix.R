## Caching the Inverse of a Matrix:
## Matrix inversion is usually a costly computation and there may be some benefit to caching
## the inverse of a matrix rather than compute it repeatedly. 
## Below are two function that help to create a special object that can be stored and called later.
## These two functions specifically look to creat a matrix and store its inverse, so it can be 
## called in the future.  If the matrix changes values the function will than calculate 
## the function

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
                invs <- NULL
                set <- function(y) {
                        x <<- y
                        invs <<- NULL
                }
                get <- function() x
                setinverse <- function(solve) invs <<- solve
                getinverse <- function() invs
                list(set = set, 
                     get = get,
                     setinverse = setinverse,
                     getinverse = getinverse)
        }

## This Function looks to see if the inverse has already been calculated and stored, and
## if it has provides a message with the inverse, if it hasn't it goes aheads and calculates

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}

