## Matrix inversion is usually a costly computation and there may be some benefit 
## to caching the inverse of a matrix rather than compute it repeatedly 

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)

}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data.")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setinverse(inv)
        inv
}

## Usage example:
## x <- matrix(1:4, nrow=2, ncol=2)
## m <- makeCacheMatrix(x)
## s <- cacheSolve(m)
## print(s)
## s should return:
##     [,1] [,2]
##[1,]   -2  1.5
##[2,]    1 -0.5
##
## s2 <- cacheSolve(m)
## This should display a "Getting cached matrix" message
## print(s2)
## s2 should return
##     [,1] [,2]
##[1,]   -2  1.5
##[2,]    1 -0.5
