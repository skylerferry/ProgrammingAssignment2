## The functions makeCacheMatrix and cacheSolve work together to solve the inverse of an invertible 
## matrix. The main action of inverting a matrix can be done by using the solve() function, these
## functions were written to demonstrate caching functionality and how caching can save time 
## by storing values for operations rather than having to re-execute often cumbersome functions

## makeCacheMatrix creates a special matrix that is a list with componenets that set and get the 
## values of the matrix and set and get the values of the matrix's inverse

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


## cacheSolve computes the inverse of the special matrix returned by makeCacheMatrix. In the case
## that the inverse has already been computed, cacheSolve will print "getting cached data" and return
## the matrix m that is stored in makeCacheMatrix. Otherwise, cacheSolve will compute the inverse and then 
## store that information back into makeCacheMatrix to be used later if needed

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
        ## Return a matrix that is the inverse of 'x'
}
