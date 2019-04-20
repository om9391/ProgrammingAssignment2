## Following functions are used to create a special "matrix" object and cache the inverse of a matrix.

## makeCacheMatrix function creates a special "matrix", which is really a list containing a function to:
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse
## get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setmatrix <- function(solve) m <<- solve
        getmatrix <- function() m
        list(set = set,
             get = get,
             setmatrix = setmatrix,
             getmatrix = getmatrix)
}


## The following function calculates the inverse of the special "matrix" created with the above function. 

cacheSolve <- function(x, ...) {
        ## check if the inverse has already been calculated. 
        m <- x$getmatrix()
        
        ## if inverse matrix already calculated, then get it from the cache and skip the computation. 
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        
        ## otherwise, calculate the inverse matrix and set the value of inverse matrix in the cache via the setmatrix function.
        data <- x$get()
        m <- solve(data, ...)
        x$setmatrix(m)
        m
}
