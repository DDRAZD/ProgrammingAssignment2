## Put comments here that give an overall description of what your
## functions do

##The first function, makeCacheMatrix creates a special "matrix", which is 
##really a list containing a function to

## set the value of the mattix
## get the value of the mattix
## the value of the inverted matrix
## get the value of the inverted matrix




makeCacheMatrix <- function(x = matrix()) {
        inverted_matrix <-NULL
        set <- function(y) {
                x               <<- y
                inverted_matrix <<- NULL
        }
        get <- function() x
        setinverted<- function(inverted) inverted_matrix <<- inverted
        getinverted <- function() inverted_matrix
        list(set = set, get = get,
             setinverted = setinverted,
             getinverted = getinverted)

}


## Write a short comment describing this function

##The following function calculates the inverted matrix of the special "matrix"
##created with the above function. However, it first checks to see if the inverted 
##matrix has already been calculated. If so, it gets the inverted matrix from the
##cache and skips the computation. Otherwise, it calculates the inverted matrix 
##of the data and sets the value of the inverted matrix in the cache via the
##setinverted function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        inverted_matrix <- x$getinverted()
        if(!is.null(inverted_matrix)) {
                message("getting cached data")
                return(inverted_matrix)
        }
        data <- x$get()
        inverted_matrix <- solve(data, ...)
        x$setinverted(inverted_matrix)
        inverted_matrix
}
