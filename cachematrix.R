## makeCacheMatrix creates an object that stores a matrix and its inverse
## cacheSolve retrieves the Inverse of a matrix from cache if it exists. 
## Otherwise it will compute the inverse and store it in cache

## this function stores a matrix and its inverse internally 
## and allows getting or setting these values through 4 functions (returned as a list)
makeCacheMatrix <- function(x = matrix()) {
    
    ## initialize empty inverse matrix
    InvMatrix <- NULL
    ## function to store a matrix internally
    set <- function(y) {
        x <<- y
        InvMatrix <<- NULL
    }
    ## function that returns the original matrix
    get <- function() x
    ## function that sets the Inverse matrix (computed by a different function)
    setInverse <- function(InvertedMatrix) InvMatrix <<- InvertedMatrix
    ## function that returns the inverse matrix
    getInverse <- function() InvMatrix
    ## list of functions through which the matrix and inverse matrix are accessed
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)    

}


## Function that retrieves an inverse matrix of a given Special matrix 'makeCacheMatrix' x
## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {

    ## get the Inverse Matrix from the makeCacheMatrix x.       
    InvertedMatrix <- x$getInverse()
    
    ## if the inverse matrix already exists, display a message and return the inverse matrix.
    if(!is.null(InvertedMatrix)) {
        message("getting cached data")
        return(InvertedMatrix)
    }

    ## if the inverse matrix does not exist, get the original matrix and compute the inverse.
    data <- x$get()
    InvertedMatrix <- solve(data)

    ## add the inverse matrix to the cache
    x$setInverse(InvertedMatrix)
    ## return the inverted matrix.
    InvertedMatrix
}
