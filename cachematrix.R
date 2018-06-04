## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object 
## that can cache is inverse
makeCacheMatrix <- function(x = matrix()) {
    #starts the inversed value with NULL
    i <- NULL
    #set the matrix data
    set <- function(y){
        x <<- y
        i <<- NULL
    }
    #get the matrix data
    get <- function() x
    #set the computed inverse
    setinverse <- function(inverse) i <<- inverse
    #get the inversed matrix
    getinverse <- function() i
    #return the above functions as a list
    list(
        set = set,
        get = get,
        setinverse = setinverse,
        getinverse = getinverse)
    
}


## This function computes the inverse of the special "matrix"
## returned by `makeCacheMatrix`
cacheSolve <- function(x, ...) {
    ## get the inversed value of the matrix 'x'
    i <- x$getinverse()
    ## verify if is cached (not null)
    if (!is.null(i)){
        ##if is cached return the cached value
        message("getting cached data")
        return(i)
    }
    #not cached, then get the matrix data
    data <- x$get()
    #solve (inverse) the matrix
    i <- solve(data, ...)
    #cache the computed in the matrix
    x$setinverse(i)
    #return the inversed matrix
    i
}
