## A pair of functions to store matrices and their inversions
##

## Returns a list of four functions that can store a matrix and its inversion
## set() and get() are accessors for the raw matrix
## A set will clear out a previously inverse 
## setInverse() and getInverse() are accessors for the stored inversion
##
## Note that calling set() will only clear out the previous inversion -
## It will NOT automatically reinvert

makeCacheMatrix <- function( matrix = matrix() ) {
    cachedInverse <- NULL
    set <- function( matrix.new ) {
        matrix <<- matrix.new
        cachedInverse <<- NULL
    }
    
    get <- function() matrix
    setInverse <- function( inverse.new ) cachedInverse <<- inverse.new
    getInverse <- function() cachedInverse
    list( set = set, get = get, setInverse = setInverse, getInverse = getInverse )
}


## Takes a matrix cache as created by makeCacheMatrix() above, and returns the inverse.
## If the inverse had been previously calculated and the raw matrix has not changed,
## we will simply return the cached inverse.
## Otherwise we calculate the inverse, cache it for future use, and return it
##
## Note that we assume that the cached raw matrix is invertible.
## Calling cacheSolve() with a non-invertible matrix will throw an error
cacheSolve <- function( matrixCache, ...) {
    inverse <- matrixCache$getInverse()
    
    if ( !is.null( inverse ) ) {
        message( "returning previously computed inverse" )
        return( inverse )
    }
    
    matrix <- matrixCache$get()
    inverse <- solve( matrix, ... )
    matrixCache$setInverse( inverse )
    inverse
}
