## This small library allows caching matrix along with its inversed
## version. Since inversing a matrix is time consuming, for large
## matrices where inversed matrix is accessed often, it is wise to cache
## result.

## This function returns wrapper object for matrix type and caches 
## original matrix in it.

makeCacheMatrix <- function(x = matrix()) {

    ## set internal cache to NULL
    inverse <- NULL

    ## set new value of matrix
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }

    ## get new value of matrix
    get <- function() x

    ## set inverse matrix cache
    setInverse <- function(inv) inverse <<- inv

    ## retrieve inverse matrix from cache
    getInverse <- function() inverse
    
    ## return accessors
    list(get = get,
         set = set,
         setInverse = setInverse,
         getInverse = getInverse)
}

## This function returns inverse of the matrix. If it was already 
## calculated, cached version will be returned

cacheSolve <- function(x, ...) {
        
    ## check if there is cached inversed matrix of x
    inverseMatrix <- x$getInverse()
    if(!is.null(inverseMatrix)) return(inverseMatrix)
    
    ## calculate and cache inverse of x
    inverseMatrix <- solve(x$get(), ...)
    x$setInverse(inverseMatrix)

    ## Return a matrix that is the inverse of 'x'
    inverseMatrix
}
