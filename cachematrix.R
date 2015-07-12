## Caching for matrix inversions

## makeCacheMatrix - Create a "special" matrix object that has the ability to cache its inversion

makeCacheMatrix <- function(x = matrix()) {
    inv_matrix <- NULL
    set <- function(y) {
        x <<- y
        inv_matrix <<- NULL
    }
    get <- function() x
    set_inverse <- function(inv) inv_matrix <<- inv
    get_inverse <- function() inv_matrix
    list(set = set, get = get,
         set_inverse = set_inverse,
         get_inverse = get_inverse)
}


## cacheSolve - invert the "special" matrix object, using the cached value if already computed

cacheSolve <- function(x, ...) {
    ## Check if we've alredy computed the inverse
    inv <- x$get_inverse()
    if(!is.null(inv))
    {
        # We already have the inverse, return that instead
        message("getting cached data")
        return(inv)
    }
    # We dont have a cached inverse, compute it
    mtx <- x$get()
    inv <- solve(mtx, ...)
    # cache the solution
    x$set_inverse(inv)
    inv
}
