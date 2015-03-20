## This function takes a square matrix from argument 'x',
## and return a list of elements-functions.
makeCacheMatrix <- function(x = matrix()) {
    ## set local variable 'cached_inverse' to NULL
    cached_inverse <- NULL
    ## the function assigns value 'y' (i.e 'x') to 'my_matrix'
    ## and value NULL to 'cached_inverse' defined previously
    set_matrix <- function(y) {
        my_matrix <<- y
        cached_inverse <<- NULL
    }
    ## the function returns the value of 'my_matrix'
    get_matrix <- function() my_matrix
    ## the function takes the value passed in (i.e function 'solve'),
    ## and the returned value replaces the one stored in 'cached_inverse'
    set_matrix_inverse <- function(solve) cached_inverse <<- solve
    # the function returns the value of 'cached_inverse'
    get_matrix_inverse <- function() cached_inverse
    # list of four functions
    list(set_matrix = set_matrix, get_matrix = get_matrix,
         set_matrix_inverse = set_matrix_inverse,
         get_matrix_inverse = get_matrix_inverse)
}

## This function evaluates the argument 'x' (i.e a matrix), calls for
## elements-functions from the list returned by makeCacheMatrix
## and return the inverse of matrix cached in 'cached_matrix'.
cacheSolve <- function(x, ...) {
    ## if 'cached_inverse' already exists, returns the message
    ## and value
    cached_inverse <- x$get_matrix_inverse()
    if(!is.null(cached_inverse)) {
        message("getting cached data")
        return(cached_inverse)
    }
    ## otherwise calculates inverse and stores it in 'cached_inverse'
    data <- x$get_matrix()
    cached_inverse <- solve(data, ...)
    x$set_matrix_inverse(cached_inverse)
    cached_inverse
}
