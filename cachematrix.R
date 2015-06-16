# The `makeCacheMatrix` and `cacheSolve` functions implement
# a method to cache the inverse of a matrix. `makeCacheMatrix(mat)`
# is a wrapper to a matrix `mat` that stores the cached inverse.
# `cacheSolve` then behaves like regular `solve(mat)` on the wrapper,
# calculating the inverse if it has not yet been calculated, or
# retrieving it from the cache if it has.
# 
# Create a wrapper to a square numeric/complex matrix `mat` as:
#       x <- makeCacheMatrix(mat)
# To get the matrix use:
#       mat <- x$get()
# To get the matrix inverse use:
#       mat_inv = cacheSolve(x)
# To change the matrix stored in x use:
#       x$set(new_mat)
# The inverse stored in the cache will be updated the next
# time `cacheSolve` is called on x.

# Wrapper function for a numeric/complex square matrix
# that allows the inverse of the matrix to be cached.
makeCacheMatrix <- function(x = matrix()) {
    x_inv <- NULL
    set <- function(y) {
            x <<- y
            x_inv <<- NULL
    }
    get <- function() x
    setInv <- function(inverse) x_inv <<- inverse
    getInv <- function() x_inv
    list(set = set, get = get,
         setInv = setInv,
         getInv = getInv)
}


# Function to retrieve the matrix inverse from a numeric/
# complex square matrix stored in the `makeCacheMatrix` 
# wrapper.
cacheSolve <- function(x) {
    inverse <- x$getInv()
    if(!is.null(inverse)) {
            message("getting cached data")
            return(inverse)
    }
    mat <- x$get()
    # note that we do not allow the `b` or `...` arguments to be passed
    # to solve(a, b, ...) as we only want the inverse of the matrix.
    inverse <- solve(mat) 
    x$setInv(inverse)
    inverse
}
