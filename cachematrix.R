makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL  # Initialize inverse as NULL
    
    # Function to set the matrix
    set <- function(y) {
        x <<- y
        inv <<- NULL  # Reset cached inverse
    }
    
    # Function to get the matrix
    get <- function() x  
    
    # Function to set the inverse
    setInverse <- function(inverse) inv <<- inverse  
    
    # Function to get the inverse
    getInverse <- function() inv  
    
    # Return a list of the functions
    list(set = set, get = get, 
         setInverse = setInverse, 
         getInverse = getInverse)
}
cacheSolve <- function(x, ...) {
    inv <- x$getInverse()  # Get cached inverse if available
    
    # Check if inverse is already computed
    if (!is.null(inv)) {
        message("Getting cached data")
        return(inv)
    }
    
    # Otherwise, compute the inverse
    mat <- x$get()  # Retrieve the matrix
    inv <- solve(mat, ...)  # Compute the inverse using solve()
    
    # Cache the computed inverse
    x$setInverse(inv)
    
    return(inv)  # Return the computed inverse
}
