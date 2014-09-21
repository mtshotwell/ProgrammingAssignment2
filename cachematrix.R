## R Programming Class Assignment #2. The two functions below help speed up the calculation of the inverse against a matrix. Returning a cached value when possible.


## Function sets and caches the inverse of a matrix 

makeCacheMatrix <- function(x = matrix()) {
        invrs <- NULL
        
        set <- function(y) {
                x <<- y
                invrs <<- NULL
        }
        get <- function() x
        
        setInvrs <- function(inverse) invrs <<- inverse
        getInvrs <- function() invrs
        list(set = set, get = get, 
             setInvrs = setInvrs,
             getInvrs = getInvrs)

}


## The following function will calculate the inverse of values in a matrix. It will check the "cache" to see if the solution already exists

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ## Return inverse of matrix, checking cache for solution first
        
        invrs <- x$getInvrs()
        
        # Check if invrs has already been solved
        
        if (!is.null(invrs)) {
                message("getting cached data")
                return(invrs)
        }
        
        matr1.data <- x$get()
        invrs <- solve(matr1.data, ...)
        
        x$setInvrs(invrs)
        return(invrs)
				
}
