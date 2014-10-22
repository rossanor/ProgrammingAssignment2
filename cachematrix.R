#----------------------------------------------------------------
# function makeCacheMatrix()
#
# It creates a matrix that allows to recover the inverse (function 'solve()')
# if it was alreary estimated
#
# arguments: x = matrix
#----------------------------------------------------------------
makeCacheMatrix <- function(x = matrix()) {
        INV <- NULL # set INV = NULL; matriz is being created
                    # 'x' stores the matriz in the environment inside the object 
        
        # functions inside object
        set <- function(y) {
                x <<- y
                INV <<- NULL
        }
        get <- function() x
        setSolve <- function(inv) INV <<- inv
        getSolve <- function() INV
        
        # return result
        list(set = set, get = get,
             setSolve = setSolve,
             getSolve = getSolve)
}


#----------------------------------------------------------------
# function cacheSolve()
#
# It estimates the inverse
#
# arguments: x = object createde by makeCacheMatrix() 
#----------------------------------------------------------------
cacheSolve <- function(x, ...) {
        # Return a matrix that is the inverse of 'x'
        inv <- x$getSolve()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setSolve(inv)        
        inv  # return result
}
