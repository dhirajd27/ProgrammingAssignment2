makeCacheMatrix <- function(x = numeric()) {       ## Makes special Matrix
        m <- NULL
        set <- function(y) {                       ## Makes inverse Matrix
                x <<- y
                m <<- NULL
        }
        get <- function() x                        
        setsolve <- function(solve) m <<- solve    ## setsolves the function
        getsolve <- function() m                   ## getsolves the function
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}

cacheSolve <- function(x, ...) {
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")## gets the cached data and stores it
                return(m)                     
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)                      ## returns the saved data
        m
}
