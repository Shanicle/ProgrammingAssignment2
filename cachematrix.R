## The two functions below will create a dummy matrix that can
## be used to cache the inverse of a matrix and will either calculate
## the inverse of a matrix or use a cached version of the result if
## the calculation was done previously

## This function will create the dummy matrix used for caching a result
## It will allow you to both set the values in the dummy matrix as well
## as retrieve the values from the dummy matrix. It is a set of 4 functions
## that will set and or retrieve the matrix values or compute the matrix

makeCacheMatrix <- function(x = matrix()) {
        ##Creates an empty dummy variable to store the cache
        m <- NULL

        ##Sets the value of the sent variable in the other called environment
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        
        ##Retrieves a variable
        get <- function() x
        
        ##Will calculate the inverse of the matrix
        setinverse <- function(solve) m <<- solve
        
        ##will get the cached inverse of the matrix that was already solved
        getinverse <- function() m
        
        ##returns the list of available functions
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
        

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        makeCacheMatrix()
        m <- getinverse()
        
        ##If there is something cached, send the cached values
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        
        ##This will only run if the cache is empty (NULL)
        ##Will first get the values for the matrix and then compute
        ##the inverse
        data <- x$get()
        m <- solve(data, ...)
        
        ##storing the calculated inverse as cache for later
        x$setinverse(m)
        m
}
