## These two functions cache the inverse of a matrix.  The cached inverse
##  of the matrix can be reused without repeatedly computing it.

## This first function creates a special "matrix" object 

makeCacheMatrix <- function(cache_matrix = matrix()) {
        i <- NULL             
        set <- function(new_matrix) {               ## When a new matrix is entered, set invalidates the cache 
                cache_matrix <<- new_matrix         ##  by changing Cache_matrix with new_matrix.   
                i <<- NULL                         ## and resets any cached inverse matrix "i" to NULL,
        }
        get <- function() cache_matrix             ## returns the cached matrix
        setinverse <- function(solve) i <<- solve  ## caches the inverse of the cache_matrix
        getinverse <- function() i                 ## returns the cached inverse matrix "i"
        list(set = set, get = get,                 ## list of functions for accessing the cached matrix and inverse
             setinverse = setinverse,
             getinverse = getinverse)
}



##  This second function computes the inverse of the special
##  "matrix" returned by `makeCacheMatrix` function above.  If the inverse matrix 
##  has already been computed, it will return the cached inverse for that unchanged matrix. 

cacheSolve <- function(cache_matrix, ...) {        ## Return the inverse of a matrix
        
        i <- cache_matrix$getinverse()             ## pulls the inverse of cache_matrix and set to variable i
        
        if(!is.null(i)) {                         ## checks if "i", the inverse, exists.  
                message("getting cached data")    ## If TRUE, prints message, and
                return(i)                         ## skips the computation, returns the cached inverse
        }
        data <- cache_matrix$get()                 ## if "i" is FALSE (NULL), pulls the cached matrix(cache_matrix)
        i <- solve(data, ...)                      ## and computes the inverse of cache_matrix
        cache_matrix$setinverse(i)                 ## caches the inverse of cache_matrix
        i                                          ## returns "i"
}

