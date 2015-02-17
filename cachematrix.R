## These two functions cache the inverse of a matrix.  The cached inverse
##  of the matrix can be reused without repeatedly computing it.

## This first function creates a special "matrix" object that can 
## cache the inverse.

makeCacheMatrix <- function(cache_matrix = matrix()) {
        i <- NULL             
        set <- function(new_matrix) {               ## sets a newmatrix to the cache_matrix variable 
                cache_matrix <<- new_matrix     
                i <<- NULL                         ## and resets any cached inverse matrix "i" to NULL
        }
        get <- function() cache_matrix              ## gets the cached matrix
        setinverse <- function(solve) i <<- solve  ## sets the inverse of the cached matrix to variable "i"
        getinverse <- function() i                 ## gets the inverse matrix "i"
        list(set = set, get = get,                 ## list of functions for accessing the cached matrix and inverse
             setinverse = setinverse,
             getinverse = getinverse)
}



##  This second function computes the inverse of the special
##  "matrix" returned by `makeCacheMatrix` function above.  If the inverse matrix 
##  has already been computed, it will return the cached inverse for that matrix. 

cacheSolve <- function(cache_matrix, ...) {        ## Return the inverse of a matrix
        
        i <- cache_matrix$getinverse()             ## get the inverse of cachematrix
        if(!is.null(i)) {                         ## checks if "i" is NULL or is a cached inverse 
                message("getting cached data")    ## if "i" is a cached inverse, prints message
                return(i)                         ## skips the computation and returns the cached inverse
        }
        data <- cache_matrix$get()                 ## if "i" is NULL, computes the inverse of the matrix
        i <- solve(data, ...)
        cache_matrix$setinverse(i)                 ## caches the inverse of the matrix
        i
}

