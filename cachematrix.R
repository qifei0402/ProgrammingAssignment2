## This function is able to cache the inverse of a matrix rather than compute it repeatedly.

## The following function is used to create a special "matrix", which is really a list
## containing a function to
## set the value of the matrix
## get the valuse of the matrix
## set the value of the inverse of the matrix
## get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## The following function comes out with the inverse matrix of the special "matrix" created the
## above function. It first checks to see if the inverse matrix has already been calculated. 
## If so, it get the inverse from the cache and skips the computation. Otherwise, it calculates
## the inverse matrix of the data and sets the value of the inverse in the cache 
## via setinverse function.

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}

