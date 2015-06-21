## caching the inverse of a matrix

## makeCacheMatrix function creates a special "matrix" object that can cache its inverse.
#1.  set the value of the matrix
#2.  get the value of the matrix
#3.  set the value of the inverse
#4.  get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinv <- function(solve) i <<- solve
        getinv <- function() i
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}


# cacheSolve: Compute the inverse of the matrix. If the inverse is already
cacheSolve <- function(x, ...) { 
        y<- x$getinv()
        
        if(!is.null(y)) 
        {
                message("...Getting cached data...")
                return(y)
        }
        
        data <- x$get()
        y<- solve(data, ...)
        
        x$setinv(y)
        
        y
        
}