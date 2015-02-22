##The two functions programmed below will respectively create and fill a matrix which is the inverse of the input matrix 'x'

##The first function, "makeVector" creates a special "matrix", which can store the inverse of a specific matrix 'x'.
#1.  set the value of the matrix
#2.  get the value of the matrix
#3.  set the value of the inverse
#4.  get the value of the inverse


makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
        
}


## "cacheSolve" calculates the inverse of the special "matrix" returned by the function "makeCacheMatrix" as was created above. 
## If the inverse was calculated before (and the matrix is unchanged), then"cacheSolve" retrieves the inverse from the cache.

## Note: assumption in this function is that the matrix at hand is always invertable. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m<- x$getinverse ()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m<- solve(data, ...)
        x$setinverse(m)
        m
        
}
