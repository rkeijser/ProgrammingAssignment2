# makeCacheMatrix is a function that returns a list of functions
# Its puspose is to store a martix and a cached value of the inverse of the 
# matrix. Contains the following functions:
# 1. set        set the value of a matrix
# 2. get        get the value of a matrix
# 3. setmatrix  get the cahced value (inverse of the matrix)
# 4. getmatrix  get the cahced value (inverse of the matrix)

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        
        get <- function() x
        setmatrix <- function(solve) m <<- solve
        getmatrix <- function() m
        list(set = set, get = get,
             setmatrix = setmatrix,
             getmatrix = getmatrix)
}


# The following function calculates the inverse of a "special" matrix created with 
# makeCacheMatrix. It first checks to see if the inverse has already been calculated.
# If so, it gets the inverse of the matrix and skips the computation.
# Otherwise, it calculates the inverse of the matrix and sets the value of the inverse matrix
# in the cache via the setmatrix function.

cacheSolve <- function(x, ...) {
        m <- x$getmatrix()
        if(!is.null(m)) {
                message("getting cached matrix")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setmatrix(m)
        m
        
}
