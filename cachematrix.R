## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# makeCacheMatrix creates a vector that is really just a list that contains a function
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        # set the value of the vector
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        # get the value of the vector
        get <- function() x
        # set the value of the inv matrix
        setinvmatrix <- function(invmatrix) m <<- invmatrix
        # get the value of the inv matrix
        getinvmatrix <- function() m
        list(set = set, get = get,
             setinvmatrix = setinvmatrix,
             getinvmatrix = getinvmatrix)
}


## Write a short comment describing this function
# cacheSolve calculates the inverse matrix of the special vector 
# that was created with the above function
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinvmatrix()
        # checks to see if the inverse matrix has already been calculated
        if(!is.null(m)) { # get the inverse matrix from the cache and skip the computation
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        # calculates the inverse matrix of the data 
        m <- solve(data, ...)
        # and sets the value of the inverse matrix in the cache via the setinvmatrix function
        x$setinvmatrix(m)
        m
}

