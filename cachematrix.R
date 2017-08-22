## The two functions below cache the inverse of a matrix and cache it.
## the subsequent request for matrix inversion will be retrieved from cache


## makeCacheMatrix will craete a special matrix object that can cache its inverse 

makeCacheMatrix <- function(x = matrix()) {
        invMatrix <- NULL
        set <- function(y){
                x <<- y
                invMatrix <<- NULL
        }
        get <- function() x
        setMatrix <- function(matrix) invMatrix <<- matrix
        getMatrix <- function() invMatrix
        list(set = set, get = get, setMatrix = setMatrix, getMatrix = getMatrix)
}


## cacheSolve - This function will inverse the special matrix returned by makeCacheMatrix
## function from above

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        invMatrix <- x$getMatrix()
        if(!is.null(invMatrix)) {
                message("getting cached data")
                return(invMatrix)
        }
        data <- x$get()
        invMatrix <- solve(data)
        x$setMatrix(invMatrix)
        invMatrix
}
