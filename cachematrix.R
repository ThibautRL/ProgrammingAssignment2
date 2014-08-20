## Put comments here that give an overall description of what your
## 

##  creates a matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        get <- function() x
        setinvert <- function(solve) s <<- solve
        getinvert <- function() s
        list(set = set, get = get,
             setinvert = setinvert,
             getinvert = getinvert)
}


## computes inverse of the matrix returned by makeCacheMatrix
## If inverse has already been calculated, retrieve inverse from cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        s <- x$getinvert()
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        data <- x$get()
        s <- solve(data, ...)
        x$setinvert(s)
                s
}
