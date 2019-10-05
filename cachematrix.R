## If inputing a reversible matrix, the functions will calculate its inverse and 
## cache it.
## If the input matrix has been calculated once, the function will retrieve its 
## inverse from the cache instead of calculate it again.

## This function returns a list of functions that are used to:
## 1.set the input matrix. 2.get the input matrix. 3.cache the calculated inverse matrix
## 4.get the calculated inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function firstly check if the inverse of the input matrix has been calculated.
## If so, the function will return the calculated inverse matrix as the result.
## If not, the function will calculate the inverse and cache it by using the function above.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("Retrieving cached inverse")
                return(inv)
        }
        x_matrix <- x$get()
        inv <- solve(x_matrix, ...)
        x$setinverse(inv)
        inv
}
