## The following function creates a special "matrix", which is really a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse matrix
## 4. get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        ## initialize the value of the inverse matrix to NULL
        inv <- NULL
        ## delcare another function set, where the value will be cached in
        set <- function(y) {
                x <<- y
                ## change the value of inverse of the matrix in case the matrix was changed
                inv <<- NULL
        }
        ## delcare another function get, that gets the value of the inverse
        get <- function() x
        ## calculates the inverse of non-singular matrix via the solve function
        setinverse <- function(inverse) inv <<- inverse
        ## gets the inverse 
        getinverse <- function() inv
        ## passes the value of the function makeCacheMatrix
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## The following function calculates the inverse matrix of the special "matrix" created with the above function

cacheSolve <- function(x=matrix(), ...) {
        inv <- x$getinverse()
        ## if the inverse exists, it gets it
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        #if the inverse if not there, first it is calculated and then retrieved
        data <- x$get()
        inv <- solve(data,...)
        x$setinverse(inv)
        inv
}
