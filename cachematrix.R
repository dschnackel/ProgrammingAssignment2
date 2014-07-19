## The two functions below work to create a special object
## that caches the inverse of a given matrix. Once the inverse 
## of a give matix is cached the cacheSolve function returns the
## cached inverse value instead of recalulating the inverse.

## This function creates a list object that contains the original
## matrix, it's inverse and exposes the (get,set,getinv,setinv) functions.
## x$get()      returns the original matrix
## x$getinv()   returns the inverse matrix
## x$set()      sets the matrix value
## x$setinv()   sets the inverse matrix value

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv <- function(inv) m <<- inv
        getinv <- function() m
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## This function performs the creation of the inverse matrix
## and facilitates the return of a cached inverse value if 
## one has already been calculated and cached.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m
}
