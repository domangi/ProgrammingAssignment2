## makeCacheMatrix(X = matrix()) returns a list of functions that can be uses to set or get the matrix and to set and get the cached matrix inverse
## if X is passed as parameter, then the matrix is initialized with it, else an empty matrix is used
## cacheSolve(x) returns the inverse of the matrix actually setted on x
## if inverse cached, then just returns cached value, else calculates and caches the inverse first
## x is the list of functions return by makeCacheMatrix


## params
## -> (optional) x should be a squared and invertible matrix. Sets the actual matrix.
##
## returns a list of functions 
## -> set(y): sets the matrix to y. Resets inverse cache.
## -> get: returns the actual matrix
## -> setinverse(inverse): caches the inverse of the matrix
## -> getinverse: returns the cached matrix inverse
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


## params
## -> x is the list of functions returned by makeCacheMatrix
## -> ... are arbitrary parameters that could be passed to "solve" function
##
## returns the inverse of the matrix. 
## If inverse was already calculated, then it returns the cached copy, and prints a notice message "getting cached data"
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
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