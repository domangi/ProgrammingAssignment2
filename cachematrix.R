## makeCacheMatrix(X = matrix()) returns a list of functions that can be uses to set or get the matrix and to set and get the cached matrix inverse
## if X is passed as parameter, then the matrix is initialized with it, else an empty matrix is used
## cacheSolve(x) returns the inverse of the matrix actually setted on x
## if inverse cached, then just returns cached value, else calculates and caches the inverse first
## x is the list of functions return by makeCacheMatrix
##
## Test Code
## 1) create a 2x2 invertible matrix
## > A <- matrix(c(1,2,2,3), 2, 2)
##       [,1] [,2]
## [1,]    1    2
## [2,]    2    3
##
## 2) call makeCacheMatrix to get the special matrix
## > X <- makeCacheMatrix(A)
##
## 3) call cacheSolve to get the inverse of the matrix
## > inverse <- cacheSolve(X)
##       [,1] [,2]
## [1,]   -3    2
## [2,]    2   -1
##
## 4) Test that the inverse is correct: A * inverse = I
## > A %*% inverse
##
## 5) Call cacheSolve again should print the message "getting cached data"



## params
## -> (optional) x should be a squared and invertible matrix. Sets the actual matrix.
##
## returns a list of functions 
## -> set(y): sets the matrix to y. Resets inverse cache.
## -> get: returns the actual matrix
## -> setinverse(inverse): caches the inverse of the matrix
## -> getinverse: returns the cached matrix inverse
##
## example
## > X <- makeCacheMatrix(matrix(c(1,2,2,3), 2, 2))
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
##
## example
## > X <- makeCacheMatrix(matrix(c(1,2,2,3), 2, 2))
## > inverse <- cacheSolve(X)
## > matrix(c(1,2,2,3), 2, 2) %*% inverse # should return the 2x2 identity matrix
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