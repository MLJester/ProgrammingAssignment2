## At the highest level these function store the inverse of a matrix in cache
## so that it can be retrieved from memory instead of being recalculated each
## time it is needed.


## The function below is setting up a list of 4 items which are read, and
##         in some cases changed by cacheSolve.
##         set - assigns values to the functions parent environment
##         get - the matrix
##         setinverse - the solve function to calculate the inverse matrix
##         getinverse - set to null since the inverse has not be calculated

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) inv <<- solve
        getinverse <- function() inv
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## The function below is passed the list created by makeCacheMatrix. It then 
##         tests the list item 'getinverse' to see if the inverse has already
##         been calculated. If 'getinverse' is not null, a message is 
##         displayed on the console followed by the inverse of the matrix
##         passed to makeCacheMatrix. Otherwise, it calculates the inverse and
##         stores the value in 'setinverse' before returning the inverse.

cacheSolve <- function(x, ...) {
        inv<-x$getinverse()
        if(!is.null(inv)) {
                message('getting cached data')
                return(inv)
        }
        data<-x$get()
        inv<-solve(data)
        x$setinverse(inv)
        inv
}
