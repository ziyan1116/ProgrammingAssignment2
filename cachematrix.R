## These 2 functions are written to cache the inverse of a matrix without repeat computation.

## The first function, makeCacheMatrix, aims to create a special "matrix" object that can cache its inverse,
## and it returns the list containing a function to
##   1.  set the value of the matrix
##   2.  get the value of the matrix
##   3.  set the value of the inverse
##   4.  get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        s<-NULL
        set<-function(y=matrix()){
                x <<- y
                s <<- NULL
        }
        get<-function() x
        setinverse <- function(solve) s <<- solve(x)
        getinverse <- function() s
        list(get=get,set=set,
             setinverse=setinverse,
             getinverse=getinverse)
}


## The second function returns the inverse of the special "matrix" created with the  above function.
## However, it first checks to see if the inverse has already been calculated. 
## If so, it gets the inverse from the cache and skips the computation.
## Otherwise, it calculates the inverse of the "matrix" and sets the value of the inverse in the cache via the setinverse function.


cacheSolve <- function(x, ...) { s <- x$getinverse()
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        data <- x$get()
        s <- solve(data, ...)
        x$setinverse(s)
        s
}
