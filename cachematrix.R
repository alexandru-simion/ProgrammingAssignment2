## These functions are the solution for the 2nd Coursera Programming Assignement in R.
## The assignement is to first build a function that creates "special matrix object 
## that can cache its inverse". 
## The second part of the assignement asks for a second function that
## "computes the inverse of the special "matrix" returned by the perious function.
## If the inverse has already been calculated (and the matrix has not changed), 
## then this second function should retrieve the inverse from the cache."
## The first function should be called "makeCacheMatrix" and the second "cacheSolve".



## Following the example given in the assignement's description, this function
## creates a special "vector", which is a list containing a function to: set a value
## of the vector, get the value of the vector, set the value of the inverse, get 
## the value of the inverse.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<-NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}



## The following function calculates the mean of the special "vector" created 
## with the above function. However, it first checks to see if the mean has already 
## been calculated. If so, it gets the mean from the cache and skips the computation. 
## Otherwise, it calculates the mean of the data and sets the value of the mean in 
## the cache via the setmean function.Write a short comment describing this function


cacheSolve <- function(x, ...) {
    inv <-x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data.")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setinverse(inv)
    inv
}