## Matrix inversion is usually a costly computation and there may be 
## some benefit to caching the inverse of a matrix rather than compute
## it repeatedly (there are also alternatives to matrix inversion that 
## we will not discuss here). Your assignment is to write a pair of 
## functions that cache the inverse of a matrix.

## makeCacheMatrix creates a special "matrix",  containing a function to
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse of the matrix
## get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## calculates the inverse of the special "matrix" created with
## the above function. However, it first checks to see if the 
## mean has already been calculated. If so, it gets the mean 
## from the cache and skips the computation. Otherwise, it 
## calculates the mean of the data and sets the value of the 
## mean in the cache via the setmean function.
cacheSolve <- function(x, ...) {
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
## Test 
# m1<-matrix (c(4,2,7,6),nrow=2, ncol=2)
# m2<- makeCacheMatrix(m1)
# m3<- cacheSolve(m2)

# m4<- makeCacheMatrix(matrix (c(1,1,1,3,4,3,3,3,4),nrow=3, ncol=3))
# m5<- cacheSolve(m4)# second time you run this line, expect "getting cached data"
#
