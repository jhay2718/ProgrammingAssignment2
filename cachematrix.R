## These two functions caculate the inverse of a matrix and then cache this value.  When the inverse is needed again the program checks
##the cache and returns the inverse if it is there and, if not, calculates this value and puts in cache 

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        inv<-NULL
        set<-function(y) {
                x<<-y
                inv<<-NULL
        }
        get<-function() x
        setinverse<-function(solve) inv <<- solve
        getinverse<-function() inv
        list(set=set, get=get,
             setinverse=setinverse,
             getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        inv<-x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        mat<-x$get()
        inv<-solve(mat,...)
        x$setinverse(inv)
        inv
}
