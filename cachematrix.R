## Functions to create a cache the inverse of a matrix
## 

## function to create a matrix object that can cache it's inverse

makeCacheMatrix <- function(x = matrix()) {
        
        m<-NULL
        
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        
        get <- function() x
        
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)

}


## Function to compute the inverse of the matrix

cacheSolve <- function(x, ...) {
        
        m <- x$getinverse()
        
        if(!is.null(m)){
                message ("getting cached data")
                return(m)
        }
        data <- x$get()
        m<-solve(data) %*% data
        x$setinverse(m)
        m
}
