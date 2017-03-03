## The two functions below create a special object 
## that stores a matrix and cache's its inverse.

## Create a special "matrix" to set and get the value of   
## the matrix and to set and get the value of the inverse


makeCacheMatrix <- function(x = numeric()) {
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


## Check to see if the inverse has already been calculated  
## and, if not, calculate the inverse of the special "matrix"    
## created with makeCachMatrix.


m1 <- makeCacheMatrix(mat)

CacheSolve <- function(x, ...) {
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

mat <- matrix(c(4,2,7,6),2,2)
mat
m1 <- makeCacheMatrix(mat)
CacheSolve(m1)