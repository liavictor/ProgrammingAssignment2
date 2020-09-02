## The functions for "Caching the Inverse of a Matrix" may reduce costly matrix inversion computations 
## by returning the cached data instead of compute it repeatedly. 

## The makeCacheMatrix function creates a special "matrix" object that can cache its inverse.

## 1.set the value of the matrix
## 2.get the value of the matrix
## 3.set the value of the inverse
## 4.get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set,
             get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## The cacheSolve function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if (!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}

## test case 1 
m <- matrix(1:4, 2, 2)
mc <- makeCacheMatrix(m)
cacheSolve(mc) 
cacheSolve(mc) 

## test case 2
m <- matrix(c(1,2,3,4,5,6,8,7,9), 3, 3)
mc <- makeCacheMatrix(m)
cacheSolve(mc) 
cacheSolve(mc) 

## test case 3 
m <- matrix(c(3,5,4,2,7,9,7,6,2), 3, 3)
mc <- makeCacheMatrix(m)
cacheSolve(mc) 
cacheSolve(mc)

## test case 4 
x <-makeCacheMatrix(matrix(c(1,0,0,0,1,0,0,0,2), 3, 3))
cacheSolve(x) 
cacheSolve(x)


