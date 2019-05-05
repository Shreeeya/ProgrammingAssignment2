## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
	  invMatrix <- NULL
	
	#set the value of the Matrix
	  setMatrix <- function(y) {
	    x <<- y
	    invMatrix <<- NULL
	  }
	
	  getMatrix <- function() x                              #get the value of the Matrix
	  setInverse <- function(inverse) invMatrix <<- inverse  #set the value of the invertible matrix
	  getInverse <- function() invMatrix                     #get the value of the invertible matrix
	  list(setMatrix = setMatrix, getMatrix = getMatrix,
	       setInverse = setInverse, getInverse = getInverse)
	
	}

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        invMatrix <- x$getInverse()
	        if(!is.null(invMatrix)) {                       #if inverse matrix is not NULL
	          message("Getting Cached Invertible Matrix")   #Type message: Getting Cached Invertible Matrix 
	          return(invMatrix)                             #return the invertible matrix
	        }
	MatrixData <- x$getMatrix()                     #get the original Matrix Data 
	        invMatrix <- solve(MatrixData, ...)             #use solve function to inverse the matrix
	        x$setInverse(invMatrix)                         #set the invertible matrix 
	        return(invMatrix)  
}



TestMatrix <- matrix(1:4,2,2)
	TestMatrix
	
	CacheMatrix <- makeCacheMatrix(TestMatrix)
	CacheMatrix$getMatrix()
	CacheMatrix$getInverse()
	
	cacheSolve(CacheMatrix)
	cacheSolve(CacheMatrix)


TestMatrix <- matrix(c(1,5,8,2),2,2)
	TestMatrix
	
	CacheMatrix <- makeCacheMatrix(TestMatrix)
	CacheMatrix$getMatrix()
	CacheMatrix$getInverse()
	
	cacheSolve(CacheMatrix)
	cacheSolve(CacheMatrix)

TestMatrix <- matrix(1:9,3,3)
	TestMatrix
	
	CacheMatrix <- makeCacheMatrix(TestMatrix)
	CacheMatrix$getMatrix()
	CacheMatrix$getInverse()
	
	cacheSolve(CacheMatrix)
	cacheSolve(CacheMatrix)



TestMatrix <- matrix(1:8,3,3)
	TestMatrix
	
	CacheMatrix <- makeCacheMatrix(TestMatrix)
	CacheMatrix$getMatrix()
	CacheMatrix$getInverse()
	
	cacheSolve(CacheMatrix)
	cacheSolve(CacheMatrix)

TestMatrix <- matrix(c(2,3,5,1,3,7,4,5,6,8,0,0,4,5,6,0),4,4)
	TestMatrix
	
	CacheMatrix <- makeCacheMatrix(TestMatrix)
	CacheMatrix$getMatrix()
	CacheMatrix$getInverse()
	
	cacheSolve(CacheMatrix)
	cacheSolve(CacheMatrix)

TestMatrix <- matrix(5:21,4,4)
	TestMatrix
	
	CacheMatrix <- makeCacheMatrix(TestMatrix)
	CacheMatrix$getMatrix()
	CacheMatrix$getInverse()
	
	cacheSolve(CacheMatrix)
	cacheSolve(CacheMatrix)
