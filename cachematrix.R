## The following function 'makeCacheMatrix' creates a special "matrix" object 
## that can cache the inverse of the matrix.The matrix must be a square as the
## solve() function in the 2nd function(see below) can only compute square 
##matrices.

makeCacheMatrix <- function(x = matrix()) {
  
  a <- NULL
  set <- function(y){
    x <<- y
    a <<- NULL
  }
  
  get <- function() x
  
  setinverse <- function(inverse) a <<- inverse
  
  getinverse <- function() a
  
  list(set = set, 
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## The 'cacheSolve' function computes the inverse of the 'special' matrix that was
## cached in the 'makeCacheMatrix'.

cacheSolve <- function(x, ...) {
  a <- x$getinverse()
  if(!is.null(a)){
    message("getting cached data")
    return(a)
  }
  data <- x$get()
  a <- solve(data,...)
  x$setinverse(a)
  a
  ## Returns a matrix that is the inverse of 'x'
}

#testing the function, should return an inverse of a 5 by 5 square matrix

f <- matrix(rnorm(25),5,5)
f1 <- makeCacheMatrix(f)
cacheSolve(f1)
