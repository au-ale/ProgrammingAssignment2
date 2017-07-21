## The following function 'makeCacheMatrix' creates a special "matrix" object 
## that can cache the inverse of the matrix.

## 'x' and 'a' are first initialised as objects within the function, 'a' is set to NULL
## so that it can be used later. The default value of 'x' is an empty matrix.
## The 'set' function assigns the 'y' input to 'x' in the parent environment and 'a' to NULL. 
## This clears the cached value of 'a' if you have already used the function. 
## The 'get' function retrieves 'x' from the parent environment of makeCacheMatrix.
## 'setinverse' assigns input argument to the value of 'a' in parent environment.
## 'getinverse' defines the retrieval of the inverse 'a'
## The list defines each function in the form of a list and returns it to makeCacheMatrix.

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

## 'cacheSolve' is given the argument 'x' and '...' 
## The function defines 'a' which tries to getinverse() on the object that was passed as the argument.
## If 'a' is not NULL it will return the inverse of the cached matrix.
## If 'a' is NULL it gets the matrix from the input object and calculates the inverse by using 'setinverse'
## It then returns the inverse by printing the object 'a'

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
