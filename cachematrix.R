## These functions will take a matrix and create an object that stores the
## matrix as well as its inverse.  This allows the user to access the 
## matrix's inverse whenever needed, without having to compute it each time
## it is used.

## This function creates the object which stores the matrix and its inverse.
## Call it with makeCacheMatrix(x), where x is an invertible matrix.  This
## will create a variable, "cacheinverse", where the inverse of the matrix
## will be stored, but it will not actually compute and store the inverse.
## To compute and store the inverse, you need to call cacheSolve.

makeCacheMatrix <- function(x = matrix()) {
  cacheinverse <- NULL
  set <- function(y){
    x <<- y
    cacheinverse <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) cacheinverse <<- inverse
  getinverse <- function() cacheinverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function computes the inverse of the matrix and stores it as 
## cacheinverse.  After calling a <- makeCacheMatrix(x), call
## cacheSolve(a) to compute the inverse of x and store it in cacheinverse.
## After that, a$getinverse() will return the inverse of x.  To replace a
## with a different matrix y, call a <- makeCacheMatrix(y) and then 
## cacheSolve(a).

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  cacheinverse <- x$getinverse()
  if(!is.null(cacheinverse)){
    message("getting cached data")
    return(cacheinverse)
  }
  data <- x$get()
  cacheinverse <- solve(data)
  x$setinverse(cacheinverse)
  cacheinverse
}
