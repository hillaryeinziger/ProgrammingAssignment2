## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

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


## Write a short comment describing this function

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
