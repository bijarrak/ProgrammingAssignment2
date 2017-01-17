## set of functions to store the results of matrix invert in chache
## and to retrieve the same from chache ,saving computation time

## makeCacheMatrix will create a list containig functions get,set,setinv and getinv
## to set the values of matrix, 
## get the value of matrix, 
## setinv to set the inverse of matrix 
## getinv to get the inverse of matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(n) m <<- n
  getinv <- function() m
  list (set=set,get=get,setinv=setinv,getinv=getinv)

}


## cachesolve will return the inverse of the matrix,if available from chache
## if not available then calculate the same.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  if(!is.null(m))
  {
   message("getting from cache") 
    return(m)
  }
  data<- x$get()
  m <- solve(data)
  x$setinv(m)
  m
    
}
