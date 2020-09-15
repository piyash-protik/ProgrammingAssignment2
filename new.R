library(MASS)
makeCacheMatrix <- function(x = matrix()) {
  j <- NULL
  set <- function(y){
    x <<- y
    j <<- NULL
  }
  get <- function()x
  setInverse <- function(inverse) j <<- inverse
  getInverse <- function(){
    inver <- ginv(x)
    inver%*%x
  }
  list(set = set, get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)
}


cacheSolve <- function(x, ...) {
  j <- x$getInverse()
  if(!is.null(j)){
    message("getting cached data")
    return(j)
  }
  mat <- x$get()
  j <- solve(mat,...)
  x$setInverse(j)
  j
}



f <- makeCacheMatrix(matrix(1:8,4,4))
f
f$get()
f$getInverse()
cacheSolve(f)








