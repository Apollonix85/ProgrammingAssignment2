##Assignment 2

makeCacheMatrix <- function(x = matrix()) {
  t <- NULL 						  ## solved value "t"= 0
  set <- function(y){
    x <<- y
    t <<- NULL
  }
                                                          ##mean -> solve
  get <- function() x
  setI <- function(solveM) t <<- solveM
  getI <- function() t
  list(set = set, get = get, setI = setI, getI = getI)
}

cacheSolve <- function(x, ...) {
 t <- x$getI()
  if(!is.null(t)){
    message("CAVE inverse matrix")
    return(t)
  }
  data <- x$get()
  t <- solve(data,...)
  x$setInverse(t)
  t  
}

