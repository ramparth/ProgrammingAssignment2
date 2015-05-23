## The below functions are to invert a matrix. 

##This function, `makeCacheMatrix` creates an empty "matrix" and clears out the previously inverted matrix
##The function returns a list of functions to
##1.  set the value of the matrix. This also clears out the previously inverted matrix
##2.  get the value of the matrix
##3.  set the inverse of the matrix 
##4.  get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(mat) {
    m <<- solve(mat)
  }
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function
## This function cacheSolve checks if the inverse already exists (set using the setinverse)
## if it exists, it returns the same and exits the function
## if not, this inverts the matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  message("setting cached data")
  data <- x$get()
  m <- solve(data)
  x$setinverse(m)
  m
}

