## The below functions are to invert a matrix. 

##This function, `makeCacheMatrix` creates an empty "matrix" and clears out the previously inverted matrix
##The function returns a list of functions to
##1.  set the value of the matrix. This also clears out the previously inverted matrix
##2.  get the value of the matrix
##3.  set the inverse of the matrix 
##4.  get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  ## sets previously inverted matrix
  m <- NULL
  ## sets the matrix  
  set <- function(y) {
    x <<- y
    ## sets previously inverted matrix
    m <<- NULL
  }
  ##gets the matrix set
  get <- function() x
  
  ## inverses the matrix which is provided to the function using variable mat
  setinverse <- function(mat) {
    m <<- solve(mat)
  }
  
  ## gets the matrix which was inverted using the setinverse 
  getinverse <- function() m
  
  ## returns the list of function to set on the matrix
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function
## This function cacheSolve checks if the inverse already exists (set using the setinverse)
## if it exists, it returns the same and exits the function
## if not, this inverts the matrix

cacheSolve <- function(x, ...) {
  ##checking if inverse of matrix is already present
  m <- x$getinverse()
  
  if(!is.null(m)) {
    message("getting cached data")
    ## Return a matrix that is the inverse of 'x' but from the cache
    return(m)
  }
  
  ## if the matrix is not cached then this fucntions inverts the matrix
  message("setting cached data")
  data <- x$get()
  m <- solve(data)
  x$setinverse(m)
  
  ## Return a matrix that is the inverse of 'x'
  m
}

