## These functions calculate the inverse of a matrix.
## Instead of calculating the inverse of the same matrix over and over (ex: in a loop) 
## the previously calculated matrix inverse will be cached to be used later and only. 
## calculate the inverse for unseen matrices before.  

## This function create special list of functions to be called on the matrix given.

makeCacheMatrix <- function(mat.x = matrix()) {
## Cache the matrix values and set and get the values of the inverse.
##
## Args:
## mat.x: The matrix i need to calculate its inverse.
## Returns:
## list of functions to be called to calculate the inverse. 
  mat.inverse <- NULL
  set <- function(mat.y) {
  mat.x <<- mat.y
  mat.inverse <<- NULL
  }
  get <- function() mat.x
  setInverse <- function(solve) mat.inverse <<- solve  # solve is the function used to calculate the inverse
  getInverse <- function() mat.inverse
  list(set = set, get = get,  
       setInverse = setInverse,
       getInverse = getInverse)
}


## This function check if the matrix was seen before so return its cached inverse value or,
## it's a new one so call the function setInverse to calculate the inverse.

cacheSolve <- function(mat.x, ...) {
## Get the values of the inverse.
##
## Args:
## mat.x: The matrix i need to calculate its inverse.
## Returns:
## The matrix which is inverse to mat.x. 
  mat.inverse <- mat.x$getInverse()
  if (!is.null(mat.inverse)) {  # Check if this matrix was seen befor so its inverse has a value not NULL
    message("getting cached data")
    return(mat.inverse)  # return the matrix inverse already chached.
    }
    data <- mat.x$get()  # get the matrix from the get function.
    mat.inverse <- solve(data, ...)
    mat.x$setInverse(mat.inverse)
    mat.inverse
        ## Return a matrix that is the inverse of 'mat.x'
}
