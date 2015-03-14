## The first function creates a special matrix that stores its inverse
## The second function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated the cached inverse is returned

## Returns a list containing four functions to get and set the data and the inverse
makeCacheMatrix <- function(x = matrix()) {
  
  invertedMatrix <- NULL
  
  get <- function() { x }
  set <- function(newX) { x <<- newX ; invertedMatrix <<- NULL }
  setInvertedMatrix <- function(newInvertedMatrix = matrix()) { invertedMatrix <<- newInvertedMatrix }
  getInvertedMatrix <- function() { invertedMatrix }

  list(get = get, set = set, getInvertedMatrix = getInvertedMatrix, setInvertedMatrix = setInvertedMatrix)
}

## Returns a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
  
  matrixToBeInverted <- x$get()
  cachedInvertedMatrix <- x$getInvertedMatrix()
  
  ## If the data are different the cachedInvertedMatrix would be NULL
  ## See set() function in the makeCacheMatrix() function
  if (!is.null(cachedInvertedMatrix)) {
    message("getting cached data")
    return(cachedInvertedMatrix)
  }
  
  invertedMatrix <- solve(matrixToBeInverted)
  x$setInvertedMatrix(invertedMatrix)
  
  return(invertedMatrix)
}
