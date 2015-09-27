## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# This function stors a matrix x into a "class" makeCacheMatrix and provides
# methods to set/get the stored matrix and set/get the inverse matrix
# read margins for detailed flow
makeCacheMatrix <- function(x = matrix()) {

  invX <- NULL; #Initialize invX to null, (invX stores the inverse matrix )
  set <- function(y) {
    x <<- y; #sets the matrix y into data x
    invX <<- NULL; #Initialize invX to null
  }
  get <- function() x ; #Empty function, returns matrix x
  setInv <- function(inverse) invX <<- inverse; #Stores inverse into invX
  getInv <- function() invX #Empty functions, returns inverse matrix invX
  #lists valid function for the "class" makeCacheMatrix (last line is the
  #return for a function)
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}


## Write a short comment describing this function
# attempt to get the inverse matrix from the cache if available otherwise
# calculate the inverse matrix if the matrix is invertible
# read margins for detailed flow
cacheSolve <- function(x, ...) {
  invX <- x$getInv(); # checks if the inverse matrix is stored
  if(!is.null(invX)) {
    message("getting cached data"); # if invX is available (and it not null)
    return(invX)                    # returns invX   
  }
  data <- x$get()                   # else gets the matrix and store into data
  if(nrow(data)!=ncol(data)) {      # checks if data is a square matrix
    message("matrix is not square, unable to invert using solve")
    return(NULL)                    # errors out if data is not a square matrix (no inverse, there is a pseudo inverse but that is an overeach of the assignment)
  } 
  invX <- solve(data, ...)          # else calculate the inverse into invX
  x$setInv(invX)                    # store the inverse in the cache
  invX                              # print and return invX (last line of the function)
}
