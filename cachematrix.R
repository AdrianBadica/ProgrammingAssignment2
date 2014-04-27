

# the function receives a matrix as an argument
# and returns a list of 4 functions
# needed by the cacheSolve function

makeCacheMatrix <- function(x = matrix()) {
  # m is the inverted matrix that will be returned 
  # initialiazed to NULL
          m  <- NULL
  # in case the matrix passed as a parameter to makeCacheMatrix (i.e. "x")
  # should be changed, x is set to the new value, in the parent enviroment
  # so as not to return the inverse matrix  computed for the previous x
  # but to compute a new inverse for the new x
  # and m is set to NULL again, in the enviroment of makeCacheMatrix, not just the one
  # of the set function, since both m and x will need to be used by the 
  # following functions
          set <- function(y){
          x <<- y
          m <<- NULL
        }
  # get stores the value of x (the matrix)
          get  <- function() {x}
  # setinverse assigns the value of the inverse matrix to m
  # in the makeCacheMatrix enviroment, computed in the cacheSolve function
          setinverse <- function(solve) 
          m <<- solve
  # stores the value of the inverted matrix 
          getinverse <- function() {m}
  # returns a list of 4 functions needed by the cacheSolve function below
            list(set=set, get=get,
                 getinverse=getinverse,
                 setinverse=setinverse)
}



## returns the inverse of a matrix
# if the value was already computed, it will return the value 
# stored by the makeCaheMatrix function
# else it will compute it 

cacheSolve <- function(x, ...) {
  ## m ( the value to be returned by this function) is set to the value 
  # stored by the makeCacheMatrix function
  
  m <- x$getinverse()
  
  # check to see if the value was already computed so as not to make the calculations again
  # if it was, it will return a message to signal to the user that it returns the already computed inverse matrix
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  # if it was not already computed, data parameter is assigned the value of the matrix that needs to be inverted
  # stored in the makeCacheMatrix function
  data <- x$get()
  # the matrix is inverted by the solve function, with the data parameter passed to it
  # the result is assigned to m, that will be returned by the function
  m <- solve(data, ...)
  # the result is then passed to the makeCacheMatrix to be stored
  x$setinverse(m)
  # the inverse of the matrix , m, will be returned
  m
  
}
