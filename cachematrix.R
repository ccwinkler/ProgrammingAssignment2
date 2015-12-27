    ## makeCacheMatrix is function which will retrieve data, convert it into a matrix and then calculate the inverse of the matrix 
## before caching the results(storing in memory)


makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  
  set<-function(y){ ## this function changes the vector stored in the main function
    x<<-y  ##substitutes the vector x with y 
    m<<-NULL ##restores the null value to the matrix m; the new matrix needs to be recalculated through the function cacheSolve
  }
  get<-function() x  ## gets the matrix
  setmatrix <-function (matrix) m<<-matrix ## function creates the matrix
  invertmatrix <-function (solve) x<<-solve ## function inverts the matrix
  getinvertmatrix <-function() m ## function retrieves the inverted matrix
  list (get=get, setmatrix=setmatrix, invertmatrix=invertmatrix, getinvertmatrix=getinvertmatrix)
}  


## cacheSolve is a function that will determine if the desired output resides in cached memory prior to running
## makeCacheMatrix thereby reducing computation time required to unnecessarily run makeCacheMatrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <-x$getinvertmatrix()
  if (!is.null(m)){
          message("getting cached data") ## this message is displayed if inverted matrix results are already cached
          return (m) ## returns the output currently in cache
    }
    data <- x$get()
    m <- matrix(data, ...)
    x$setmatrix(m)
    m
}
            
