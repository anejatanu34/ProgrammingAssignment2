## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##list of four functions is returned
##setdata sets the data to input given, inv variable is set NULL
##getdata  returns the data set
##setinv  sets the inv to input given
##getinv gets inv variable value

makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL
  
  setdata <- function(data)
  {
    x<<- data
    inv <<- NULL
  }
  
  getdata <- function() x
  
  setinv <- function(inverse) inv<<- inverse
  
  getinv <- function() inv
  
  list(setdata = setdata,getdata = getdata,setinv = setinv, getinv = getinv)
  
}


## Write a short comment describing this function
##first variable inv is fetched using getinv function 
#if inverse is present then cached copy is provided
#else it is calculated and stored

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  
  if(!is.null(inv))
  {
    message("getting cached data")
    return(inv)
  }
  
  data<- x$getdata()
  inv <- solve(data)
  x$setinv(inv)
  inv
}
