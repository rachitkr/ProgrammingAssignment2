## Following are the two functions used to calculate the inverse of a matrix and store the value in cache.
## If the value of the matrix changes then a new inverse is computed, else the inverse value is read from the cache.
## As the question assumes an inversibale matrix no check is being perfomed to test the 
## dimension or invertibility of matrix of the input martix in the set function of makeCacheMatrix

## Following function set the value of the matrix using set function, sets the value of the 
## inverse using setinv, gets the value of the matrix using get and the value of the mean 
## using getmean.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() {
    x
  }
  setinv <- function(inv) {
    m <<- inv
  }
  getinv <- function(){
   m 
  }
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Following function calculates the inverse of the special "matrix" created with the above function. 
## However, it first checks to see if the inverse has already been calculated. If so, it gets the inverse from 
## the cache and skips the computation. Otherwise, it calculates the inverse of the data and sets the value of 
## the inverse in the cache via the setinv function.

cacheSolve <- function(x, ...) {
  m <- x$getinv()
  if(!is.null(m)) {
    message("Getting cached data!")
    return(m)
  }
  mat <- x$get()
  message("Calculating new mean!")
  m <- solve(mat)
  x$setinv(m)
  m
}
