## makeCacheMatrix makes an R object that stores matrix and cache its inverse, while
## cachesolve will retrieve the inverse
makeCacheMatrix <- function(x = matrix()) { ##nrow and ncol must have same value
  inv <- NULL ##set inv before making the 'set' function
  set <- function(y) { ##set the input of x
    x <<- y ##assign y to x
    inv <<- NULL ##default of inv is NULL
  }
  get <- function() x ##get x from the 'set' function
  setsolve <- function(solve) inv <<- solve ##set the input of inv from solve()
  getsolve <- function() inv ##get inv from the 'setsolve' function
  list(set=set, get=get,
       setsolve=setsolve, getsolve=getsolve) ##give names for each function
}



cacheSolve <- function(x, ...) {
  inv <- x$getsolve() ##assign inv function from makeCacheMatrix to new object 'inv'
  if (!is.null(inv)) { 
    message("getting cache data") ##show message while processing
    return(inv) ##return the value of inv
  }
  dat <- x$get() ## assign get function from makeCacheMatrix to new object 'dat'
  inv <- solve(dat, ...) ##assign inverse of dat
  x$setsolve(inv) ##apply 'set solve' to inv
  inv
  ## Return a matrix that is the inverse of 'x'
}
