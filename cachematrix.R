##my function will calculate the inverse of a Martix using the solve() function
##it will then cache this inverse and be able to recall it in the case the matrix has not changed
##thus reducing the number of times the matrix solution is calculated



##the following function uses the <<- operator to assign a value to an object in the cache environment
##which is different from the current environment

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x<<- y
    m<<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m<<-solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## The following function will calculate the inverse of the special matrix from the above function
##HOWEVER, it will first check to see if the inverse has already been calculated and cached
##if it has already been calculated, it will just return the cached solution

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getsolve()
  if(!is.null(m)) {
      message("getting cached data")
      return(m)
  }
  data <- x$get()
  m <- solve(data,...)
  x$setsolve(m)
  m
}
