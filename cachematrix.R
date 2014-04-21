##

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

cacheSolve <- function(x) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}

testCacheMatrix  <- function(){
  mat1 <- matrix(c(1,1,1,3,4,3,3,3,4), 3)
  N  <- 1000
  mat2  <- matrix( rnorm(N^2,mean=0,sd=1), N, N) 
  list1  <- list(mat2)
  function1  <- function(x=matrix()){
    y  <- makeCacheMatrix(x)
    message("Original Matrix:")
    y$get()
    
    ptm <- proc.time()
    message("Inverted Matrix (pass 1)")
    cacheSolve(y)  
    print(proc.time() - ptm)
    
    ptm <- proc.time()
    message("Inverted Matrix (pass 2)")
    cacheSolve(y)  
    print(proc.time() - ptm)
  }
  z  <- lapply(list1, function1)
}