## Filename:        cachematrix.R
## Author:          JamesAKirk
## Course:          R Programming with Roger D. Peng, April 2014
## Course page:     class.coursera.org/rprog-002
## Repository:      https://github.com/rdpeng/ProgrammingAssignment2
## Working fork:    https://github.com/jamesakirk/ProgrammingAssignment2
##
## Description:     This file defines 3 functions:
##                    makeCacheMatrix:  object that holds a matrix and it's inverse
##                    cacheSolve: returns the inverse from a makeCachMatrix object
##                    testCacheMatrix: a function which automates testing
##
## Usage Example:   > mat1 <- matrix(c(1,1,1,3,4,3,3,3,4), 3)
##                  > cacheMatrix1  <- makeCacheMatrix(mat1)
##                  > cacheMatrix1$get()
##                  > cacheSolve(cacheMatrix1)
##                  > cacheSolve(cacheMatrix1)
##
## See Also: http://cartesianfaith.com/2014/04/16/modeling-data-with-functional-programming-in-r-chapter-1/
  

## makeCacheMatrix creates a data object that holds:
##      x           : a matrix x
##      inv         : the inverse of x
##      set         : a function that sets x
##      get         : a function that returns x
##      setinverse  : a function that sets inv
##      getinverse  : a function that returns inv
##
## makeCacheMatrix can be called with ot without arguments.
## makeCacheMatrix() creates an object with a null x, x can be set later with set(someMatrix)
## makeCacheMatrix(someMatrix) creates an object with someMatrix as the initial matrix
## Note that while makeCacheMatrix hold the value of inv, it does not compute it

makeCacheMatrix <- function(x = matrix()) {
  #x has been initiated by invoking this function
  #here, we explicitly initiate inv
  inv <- NULL
  
  #Next, we define the four functions to set/get x/inv
  set <- function(y) {
    #x <<- y will set x to the value of y.
    #however, since there is no x defined inside the set function,
    #R will traverse parent environments, until it finds a symbol x to map to.
    #In this case, it will find a match in it's immediate parent makeCacheMatrix
    #If it did not find a match, it would eventually traverse to the global environment
    #where it would create a variable named x.
    #
    #Since the operator <- does not traverse the a heirarchy of data environments,  
    #it would fail to find the varible x in it's parent environment, and it would not 
    #set the value of the matrix x in the the makeCacheMatrix object
    x <<- y
    #Similarly, we must user the <<- operator to clear the inverse
    #(if the matrix has been changed, then we must assume that the current inverse is bogus)
    inv <<- NULL
  }
  #the following 3 functions do not require brackets, as they have only one command each:
  get <- function() x
  #Again, the <<- operator is necessary... 
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  
  #Finally, we create and return a list to hold the 4 get/set functions
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


## This function is beyond the scope of the assignment. I will include it in hopes that it may be useful. 
testCacheMatrix  <- function(){
  mat1 <- matrix(c(1,1,1,3,4,3,3,3,4), 3)
  N  <- 1000
  mat2  <- matrix( rnorm(N^2,mean=0,sd=1), N, N) 
  list1  <- list(mat2)
  function1  <- function(x=matrix()){
    y  <- makeCacheMatrix(x)
    message("Original Matrix:")
    #print(y$get())
    
    ptm <- proc.time()
    message("Inverted Matrix (pass 1)")
    s  <- cacheSolve(y)
    # print(s)
    print(proc.time() - ptm)
    
    ptm <- proc.time()
    message("Inverted Matrix (pass 2)")
    s <- cacheSolve(y) 
    #print(s)
    print(proc.time() - ptm)
  }
  z  <- lapply(list1, function1)
}