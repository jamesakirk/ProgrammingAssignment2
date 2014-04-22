## NB: I may have over-commented this file, I sincerely hope this does not impair readability.

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
## makeCacheMatrix can be called with out without arguments.
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

## cacheSolve takes a makeCacheMatrix object as an argument
## and returns it's inverse. If the inverse has been already 
## calculated and cached, it returns it. Else it caclculates,
## caches and returns it.
## 
cacheSolve <- function(cacheMatrix) {
  #first, get the inverse stored in the cacheMatrix object
  inv <- cacheMatrix$getinverse()
  #Check to see if the the inverse has been calculated yet
  if(!is.null(inv)) {
    #If it has, let user know that it is getting the cached copy
    message("getting cached data")
    #return the cached copy
    return(inv)
    #since it has returned a value, function cacheSolve terminates
  }
  #If we are here, then the inverse has not been calculated and cached
  #so, we will have to calculate and cache it
  #First, get the data matrix stored in the makeCacheMatrix object named cacheMatrix
  # we will assign this to 'data', but inside the makeCacheMatrix environment, it is know as 'x'
  data <- cacheMatrix$get()
  #Next, the matrix data obtained above is inverted thusly:
  inv <- solve(data)
  #Next, since the inverse of the matrix data has been calculated,
  #cache it inside the cacheMatrix object
  cacheMatrix$setinverse(inv)
  #Finally, we return 'inv' and terminate the function
  inv
}


## This function is beyond the scope of the assignment. I will include it in hopes that it may be useful. 
testCacheMatrix  <- function(verbose=0, N=5){
  #A good clean test matrix:
  #mat1 <- matrix(c(1,1,1,3,4,3,3,3,4), 3)
  #generatea random matrix to invert
  mat1  <- matrix( rnorm(N^2,mean=0,sd=1), N, N) 
  list1  <- list(mat1)
  function1  <- function(x=matrix()){
    #this is what we are going to do for test matrix:
    #make the matrix object
    y  <- makeCacheMatrix(x)
    message("Original Matrix:")
    if(verbose) print(y$get())
    
    #Start the timer
    ptm <- proc.time()
    message("Inverted Matrix (pass 1)")
    #solve the matrix inverse
    s  <- cacheSolve(y)
    if(verbose) print(s)
    print(proc.time() - ptm)
    #Stop the timer and determine how much time it took
    
    #Start the timer
    ptm <- proc.time()
    message("Inverted Matrix (pass 2)")
    #solve the matrix inverse
    s <- cacheSolve(y) 
    if(verbose) print(s)
    print(proc.time() - ptm)
    #Stop the timer and determine how much time it took
    #This time through, it should grab a cached copy and 
    #be much faster for large matricies
  }
  z  <- lapply(list1, function1)
}