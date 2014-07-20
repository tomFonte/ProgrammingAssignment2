## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## the function makeCacheMatrix 
## 1.- sets the value of the matrix
## 2.- get the values of the matrix.
## 3.- sets the value of the inverse matrix.
## 4.- gets the value of the inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL
  set<-function(y){
    x<<-y
    inv<<-NULL
  }
  get<-function()x
  setinv<-function(inverse) inv<-inverse
  getinv<- function() inv
  list(set=set,get=get,
       setinv=setinv,
       getinv=getinv)
}


## Write a short comment describing this function

## This functions uses the matrix created with the function makeCacheMatrix calculating
## the inverse, checking if this operation has already been created first. If it has
## been created, it skips the calculation and retrieves the result from the cache memory.
## Otherwise, it calculates the inverse and sets the value with the setinv function
## to the matrix in the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv<-x$getinv()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data<-x$get()
  inv<-solve(data, ...)
  x$setinv(inv)
  inv
}

