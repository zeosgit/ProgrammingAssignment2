## The cachematrix.R file provides two functions that is designed to cache the results of matrix 
## inversion so as to avoid any costly computation when possible. The two functions are -
## makeCacheMatrix and cacheSolve. Description for both are below

## The makeCahceMatrix takes in a matrix as an input and produces a special matrix that is
## cache for later use. This function enables this by implementing a set of 4 functions -
## 1. set - to set the value of the matrix
## 2. get - to get the value of the matrix
## 3. setinverse - to set the inverse of the matrix
## 4. get inverse - to get the inverse of the matrix if it is cached
## The makeCacheMatrix returns a list of these four functions

makeCacheMatrix <- function(x = matrix()) {
  
  xinverse <- matrix(,nrow(x),ncol(x))
  
  ## implement function to set the value of the matrix
  set <- function(y) {
    x <<- y
    xinverse <<- matrix(,nrow(x),ncol(x))
  }
  
  ## implement function to get the value of the matrix
  get <- function() x
  
  ## implement function to set the value of the inverse of the matrix
  setinverse <- function(inverse_mat) xinverse <<- inverse_mat
  
  ## implement function to get the value of the inverse of the matrix
  getinverse <- function() xinverse
  
  list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}


## The cacheSolve function is used to get the value of the inverse matrix for a given input
## matrix. If the inverse matrix has been precomputed, the function will fetch it from
## cache otherwise it will compute it again and also save it in cache

cacheSolve <- function(x, ...) {
        
        ## Get the value of the inverse from cache
        xinverse <- x$getinverse()
        
        ## Check if inverse exists and return it
        if(!is.na(w[1,1])) {
          message("getting inverse from cache")
          return(xinverse)
        }
        
        ## If inverse does not exist, get the matrix and compute inverse using solve()
        xmatrix <- x$get()
        xinverse <- solve(xmatrix,...)
        
        ## Save the inverse in cache
        x$setinverse(xinverse)
        xinverse
}
