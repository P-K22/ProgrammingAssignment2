## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  invmat<- NULL
  set<- function(y){
    x<<-y
    invmat<<- NULL
  }
  get<- function()x
  
  setinverse<- function(inv) invmat<<- inv
  
  getInverse<- function() invmat
  list(set=set, get=get, setinverse=setinverse,getInverse=getInverse)
}



## function to calc inverse of matrix with caching

cacheSolve <- function(matz, ...) {
  invmat<-matz$getInverse()
  if(is.null(invmat)==FALSE){
    print("Getting cached output")
    print(invmat)
  }       
  
  mat<-matz$get()
  invmat<-solve(mat,...)
  matz$setinverse(invmat)
  invmat
  
 
}
## Given below is a trial run
matrix_obj1 <- makeCacheMatrix()
matrix_obj1$set(matrix(c(1, 2, 3, 4), nrow = 2))

# Calculate the inverse
inverse_result1 <- cacheSolve(matrix_obj1)
print(inverse_result1)

# Calculate the inverse again to see the caching in action
inverse_result1 <- cacheSolve(matrix_obj1)
# Output: getting cached inverse
# Output: the cached inverse matrix
