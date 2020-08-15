## This function creates inverse matrix (assuming matrix is squared and invertible)
# This function contains the list of:
# 1.  set the value of the matrix
# 2.  get the value of the matrix
# 3.  set the value of its inverse 
# 4.  get the value of its inverse 

makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL         # Initiate inverse property
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x         # view matrix
  setinv <- function(solve) inv <<- solve     # function to solve inverse of matrix
  getinv <- function() inv
  list(set=set, get=get, 
       setinv=setinv, getinv=getinv)
  
}


## This function returns the cached inverse matrix if the inverse has been previously calculated from makeCacheMatrix,
# and provided the matrix has not changed. Reduces effort of repeated computation.

cacheSolve <- function(x, ...) {
  
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()    # cache inverse matrix 
  if(!is.null(inv)) {           
    message("getting cached data")  # if cached, retrieve message and the corresponding inverse matrix
    return(inv)
  }
  data <- x$get()    # calculates inverse matrix
  inv <- solve(data) 
  x$setinv(inv)   
  inv
  
}

# Test (2x2 matrix)
mat0 <- makeCacheMatrix(matrix(1:4,2,2))
mat0$get()
mem0 <- cacheSolve(mat0)
mem0
mat0$getinv()

# (3x3 matrix)
mat1 <- makeCacheMatrix(matrix(sample(9),3,3))
mat1$get()
mem1 <- cacheSolve(mat1)
mem1
mat1$getinv()



