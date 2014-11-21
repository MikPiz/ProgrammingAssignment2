

## These are two functions that in conjunction create a special matrix which caches
## data from a matrix invertion and stores it for later retrieval. 

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  
  inv_m <- NULL
  set <- function(y) {
    x <<- y
    inv_m <<- NULL
  }
  
  get <- function() x
  setinverse <- function(inverse) inv_m <<- inverse #Here is when the data of the
                                                    #invertion is stored when the 
                                                    #cacheSolve() is called.
  getinverse <- function() inv_m      #Here is when the data is cached in the next
                                      #cacheSolve() access or accesses.          
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


##This function returns the cached data from  the makeCacheMatrix() 
##calculation.If the data is NULL it runs the calculation with the solve()
##function, caches the data and returns the inverted matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv_m <- x$getinverse()
  
  if(!is.null(inv_m)) {    
    message("getting cached data")
    return(inv_m)
  }
  data <- x$get()
  inv_m <- solve(data, ...)
  x$setinverse(inv_m)
  return(inv_m)
}





#The code below is not part of the functions. It's a test to make sure that 
#the functions actually work!

#Test 1.
#Creates a matrix of 2 x 2 an assigns its values to mat.
mat <- matrix(1:4, 2,2)
solve(mat)


matrix_test <- makeCacheMatrix(mat)    #Runs the "mat" object through the function,
cacheSolve(matrix_test)                #inverts the matrix and caches its data.

cacheSolve(matrix_test)      #Fetches the data cached in makeCacheMatrix.



#Test 2.
#Creates a matrix of 3 x 3 an assigns its values to mat_2.
mat_2 <- matrix(1:8, 3, 3)
solve(mat_2)


matrix_test$set(mat_2)       #Sets inv_m to NULL, cacheSolve() generates a 
cacheSolve(matrix_test)      #new invertion and stores it.        


cacheSolve(matrix_test)       #Fetches the cached data from the new invertion.

matrix_test$get()             #Shows that a matrix was stored.



