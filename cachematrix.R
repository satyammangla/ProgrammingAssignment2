## makeCacheMatrix() creates a matrix object which can cache its inverse and creates a list to accept, store and return the values of matrix and its inverse.
## cacheSolve() computes the inverse of the special "matrix" returned by makeCacheMatrix. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve retrieves the inverse from the cache.

## makeCacheMatrix() takes a matrix as an argument and returns a list having functions as elements to accept and return the matrix and cache its inverse.

makeCacheMatrix <- function(x = matrix()) {

    cache <- matrix()					            #initialising an empty matrix to store the cache of inverse matrix
    
    set <- function(y = matrix()) 	    	#this function is used to set a new matrix 
    {   x <<- y
        cache <<- matrix()				      	#a new matrix is used, hence the inverse gets initialised to an empty matrix
    }
    
    get <- function() x			            	#function to return the matrix object
    
    setinverse <- function(inverse) cache <<- inverse	       #function to set cache as the inverse of matrix
    
    getinverse <- function() cache				#function to return the cache of inverse of matrix
    
    list( set = set, 					            #a list storing all the elements of makeCacheMatrix object
          get = get,
          setinverse = setinverse,
          getinverse = getinverse)
  }
  




## cacheSolve() takes as argument the matrix object returned by makeCacheMatrix() and calculates its inverse if it has not been calculated previously in which case it retrieves it.

cacheSolve <- function(x, ...) {
 
  m <- x$getinverse()			#assigns the inverse of matrix x to m if it already exists
  
  if(all(!is.na(m))) 			#condition to check if the inverse already exists, then proceed to the code
  {
    message("getting cached data")
    return(m)					    #returns the already calculated inverse of the matrix
  }
  
  data <- x$get()				  #if inverse does not already exist, then data is assigned the matrix x
  
  m <- solve(data, ...)		#inverse of matrix data is calculated and assigned to m
  
  x$setinverse(m)				  #assigning the inverse of matrix object x as m (in makeCacheMatrix function)
  
  m						            #returning the inverse of matrix
}

