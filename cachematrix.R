## The first function, stores the matrix m, and solves the inverse of the matrix through the function setinverse.
makeCacheMatrix <- function(x = matrix()) { #creates matrix
  m <- NULL                                 #null the (m)atrix
  set <- function(y) {                      #set (funtion) stores the x and nulls the m
    x <<- y          
    m <<- NULL
  }
  get <- function() x                       #get (funtion) returns cached matrix  
  setsolve <- function(solve) m <<- solve   #setsolve (funtion) stores the (m)atrix in cache
  getsolve <- function() m                  #getsolve (funtion) returns the cached matrix
  list(set = set, get = get,                #list(creates the list) containing the set,get,setsolve,getsolve functions
       setsolve = setsolve,
       getsolve = getsolve)
}


## The second functions, calls the stored m, and if m has changed, recalculates the inverse. 
cacheSolve <- function(x, ...) {            #creates a function cacheSolve 
  m <- x$getsolve()                         #x$getsolve returns the cached matrix 
  if(!is.null(m)) {                         #if(m) checks to see if the (m)atrix has changed  
    message("getting cached matrix")
    return(m)
  }
  data <- x$get()                           #x$get returns the cached matrix
  m <- solve(data, ...)                     #(m)atrix inverses the matrix 
  x$setsolve(m)                             #x$setsolve(function) caches the solved matrix 
  m                                         #prints the inverse (m)atrix 
}

