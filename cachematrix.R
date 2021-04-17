# The role of this function is to take in a matrix, check to see if the matrix 
# already exists and if it does it then returns that matrix. If it finds
# that no such matrix exists then it uses input to calculate the inverse of a
# matrix, and thenstore it in the global environment to be called upon later. 


# makeCacheMatrix 
# contains the four functions that either solve or store a matrix
# first I instantiate the global object as a numeric matrix
# I then instantiate the global output object
# set is defined to be a function that assigns user input to the working 
#   variable inside and also cleans out any previous data inside 
#   the stored global matrix
# get function is defined as "getter / mutator" function the "working" variable
# setinverse function uses the built in ability to find the inverse of
#   a square matrix and assign that inverse to object in the parent envir
# getinverse function defines the function for the global object
# list allows the use of the $ extract operator later


makeCacheMatrix <- function(x = as.numeric((matrix()))){ 
  m_nought <- NULL 
  set <- function(m1){ 
    x <<- m1 
    m_nought <<- NULL 
  }
  get <- function() x 
  setinverse <- function(solve) m_nought <<- solve 
  getinverse <- function() m_nought 
  list( set = set, get = get, 
        setinverse = setinverse, 
        getinverse = getinverse) 
}
# cacheSolve is used to determine if a matrix already exists. If it does then
# it displays the message that it is retrieving the existing inverse.
# If it detremines that the global object is empty it then calls the 
# makeCacheMatrix function to calculate the inverse of the new input
cacheSolve <- function(x, ...){ 
  m_nought <- x$getinverse() 
  if(!is.null(m_nought)){  
    message("getting cached data")
    return(m_nought) 
  }
  data <- x$get() 
  m_nought <- solve(data, ...) 
  x$setinverse(m_nought) 
  m_nought 
}