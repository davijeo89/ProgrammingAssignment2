##This function takes a matrix and computers the inverse of said matrix then caches the output. 
##If the inverse does not change then no computations are done and the previous inverse is returned.
##If a new matrix is inputted, the function will replace the previous inverse value with the new inverse. 

makeCacheMatrix <- function(x) ##This functions caches the inverse of the matrix if it does not exist and redefines it if it already exisists
{
  m <- NULL ##Setting m to nothing
  set = function(y) 
  {
    x <<- y ##Setting x as a global variable 
    m <<- NULL ##Setting m as a global variable
  }
  get <- function() x
  setInverse <- function(Inverse) m <<- Inverse ##sets global variable m as Inverse 
  getInverse <- function() m ##defines getInverse as m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse) ##creates a list of functions that will be used/callled between the two functions 
}

cacheSolve <- function(x) #this function solves the inverse of a matrix if it does not already exisist. 
{
  m <- x$getInverse() ##Grabbing m from makeCacheMatrix function
  if(!is.null(m)) ##if m already exisits it will print the statement and then return the value of m
  {
    message("getting cached data")
    return(m)
  }
  data <- x$get()##grapping the matrix from makeCacheMatrix
  #if (det(x)==0) ##You can only take the inverse when the matrix's determinant is not equal to 0
  #{
  # message("Eror, determinant is equal to 0 cannot take the invese")
  #}
  #else ##compute the inverse of matrix
  #{
  m <- solve(data)
  x$setInverse(m)
  #}
  m ##return m, the inverse
}