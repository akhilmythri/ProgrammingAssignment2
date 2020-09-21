## This code is the solution to programmming assignment 2
## It checks if the inverse of a matrix is already cached
#If cached, it avoids computing it and simply pulls it out of cache
#If not cached it computes the inverse of a matrix

#it is assumed that the input matrix is invertible- this is not checked in the code

## This function makeCacheMatrix creates a cache for a matrix that is passed into it


makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  get<-function() x
  setinverse<-function(inverse) m<<-inverse
  getinverse<-function() m
  list(set=set, get= get, setinverse=setinverse, getinverse=getinverse)
}


## This function computes the inverse of a matrix only if it is not already cached 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m<-x$getinverse()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  data<-x$get
  data1<-as.matrix(data()) #This is to coerce the data into matrix form so that solve() can be applied next
  m<-solve(data1)
  x$setinverse(m)
  m
}
