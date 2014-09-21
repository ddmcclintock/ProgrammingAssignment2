## A pair of functions that cache the inverse of a matrix


## Creates a special matrix that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  get<-function(){
    x
  }
  setInverse<-function(solve){
   m<<-solve
  }
  getInverse<-function(){
    m
  }
  list(set=set, get=get,setInverse=setInverse,getInverse=getInverse)
}

## The following function calculates the inverse of the special matrix created with makeCacheMatrix. However, 
## it first checks to see if the inverse has already been calculated. 
## If so, it gets the inverse from the cache and skips the computation. 

cacheSolve <- function(x=matrix(), ...) {
  m<-x$getInverse()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  data<-x$get()
  m<-solve(data) 
  x$setInverse(m)
  m   
}
