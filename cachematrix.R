  makeCacheMatrix <- function(x = matrix()) {
    m<-NULL
    set<-function(y) {
    x<<-y
    m<<-NULL
  }
  get<-function()x
  setinvmatrix<-function(solve) 
    m<<-solve
  getinvmatrix<-function()m
  list(set=set,get=get,setinvmatrix=setinvmatrix,getinvmatrix=getinvmatrix)
  }
  
  cacheSolve <- function(x, ...) {
    m<-x$getinvmatrix()
    if(!is.null(m)){
      message ("getting cached data")
      return(m)
    }
    data<-x$get()
    m<-solve(data,...)
    x$setinvmatrix(m)
    m
  }
  #Now let's take an example to see if it works
  a <- matrix(c(22,5,7,98),2,2)
  b <- makeCacheMatrix(a)
  cacheSolve(b)

