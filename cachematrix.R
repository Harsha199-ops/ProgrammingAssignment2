
#Here are two functions makeCacheMatrix and cacheSolve for retreiving our solution
# makeCacheMatrix consists of four methods set,get,setinv, getinv
library(MASS)
makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL
  set<-function(y){
    x<<-y
    inv<<-NULL
  }
  get<-function()x  #function to get matrix x
  setinv<-function(inverse)inv<<-inverse
  getinv<-function(){
    inver<-ginv(x)
    inver%*%x    #fun to obtain inverse of matrix x
  }
  list(set = set, get = get, setinv = setinv, getinv = getinv)

}


#cacheSolve function has methods getinv, get, solve, setinv
cacheSolve <- function(x, ...) {
  inv<-x$getinv()
  if(!is.null(inv)){
    message("getting cached data!")
    return(inv)
  }
  data<-x$get()
  inv<-solve(data,...)  #calculate inverse
  x$setinv(inv)
  inv      ## Return a matrix that is the inverse of 'x'
}
