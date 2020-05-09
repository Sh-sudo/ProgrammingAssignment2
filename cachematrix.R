## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##the below function creates a list of functions to set and get the 
##values of corresponding variables from the parent enviroment of 
##that function

makeCacheMatrix <- function(x = matrix()) {
  
      m<-NULL
      #below we are creating a function to put value of x from 
      #the enviroment in which this function is called using 
      #lexical scoping
      set(y){
        x<<-y
        m<<-NULL
      }
      get<-function() x
      #the above get function returns x when called 
      #now we'll form setinverse and getinverse analogous to the 
      #above functions for x
      getinverse<-function(inverse) m<<-inverse
      setinverse<-function() m
      #creating a list of functions
      list(ste=set,get=get,setinverse=setinverse,getinverse=getinverse)

}


## Write a short comment describing this function
##when we fetch the value from the parent enviroment and are 
##unable to find the inverse then this function calculates the inverse
##and returns it or if it already being created then it returns the 
##same inverse without any further computations.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ## Return a matrix that is the inverse of 'x'
        #checking whether inverse was already calculated or not
        m<-x$getinverse()
        #finding the inverse only if its not being calculated before
        if(!is.null(m)){
          message("getting data")
          return(m)
        }
        #getting the x matrix
        mat<-x$get()
        #finding mean 
        m<-solve(mat,...)
        x$setinverse(m)
        m
}



