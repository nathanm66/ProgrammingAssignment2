makeCacheMatrix <- function(x=matrix()) {         #This function returns an 
  nvr=NULL                                        #     inverse of a matrix
      set=function(y) {
            x <<- y                               #assigns a value to an object 
            nvr<<-NULL                            #     that is in a different
      }                                           #     environment
      get=function() x
      setnvr=function(inverse) nvr<<-inverse      #list of functions are used as
      getnvr=function() nvr                       #     the input to cacheSolve()
      list(set=set, get=get, setnvr=setnvr, getnvr=getnvr)
      }



cacheSolve <- function(x, ...) {                   #This function returns the 
      nvr=x$getnvr()                               #    inverse of the matrix
      if(!is.null(nvr)){                           #if inverse has been calcualated,
      message("getting cached data")               #    this calculagtion is skipped
      return(nvr)
      }
      mat.data<-x$get()                            #otherwise, the inverse is 
                                                   #   calculated
      nvr<-solve(mat.data, ...)
    
      x$setnvr(nvr)                                #sets the value of the inverse
      return(nvr)                                  #   in the chache

}


##sample run
##x=rbind(c(5, -2/4), c(-2/4, 5))
##m=makeCacheMatrix(x)
##m$get()
##     [,1] [,2]
##[1,]  5.0 -0.5
##[2,] -0.5  5.0

##cachesolve(m)
##           [,1]       [,2]
##[1,] 0.20202020 0.02020202
##[2,] 0.02020202 0.20202020
