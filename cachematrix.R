+ ## constructing the cache. 

makeCacheMatrix <- function(x = matrix()) {
  m<-NULL                                       ## placeholder for the matrix, where inverse matrix is stored
  set<-function(y){                             ## define the set function to assign a new           
    x<<-y                                       ##      value for the cached matrix in the parent environment
    m<<-NULL                                    ##      if there is a new matrix, reset m to NULL
  }
  get<-function() x                             ## define the get function - returns value of the matrix argument
  setmatrix<-function(solve) m<<- solve         ## assigns value of inverse matrix m in parent environment
  getmatrix<-function() m                       ## gets the value of inverse matrix m where it is called
  list(set=set, get=get,                        ## list of arguments, allowing them to be called in other environments
       setmatrix=setmatrix,
       getmatrix=getmatrix)
}

+ ## calculating the inverse of the cached matrix. if it already exists it will be retrieved from the cache instead
cacheSolve <- function(x=matrix(), ...) {
  m<-x$getmatrix()                              ## check value for inverse matrix m
  if(!is.null(m)){                              ## if it's not null, 
    message("getting cached data")              ## show 'cache data' message
    return(m)                                   ## return the cached inverse matrix m
  }
  matrix<-x$get()                               ## if it is null
  m<-solve(matrix, ...)                         ## solve for the inverse
  x$setmatrix(m)                                ## assign value inverse matrix
  m                                             ## print inverse matrix m
}

+ ##Example. Testing functionality of script
MAT<-matrix(c(1,1,1,1,1,2,1,2,1,1,1,0,1,4,2,3),4,4) ## Creating inverse matrix INV from a cached matrix CACHE 
                                                    ##      obtained from arbitrarily chosen matrix MAT
CACHE<-makeCacheMatrix(MAT)
INV <- cacheSolve(CACHE)
          
print(MAT)                                          ## printing the matrices, and showing that that inverse identity applies.
print(INV)
INV%*%MAT
