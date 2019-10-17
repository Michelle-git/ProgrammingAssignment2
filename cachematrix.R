

makeCacheMatrix <- function(matrix1=matrix(numeric,nrow=2,ncol=2)) {
  
  s <- NULL
  set <- function(matrix2=matrix(numeric,nrow=2,ncol=2)) {
    matrix1 <<- matrix2
    s <<- NULL
  }
  get <- function() matrix(matrix1,2,2)
  setsolve <- function(solve) s <<- solve
  getsolve <- function() s
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}





  
  cacheSolve <- function(matrix1=matrix(numeric,nrow=2,ncol=2), ...) {
    s <- matrix1$getsolve()
    if(!is.null(s)) {
      
      return(s)
    }
    data <- matrix1$get()
    s <- solve(data, ...)
    matrix1$setsolve(s)
    s
  }

  
  
 ## Run Below to check the function
  
  m1<-matrix(5:8,2,2)
  m1
  m2<- makeCacheMatrix(m1)
  m2$get()
  m2$set(7:10)
  cacheSolve(m2)
  m2$getsolve()
