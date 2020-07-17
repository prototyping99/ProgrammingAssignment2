## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    inv<- NULL
    set<- function(var){
        x<<-var
        inv<<-NULL
    }
    get<-function() x
    setInv<-function(inverse) inv<<-inverse
    getInv<-function() inv
    list(set=set,get=get,setInv=setInv,getInv=getInv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
            ## Return a matrix that is the inverse of 'x'
    inv<- x$getInv()
    if(!is.null(inv)){
        print("getting cached data...")
        return(inv)
    }
    data<-x$get()
    inv<-solve(data,...)
    x$setInv(inv)
    inv
    
}
A <- matrix( c(5, 1, 0,
               3,-1, 2,
               4, 0,-1), nrow=3, byrow=TRUE)
x<-makeCacheMatrix(A)
cacheSolve(x)
data<-c(-1,2,3,-2,1,4,2,1,5)
mat<-matrix(data,3,3)
x$set(mat)
cacheSolve(x)
cacheSolve(x)
cacheSolve(x)


