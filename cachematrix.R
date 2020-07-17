## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#the function takes a matrix and assign to x as gobal variable during the process the mean m is set to NULL
#the function then return the list of operations that can be perfomed to x and m

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
#this function takes  the result returned by function above and checks if the mean is computed
#if not it computes the mean otherwise it return previously computed mean.
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




