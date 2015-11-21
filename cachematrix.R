## Efficient computation of matrix inverse
## Inverse is computed for the first time. 
## Subsequent calls to matrix inversion return cached matrix
## Cache is appropriately invalidated

## Create an object that represents a matrix and its inverse
## Define functions that allow access to the matrix and the inverse

makeCacheMatrix <- function(m = matrix()) {
        ## invalidate inverse cache
        invM <- NULL
        ## get function returns matrix object
        get<-function() m
        ## set function updates matrix object
        ## invalidate inverse cache
        set<-function(m2) {
                m<<-m2
                invM<<-NULL
        }
        ## getInv function returns cached matrix inverse
        getInv<-function() invM
        ## setInv function updates matrix inverse cache
        setInv<-function(inv) invM<<-inv
        ## return list that allows user to define the matrix and 
        ## invoke all functions defined above
        list(get=get,set=set,getInv=getInv,setInv=setInv)
}


## Compute matrix inverse

cacheSolve <- function(x, ...) {
        mt<-makeCacheMatrix(x)
        ## Return a matrix that is the inverse of 'x'
        if(is.null(mt$getInv())) {
                mt$setInv(solve(mt$get(),...))
        }
        ## Return cached matrix inverse
        mt$getInv()
}
