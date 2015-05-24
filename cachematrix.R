## This function creates a special "matrix" object that can cache its inverse.

## MAKECACHEMATRIX is fed and invertible square matrix herto defined as SqMat

makeCacheMatrix <- function(SqMat = matrix()) {

        ## inverse is init as NULL
        inv < NULL
        
        ## $set recast input matrix SqMat with supplied NewMat
        ## reinit inv as NULL
        ## returns nothing
        set <- function(NewMat){
                SqMat <<- NewMat
                inv <<- NULL
        }

        ## $get is assigned value of function output
        ## returns input matrix SqMat
        get <- function(){
                SqMat
        }

        ## $setinv takes a given inverse and assigns that value as inv
        ## return nothing
        setinv <- function(inverse){
                inv <<- inverse
        }

        ## $getinv runs the supplied function
        getinv <- function(){
                inv
        }

        ## return of cachematrix in list form
        list(set=set,get=get,setinv=setinv,getinv=getinv)
}



## This function computes the inverse of the special "matrix" returned by makeCacheMatrix
## If the inverse has already been calculated (and the matrix has not changed)
## then cacheSolve should retrieve the inverse from the cache

cacheSolve <- function(SqMat, ...) {
        
        ## assign the value of stored value of inv from "make function" to inv
        inv <- SqMat$getinv()
        
        ## if inv is not null (ie has been passed a value)
        ## then print message
        ## return value of inv and exit
        if(!is.null(inv)){
                message("Oh, I know this one!")
                message("The inverse is ...")
                return(inv)
        }
        
        ## if inv is null
        ## then retrieve SqMat with $get and assign to invertMe to be inverted
        invertMe <- SqMat$get()
        
        ## use solve() on matrix and update inv to be assigned the return
        inv <- solve(invertMe, ...)
        
        ## store the newly calculated vlaue of in using $setinv
        SqMat$setinv(inv)
        
        ## print messages
        message("Had to think about this for a bit")
        message("The inverse is ...")
        
        ## return calculate inverse of SqMat
        inv
        
}
