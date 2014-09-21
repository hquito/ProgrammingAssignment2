## Put comments here that give an overall description of what your
## functions do
##the function takes a matrix and creates an special object with the
##ability to set and get the values of the inverse of the data matrix
##step to use:
##create a matrix:              m<-matrix(c(7,3,-2,5),2,2)
##create a cached matrix:       cm<-makeCacheMatrix(m)
##get the inverse matrix of cm: cacheSolve(cm)

## Write a short comment describing this function
##this function will contain the matrix and implements 4 funtcions
##to get and put data, and to get and put the inverse
makeCacheMatrix <- function(x = matrix()) {
        i<-NULL
        set <-function(y)
        {
                x<<-y
                m<<-NULL
        }
        get <-function()x
        setInverse <- function(inverse) i<<-inverse
        getInverse <- function()i
        list(set=set,get=get,setInverse=setInverse,getInverse=getInverse)

}


## Write a short comment describing this function
## this function takes a special object with a matrix in it
## and try to recover the inverse from the object usign the 
## getInverse function. the first time it will return a null value
## and get the data to calculare the inverse using the "solve()" function
## the next intents over the same object ("cm" in my example) will use the
## value directly from cached memmory. (with x$getInverse) so will print
## "get inverse from cached"
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i<-x$getInverse()
        if(!is.null(i))
        {
                message("get inverse from cached")
                return(i)
        }
        data <-x$get()
        i<-solve(data)
        x$setInverse(i)
        i
}
