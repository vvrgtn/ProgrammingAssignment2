##The first function creates a list of 4 functions
##the second function checks whether the inverse of the matrix has already been calculated, it checks if there has been no changes
##if yes it retrieves it not recomputing it, 
##if not it computes it and , superassigns it to variable 'i'  by using the function setinverse in makeCacheMatrix function 
##and finally returns it 


## Put comments here that give an overall description of what your
## functions do

##the first line superassigns to NULL the initial value of the variable i 
##the function set,when called, superassigns to the variable x the matrix passed as y  and to i NULL
##g
## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix(),...) {
        print(x)
        i<<-NULL
        set<-function(y){
                x<<-y
                i<<-NULL
        }
        get <-function() x
        setinverse <-function(inverse) i<<-inverse
        getinverse <-function () i
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
                }


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        inv <-x$getinverse()
        if(!is.null(inv)){       ##error i'm comparing a matrix to a list
                print("getting cached data")
                return(inv)
        }
        inv <-solve(data)
        x$setinverse(inv)
        inv        ## Return a matrix that is the inverse of 'x'
}
