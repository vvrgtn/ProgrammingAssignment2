##Programming Assignment 2 for Coursera R Programming course 
##The assignment asks to write two functions. They should compute and store in cache the inverse of a matrix
##General Hipothesis: all matrix supplied are always invertible

##The first function creates a list of 4 functions 
##The second function computes the inverse of a matrix. Before that it checks though if it has already been calculated. 
##if positive it returns the result without redoing the computing.


##Description of makeCacheMatrix function: 
##the first line superassigns to NULL the initial value of the variable i 
##the function set,when called, superassigns to the variable x the matrix passed as y  and to i NULL
##get prints what is stored in x
##setinverse superassigns the value passed to the function to the variabel i
##getinverse prints what is stored in the value i

makeCacheMatrix <- function(x = matrix(),...) {
        i<<-NULL                               ##thanks to this line there is no need to check if the matrix has been changed
        set<-function(y){
                x<<-y
                i<<-NULL
        }
        get <-function() x
        setinverse <-function(inverse) i<<-inverse
        getinverse <-function () i
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


##Description of cacheSolve function: 
##checks whether the inverse of the matrix has already been calculated and if the matrix to be inverted is stil the same
##if yes it retrieves and prints it without recomputing it
##if not it computes it
##then it superassigns it to variable 'i'  by using the function setinverse in makeCacheMatrix function 
##and finally returns (prints) it 

cacheSolve <- function(x, ...) {
        inv <-x$getinverse()
        if(!is.null(inv)){       #don't need to check if the matrix is the same: if the matrix get changed through the makeCacheMatrix, inv becomes = NULL
                print("getting cached data")
                return(inv)
        }
        data<-x$get()
        inv <-solve(data)
        x$setinverse(inv)  ##uses the function setinverse in makeCacheMatrix to superassing inv to the variabel i
        inv        ## Return a matrix that is the inverse of 'x'
}
