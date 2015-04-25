## The following functions when used in conjunction, return the inverse of a matrix. 
##To reduce the computation time it is first checked in Cache whether 
##the inverse for the matrix already exists from a previous computation.
##If so, re-computation of inverse is avoided. If not, computation is carried out.


## The makeCacheMatrix takes a matrix as input(default is a 1X1 "NA" matrix)
## and creates a special list out of it which will be the input for cacheSolve function


makeCacheMatrix <- function(x = matrix()) {
        
                
        i<- matrix(,nrow=nrow(x),ncol=ncol(x))
                
        set<- function(y=matrix()){
                x<<-y
                i<<- matrix(,nrow=nrow(x),ncol=ncol(x))
        }
        
        get<-function() x
        
        setinverse<- function(inverse) i<<-inverse
        
        getinverse<- function() i
        
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)

}


## cacheSolve function returns inverse of an invertible matrix 
##It sources the inverse from cache if it is already computed.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        
        i<-x$getinverse()    
                
        if(!is.na(i) ) {
                
                message("getting cached data")
                
                return(i)                
                
        } else{          
                datamat<- x$get()
                                
                i<- solve(datamat)       #It is assumed that the data matrix is invertible
                
                x$setinverse(i)
             
                i        
                
}  
        
}
