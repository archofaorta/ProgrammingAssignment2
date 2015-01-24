##----------------------------------------------------------------------------------------------------------------
## Solution for Assignment 2 
## Functions to return the inverse of Matrix, If Inverse is found in the Cache and Matrix has not changed, 
##	return the inverse from Cache 
##	else compute the inverse and populate in Cache and return the computed inverse
## 
## Assumptions:- 
##	matrix Passed to the function as an argument is  a valid invertible matrix
##	(functions does not perform any checks to determine this)
##  
## Author: Rajen Sarda
##------------------------------------------------------------------------------------------------------------------

##------------------------------------------------------------------------------------------------------------------
## Following function 'makeCacheMatrix' creates a R object to 
## - Initialize a variable 'm' to store the inverse matrix
## - get() function to return original matrix
## - setInverseMatrix() function to store/cache the inverse of the original matrix
## - getInverseMatrix() function to return the cached inverse 
## - list object ("vector"), which is a list containing above functions 
##------------------------------------------------------------------------------------------------------------------

makeCacheMatrix <- function(x = matrix()) {
	
	#Initialize a variable 'm' to store the inverse matrix
	m <- NULL
        
	set <- function(y) {
			x <<- y
			m <<- NULL
	}
    
	#get original matrix
	get <- function() x
	
	#set inverse matrix and store it in object 'm' for caching
	setInverseMatrix <- function(InvMatrix) m <<- InvMatrix
    
	#get inverse matrix, returns cached object 'm' 
	getInverseMatrix <- function() m
	
	# return a list of functions as an R object
	list(set = set, get = get, setInverseMatrix = setInverseMatrix,getInverseMatrix = getInverseMatrix)
             
			 
}


##------------------------------------------------------------------------------------------------------------------
## Following function 'cacheSolve' creates a R object to 
## - return the inverse matrix from cache with appropriate message
## - If no cache data found,
##	- retrieve the original matrix
## 	- calculate the inverse of original matrix
## 	- store the result in cached object 
##  - return the inverse
##--------------------------------------------------------------------------------------------------------------------


cacheSolve <- function(x, ...) {
	
	#Get the cached value
	m <- x$getInverseMatrix()
	
	#If cache data found, return cached data
	
	if(!is.null(m)) {
		message("Found cached data, returning inverse from cache...")
		return(m)
	}
	
	else {
		message("cached data not found, Computing inverse matrix...")
		#retrieve the original matrix
		data <- x$get()
		
		#calculate the inverse of original matrix
		m <- solve(data)
		
		#store the result (inverse matrix) in cached object
		x$setInverseMatrix(m)
		
		#Return a matrix that is the inverse of 'x'
		return(m) 
	}
	  
}
