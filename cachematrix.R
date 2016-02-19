
makeCacheMatrix <- function(x = matrix()) {
  q <- NULL
  set <- function(item) {
    x <<- item
    q <<- NULL
  }
  get <- function() x
  setInverse <- function(inv) q <<- inv
  getInverse <- function() q
  list(set = set, get = get,setInverse = setInverse,getInverse = getInverse)
}

cacheSolve <- function(x, ...) {
  inversedMatrix <- x$getInverse()
  if(!is.null(inversedMatrix)) {
    message("getting cached data")
    return(inversedMatrix)
  }
  inversedMatrix <- solve(x$get())
  x$setInverse(solve(x$get()))
  inversedMatrix
}
#Run Time Example
# 
# squareMatrix <- matrix( c(2, 4, 3, 1, 5, 7, 6, 1, 5), nrow=3,  ncol=3) 
#> squareMatrix
#     [,1] [,2] [,3]
#[1,]    2    1    6
#[2,]    4    5    1
#[3,]    3    7    5
#> my <- makeCacheMatrix(squareMatrix)
#> cacheSolve(my)
#           [,1]        [,2]        [,3]
#[1,]  0.1855670  0.38144330 -0.29896907
#[2,] -0.1752577 -0.08247423  0.22680412
#[3,]  0.1340206 -0.11340206  0.06185567
#> cacheSolve(my)
#getting cached data
#           [,1]        [,2]        [,3]
#[1,]  0.1855670  0.38144330 -0.29896907
#[2,] -0.1752577 -0.08247423  0.22680412
#[3,]  0.1340206 -0.11340206  0.06185567
#> solve(squareMatrix)
#           [,1]        [,2]        [,3]
#[1,]  0.1855670  0.38144330 -0.29896907
#[2,] -0.1752577 -0.08247423  0.22680412
#[3,]  0.1340206 -0.11340206  0.06185567
