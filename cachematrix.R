## Esta función toma una matriz cuadrada y la convierte en una 
## matriz especial que guarda en caché como inversa

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function()
    x
  setinverse <- function(inverse)
    m <<- inverse
  getinverse <- function()
    m
  list(
    set = set,
    get = get,
    setinverse = setinverse,
    getinverse = getinverse
  )
  
}


## Esta función devuelve el cálculo de la inversa y lo carga o 
## lo trae del caché

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if (!is.null(m)) {
    message("Matriz recuperada del caché")
    return(solve(m)) ## Retorna la matriz original
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  message("Matriz inversa")
  m  ## Retorna la matriz inversa de 'x'
}