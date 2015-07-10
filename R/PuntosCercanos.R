#' Calcula los puntos más cercanos
#' 
#' Identifica la pareja de puntos mas cercanos en un espacion N-dimensional
#' @param x Variable de tipo numerico que puede ser un vector, una matriz, o un data.frame.
#' Este campo es obligatorio. 
#' @param y Variable de tipo numerico que puede ser un vector, una matriz, o un data.frame.
#' Este campo es opcional. 
#' @return Devuelve mensaje indicando el par de puntos más cercanos.
#' @export
#' @examples
#' Creamos unos datos cualquiera (vectores y matrices)
#' x <- rnorm(1:10, 2, 11)               # Un vector X formado por 10 elementos
#' y <- rnorm(1:10, 3, 7)                # Un vector Y formado por 10 elementos
#' z <- rnorm(1:10, 5, 5)                # Un vector Z formado por 10 elementos
#' w <- rnorm(1:10, 7, 3)                # Un vector W formado por 10 elementos
#' coords.2D <- cbind(x, y)              # Una matriz de dimensiones 10x2 (ie. puntos en espacio 2D)
#' coords.2Db <- cbind(z, w)             # Una matriz 'alternativa' de dimensiones 10x2 (ie. puntos en espacio 2D)
#' coords.3D <- cbind(x, y, z)           # Una matriz de dimensiones 10x3 (ie. puntos en espacio 3D)
#' coords.4D <- cbind(x, y, z, w)        # Una matriz de dimensiones 10x4 (ie. puntos en espacio 4D)
#' coords.10D <- matrix(rnorm(100), 10)  # Una matriz de dimensiones 10x10 (ie. puntos en espacio 10D)
#' 
#' Buscamos los elementos mas cercanos en 1 dimension
#' > PuntosCercanos(x)
#' Podemos visualizar el dendograma obtenido tras realizar un cluster jerarquico 
#' y asi confirmar visualmente que los puntos identificados por la funcion son, 
#' en efecto, los mas cercanos.
#' > plot((hclust(dist(x))), hang=-1)
#' 
#' Buscamos los elementos mas cercanos en 2 dimensiones
#' > PuntosCercanos(x, y)                       # opcion A: proporcionamos dos vectores
#' > PuntosCercanos(coords.2D)                  # opcion B: proporcionamos una matriz con dos columnas
#' > plot((hclust(dist(coords.2D))), hang=-1)   # Confirmamos visualizando el cluster
#' 
#' Buscamos los elementos mas cercanos en 3 dimensiones
#' > PuntosCercanos(coords.2D, z)               # opcion A: proporcionamos una matriz (10x2) y un vector
#' > PuntosCercanos(coords.3D)                  # opcion B: proporcionamos una matriz (10x3)
#' > plot((hclust(dist(coords.3D))), hang=-1)   # Confirmamos visualizando el cluster
#' 
#' Buscamos los elementos mas cercanos en 4 dimensiones
#' PuntosCercanos(coords.2D, coords.2Db)        # opcion B: proporcionamos dos matrices [2x(10x4)]
#' PuntosCercanos(coords.4D)                    # opcion B: proporcionamos una matriz (10x4)
#' > plot((hclust(dist(coords.4D))), hang=-1)   # Confirmamos visualizando el cluster
#' 
#' Buscamos los elementos mas cercanos en 10 dimensiones
#' > PuntosCercanos(coords.10D)                 # Proporcionamos una matriz (10x10)
#' > plot((hclust(dist(coords.10D))), hang=-1)  # Confirmamos visualizando el cluster

PuntosCercanos <- function(x, y = NULL){
  no.null = function(x){!is.null(x)}
  if(no.null(y) == TRUE)
    X <- cbind(x, y)
  else
    X <- x
  distancias <- dist(X, method = "euclidean")
  dist.minima <- min(distancias)
  if (is.null(dim(X)) == TRUE)
    par.de.elementos <- combn(1:length(X), 2)[, which.min(distancias)]
  else
    par.de.elementos <- combn(1:nrow(X), 2)[, which.min(distancias)]
  cat("Los dos puntos mas cercanos son los puntos", par.de.elementos[1], "y", par.de.elementos[2], 
      "los cuales se encuentran a una distancia (euclidea) de", dist.minima, "\n", "\n")
}