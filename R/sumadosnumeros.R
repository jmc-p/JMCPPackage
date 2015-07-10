#' Suma dos números
#' 
#' Esta función acepta dos números y los suma.
#' @param x Primer valor numérico de la suma
#' @param y Segundo valor numérico de la suma
#' @note Esta función acepta como parámetros sólamente valores únicos.
#' En el caso de que se proporcionen más o menos parametros de los dos aceptados, 
#' la función dará error y parará.
#' @return Devuelve la suma de los dos valores
#' @export
#' @examples
#' > suma.dos.numeros(2,2)
#' > suma.dos.numeros(2.5)              # Devuelve error
#' > suma.dos.numeros(c(2,2), c(2,5))   # Devuelve error

suma.dos.numeros <- function(x,y){
  return (x+y)
}