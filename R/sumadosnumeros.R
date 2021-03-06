#' Suma dos n�meros
#' 
#' Esta funci�n acepta dos n�meros y los suma.
#' @param x Primer valor num�rico de la suma
#' @param y Segundo valor num�rico de la suma
#' @note Esta funci�n acepta como par�metros s�lamente valores �nicos.
#' En el caso de que se proporcionen m�s o menos parametros de los dos aceptados, 
#' la funci�n dar� error y parar�.
#' @return Devuelve la suma de los dos valores
#' @export
#' @examples
#' > suma.dos.numeros(2,2)
#' > suma.dos.numeros(2.5)              # Devuelve error
#' > suma.dos.numeros(c(2,2), c(2,5))   # Devuelve error

suma.dos.numeros <- function(x,y){
  return (x+y)
}