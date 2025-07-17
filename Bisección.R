# Método de Bisección en R

biseccion <- function(f, a, b, tol = 1e-6, max_iter = 100) {
  if (f(a) * f(b) > 0) {
    stop("La función no cambia de signo en el intervalo dado")
  }
  
  iter <- 0
  while ((b - a)/2 > tol && iter < max_iter) {
    c <- (a + b)/2
    if (f(c) == 0) {
      return(c)
    } else if (f(a) * f(c) < 0) {
      b <- c
    } else {
      a <- c
    }
    iter <- iter + 1
  }
  return((a + b)/2)
}

# Definir una función ejemplo
f <- function(x) 3*x - 5

# Llamar al método de bisección
raiz <- biseccion(f, 0, 5)
cat("La raíz aproximada es:", raiz)
