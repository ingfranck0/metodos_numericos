# Función para Gauss-Seidel
gauss_seidel <- function(A, b, tol = 1e-10, max_iter = 100) {
  n <- nrow(A)
  x <- rep(0, n)
  
  for (iter in 1:max_iter) {
    x_old <- x
    
    for (i in 1:n) {
      sum1 <- sum(A[i, -i] * x[-i])
      x[i] <- (b[i] - sum1) / A[i, i]
    }
    
    # Criterio de parada
    if (sqrt(sum((x - x_old)^2)) < tol) {
      cat("Convergencia alcanzada en", iter, "iteraciones.\n")
      return(x)
    }
  }
  cat("Máximo número de iteraciones alcanzado.\n")
  return(x)
}

#generar la matrices 
A<- matrix(c(3,-1,-1,
             -1,3,1,
             2,1,4), nrow = 3,byrow= TRUE)
b<-c(1,3,7)

# Resolver usando Gauss-Seidel
solution <- gauss_seidel(A, b)

# Resultado
print("Solución del sistema (Gauss-Seidel):")
print(solution)

# Gráfica de la solución
barplot(solution, 
        names.arg = paste("x", 1:length(solution), sep = ""),
        col = "#FF9494",
        main = "Solución por Gauss-Seidel",
        xlab = "Incógnitas",
        ylab = "Valor",
        ylim = c(0, max(solution) + 0.2))
grid(nx = NA, ny = NULL)