
# Valor inicial de x0 (valor supuesto)
x0=5

# Valor de precisión (delta)
delt=0.00001 

#Número de iteraciones
n=15

# Escritura de la función (cambiar por los valores deseados)
f=function(x) 2*x**3-8*x**2+10*x-15


plot(f,-3,3,
     lwd=1,
     main="Grafico de f(X)",
     col="purple",
     xlab="X",
     ylab="Y",
     axes=TRUE,
     n=1000)

# Derivada de la función (calcular la derivada de la función anterior)
df=function(x) 6*x**2-16*x+10

# Ciclo de iteraciones y resultados
for (i in 1:n) {
  x1=x0-f(x0)/df(x0)
  print(c(i,x0,x1)); error=abs(x1-x0)
  if (error<delt){
    cat("La solución converge en ",i , "iteraciones. raíz= ", x1);
    break()}
  x0=x1}
print("Máximo número de iteraciones alcanzada !!!")