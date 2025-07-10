 #        == Método de la Secante == 

# Valores iniciales
x0=1; xant=0;error=0;
# Error permitido (delta)
delt=0.00000001;
# Número de máximo iteraciones 
n=50

# Función para encontrar si raíz
f=function(x) sin(x)+cos(1-x**2)-1

plot(f,-3,3,
     lwd=1,
     main="Grafico de f(X)",
     col="purple",
     xlab="X",
     ylab="Y",
     axes=TRUE,
     n=1000)

# Cálculos usando el método
for (i in 1:n) {
  numera=f(x0)*(xant-x0)
  denomi=f(xant)-f(x0)
  x1=x0-(numera/denomi)
  print(c(i, x0, xant, x1)); error=abs(x1-x0)
  if (error<delt){
    print(" ")
    cat("La solución converge en ",i , "iteraciones. raíz= ", x1);
    break()}
  x0=x1}

print("Máximo número de iteraciones alcanzada !!!")
