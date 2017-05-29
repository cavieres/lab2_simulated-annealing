# Algoritmo QAP.
# Para ejecutar script, realizar en consola:
# > setwd("<letra-unidad>:\\<carpeta-proyecto>")
#   Ej: setwd("C:\\Users\\cavie\\desarrollo\\lab2_simulated-annealing")
# > source("qap.R")
# > Costo(c(2, 1, 4, 3))

# Matriz de flujos.
#F <- matrix(c(0, 3, 0, 2, 3, 0, 0, 1, 0, 0, 0, 4, 2, 1, 4, 0), nrow = 4, ncol = 4)
F <- read.table("inputs\\chr12a-f.txt", header = FALSE)

# Matriz de distancias
#D <- matrix(c(0, 22, 53, 0, 22, 0, 40, 0, 53, 40, 0, 55, 0, 0, 55, 0), nrow = 4, ncol = 4)
D <- read.table("inputs\\chr12a-d.txt", header = FALSE)

# Ubicacion de instalacion i en arreglo de ubicaciones
Fi = function(i, ubicaciones) {
  return(which(ubicaciones == i))
}

# Flujo entre las instalaciones i y j.
f = function(i, j) {
  return(F[i, j])
}

# Distancia entre las ubicaciones k y l.
d = function(k, l) {
  return(D[k, l])
}

# Costo solucion (funcion objetivo) 
# de arreglo de ubicaciones.
Costo = function(ubicaciones) {
  costo = 0
  
  for(i in 1:length(ubicaciones)) {
    for(j in 1:length(ubicaciones)) {
      
      flujo = f(i, j)
      distancia = d(Fi(i, ubicaciones), Fi(j, ubicaciones))
      
      if (!is.null(flujo) && !is.null(distancia)) {
        costo = costo + flujo * distancia
      }
    }
  }
  
  return(costo)
}

# Busqueda de vecindad aleatoria por medio de swap.
N = function(s) {
  
  sPrima = s
  
  instalacionInicial = sample(length(sPrima), 1)
  
  if (instalacionInicial == length(sPrima))
    instalacionFinal = instalacionInicial - 1
  else
    instalacionFinal = instalacionInicial + 1
  
  aux = sPrima[instalacionInicial]
  sPrima[instalacionInicial] = sPrima[instalacionFinal]
  sPrima[instalacionFinal] = aux
  
  return(sPrima)
}

# Generacion de solucion inicial.
s0 = function() {
  #return(c(2, 1, 4, 3))
  
  #cargar arreglo de flujos:
  flujos = F
  
  #llamar a libreria:
  library("plyr")
  
  #ordenar el arreglo de flujos de forma creciente:
  
  #creación del vector(arreglo de flujos):
  suma_f<- rep(0, len = nrow(flujos)) 
  resultado=0;
  
  #sumar los flujos:
  for(i in 1:nrow(flujos))
  {
    resultado=0;
    for(j in 1:ncol(flujos))
    {
      resultado <- resultado + flujos[i,j];
    }		
    suma_f[i]=resultado;
  }
  
  #ordenar de forma decreciente:
  indice_flujos <-order(suma_f,decreasing = TRUE)
  
  #ver valor del arreglo:
  suma_f
  
  #cargar arreglo de distancias:
  distancias = D
  
  
  #creación del vector(arreglo de distancias):
  suma_d<- rep(0, len = nrow(flujos)) 
  
  #sumar las distancias:
  for(i in 1:nrow(flujos))
  {
    resultado=0;
    for(j in 1:ncol(flujos))
    {
      resultado <- resultado + distancias[i,j];
    }		
    suma_d[i]=resultado;
  }
  
  #ver valor del arreglo:
  suma_d
  
  #ordenar de forma creciente para las distancias:
  indice_distancias <- order(suma_d)
  
  #generación de la solución óptima:
  solucion<- rep(0, len = nrow(flujos))
  
  #arma el arreglo de soluciones:
  for(i in 1:nrow(flujos))
  {
    solucion[indice_distancias[i]]<- indice_flujos[i]
  }

  return(solucion)
}