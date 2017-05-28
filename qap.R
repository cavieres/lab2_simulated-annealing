# Algoritmo QAP.
# Para ejecutar script, realizar en consola:
# > setwd("<letra-unidad>:\\<carpeta-proyecto>")
#   Ej: setwd("C:\\Users\\cavie\\desarrollo\\lab2_simulated-annealing")
# > source("qap.R")
# > Costo(c(2, 1, 4, 3))

# Matriz de flujos.
# TODO: leer flujos desde archivo de texto.
F <- matrix(c(0, 3, 0, 2, 3, 0, 0, 1, 0, 0, 0, 4, 2, 1, 4, 0), nrow = 4, ncol = 4)

# Matriz de distancias
# TODO: leer distancias desde archivo de texto.
D <- matrix(c(0, 22, 53, 0, 22, 0, 40, 0, 53, 40, 0, 55, 0, 0, 55, 0), nrow = 4, ncol = 4)

# Ubicacion inicial.
# TODO: Generar ubicaciones optimas desde algoritmo.
# ubicaciones <- c(2, 1, 4, 3)

# Ubicacion de instalacion i en arreglo de ubicaciones
Fi = function(i, ubicaciones) {
  return(which(ubicaciones == i))
}

# Flujo entre las distancias i y j.
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
    for(j in i:length(ubicaciones)) {
        costo = costo + f(i, j) * d(Fi(i, ubicaciones), Fi(j, ubicaciones))
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
# TODO: Implementarla.
s0 = function() {
  return(c(2, 1, 4, 3))
}