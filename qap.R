# Algoritmo QAP.
# Para ejecutar script, realizar en consola:
# > setwd("<letra-unidad>:\\<carpeta-proyecto>")
# > source("qap.R")
# > Costo()

# Matriz de flujos.
# TODO: leer flujos desde archivo de texto.
F <- matrix(c(0, 3, 0, 2, 3, 0, 0, 1, 0, 0, 0, 4, 2, 1, 4, 0), nrow = 4, ncol = 4)

# Matriz de distancias
# TODO: leer distancias desde archivo de texto.
D <- matrix(c(0, 22, 53, 0, 22, 0, 40, 0, 53, 40, 0, 55, 0, 0, 55, 0), nrow = 4, ncol = 4)

# Ubicacion inicial.
# TODO: Generar ubicaciones optimas desde algoritmo.
ubicacion <- c(2, 1, 4, 3)

# Ubicacion de instalacion i
Fi = function(i) {
  return(which(ubicacion == i))
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
# TODO: pasar por parametro de funcion el arreglo de ubicaciones.
Costo = function() {
  costo = 0
  
  for(i in 1:length(ubicacion)) {
    for(j in i:length(ubicacion)) {
        costo = costo + f(i, j) * d(Fi(i), Fi(j))
    }
  }
  
  return(costo)
}

# Busqueda de vecindad
# TODO: implementar metodo.
Vecindad = function() {
  
}