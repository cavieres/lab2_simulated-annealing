SolucionInicial = function(flujosFile, distanciasFile) {
  #cargar arreglo de flujos:
  flujos <-read.table(flujosFile, header = FALSE)
  
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
  distancias <-read.table("distancias.txt", header = FALSE)
  
  
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
  
  #testear el valor de los arreglos para revisar el correcto funcionamiento:
  #solucion
  #indice_flujos
  #indice_distancias
  return(solucion)
}