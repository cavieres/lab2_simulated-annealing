#cargar arreglo de flujos:
flujos <-read.table("C:\\Users\\fdo\\Desktop\\lab2_simulated-annealing-master/flujos.txt", header = FALSE)

#llamar a libreria:
library("plyr")

#renombrar las variables:
flujos<-rename(flujos, c(V1="f1"))
flujos<-rename(flujos, c(V2="f2"))
flujos<-rename(flujos, c(V3="f3"))
flujos<-rename(flujos, c(V4="f4"))
flujos<-rename(flujos, c(V5="f5"))
flujos<-rename(flujos, c(V6="f6"))
flujos<-rename(flujos, c(V7="f7"))
flujos<-rename(flujos, c(V8="f8"))
flujos<-rename(flujos, c(V9="f9"))
flujos<-rename(flujos, c(V10="f10"))

#ordenar el arreglo de flujos de forma creciente:

#creación del vector(arreglo de flujos):
suma_f<- rep(0, len = 40) 
resultado=0;

#sumar los flujos:
for(i in 1:40)
{
  resultado=0;
  for(j in 1:10)
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
distancias <-read.table("C:\\Users\\fdo\\Desktop\\lab2_simulated-annealing-master/distancias.txt", header = FALSE)


#renombrar las variables:
distancias<-rename(distancias, c(V1="d1"))
distancias<-rename(distancias, c(V2="d2"))
distancias<-rename(distancias, c(V3="d3"))
distancias<-rename(distancias, c(V4="d4"))
distancias<-rename(distancias, c(V5="d5"))
distancias<-rename(distancias, c(V6="d6"))
distancias<-rename(distancias, c(V7="d7"))
distancias<-rename(distancias, c(V8="d8"))
distancias<-rename(distancias, c(V9="d9"))
distancias<-rename(distancias, c(V10="d10"))


#creación del vector(arreglo de distancias):
suma_d<- rep(0, len = 40) 

#sumar las distancias:
for(i in 1:40)
{
  resultado=0;
  for(j in 1:10)
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
solucion<- rep(0, len = 40)

#arma el arreglo de soluciones:
for(i in 1:40)
{
  solucion[indice_distancias[i]]<- indice_flujos[i]
}

#testear el valor de los arreglos para revisar el correcto funcionamiento:
solucion
indice_flujos
indice_distancias

