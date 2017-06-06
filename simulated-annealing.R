# Para ejecutar script, realizar en consola:
# > setwd("<letra-unidad>:\\<carpeta-proyecto>")
#   Ej: setwd("C:\\Users\\cavie\\desarrollo\\lab2_simulated-annealing")
# > Establecer los parametros de configuracion del algoritmo en el archivo "config.yaml".
# > source("simulated-annealing.R")
# > SimulatedAnnealing()

library(tictoc)
library(yaml)

config = yaml.load_file("config.yaml")

# Incluir liberia QAP para 
# calculos de costos.
source("qap.R")

SimulatedAnnealing = function() {

  tic("Tiempo") # Medicion tiempo inicio ejecución.
  
  Tmax = config$SA$Tmax
  Tmin = config$SA$Tmin
  maxRepPorTemp = config$SA$maxRepPorTemp
  
  s = s0() # Generacion de la solucion inicial.
  sOptima = s0() # Se asume que la solucion optima es la inicial.
  Temp = Tmax # Temperatura inicial.
  
  repetTotal = 0
  
  repeat
  {
    repetPorTemp = 0 # Contador de repeticiones por temperatura.
    
    repeat # En una temperatura fija.
    {
      repetPorTemp = repetPorTemp + 1
    
      sPrima = N(s) # Generar un vecino aleatorio.
      
      deltaE = Costo(s) - Costo(sPrima)
      
      # Evaluacion para aceptar o no la solucion que minimiza la función:
      if(deltaE <= 0)
      {
        s = sPrima # Aceptar la solucion vecina.
      }
      else
      {
        
        pBoltzman = exp((-abs(deltaE))/Temp)
        
        if(runif(1,0,1) < pBoltzman)
        {
          s = sPrima # Aceptar la solucion vecina.
        }
      
      }
      
      # Retener mejor solucion global.
      if (Costo(s) < Costo(sOptima))
        sOptima = s
      
      # Hasta condicion de equilibrio.
      if (repetPorTemp == maxRepPorTemp)
      {
        repetTotal = repetTotal + repetPorTemp
        break
      }
        
    }
    
    Temp = gLinear(Tmax, repetTotal) # Actualizacion de temperatura.
    
    print(paste("Temp  : ", Temp))
    print(paste("Optimo: ", Costo(sOptima)))
    
    # Hasta criterio de detencion.
    if (Temp < Tmin)
      break
  }
  
  toc()
  
  print(paste("Flujos: ", config$QAP$Flows))
  print(paste("Distancias: ", config$QAP$Distances))
  print(paste("Costo: ", Costo(sOptima))) # Costo de solucion.
  
  return(sOptima) # Mejor solucion encontrada.
  
}

# Decrecimiento de temperatura lineal.
gLinear = function(T0, i){
  beta = config$Cooling$Linear$Beta # Valor Cte
  return(T0 - i * beta)
}

# Decrecimiento de temperatura geometrica.
# TODO: implementarla.
gGeometric = function() {
  
}