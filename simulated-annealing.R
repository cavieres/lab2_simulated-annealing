# Para ejecutar script, realizar en consola:
# > setwd("<letra-unidad>:\\<carpeta-proyecto>")
#   Ej: setwd("C:\\Users\\cavie\\desarrollo\\lab2_simulated-annealing")
# > source("simulated-annealing.R")
# > SimulatedAnnealing()

# Incluir liberia QAP para 
# calculos de costos.
source("qap.R")

SimulatedAnnealing = function(Tmax, Tmin, maxRepPorTemp) {

  s = s0 # Generacion de la solucion inicial.
  sOptima = s0 # Se asume que la solucion optima es la inicial.
  Temp = Tmax # Temperatura inicial.
  
  repeat
  {
    repetPorTemp = 0 # Contador de repeticiones por temperatura.
    
    repeat # En una temperatura fija.
    {
      repetPorTemp = repetPorTemp + 1
    
      sPrima = N(s) # Generar un vecino aleatorio.
      
      deltaE = Costo(s) - Costo(sPrima)
      
      # Evaluacion para aceptar o no la solucion # que minimiza la función:
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
        repetPorTemp = 0 # Contador de repeticiones por temperatura. 
        break
      }
        
    }
    
    Temp = g(Temp) # Actualizacion de temperatura.
    
    # Criterio de detencion.
    if (Temp < Tmin)
      break
  }
  
  return(Costo(sOptima)) # Mejor solucion encontrada.
}