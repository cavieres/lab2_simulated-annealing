# Para ejecutar script, realizar en consola:
# > setwd("<letra-unidad>:\\<carpeta-proyecto>")
#   Ej: setwd("C:\\Users\\cavie\\desarrollo\\lab2_simulated-annealing")
# > source("simulated-annealing.R")
# > SimulatedAnnealing(500, 0.01, 6)

# Incluir liberia QAP para 
# calculos de costos.
source("qap.R")

SimulatedAnnealing = function(Tmax, Tmin, maxRepPorTemp) {

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
    print(Temp)
    # Hasta criterio de detencion.
    if (Temp < Tmin)
      break
  }
  
  return(sOptima) # Mejor solucion encontrada.
}

# Decrecimiento de temperatura lineal.
gLinear = function(T0, i){
  # TODO: obtener esta constante desde alguna confuguracion.
  beta = 12 # Valor Cte
  
  return(T0 - i * beta)
}

# Decrecimiento de temperatura geometrica.
# TODO: implementarla.
gGeometric = function() {
  
}