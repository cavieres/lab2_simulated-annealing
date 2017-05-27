# Para ejecutar script, realizar en consola:
# > setwd("<letra-unidad>:\\<carpeta-proyecto>")
#   Ej: setwd("C:\\Users\\cavie\\desarrollo\\lab2_simulated-annealing")
# > source("simulated-annealing.R")
# > SimulatedAnnealing()

# Incluir liberia QAP para 
# calculos de costos.
source("qap.R")

# Funcion Objetivo.
#fdx que se encarga de evaluar en la funci�n los valores:
funciondefinida=function(x)
{
  fdx=0 
  for(i in 1:1)
  {
    fdx=fdx+((100*(((x[i]^2)-x[i+1])^2))+((1-x[i])^2))
  }
  
  return(fdx)
  
}

SimulatedAnnealing = function() {
  #declaraci�n de las variables iniciales:
  
  #Limites del dominio de la variable x:
  limInf =-5.12
  limSup = 5.12 
  
  #velocidad de enfriamiento:
  velEnfriamiento=5
  
  # c�lculo de la temperatura inicial:
  alfaInicial=0.99
  
  #limites de la temperatura inicial entre 0.8 y 0.99:
  alfaEnfriamiento=runif(1,0.8,0.99)
  
  #temperatura final del sistema:
  tempFinal=0.05
  
  #Contador de ejecuciones del algoritmo:
  cuentaRepeticiones=0
  
  #N�mero m�ximo de ejecuciones del algoritmo:
  maxRepeticiones=5
  
  repeat
  {
    
    #escoger valores iniciales para inicializar las variables:
    x=c(3,3)
    
    # Calculamos el valor de la funcion # con base en x1 y x2 iniciales:
    g=funciondefinida(x)
    
    #estructura del vecindario a visitar:
    sigma=(limSup -limInf)/6
    
    #temperatura inicial tomada:
    temp=10000
    
    #Flag para saber si fue aceptada la soluci�n:
    aceptado=0
    
    #mejor soluci�n encontrada para el problema:
    anterior=g
    mejor=g
    
    repeat
    {
      #control de la velocidad de enfriamiento:
      cVelEnfriamiento=1
      
      #inicia la velocidad de enfriamiento:
      repeat
      {
        #ciclo que valida que los datos sigan en el dominio de la funci�n:
        repeat
        {
          xPrima1=x[1]+rnorm(1,0,sigma)
          xPrima2=x[2]+rnorm(1,0,sigma)
          
          
          # Control de parada del ciclo de perturbacion 
          if(( xPrima1 >= limInf && xPrima1<= limSup) && (xPrima2 >= limInf && xPrima2 <=limSup))
          {
            break
          }
        }
        
        #se crea una lista de x1 y x2 perturbadoras:
        xPrima=c(xPrima1,xPrima2)
        
        #evaluaci�n de la funci�n con respecto a la principal:
        rho = funciondefinida(xPrima) - funciondefinida(x)
        
        # Evaluacion para aceptar o no la solucion # que minimiza la funci�n:
        if(rho < 0)
        {
          aceptado = 1 # Aceptar la solucion vecina.
        }
        else
        {
          
          pBoltzman=exp((-abs(rho))/temp)
          
          if(runif(1,0,1)< pBoltzman)
          {
            aceptado=1 
          }
          
          if(aceptado==1)
          {	
            #soluci�n actual, soluci�n perturbada:
            x=xPrima
            
            #calculo de la funci�n nuevamente:					
            g=funciondefinida(x)
            
            #control de funciones aceptado:
            aceptado=0
            
            if(g<=mejor)
            {
              mejor=g 
              guardar=c(x,temp,mejor) 
            }
            anterior=g
            
          }
          
          #velocidad de enfriamiento:
          cVelEnfriamiento=cVelEnfriamiento+1
          
          #control de la velocidad de enfriamiento:
          if(cVelEnfriamiento==velEnfriamiento){break}
        }
        
        # Funci�n de enfriamiento: Enfriamiento geom�trico:
        tempActual=temp 
        temp=alfaEnfriamiento*tempActual
        
        #control del ciclo de la temperatura:
        if(temp<=tempFinal){break}
        
      }
      
      #incremento de contador de repeticiones:
      cuentaRepeticiones=cuentaRepeticiones+1
      
      
      # Control del ciclo de ejecuciones del algoritmo 
      if(cuentaRepeticiones==maxRepeticiones){break}
    }
  }
}