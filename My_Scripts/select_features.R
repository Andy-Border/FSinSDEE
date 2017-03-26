require(FSelector)
require(VIM)

#_______________________________________________________________________________ 
# Función para eliminar los casos en los que el valor de
# una determinada variable está missing (var.name)
elimina.casos.con.NAs <- function(var.name, data, verbose=FALSE){
  nrows <- nrow(data)
  casos.sin.NAs <- !is.na( data[[var.name]] )
  if(verbose){
    cat("\nLa variable",var.name,"tiene",nrows,"casos, de los cuales",sum(casos.sin.NAs),"son válidos.\n")
  }
  # Estaría bien que el defecto fuera drop=FALSE, aunque en este caso con varias columnas en data
  # sería equivalente a return(data[casos.sin.NAs,])
  return(data[casos.sin.NAs,,drop=FALSE])
}

#_______________________________________________________________________________
# Función para eliminar las columnas con un porcentaje de datos perdidos
# superior a un cierto umbral (por defecto umbral=60)
elimina.var.con.demasiados.NAs <- function(data, umbral=60, verbose=FALSE){
  # nº de filas
  nrows <- nrow(data)
  # Porcentaje de datos perdidos por columna
  porcentajes.de.NAs <- 100 * colSums(is.na(data))/nrows
  if(verbose){
    cat("\nPorcentajes de datos perdidos por columna:\n")
    print(porcentajes.de.NAs)
    cat("\nSe eliminan las variables con más de un",umbral,"%.\n")
    }
  return(data[,porcentajes.de.NAs < umbral])
}

#_______________________________________________________________________________
# Función para ordenar las variables independientes según ganancia de información.
# El resultado tiene varios campos:
# - var.dependiente: nombre de la variable sobre la que se calcula
                            # la ganancia de información
# - campo formula: fórmula utilizada para calcular la ganancia de información
# - campo ganancias.ordenadas: data.frame con los valores de ganancia de información.
# Los nombres de cada fila corresponden a los nombres de cada variable.
# - campo var.nominales: vector con todas las variables nominales,
# ordenadas según ganancia de información
# - campo var.continuas: vector con todas las variables continuas,
# ordenadas según ganancia de información

ordena.por.ganancia <- function(var.dependiente, data){
  # Basic format of a formula: response variable~predictor variables
  # Use �?." to indicate “include all variables"
  formula <- as.formula(paste(var.dependiente,".",sep="~"))
  ganancias <- information.gain(formula, data)
  indices.ordenados <- order(ganancias$attr_importance, decreasing=TRUE)
  # You have to add drop=FALSE to keep R from converting your data frame to a vector
  # if you only select a single column
  ganancias.ordenadas <- ganancias[indices.ordenados,,drop=FALSE]
  var.nominales <- vector()
  var.continuas <- vector()
  
  for(nombre in rownames(ganancias.ordenadas)){
    # Aunque solo tenemos factores (ni ordered ni character)
    if(is.factor(data[[ nombre ]]) | 
         is.ordered(data[[ nombre ]]) | 
         is.character(data[[ nombre ]])){
      var.nominales <- c(var.nominales, nombre)
    }else{
      var.continuas <- c(var.continuas, nombre)
    }
  }
  
  resultado <- list(var.dependiente=var.dependiente)
  resultado$formula <- formula
  resultado$ganancias.ordenadas <- ganancias.ordenadas
  resultado$var.nominales <- var.nominales
  resultado$var.continuas <- var.continuas
  
  return(resultado)
}

#_______________________________________________________________________________
# minimum redundancy - maximum relevance ("mRMR" for short)
ordena.por.coeficiente <- function(var.dependent, data){
  #browser()
  formula <- as.formula(paste(var.dependent,".",sep="~"))
  gains <- information.gain(formula, data)
  indices.ordered <- order(gains$attr_importance, decreasing=TRUE)
  var.ordered <- rownames( gains[indices.ordered,,drop=FALSE] )
  
  var.seleccionadas <- vector()
  # MRMR of selected variables
  # In the first position contains the highest GI of the independent variables.
  mrmr <- vector()
  mrmr[1] <- gains$attr_importance[indices.ordered[1]]
  
  total.iter <- length(var.ordered)
  for( iteracion in 1:total.iter ){
    
    # The first fruit of the following order (MRMR)
    # In the first iteration, it inserts directly the first ordered by GI simply
    # In the last iteration, they make calculations that do not work at all
    var.seleccionadas <- c(var.seleccionadas, var.ordered[1])
    # Variables that have just been entered in var.selected are removed from var.
    var.ordered <- var.ordered[-1]
    coefs <- vector()
    # Selecting the next variable takes into account your GI
    # But also the gain over the already selected
    for(var.prueba in var.ordered){
      formula <- as.formula(paste(var.dependent,var.prueba,sep="~"))
      # GI of the test variable with respect to the dependent
      # The bigger the better.
      I <- information.gain(formula , data)[1,1]
      info.parciales <- vector()
      # With respect to those already selected, the GI
      # The mean of the partial the lower the better
      for(var.selec in var.seleccionadas){
        formula <- as.formula(paste(var.selec,var.prueba,sep="~"))
        I.parcial <- information.gain(formula , data)[1,1]
        info.parciales <- c(info.parciales, I.parcial)
      }
      # And GI is preserved - the mean of the partial
      coef <- I - mean(info.parciales)
      coefs <- c(coefs, coef)
    }
    # Then re-order the indexes taking into account coefs
    # The bigger the better.
    indices.ordered <- order(coefs, decreasing=TRUE)
    #result <- data.frame(coef=coefs[indices.ordered],row.names=var.ordered[indices.ordered])
    var.ordered <- var.ordered[indices.ordered]
    result <- data.frame(coef=coefs[indices.ordered],row.names=var.ordered)
    #print(result)
    mrmr <- c(mrmr, coefs[indices.ordered][1])
    
  }
  
  # Remove the last element of vector (12)
  mrmr <- mrmr[-length(mrmr)]
  
  var.norminal <- vector()
  var.continuous <- vector()
  
  for(nombre in var.seleccionadas){
    if(is.factor(data[[ nombre ]]) | 
       is.ordered(data[[ nombre ]]) | 
       is.character(data[[ nombre ]])){
      var.norminal <- c(var.norminal, nombre)
    }else{
      var.continuous <- c(var.continuous, nombre)
    }
  }
  
  result <- list(var.dependent=var.dependent)
  result$coef.ordered <- var.seleccionadas
  result$mrmr <- mrmr
  result$var.norminal <- var.norminal
  result$var.continuous <- var.continuous
  
  return(result)
  
}
#_______________________________________________________________________________
# Sobre un campo concreto, va recorriendo toda la columna e imputando
# con la función KNN de VIM el único NA que hay generado en la columna.
# El resultado de la función es el MMRE así obtenido.
calculaMMRE <- function(campo.a.imputar, data, valor.de.k=5){
  ntotal <- nrow(data)
  # Matriz de 2 columnas (Valor original y Valor imputado) y tantas filas como el dataframe
  matriz.resultados <- matrix(2*ntotal,ncol=2,nrow=ntotal)
  colnames(matriz.resultados) <- c("Valor original","Valor imputado")
  # Allows you to redirect output somewhere else, such as a file or /dev/null,
  # para que knn no imprima mensajes en pantalla (hace que vaya más rápido)
  # Si se descomenta aquí, descomentar también más abajo
  # on Unix-like systems
  #sink("/dev/null")
  # on Windows
  sink("NUL")
  for(i in 1:ntotal){
    dataset.testeo <- data
    # En la primera columna de la matriz de resultados meto el valor original
    matriz.resultados[i,1] <- dataset.testeo[[campo.a.imputar]][i]
    # Una vez me he guardado el valor original, le asigno NA a esa celda para imputarla
    dataset.testeo[[campo.a.imputar]][i] <- NA  
    # Imputa con la función kNN de VIM el único NA que hay en la columna campo.a.imputar
    dataset.testeo <- kNN( dataset.testeo, variable=c(campo.a.imputar), numFun=mean, k=valor.de.k )
    # Copio en la segunda columna el valor imputado
    matriz.resultados[i,2] <- dataset.testeo[[campo.a.imputar]][i]
  }
  # Stuff you do want to see
  # Descomentar si arriba se ha descomentado
  sink()
  # MMRE (Mean Magnitude of Relative Error)
  MMRE <- (1/ntotal)*sum(abs(matriz.resultados[,1] - matriz.resultados[,2])/matriz.resultados[,1])
  return(MMRE)
}

#_______________________________________________________________________________
# how to determine the optimal number of features?
# umbral.MMRE (%): Las variables que no producen una mejora
# en los resultados son descartadas
# Esta mejora puede ser un x% más del MMRE anterior
# Por defecto solo se introduce una variable si mejora el MMRE anterior
extrae <- function(var.dependiente,varNumericas,varNominales,data,umbral.MMRE=0,verbose=FALSE){
  cat("Comenzando el cálculo de MMREs\n")
  total.iteraciones <- length(varNumericas)+length(varNominales)
  # Cálculo del umbral teniendo en cuenta dato de entrada: la mejora puede ser un x% más
  umbral <- 1 + umbral.MMRE/100
  valor.de.k <- 2
  var.elegidas <- vector()
  var.eliminadas <- vector()
  MMREs <- vector()
  hay.var.numericas <- hay.var.nominales <- TRUE
  iteracion <- 1
  # Inicialización de MMRE.min,
  # luego irá adoptando el valor calculado mínimo entre numéricas o nominales
  MMRE.min <- Inf
  while(hay.var.numericas | hay.var.nominales){
    # Son necesarias estas inicializaciones cuando se acaban
    # las varNumericas o varNomiales y el MMRE correspondiente no se calcula entonces
    MMRE.num <- MMRE.nom <- Inf
    
    if(length(varNumericas) > 0){
      # A las variables ya elegidas, se añade la siguiente varNumerica
      campos <- c(var.dependiente,var.elegidas,varNumericas[1])
      # Y se calcula el MMRE teniendo en cuenta este subset de variables
      MMRE.num <- calculaMMRE(var.dependiente,data[,campos,drop=FALSE],valor.de.k)
    }else{
      hay.var.numericas<- FALSE
    }
    
    if(length(varNominales) > 0){
      # A las variables ya elegidas, se añade la siguiente varNominal
      campos <- c(var.dependiente,var.elegidas,varNominales[1])
      MMRE.nom <- calculaMMRE(var.dependiente,data[,campos,drop=FALSE],valor.de.k)
    }else{
      hay.var.nominales<- FALSE
    }
    
    # Aquella variable (numérica o nominal) cuya incorporación
    # al modelo proporcione mejores resultados, será elegida
    # Se compara con respecto a un x% más del MMRE anterior (umbral*MMRE.min),
    # teniendo en cuenta que MMRE.min contiene el MMRE anterior
    if(MMRE.num <= MMRE.nom){
      # varNumerica elegida
      if( (umbral*MMRE.min) >= MMRE.num ) {
        var.elegidas <- c(var.elegidas,varNumericas[1])
        MMREs <- c(MMREs,MMRE.num)
        if(MMRE.min > MMRE.num) MMRE.min <- MMRE.num
      }else{
        # Las variables que no producen una mejora en los resultados
        # serán descartadas
        var.eliminadas <- c(var.eliminadas,varNumericas[1])
      }
      varNumericas <- varNumericas[-1]
    }else{
      # varNominal elegida
      if( (umbral*MMRE.min) >= MMRE.nom ) {
        var.elegidas <- c(var.elegidas,varNominales[1])
        MMREs <- c(MMREs,MMRE.nom)
        if(MMRE.min > MMRE.nom) MMRE.min <- MMRE.nom
      }else{
        var.eliminadas <- c(var.eliminadas,varNominales[1])
      }
      varNominales <- varNominales[-1]
    }
    
    if( verbose & (hay.var.numericas | hay.var.nominales) ){
      cat("## Iteración",iteracion,"de",total.iteraciones,"\n")
      iteracion <- iteracion + 1
      cat("   Variables elegidas:\n",var.elegidas,"\n")
      cat("   Variables eliminadas:\n",var.eliminadas,"\n")
    }
  }
  
  resultado <- list(var.dependiente=var.dependiente,elegidas=var.elegidas,eliminadas=var.eliminadas,MMREs=MMREs,umbral.MMRE=umbral.MMRE)
  return(resultado)
}

#_______________________________________________________________________________
