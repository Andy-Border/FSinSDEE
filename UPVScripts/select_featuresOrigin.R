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
# minimum redundancy �? maximum relevance ("mRMR" for short)
ordena.por.coeficiente <- function(var.dependiente, data){
  
  formula <- as.formula(paste(var.dependiente,".",sep="~"))
  ganancias <- information.gain(formula, data)
  indices.ordenados <- order(ganancias$attr_importance, decreasing=TRUE)
  var.ordenadas <- rownames( ganancias[indices.ordenados,,drop=FALSE] )
  
  var.seleccionadas <- vector()
  # mRMR de las variables seleccionadas
  # En primera posición contiene la GI mayor de entre las variables independientes.
  mrmr <- vector()
  mrmr[1] <- ganancias$attr_importance[indices.ordenados[1]]
  
  total.iter <- length(var.ordenadas)
  for( iteracion in 1:total.iter ){
  
    # Se mete en var.seleccionadas la primera fruto de la ordenación que sigue (MRMR)
    # En la primera iteración, se mete directamente la primera ordenada por GI simplemente
    # En la última iteración, se hacen cálculos que no sirven de nada
    var.seleccionadas <- c(var.seleccionadas, var.ordenadas[1])
    # Se quita de var.ordenadas la variable que se acaba de meter en var.seleccionadas
    var.ordenadas <- var.ordenadas[-1]
    coefs <- vector()
    # Para seleccionar la siguiente variable se tiene en cuenta su GI
    # pero también la ganancia con respecto a las ya seleccionadas
    for(var.prueba in var.ordenadas){
      formula <- as.formula(paste(var.dependiente,var.prueba,sep="~"))
      # GI de la variable de prueba con respecto a la dependiente
      # Cuanto mayor mejor
      I <- information.gain(formula , data)[1,1]
      info.parciales <- vector()
      # Con respecto a las ya seleccionadas se calcula la GI
      # La media de las parciales cuanto menor mejor
      for(var.selec in var.seleccionadas){
        formula <- as.formula(paste(var.selec,var.prueba,sep="~"))
        I.parcial <- information.gain(formula , data)[1,1]
        info.parciales <- c(info.parciales, I.parcial)
      }
      # Y se conserva GI - la media de las parciales
      coef <- I - mean(info.parciales)
      coefs <- c(coefs, coef)
      }
    # Entonces se vuelven a ordenar los índices teniendo en cuenta coefs
    # Cuanto mayor mejor
    indices.ordenados <- order(coefs, decreasing=TRUE)
    #result <- data.frame(coef=coefs[indices.ordenados],row.names=var.ordenadas[indices.ordenados])
    var.ordenadas <- var.ordenadas[indices.ordenados]
    result <- data.frame(coef=coefs[indices.ordenados],row.names=var.ordenadas)
    #print(result)
    mrmr <- c(mrmr, coefs[indices.ordenados][1])
    
  }
  
  # Se quita el último elemento del vector (12)
  mrmr <- mrmr[-length(mrmr)]
  
  var.nominales <- vector()
  var.continuas <- vector()
  
  for(nombre in var.seleccionadas){
    if(is.factor(data[[ nombre ]]) | 
         is.ordered(data[[ nombre ]]) | 
         is.character(data[[ nombre ]])){
      var.nominales <- c(var.nominales, nombre)
    }else{
      var.continuas <- c(var.continuas, nombre)
    }
  }
  
  resultado <- list(var.dependiente=var.dependiente)
  resultado$coef.ordenadas <- var.seleccionadas
  resultado$mrmr <- mrmr
  resultado$var.nominales <- var.nominales
  resultado$var.continuas <- var.continuas
  
  return(resultado)
  
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
