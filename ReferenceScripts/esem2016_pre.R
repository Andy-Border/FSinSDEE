

# En la importación seleccionanos las 20 variables más usadas para la estimación de esfuerzo.
# Hemos sustituido simplemente Application.Type por Application.Group, variable nueva de la R12.
# También se ha sustituido Organization.Type por Industry.Sector.
# Además necesitamos 5 variables para el filtrado.
# En total ISBSG con 6006 observaciones y 25 variables.
source("import_ISBSG.R")

# Después del filtrado nos quedamos con 1884 observaciones.
source("filter_projects.R")

# Borrado de columnas que ya no son útiles, las necesarias para el filtrado,
# salvo NWEL1 (variable dependiente).
ISBSG$Data.Quality.Rating <- NULL
ISBSG$UFP.rating <- NULL
ISBSG$Count.Approach <- NULL
ISBSG$Summary.Work.Effort <- NULL

# Por filtrado RL=1
ISBSG$Resource.Level <- NULL



# Cargamos las funciones del script "select_features.R".
source("select_features.R")

# Se eliminan las variables con más del 40% de datos perdidos.
# Por el filtrado ya hemos asegurado que NWEL1 no tiene valores perdidos.
# Se eliminan 8 variables que de hecho tienen más del 60% de datos perdidos.
# En total ISBSG con 1884 observaciones y 12 variables.
ISBSG <- elimina.var.con.demasiados.NAs(ISBSG, umbral=40, verbose=TRUE)

# Dataset completo
# En total ISBSG con 621 observaciones y 12 variables.
ISBSG <- na.omit(ISBSG)

# Drop unused levels in a factor
require(gdata)
ISBSG <- drop.levels(ISBSG)

#require(car)
# Aquí estaba el tratamiento de OT (cf. recode_OT),
# ahora se ha optado por Industry.Sector.

# Recode variable PPL
# A:G
ISBSG$Primary.Programming.Language <- sub("A:G", "Unspecified", ISBSG$Primary.Programming.Language)
# ASP.Net in the same category as ASP
ISBSG$Primary.Programming.Language <- sub("ASP.Net", "ASP", ISBSG$Primary.Programming.Language)
# BASIC in the same category as "Visual Basic"
ISBSG$Primary.Programming.Language <- sub("BASIC", "Visual Basic", ISBSG$Primary.Programming.Language)
# CSP
ISBSG$Primary.Programming.Language <- sub("CSP", "Unspecified", ISBSG$Primary.Programming.Language)
# Pro*C es un embebbed SQL usado en sistemas Oracle
#ISBSG$Primary.Programming.Language <- sub("Pro*C", "Oracle", ISBSG$Primary.Programming.Language)
# Visual C++
ISBSG$Primary.Programming.Language <- sub("Visual C", "C", ISBSG$Primary.Programming.Language)

ISBSG$Primary.Programming.Language <- as.factor(ISBSG$Primary.Programming.Language)

# Recode variable 1DBS
# Sustituye ; o espacio o / (y por eso está entre []) seguido de cualquier caracter repetido todas las veces
#ISBSG$X1st.Data.Base.System <- sub("[;  /].*", ";", ISBSG$X1st.Data.Base.System)
# Sustituye ; seguido de cualquier caracter repetido todas las veces
# Eliminando solo la cadena que va a continuación de ;
# de 104 levels se pasa a 83 levels.
ISBSG$X1st.Data.Base.System <- sub("[;].*", ";", ISBSG$X1st.Data.Base.System)

# Categorización por experto
# Categoría ACCESS
ISBSG$X1st.Data.Base.System <- sub("ACCESS[; ].*", "ACCESS", ISBSG$X1st.Data.Base.System)
ISBSG$X1st.Data.Base.System <- sub("MS Access;", "ACCESS", ISBSG$X1st.Data.Base.System)
# ADABAS
ISBSG$X1st.Data.Base.System <- sub("ADABAS;", "ADABAS", ISBSG$X1st.Data.Base.System)
# Attain
ISBSG$X1st.Data.Base.System <- sub("Micosoft.*", "Attain", ISBSG$X1st.Data.Base.System)
# DB2
ISBSG$X1st.Data.Base.System <- sub("DB2[; /].*", "DB2", ISBSG$X1st.Data.Base.System)
ISBSG$X1st.Data.Base.System <- sub("IBM DB2", "DB2", ISBSG$X1st.Data.Base.System)
ISBSG$X1st.Data.Base.System <- sub("UDB2", "DB2", ISBSG$X1st.Data.Base.System)
# Domino
ISBSG$X1st.Data.Base.System <- sub("Domino[ ].*", "Domino", ISBSG$X1st.Data.Base.System)
ISBSG$X1st.Data.Base.System <- sub("LOTUS.*", "Domino", ISBSG$X1st.Data.Base.System)
ISBSG$X1st.Data.Base.System <- sub("Notes.*", "Domino", ISBSG$X1st.Data.Base.System)
# Exchange
ISBSG$X1st.Data.Base.System <- sub("Exchange.*", "Exchange", ISBSG$X1st.Data.Base.System)
# Foxpro
ISBSG$X1st.Data.Base.System <- sub("FOXPRO;", "Foxpro", ISBSG$X1st.Data.Base.System)
# HIRDB
ISBSG$X1st.Data.Base.System <- sub("HIRDB;", "HIRDB", ISBSG$X1st.Data.Base.System)
# IMS
ISBSG$X1st.Data.Base.System <- sub("DB[/].*", "IMS", ISBSG$X1st.Data.Base.System)
ISBSG$X1st.Data.Base.System <- sub("DEDB;", "IMS", ISBSG$X1st.Data.Base.System)
ISBSG$X1st.Data.Base.System <- sub("IDMS[; -].*", "IMS", ISBSG$X1st.Data.Base.System)
ISBSG$X1st.Data.Base.System <- sub("IMS.*", "IMS", ISBSG$X1st.Data.Base.System)
# MS SQL
ISBSG$X1st.Data.Base.System <- sub("MS[- ]SQL[; ].*", "MS SQL", ISBSG$X1st.Data.Base.System)
ISBSG$X1st.Data.Base.System <- sub("MSDE.*", "MS SQL", ISBSG$X1st.Data.Base.System)
ISBSG$X1st.Data.Base.System <- sub("SQL Server[; ].*", "MS SQL", ISBSG$X1st.Data.Base.System)
ISBSG$X1st.Data.Base.System <- sub("SQL;", "MS SQL", ISBSG$X1st.Data.Base.System)
ISBSG$X1st.Data.Base.System <- sub("VSE/.*", "MS SQL", ISBSG$X1st.Data.Base.System)
# NCR
ISBSG$X1st.Data.Base.System <- sub("NCR;", "NCR", ISBSG$X1st.Data.Base.System)
# ORACLE
ISBSG$X1st.Data.Base.System <- sub("Oracle.*", "ORACLE", ISBSG$X1st.Data.Base.System)
ISBSG$X1st.Data.Base.System <- sub("Personal O.*", "ORACLE", ISBSG$X1st.Data.Base.System)
ISBSG$X1st.Data.Base.System <- sub("RDB[; ].*", "ORACLE", ISBSG$X1st.Data.Base.System)
ISBSG$X1st.Data.Base.System <- sub("CICS;", "ORACLE", ISBSG$X1st.Data.Base.System)
# SAS
ISBSG$X1st.Data.Base.System <- sub("SAS;", "SAS", ISBSG$X1st.Data.Base.System)
# Solid
ISBSG$X1st.Data.Base.System <- sub("Solid;", "Solid", ISBSG$X1st.Data.Base.System)
# SYBASE
ISBSG$X1st.Data.Base.System <- sub("SYBASE.*", "SYBASE", ISBSG$X1st.Data.Base.System)
# Unspecified
ISBSG$X1st.Data.Base.System <- sub("Yes", "Unspecified", ISBSG$X1st.Data.Base.System)
ISBSG$X1st.Data.Base.System <- sub("ISAM;", "Unspecified", ISBSG$X1st.Data.Base.System)
ISBSG$X1st.Data.Base.System <- sub("multiple;", "Unspecified", ISBSG$X1st.Data.Base.System)
ISBSG$X1st.Data.Base.System <- sub("VSAM[; ].*", "Unspecified", ISBSG$X1st.Data.Base.System)
# Watcom
ISBSG$X1st.Data.Base.System <- sub("WATCOM[; ].*", "Watcom", ISBSG$X1st.Data.Base.System)
# WGRES
ISBSG$X1st.Data.Base.System <- sub("WGRES;", "WGRES", ISBSG$X1st.Data.Base.System)

ISBSG$X1st.Data.Base.System <- as.factor(ISBSG$X1st.Data.Base.System)

