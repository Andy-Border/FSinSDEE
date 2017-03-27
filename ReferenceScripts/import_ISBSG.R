
# NOMBRE DEL ARCHIVO. CAMBIAR AQUI.
# Alternativa (1)
# Se ha obtenido el csv del xls directamente
# y luego se han eliminado las 3 primeras filas.
# Queda una fila como cabecera.
kArchivoExcel <- "f. ISBSG DATA Release 12.csv"

# Los campos van separados por ;
# y las cadenas vacías deben interpretarse como NA.
# En la cabecera todos los carácteres no premitidos para formar identificadores
# (espacios en blanco, paréntesis) los convierte a puntos.
# dec ="," interpreta la coma como punto decimal.
ISBSG <- read.csv(kArchivoExcel,header = TRUE,na.strings="", sep=";", dec =",")


# Alternativa (2) leyendo directamente el xls
#require("XLConnect")
#kArchivoExcel <- "f. ISBSG DATA Release 12.xls"
#wb <- loadWorkbook( kArchivoExcel )
#ISBSG <- readWorksheet(wb, sheet = "Project Extract", header=TRUE, startRow = 4)

# El ultimo paso es hacer un data.frame() con el dataframe que ya tenemos.
# Esto lo hago porque quiero que las columnas de texto las trate como "factors"
# y la funcion data.frame() hace la conversion directamente.
# Seria lo mismo que si fuera columna por columna aplicando la funcion as.factor()
# si la columna es de tipo character, pero esto cuesta menos.
# Para ello le quito la clase al dataset con unclass()
# y se lo paso a la funcion data.frame().

# row.names=1 hace que borre la primera columna
# y ademas utilice sus valores como nombre de las filas,
# que es justo lo que queremos porque la primera columna contiene los IDs
# de los casos que para realizar estadisticas...
#ISBSG <- data.frame( unclass(ISBSG), row.names=1 )

# TODO: Para esta alternativa falta ver cómo se resolvería lo de la coma decimal.


# Alternativa (3) leyendo el csv generado con XLConnect
#write.csv(ISBSG, file= "ISBSG DATA Release 12.csv")
#kArchivoExcel <- "ISBSG DATA Release 12.csv"
#ISBSG <- read.csv(kArchivoExcel,header = TRUE,na.strings="NA")


# Problemas con el tipo de las variables
# Resource.Level es un factor ordenado. Hay que indicárselo. Estaba como INT.
ISBSG$Resource.Level <- factor(ISBSG$Resource.Level,levels=c(1,2,3,4))

# Las variables Software.Process.SPICE y Software.Process.TICKIT aparecen de tipo
# LOGI porque todos los valores son NA.
#sum(is.na(ISBSG$Software.Process.SPICE))
#sum(is.na(ISBSG$Software.Process.TICKIT))

# Las variables que representan un % por ahora se quedan como factores:
# Degree.of.Customisation
# Ratio.of.Project.Work.Effort.to.Non.Project.Activity
# Percentage.of.Uncollected.Work.Effort

# La variable Implementation.Date también es un factor.

# La variable Lines.of.Code se queda por ahora como un factor,
# aunque es de tipo INT.
# El problema es que los miles van separados por un punto.

#_______________________________________________________________________________

# La primera columna es el ID del caso.
# No tiene valor estadistico así que hay que borrarla.

# Forma "manual" para incluir o no columnas en la imputación
columnas.validas <- list(
#  "ISBSG.Project.ID"=TRUE,                 ####### ID, en el CSV esta columna no existe, al importarla lo pone como row.names()
  "Data.Quality.Rating"=TRUE,               ####### Rating
  "UFP.rating"=TRUE,
  "Year.of.Project"=FALSE,                  ####### Software age
  "Industry.Sector"=TRUE,                  ####### Major Grouping
  "Organisation.Type"=FALSE,
  "Application.Group"=TRUE,
  "Application.Type"=FALSE,
  "Development.Type"=TRUE,
  "Development.Platform"=TRUE,
  "Language.Type"=TRUE,
  "Primary.Programming.Language"=TRUE,
  "Count.Approach"=TRUE,
  "Functional.Size"=TRUE,                   ####### Sizing
  "Relative.Size"=FALSE,
  "Adjusted.Function.Points"=TRUE,
  "Value.Adjustment.Factor"=FALSE,          #17
  "Normalised.Work.Effort.Level.1"=TRUE,   ####### Effort
  "Normalised.Work.Effort"=FALSE,
  "Summary.Work.Effort"=TRUE,
  "Normalised.Level.1.PDR..ufp."=FALSE,     #21 ####### Productivity
  "Normalised.PDR..ufp."=FALSE,
  "Pre.2002.PDR..afp."=FALSE,
  "Defect.Density"=FALSE,                   ####### Other Metrics
  "Speed.of.Delivery"=FALSE,
  "Manpower.Delivery.Rate"=FALSE,
  "Project.Elapsed.Time"=TRUE,             ####### Schedule
  "Project.Inactive.Time"=FALSE,
  "Implementation.Date"=FALSE,              #29
  "Project.Activity.Scope"=FALSE,
  "Effort.Plan"=FALSE,
  "Effort.Specify"=FALSE,
  "Effort.Design"=FALSE,
  "Effort.Build"=FALSE,
  "Effort.Test"=FALSE,
  "Effort.Implement"=FALSE,
  "Effort.Unrecorded"=FALSE,
  "Minor.Defects"=FALSE,                    ####### Quality
  "Major.Defects"=FALSE,
  "Extreme.Defects"=FALSE,
  "Total.Defects.Delivered"=FALSE,
  "Business.Area.Type"=TRUE,                ####### Grouping Attributes
  "Software.Process.CMM"=FALSE,
  "Software.Process.CMMI"=FALSE,
  "Software.Process.SPICE"=FALSE,
  "Software.Process.ISO"=FALSE,
  "Software.Process.TICKIT"=FALSE,
  "Software.Process.Other"=FALSE,
  "Package.Customisation"=FALSE,
  "Degree.of.Customisation"=FALSE,
  "Architecture"=FALSE,                     ####### Architechture
  "Client.Server."=FALSE,
  "Client.Roles"=FALSE,
  "Server.Roles"=FALSE,
  "Type.of.Server"=FALSE,
  "Client.Server.Description"=FALSE,
  "Web.Development"=FALSE,
  "Development.Methodologies"=FALSE,        ####### Documents&Techniques
  "Development.Techniques"=FALSE,
  "JAD.Method.Used"=FALSE,
  "Prototyping.Used"=FALSE,
  "Planning.Documents"=FALSE,
  "Specification.Documents"=FALSE,
  "Specification.Techniques"=FALSE,
  "Design.Documents"=FALSE,
  "Design.Techniques"=FALSE,
  "Build.Products"=FALSE,
  "Build.Activity"=FALSE,
  "Test.Documents"=FALSE,
  "Test.Activity"=FALSE,
  "Implement.Documents"=FALSE,
  "Implement.Activity"=FALSE,
  "Functional.Sizing.Technique"=FALSE,
  "FP.Standard"=FALSE,
  "FP.Standards.All"=FALSE,
  "Reference.Table.Approach"=FALSE,
  "X1st.Hardware"=FALSE,                    ####### Project Attributes
  "X1st.Language"=FALSE,
  "X1st.Operating.System"=FALSE,
  "Integrated.Development.Environment"=FALSE,
  "X1st.Debugging.Tool"=FALSE,
  "X1st.Data.Base.System"=TRUE,
  "X1st.Component.Server"=FALSE,
  "X1st.Web.Server"=FALSE,
  "X1st.Message.Server"=FALSE,
  "X1st.Other.Platform"=FALSE,
  "X2nd.Hardware"=FALSE,
  "X2nd.Language"=FALSE,
  "X2nd.Operating.System"=FALSE,
  "X2nd.Data.Base.System"=FALSE,
  "X2nd.Component.Server"=FALSE,
  "X2nd.Web.Server"=FALSE,
  "X2nd.Message.Server"=FALSE,
  "X2nd.Other.Platform"=FALSE,
  "CASE.Tool.Used"=FALSE,
  "Used.Methodology"=TRUE,
  "How.Methodology.Acquired"=FALSE,
  "User.Base...Business.Units"=FALSE,       ####### Product Attributes
  "User.Base...Locations"=FALSE,
  "User.Base...Distinct.Users"=FALSE,
  "User.Base...Concurrent.Users"=FALSE,
  "Intended.Market"=FALSE,
  "Target.Platform"=FALSE,
  "Device.Embedded"=FALSE,
  "Recording.Method"=FALSE,                 ####### Effort Attributes
  "Resource.Level"=TRUE,
  "Team.Size.Group"=FALSE,
  "Max.Team.Size"=TRUE,
  "Average.Team.Size"=TRUE,
  "Ratio.of.Project.Work.Effort.to.Non.Project.Activity"=FALSE, #110
  "Percentage.of.Uncollected.Work.Effort"=FALSE,
  "Input.count"=TRUE,                      ####### Size Attributes
  "Output.count"=TRUE,
  "Enquiry.count"=TRUE,
  "File.count"=TRUE,
  "Interface.count"=TRUE,
  "Added.count"=FALSE,
  "Changed.count"=FALSE,
  "Deleted.count"=FALSE,
  "COSMIC.Entry"=FALSE,                     #120
  "COSMIC.Exit"=FALSE,
  "COSMIC.Read"=FALSE,
  "COSMIC.Write"=FALSE,
  "Lines.of.Code"=FALSE,                    ####### Size Other than FSM
  "Lines.of.Code.not.Statements"=FALSE,
  "Other.Size.Units"=FALSE)


# Nombres de las columnas
#nombres <- names(ISBSG)
#Nombre de la columna 1
#nombres[[1]]


# Filtrado de columnas
# Me quedo solo con las columnas marcadas como TRUE.
columnas.clave <- vector()
for(nombre in names(columnas.validas)){
  if( columnas.validas[[nombre]] == TRUE) columnas.clave  <- c(columnas.clave, nombre)
}
ISBSG <- ISBSG[, columnas.clave]


# Borrado de variables que no sirven para nada
# Comentar si se quiere conservar alguna.
rm(nombre)
rm(kArchivoExcel)
rm(columnas.clave)
rm(columnas.validas)

#_______________________________________________________________________________
