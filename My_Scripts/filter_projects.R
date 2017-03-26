# Selection Criteria for Effort Concerns
# High data quality
# - High general data quality
# Data Quality Rating = ”A” | Data Quality Rating = ”B”
# - High functional size quality
# Data Quality Unadjusted Function Point (UFP) Rating = ”A” | Data Quality UFP Rating = ”B”

# Comparable effort definition
# - Development team effort known
# MISSING(Normalised Work Effort Level 1) ~= 1
# - Effort across the whole life cycle
# Normalised Work Effort Level 1 = Summary Work Effort

# Comparable size definition
# - IFPUG version 4.0 or later
# CHAR.INDEX(FP Standard, ‘IFPUG 4’) ~= 0 | 
#           (FP Standard = ”IFPUG” & Year of Project > ”1994”)
# Con la R11 nos quedábamos con 875 proyectos después del filtrado con el SPSS.
# Por lo visto el OR de la última expresión no se aplicó bien.


# ISBSG R11
#ISBSG <- read.csv("ae. ISBSG11.xls - Data CD R11.csv",header = TRUE,na.strings="", sep=";", dec =",")
# ISBSG R12
#ISBSG <- read.csv("f. ISBSG DATA Release 12.csv",header = TRUE,na.strings="", sep=";", dec =",")

ISBSG <- ISBSG[ISBSG$Data.Quality.Rating %in% c("A","B"),]
ISBSG <- ISBSG[ISBSG$UFP.rating %in% c("A","B"),]

ISBSG <- ISBSG[ !is.na( ISBSG[["Normalised.Work.Effort.Level.1"]] ), ]
#casos.sin.NAs <- !is.na( ISBSG[["Normalised.Work.Effort.Level.1"]] )
#ISBSG <- ISBSG[ casos.sin.NAs, ]
ISBSG <- ISBSG[ ISBSG$Normalised.Work.Effort.Level.1 == ISBSG$Summary.Work.Effort, ]

# ISBSG R11
#ISBSG <- ISBSG[ grepl( "IFPUG 4", ISBSG$FP.Standard), ]
# ISBSG R12
# Projects using the IFPUG FSM method of version 4 or greater are indicated as 'IFPUG
# 4+', whereas projects sized using a version prior to version 4 are indicated as 'IFPUG
# old'.
ISBSG <- ISBSG[ ISBSG$Count.Approach=="IFPUG 4+", ]




# Filtrado de proyectos por calidad A or B y a continuación LD
#isbsg <- na.omit(ISBSG[ISBSG$Data.Quality.Rating %in% c("A","B"),])
# Borrado de la columna Data.Quality.Rating
#isbsg$Data.Quality.Rating <- NULL
