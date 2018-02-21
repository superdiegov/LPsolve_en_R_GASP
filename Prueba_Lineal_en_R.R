### Pruebas de optimización con LP SOLVE

library(lpSolve)

##Ejemplo de Prueba
## min: +3423.8860808 x[R1,1] +15.1002168 x[R2,1] +8.91981864 x[R3,1] +436.6706286 x[R4,1] +1.56389338 x[R5,1];

f.obj<-c(3423,15,8,436,2)
#f.con <- matrix(c(42.72,0.5,0.29,14.5,0.07,1,0,0,0,0,0,1,0,0,0,0,0,1,0,0,0,0,0,1,0,0,0,0,0,1), nrow=6, byrow=TRUE)
f.con <- nn3
f.dir<-rep (c(">=", "<="), c(1, 5))
f.rhs<-rep (c(15, 1), c(1, 5))

min_prueba<-lp("min", f.obj, f.con, f.dir, f.rhs, int.vec=1:6)$solution

#PruebaAreas
Areas<-c(42.72,0.5,0.29,14.5,0.07)
Areas

########EJEMPLO PISO 72################################################################################
########################################################################################################

#dato1<-read.delim("clipboard")
#write.table(dato1,file="Data_72.txt")
##Fijar Directorio: Puede cambiar
setwd("C:/Users/dvalencia/Documents/GitHub/LPsolve_en_R_GASP")
library(lpSolve)
data2<-read.table(file.choose(), header=TRUE)

## Construcion de funcion objetivo
#minimize seleccion: sum{i in A, t in 1..m} a[i]*x[i,t]*v[i,t]+ sum{i in A, t in 1..m} d[i]*e[i]*f[i]*x[i,t]

f.obj<- (data2$Costo*data2$Sup_ha)+(data2$DistPoly*data2$DistAPP*data2$DistAPP)
n_x<-length(f.obj)
n_x
#write.table(f.obj,file="f.obj.txt")

### Construccion de matrix Diagonal de Xi variables segun piso
Diagonal<-diag(n_x)
f.con<-rbind(data2$Sup_ha, Diagonal, deparse.level = 0)
#write.table(f.con,file="fcon.txt")

## Construcciond de direcciones 
f.dir<-rep (c(">=", "<="), c(1, n_x))

## Construcción de metas AICHI y Meta
f.rhs<-rep (c(17553, 1), c(1, n_x))
#write.table(f.rhs,file="frhs.txt")

##Resultados del modelo.Numero de parches +1 en el int.vec
Funcion_Objetivo<-lp("min", f.obj, f.con, f.dir, f.rhs, num.bin.solns=TRUE)
Funcion_Objetivo

Result<-lp("min", f.obj, f.con, f.dir, f.rhs, num.bin.solns=TRUE)$solution


##Exportacion resultados

Parches<-data2$Parche

Tabla<-data.frame(Parches,Result)

write.csv(Tabla,file="Result.csv")
