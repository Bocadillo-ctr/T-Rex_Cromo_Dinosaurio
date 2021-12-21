# T-Rex_Cromo_Dinosaurio
Bot Game
library(readxl)
library(tidyverse)
library(dplyr)
library(RODBC)

#Limpieza de dtaos
#wb <- "D:/working directory/NOMINA/PLANEACION FINANCIERA ABRIL 2021.xlsb" # Give the file name
wb <- "D:/working directory/NOMINA/ACUMULADOS Q1 JULIO.xlsb" # Give the file name
setwd("D:/working directory/NOMINA")
con2 <- odbcConnectExcel2007(wb)
data <- sqlFetch(con2,"Hoja1")

col_names<-c("Identificación","Nombre","Centro de Costo","Descripción Centro de Costo","Cargo relacionado",
             "Tipo Salario","Concepto","Nombre Concepto ","Período Acumula","Cantidad","Valor (+/-)","JORNADA")

#col_names<-c("Identificación","Nombre","Centro de Costo","Descripción Centro de Costo","Cargo relacionado",
#             "Tipo Salario","Concepto","Nombre Concepto ","Período Acumula","Cantidad","Valor (+/-)","JORNADA")


data<-data[col_names]
odbcClose(con2)
col_names<-c("Identificación","NOMBRE","Centro de Costo","Descripción Centro de Costo","Cargo Relacionado",
             "Tipo Salario","Concepto","Nombre Concepto","Período Acumula","Cantidad","Valor (+/-)","Jornada")
names(data)<-c(col_names)
data$Cantidad<-as.double(data$Cantidad)
data$`Valor (+/-)`<-as.double(data$`Valor (+/-)`)
data$Jornada<-as.integer(data$Jornada)
#write.table(data,"CONTROL.csv",sep=";",dec=",",row.names=FALSE)

#Unificar datos
###Archivos de carga

CCO<-read.csv2("cco_campaña.csv")




###conceptos (data, concepto)

concepto<-read.csv2("concepto_pph.csv")
data1<-merge(data,concepto,by = "Concepto",all.x = T,sort = F)
control<-data1[is.na(data1$Vr_hora),]
if(dim(control)[1]>0){
  write.table(control,"CONTROL.csv",sep=";",dec=",",row.names=FALSE)
  stop("concepto faltante modificar archivo concepto.csv")
}else{
data1$Cantidad[is.na(data1$Cantidad)]<-1
data1$Cantidad[data1$Cantidad==0]<-1
#data1 <- mutate(data1,tipo=ifelse(data1$Concepto=='1005'& data1$`Tipo Salario`=='SALARIO FIJO','D',data1$tipo))
data1 <- mutate(data1,horas=ifelse((data1$Concepto=='1073'| data1$Concepto=='3073'),((data1$`Valor (+/-)`)/3785.525),
                    ifelse(data1$tipo=='D',((data1$Jornada/6)*data1$Cantidad),
                           ifelse(data1$tipo=='H',data1$Cantidad,0)))*(data1$`Valor (+/-)`/abs(data1$`Valor (+/-)`)))
}
control<-data1[is.na(data1$horas),]
if(dim(control)[1]>0){
  write.table(control,"CONTROL.csv",sep=";",dec=",",row.names=FALSE)
  stop("error en horas desconocido,puede ser que falte concepto_pph.csv")
}else{
  data1<-mutate(data1,Vr_hora=ifelse((data1$FACTOR_MULT==0|data1$horas==0),as.double(0),
                                     data1$`Valor (+/-)`/data1$horas/data1$FACTOR_MULT))
  data<-data1
}
#crear base de comprobaciÃ³n data$tipo==H mayor a 20.000 e imprimir


###cargos(data1, cargo, new_cargo) data1<-data
new_cargo<-read.csv2("cargo_agregado.csv")
cargo<-read.csv2("cargo_relacionado.csv")
cargo <- cargo[!duplicated(cargo), ]

data1<-merge(data1,cargo,by.x = 'Cargo Relacionado',by.y = 'Cargo_Relacionado',all.x = T,sort = F)
data1<-merge(data1,new_cargo,by='Identificación',all.x = T,sort = F)
data1<-mutate(data1,cargo3=ifelse(is.na(data1$CARGO2),data1$NEW_CARGO,data1$CARGO2))
data1$NEW_CARGO<-NULL
data1$CARGO2<-NULL

names(data1)[names(data1)=='cargo3']<-'CARGO2'
control<-data1[is.na(data1$CARGO2),]
if(dim(control)[1]>0){
  control<-control[c("Identificación","CARGO2")]
  write.table(control,"CONTROL.csv",sep=";",dec=",",row.names=FALSE)
  data1$CARGO<-NULL
  stop("se necesitan agregar cargos cargo_agregado.csv")
  data1<-data
}else{
  data<-data1
}

#   data1<-data
#   data<-data1    
###H_horas
H_horas<-read.csv2("H_horas.csv")
horas<-data1[c('CARGO2','Identificación','Período Acumula','Vr_hora')]
horas$CARGO2<-NULL
names(horas)<-c('cedula','QUINCENA','valor_hora')
horas <- horas %>% arrange(order(horas$QUINCENA,decreasing = TRUE))%>% arrange(order(horas$valor_hora,decreasing = TRUE))
horas <- data.frame(summarize(group_by(horas,cedula,QUINCENA),valor_hora=max(valor_hora)))
horas1<-horas[horas$valor_hora==0,]
horas1$valor_hora<-NULL
horas<-horas[horas$valor_hora!=0,]
#quita duplicados
H_horas <-rbind(H_horas,horas)#ESTA
H_horas <- data.frame(summarize(group_by(H_horas,cedula,QUINCENA),valor_hora=max(valor_hora)))
H_horas <- H_horas %>% arrange(order(H_horas$QUINCENA,decreasing = TRUE))
data1<-merge(data1,H_horas,by.x = c('Identificación','Período Acumula'),by.y = c('cedula','QUINCENA'),all.x = T,sort = F,no.dups = T)
control <- data1[data1$CARGO2=="AGENTES",]
control<-control[is.na(control$Valor_hora),]

if(dim(control)[1]>0){
  
  write.table(control,"CONTROL.csv",sep=";",dec=",",row.names=FALSE)
  data1$valor_hora<-NULL
  data1$QUINCENA<-NULL  
  stop("concepto faltante agregar Valor_hora de agentes H_horas.csv")
}else{
  
  write.table(horas,"H_horas.csv",sep=";",dec=",",row.names=FALSE)
  data1$valor_hora<-ifelse(is.na(data1$valor_hora),data1$Vr_hora,data1$valor_hora)
  data1<-mutate(data1,horas2=ifelse(data1$FACTOR_MULT>0 & data1$valor_hora>0,
                                    data1$`Valor (+/-)`/data1$FACTOR_MULT/data1$valor_hora,
                                    data1$`Valor (+/-)`/data1$valor_hora))
  data1<-mutate(data1,horas2=ifelse(data1$tipo=="D" & data1$Concepto=="1005",(data1$Jornada/6)*data1$Cantidad,data1$horas2))
  data1<-mutate(data1,horas2=ifelse(data1$conceptos_unificados=="CONCEPTO SALARIAL",(data1$`Valor (+/-)`/data1$Vr_hora),data1$horas2))                                  
}



data1$cco<-1
data1 <- data1[c('Identificación','NOMBRE','Centro de Costo','Descripción Centro de Costo','Cargo Relacionado','Tipo Salario','cco',
                 'Cantidad','Valor (+/-)','Concepto','CONCEPTO','Jornada','Período Acumula','horas','conceptos_unificados',
                 'CARGO2','tipo_pph','horas2','tipo','FACTOR_MULT','FACTOR_CONCEP','Vr_hora','valor_hora')]
write.table(data1,"CONTROL.csv",sep=";",dec=",",row.names=FALSE)
#horas
# Identificación, 'Período Acumula' , Vr_hora

