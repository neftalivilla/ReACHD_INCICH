#Analisis Base RECAD
#Septiembre 2022
#Analisis: Dr. Neftali Antonio Villa

#####Library#####
library(tidyverse)
library(readxl)
library(dplyr)
library(mice)
library(haven)
library(epiR)
library(ggpubr)
library(rstatix)
library(rmapshaper)
library(ggthemes)
library(nnet)
library(ggbreak) 
library(patchwork)

#####Read Data and Setting Labels#####

setwd("/Users/nefoantonio/OneDrive - UNIVERSIDAD NACIONAL AUTÓNOMA DE MÉXICO/OASIS/Protocolo RECAD")
base <- read_excel("RECCAD INC AHA 08.09.2022.xlsx")
base<-janitor::clean_names(base)
glimpse(base)
nrow(base)


#####Recoding Variables#####

base$ESTADO_REC<-NULL
base$ESTADO_REC[base$procedencia==9]<-1
base$ESTADO_REC[base$procedencia==15]<-1
base$ESTADO_REC[is.na(base$ESTADO_REC)]<-0

base$EMPLEO_REC<-NULL
base$EMPLEO_REC[base$actividad_actual==3]<-1
base$EMPLEO_REC[base$actividad_actual==2]<-2
base$EMPLEO_REC[is.na(base$EMPLEO_REC)]<-0

#Edo Residencia
base$EDO_REC_2<-NULL

base$EDO_REC_2[base$procedencia==2]<-1 #Baja California Norte
base$EDO_REC_2[base$procedencia==3]<-1 #Baja California Sur
base$EDO_REC_2[base$procedencia==5]<-1 #Coahuila
base$EDO_REC_2[base$procedencia==8]<-1 #Chihuahua
base$EDO_REC_2[base$procedencia==10]<-1 #Durango
base$EDO_REC_2[base$procedencia==24]<-1 #San Luis Potosi
base$EDO_REC_2[base$procedencia==19]<-1 #Nuevo Leon
base$EDO_REC_2[base$procedencia==25]<-1 #Sinaloa
base$EDO_REC_2[base$procedencia==26]<-1 #Sonora
base$EDO_REC_2[base$procedencia==28]<-1 #Tamaulipas
base$EDO_REC_2[base$procedencia==32]<-1 #Zacatecas

base$EDO_REC_2[base$procedencia==18]<-2 #Nayarit
base$EDO_REC_2[base$procedencia==6]<-2 #Colima
base$EDO_REC_2[base$procedencia==14]<-2 #Jalisco
base$EDO_REC_2[base$procedencia==16]<-2 #Michoacan
base$EDO_REC_2[base$procedencia==1]<-2 #Aguascalientes
base$EDO_REC_2[base$procedencia==11]<-2 #Guanajuato

base$EDO_REC_2[base$procedencia==9]<-3 #Ciudad de Mexico
base$EDO_REC_2[base$procedencia==13]<-3 #Hidalgo
base$EDO_REC_2[base$procedencia==15]<-3 #Estado de Mexico
base$EDO_REC_2[base$procedencia==17]<-3 #Morelos
base$EDO_REC_2[base$procedencia==21]<-3 #Puebla
base$EDO_REC_2[base$procedencia==22]<-3 #Queretaro
base$EDO_REC_2[base$procedencia==29]<-3 #Tlaxcala

base$EDO_REC_2[base$procedencia==4]<-4 #Campeche
base$EDO_REC_2[base$procedencia==7]<-4 #Chiapas
base$EDO_REC_2[base$procedencia==12]<-4 #Guerrero
base$EDO_REC_2[base$procedencia==20]<-4 #Oaxaca
base$EDO_REC_2[base$procedencia==23]<-4 #Quintana Roo
base$EDO_REC_2[base$procedencia==27]<-4 #Tabasco
base$EDO_REC_2[base$procedencia==30]<-4 #Veracruz
base$EDO_REC_2[base$procedencia==31]<-4 #Yucatan

base$DEFUNCION<-NULL
base$DEFUNCION[base$estado_actual_del_paciente==2]<-1
base$DEFUNCION[base$estado_actual_del_paciente==1]<-0

base$REPARADO<-NULL
base$REPARADO[base$estatus_de_la_cardiopatia_actualmente==1]<-0
base$REPARADO[base$estatus_de_la_cardiopatia_actualmente==2]<-0
base$REPARADO[base$estatus_de_la_cardiopatia_actualmente==5]<-0
base$REPARADO[base$estatus_de_la_cardiopatia_actualmente==4]<-1
base$REPARADO[base$estatus_de_la_cardiopatia_actualmente==3]<-2

#REPARADO DIC
base$REPARADO_DIC<-NULL
base$REPARADO_DIC[base$estatus_de_la_cardiopatia_actualmente==1]<-0
base$REPARADO_DIC[base$estatus_de_la_cardiopatia_actualmente==2]<-0
base$REPARADO_DIC[base$estatus_de_la_cardiopatia_actualmente==5]<-0
base$REPARADO_DIC[base$estatus_de_la_cardiopatia_actualmente==4]<-1
base$REPARADO_DIC[base$estatus_de_la_cardiopatia_actualmente==3]<-1

base$HAP_EINSEN<-NULL
base$HAP_EINSEN[base$eisenmenger_o_hap==0]<-0
base$HAP_EINSEN[base$eisenmenger_o_hap==1]<-1
base$HAP_EINSEN[base$eisenmenger_o_hap==2]<-2
base$HAP_EINSEN[base$eisenmenger_o_hap==3]<-2

base$HAP_EINSEN_DIC<-NULL
base$HAP_EINSEN_DIC[base$eisenmenger_o_hap==0]<-0
base$HAP_EINSEN_DIC[base$eisenmenger_o_hap==1]<-1
base$HAP_EINSEN_DIC[base$eisenmenger_o_hap==2]<-1
base$HAP_EINSEN_DIC[base$eisenmenger_o_hap==3]<-1

#Clasificacion ESC

base$CLASIFICACION_ESC<-NULL
base$CLASIFICACION_ESC[base$cardiopatia==1]<-1
base$CLASIFICACION_ESC[base$cardiopatia==2]<-1
base$CLASIFICACION_ESC[base$cardiopatia==3]<-1
base$CLASIFICACION_ESC[base$cardiopatia==4]<-1
base$CLASIFICACION_ESC[base$cardiopatia==20]<-1
base$CLASIFICACION_ESC[base$cardiopatia==24]<-1
base$CLASIFICACION_ESC[base$cardiopatia==27]<-1
base$CLASIFICACION_ESC[base$cardiopatia==40]<-1
base$CLASIFICACION_ESC[base$cardiopatia==45]<-1
base$CLASIFICACION_ESC[base$cardiopatia==47]<-1

base$CLASIFICACION_ESC[base$cardiopatia==5]<-2
base$CLASIFICACION_ESC[base$cardiopatia==6]<-2
base$CLASIFICACION_ESC[base$cardiopatia==7]<-2
base$CLASIFICACION_ESC[base$cardiopatia==8]<-2
base$CLASIFICACION_ESC[base$cardiopatia==10]<-2
base$CLASIFICACION_ESC[base$cardiopatia==13]<-2
base$CLASIFICACION_ESC[base$cardiopatia==14]<-2
base$CLASIFICACION_ESC[base$cardiopatia==15]<-2
base$CLASIFICACION_ESC[base$cardiopatia==19]<-2
base$CLASIFICACION_ESC[base$cardiopatia==21]<-2
base$CLASIFICACION_ESC[base$cardiopatia==22]<-2
base$CLASIFICACION_ESC[base$cardiopatia==23]<-2
base$CLASIFICACION_ESC[base$cardiopatia==25]<-2
base$CLASIFICACION_ESC[base$cardiopatia==26]<-2
base$CLASIFICACION_ESC[base$cardiopatia==36]<-2
base$CLASIFICACION_ESC[base$cardiopatia==41]<-2
base$CLASIFICACION_ESC[base$cardiopatia==42]<-2
base$CLASIFICACION_ESC[base$cardiopatia==44]<-2
base$CLASIFICACION_ESC[base$cardiopatia==48]<-2
base$CLASIFICACION_ESC[base$cardiopatia==49]<-2
base$CLASIFICACION_ESC[base$cardiopatia==50]<-2
base$CLASIFICACION_ESC[base$cardiopatia==51]<-2
base$CLASIFICACION_ESC[base$cardiopatia==52]<-2
base$CLASIFICACION_ESC[base$cardiopatia==53]<-2
base$CLASIFICACION_ESC[base$cardiopatia==54]<-2


base$CLASIFICACION_ESC[base$cardiopatia==9]<-3
base$CLASIFICACION_ESC[base$cardiopatia==11]<-3
base$CLASIFICACION_ESC[base$cardiopatia==12]<-3
base$CLASIFICACION_ESC[base$cardiopatia==16]<-3
base$CLASIFICACION_ESC[base$cardiopatia==17]<-3
base$CLASIFICACION_ESC[base$cardiopatia==18]<-3
base$CLASIFICACION_ESC[base$cardiopatia==28]<-3
base$CLASIFICACION_ESC[base$cardiopatia==29]<-3
base$CLASIFICACION_ESC[base$cardiopatia==30]<-3
base$CLASIFICACION_ESC[base$cardiopatia==31]<-3
base$CLASIFICACION_ESC[base$cardiopatia==32]<-3
base$CLASIFICACION_ESC[base$cardiopatia==33]<-3
base$CLASIFICACION_ESC[base$cardiopatia==34]<-3
base$CLASIFICACION_ESC[base$cardiopatia==35]<-3
base$CLASIFICACION_ESC[base$cardiopatia==37]<-3
base$CLASIFICACION_ESC[base$cardiopatia==38]<-3
base$CLASIFICACION_ESC[base$cardiopatia==39]<-3
base$CLASIFICACION_ESC[base$cardiopatia==43]<-3
base$CLASIFICACION_ESC[base$cardiopatia==46]<-3
base$CLASIFICACION_ESC[base$cardiopatia==55]<-3
base$CLASIFICACION_ESC[base$cardiopatia==56]<-3
base$CLASIFICACION_ESC[base$cardiopatia==57]<-3
base$CLASIFICACION_ESC[base$cardiopatia==58]<-3

#Condiciones Extra
base$CLASIFICACION_ESC[base$CLASIFICACION_ESC==1 & base$HAP_EINSEN_DIC==1]<-3
base$CLASIFICACION_ESC[base$CLASIFICACION_ESC==2 & base$HAP_EINSEN_DIC==1]<-3
base$CLASIFICACION_ESC[base$CLASIFICACION_ESC==3 & base$HAP_EINSEN_DIC==1]<-3
base$CLASIFICACION_ESC[base$cardiopatia==15 & base$REPARADO_DIC==0]<-3
base$CLASIFICACION_ESC[base$cardiopatia==15 & base$REPARADO_DIC==1]<-2
base$CLASIFICACION_ESC[base$cardiopatia==1 & base$REPARADO_DIC==0]<-2
base$CLASIFICACION_ESC[base$cardiopatia==1 & base$REPARADO_DIC==1]<-1
base$CLASIFICACION_ESC[base$cardiopatia==2 & base$REPARADO_DIC==1]<-1
base$CLASIFICACION_ESC[base$cardiopatia==4 & base$REPARADO_DIC==1]<-1
base$CLASIFICACION_ESC[base$cardiopatia==3]<-2
base$CLASIFICACION_ESC[base$cardiopatia==11 & base$REPARADO_DIC==1]<-2


#Clasificacion de la AHA

base$CLASIFICACION_AHA<-NULL
base$CLASIFICACION_AHA[base$cardiopatia==1]<-1
base$CLASIFICACION_AHA[base$cardiopatia==2]<-1
base$CLASIFICACION_AHA[base$cardiopatia==4]<-1
base$CLASIFICACION_AHA[base$cardiopatia==36]<-1
base$CLASIFICACION_AHA[base$cardiopatia==52]<-1

base$CLASIFICACION_AHA[base$cardiopatia==3]<-2
base$CLASIFICACION_AHA[base$cardiopatia==5]<-2
base$CLASIFICACION_AHA[base$cardiopatia==6]<-2
base$CLASIFICACION_AHA[base$cardiopatia==7]<-2
base$CLASIFICACION_AHA[base$cardiopatia==8]<-2
base$CLASIFICACION_AHA[base$cardiopatia==10]<-2
base$CLASIFICACION_AHA[base$cardiopatia==13]<-2
base$CLASIFICACION_AHA[base$cardiopatia==14]<-2
base$CLASIFICACION_AHA[base$cardiopatia==15]<-2
base$CLASIFICACION_AHA[base$cardiopatia==20]<-2
base$CLASIFICACION_AHA[base$cardiopatia==22]<-2
base$CLASIFICACION_AHA[base$cardiopatia==23]<-2
base$CLASIFICACION_AHA[base$cardiopatia==24]<-2
base$CLASIFICACION_AHA[base$cardiopatia==25]<-2
base$CLASIFICACION_AHA[base$cardiopatia==26]<-2
base$CLASIFICACION_AHA[base$cardiopatia==27]<-2
base$CLASIFICACION_AHA[base$cardiopatia==40]<-2
base$CLASIFICACION_AHA[base$cardiopatia==41]<-2
base$CLASIFICACION_AHA[base$cardiopatia==42]<-2
base$CLASIFICACION_AHA[base$cardiopatia==43]<-2
base$CLASIFICACION_AHA[base$cardiopatia==44]<-2
base$CLASIFICACION_AHA[base$cardiopatia==46]<-2
base$CLASIFICACION_AHA[base$cardiopatia==47]<-2
base$CLASIFICACION_AHA[base$cardiopatia==48]<-2
base$CLASIFICACION_AHA[base$cardiopatia==49]<-2
base$CLASIFICACION_AHA[base$cardiopatia==50]<-2
base$CLASIFICACION_AHA[base$cardiopatia==51]<-2
base$CLASIFICACION_AHA[base$cardiopatia==53]<-2


base$CLASIFICACION_AHA[base$cardiopatia==9]<-3
base$CLASIFICACION_AHA[base$cardiopatia==11]<-3
base$CLASIFICACION_AHA[base$cardiopatia==12]<-3
base$CLASIFICACION_AHA[base$cardiopatia==16]<-3
base$CLASIFICACION_AHA[base$cardiopatia==17]<-3
base$CLASIFICACION_AHA[base$cardiopatia==18]<-3
base$CLASIFICACION_AHA[base$cardiopatia==19]<-3
base$CLASIFICACION_AHA[base$cardiopatia==21]<-3
base$CLASIFICACION_AHA[base$cardiopatia==28]<-3
base$CLASIFICACION_AHA[base$cardiopatia==29]<-3
base$CLASIFICACION_AHA[base$cardiopatia==30]<-3
base$CLASIFICACION_AHA[base$cardiopatia==31]<-3
base$CLASIFICACION_AHA[base$cardiopatia==32]<-3
base$CLASIFICACION_AHA[base$cardiopatia==33]<-3
base$CLASIFICACION_AHA[base$cardiopatia==34]<-3
base$CLASIFICACION_AHA[base$cardiopatia==35]<-3
base$CLASIFICACION_AHA[base$cardiopatia==37]<-3
base$CLASIFICACION_AHA[base$cardiopatia==38]<-3
base$CLASIFICACION_AHA[base$cardiopatia==39]<-3
base$CLASIFICACION_AHA[base$cardiopatia==45]<-3
base$CLASIFICACION_AHA[base$cardiopatia==54]<-3
base$CLASIFICACION_AHA[base$cardiopatia==55]<-3
base$CLASIFICACION_AHA[base$cardiopatia==56]<-3
base$CLASIFICACION_AHA[base$cardiopatia==57]<-3

#Clasificacion AHA FUNCIONALO s
base$CLASIFICACION_AHA_FUNCIONAL[base$CLASIFICACION_AHA==1 & base$estadio_fisiologico==1]<-1
base$CLASIFICACION_AHA_FUNCIONAL[base$CLASIFICACION_AHA==1 & base$estadio_fisiologico==2]<-2
base$CLASIFICACION_AHA_FUNCIONAL[base$CLASIFICACION_AHA==1 & base$estadio_fisiologico==3]<-3
base$CLASIFICACION_AHA_FUNCIONAL[base$CLASIFICACION_AHA==1 & base$estadio_fisiologico==4]<-4

base$CLASIFICACION_AHA_FUNCIONAL[base$CLASIFICACION_AHA==2 & base$estadio_fisiologico==1]<-5
base$CLASIFICACION_AHA_FUNCIONAL[base$CLASIFICACION_AHA==2 & base$estadio_fisiologico==2]<-6
base$CLASIFICACION_AHA_FUNCIONAL[base$CLASIFICACION_AHA==2 & base$estadio_fisiologico==3]<-7
base$CLASIFICACION_AHA_FUNCIONAL[base$CLASIFICACION_AHA==2 & base$estadio_fisiologico==4]<-8

base$CLASIFICACION_AHA_FUNCIONAL[base$CLASIFICACION_AHA==3 & base$estadio_fisiologico==1]<-9
base$CLASIFICACION_AHA_FUNCIONAL[base$CLASIFICACION_AHA==3 & base$estadio_fisiologico==2]<-10
base$CLASIFICACION_AHA_FUNCIONAL[base$CLASIFICACION_AHA==3 & base$estadio_fisiologico==3]<-11
base$CLASIFICACION_AHA_FUNCIONAL[base$CLASIFICACION_AHA==3 & base$estadio_fisiologico==4]<-12

base$CLASIFICACION_AHA_FUNCIONAL<-factor(base$CLASIFICACION_AHA_FUNCIONAL,labels = c("A1","A2","A3","A4",
                                                                                     "B1","B2","B3","B4",
                                                                                     "C1","C2","C3","C4"))


#####Overall Population - Table 1####

columns <- c('Parameter',"Sample of Study (n=3,459)")

#Sexo
tab <- table(base$sexo,useNA = "always")[c(1:2)]
proptab <- round(prop.table(table(base$sexo,useNA = "always")),4)[c(1:2)]*100
Sexo<-`names<-`(data.frame(c("Men (%)","Female (%)"),
                           matrix(c(paste(tab,paste0('(',proptab,')'))),ncol = 1)),
                columns)
#Edad

num1<-c(paste(round(median(base$edad,na.rm = T ),2),
              paste0('(',round(quantile(base$edad,na.rm = T,probs = c(0.25)),2),"-",
                     round(quantile(base$edad,na.rm = T,probs = c(0.75)),2),')')))

Edad<-`names<-`(data.frame(matrix(c("Age (Years)",num1),ncol = 2)),columns)

#Area Metropolitana
tab <- table(base$ESTADO_REC,useNA = "always")[c(1:2)]
proptab <- round(prop.table(table(base$ESTADO_REC,useNA = "always")),4)[c(1:2)]*100
Area.Metro<-`names<-`(data.frame(c("Outside Metropolitan Area (%)","Metropolitan Area (%)"),
                           matrix(c(paste(tab,paste0('(',proptab,')'))),ncol = 1)),
                columns)

#Empleo
tab <- table(base$EMPLEO_REC,useNA = "always")[c(1:3)]
proptab <- round(prop.table(table(base$EMPLEO_REC,useNA = "always")),4)[c(1:3)]*100
Empleo<-`names<-`(data.frame(c("Unemployed or Retired (%)","Employed (%)","Student (%)"),
                             matrix(c(paste(tab,paste0('(',proptab,')'))),ncol = 1)),
                  columns)

#NYHA
tab <- table(base$nyha,useNA = "always")[c(2:5)]
proptab <- round(prop.table(table(base$nyha,useNA = "always")),4)[c(2:5)]*100
NYHA<-`names<-`(data.frame(c("I (%)","II (%)","III (%)","IV (%)"),
                           matrix(c(paste(tab,paste0('(',proptab,')'))),ncol = 1)),
                columns)


#FEVI
tab <- table(base$ultima_fevi,useNA = "always")[c(2:4)]
proptab <- round(prop.table(table(base$nyha,useNA = "always")),4)[c(2:4)]*100
FEVI<-`names<-`(data.frame(c(">50 (%)","41-49 (%)","<40 (%)"),
                           matrix(c(paste(tab,paste0('(',proptab,')'))),ncol = 1)),
                columns)

#Sx.Genetico
tab <- table(base$asociacion_a_sindrome_genetico==15,useNA = "always")[c(1)]
proptab <- round(prop.table(table(base$asociacion_a_sindrome_genetico==15,useNA = "always")),4)[c(1)]*100
Sx.Genetico<-`names<-`(data.frame(c("Association with Genetic Syndrome (%)"),
                                  matrix(c(paste(tab,paste0('(',proptab,')'))),ncol = 1)),
                       columns)

#HAP o Eisenmenger
tab <- table(base$eisenmenger_o_hap,useNA = "always")[c(2:4)]
proptab <- round(prop.table(table(base$eisenmenger_o_hap,useNA = "always")),4)[c(2:4)]*100
Hap.Eisen<-`names<-`(data.frame(c("High Probability of PH (%)","PH (%)","Eisenmenger (%)"),
                           matrix(c(paste(tab,paste0('(',proptab,')'))),ncol = 1)),
                columns)

#Arrhythmias
tab <- table(base$arritmias,useNA = "always")[c(2)]
proptab <- round(prop.table(table(base$arritmias,useNA = "always")),4)[c(2)]*100
Arrhythmias<-`names<-`(data.frame(c("Any Arrhythmias (%)"),
                                  matrix(c(paste(tab,paste0('(',proptab,')'))),ncol = 1)),
                       columns)

#Cardio.Isqu
tab <- table(base$cardiopatia_isquemica,useNA = "always")[c(2:3)]
proptab <- round(prop.table(table(base$cardiopatia_isquemica,useNA = "always")),4)[c(2:3)]*100
Cardio.Isqu<-`names<-`(data.frame(c("Ischemic Cardiopathy (%)","Coronary Syndrome (%)"),
                                  matrix(c(paste(tab,paste0('(',proptab,')'))),ncol = 1)),
                       columns)


#Clasificacion AHA
tab <- table(base$CLASIFICACION_AHA,useNA = "always")[c(1:3)]
proptab <- round(prop.table(table(base$CLASIFICACION_AHA,useNA = "always")),4)[c(1:3)]*100
AHA.Clasification<-`names<-`(data.frame(c("Mild (%)",
                                               "Moderate (%)",
                                               "Severe (%)"),
                                             matrix(c(paste(tab,paste0('(',proptab,')'))),ncol = 1)),
                                  columns)

#Status Fisiologico
tab <- table(base$estadio_fisiologico,useNA = "always")[c(1:4)]
proptab <- round(prop.table(table(base$estadio_fisiologico,useNA = "always")),4)[c(1:4)]*100
Status.Fisiologico<-`names<-`(data.frame(c("A (%)","B (%)","C (%)","D (%)"),
                                         matrix(c(paste(tab,paste0('(',proptab,')'))),ncol = 1)),
                              columns)

#Status.CHD
tab <- table(base$estatus_de_la_cardiopatia_actualmente,useNA = "always")[c(1:5)]
proptab <- round(prop.table(table(base$estatus_de_la_cardiopatia_actualmente,useNA = "always")),4)[c(1:5)]*100
Status.CHD<-`names<-`(data.frame(c("Non-Repaired and Under Surveillance (%)",
                                    "Non-Repaired and Paliative Care (%)",
                                    "Sumited to Paliative Surgery (%)",
                                    "Total Surgical Repair (%)",
                                    "Cardiac Transplantation (%)"),
                                  matrix(c(paste(tab,paste0('(',proptab,')'))),ncol = 1)),
                       columns)

#Tipo de Procedimiento

tab <- table(base$fisiologia_univentricular,useNA = "always")[c(2:5)]
proptab <- round(prop.table(table(base$fisiologia_univentricular,useNA = "always")),4)[c(2:5)]*100
Intervencion.Realizada<-`names<-`(data.frame(c("Sistemic Pulmonary Fistule (%)",
                                               "Gleen Surgery (%)",
                                               "Fontan Surgery (%)",
                                               "Kawashima  Surgery (%)"),
                                             matrix(c(paste(tab,paste0('(',proptab,')'))),ncol = 1)),
                                  columns)

#Status
tab <- table(base$estado_actual_del_paciente,useNA = "always")[c(1:3)]
proptab <- round(prop.table(table(base$estado_actual_del_paciente,useNA = "always")),4)[c(1:3)]*100
Status.Vivo<-`names<-`(data.frame(c("Alive (%)","Death (%)","Lost at Follow-Up (%)"),
                                     matrix(c(paste(tab,paste0('(',proptab,')'))),ncol = 1)),
                          columns)

#Status
tab <- table(base[base$estado_actual_del_paciente==2,]$muerte_relacionada_a_la_cc,useNA = "always")[c(2)]
proptab <- round(prop.table(table(base[base$estado_actual_del_paciente==2,]$muerte_relacionada_a_la_cc,useNA = "always")),4)[c(2)]*100
Death.CHD<-`names<-`(data.frame(c("Related to CHD (%)"),
                             matrix(c(paste(tab,paste0('(',proptab,')'))),ncol = 1)),
                  columns)


Table0<-rbind(Sexo,Edad,Area.Metro,Empleo,NYHA,FEVI,Sx.Genetico,Hap.Eisen,Arrhythmias,Cardio.Isqu,AHA.Clasification,Status.Fisiologico,Status.CHD,Intervencion.Realizada,Status.Vivo,Death.CHD)

#####Mild Complexity Population - Table 1#####

base1.1<-base%>%dplyr::filter(CLASIFICACION_ESC==1)
columns <- c('Parameter',"Sample of Study (n=3,459)")

#Sexo
tab <- table(base1.1$sexo,useNA = "always")[c(1:2)]
proptab <- round(prop.table(table(base1.1$sexo,useNA = "always")),4)[c(1:2)]*100
Sexo<-`names<-`(data.frame(c("Men (%)","Female (%)"),
                           matrix(c(paste(tab,paste0('(',proptab,')'))),ncol = 1)),
                columns)
#Edad

num1<-c(paste(round(median(base1.1$edad,na.rm = T ),2),
              paste0('(',round(quantile(base1.1$edad,na.rm = T,probs = c(0.25)),2),"-",
                     round(quantile(base1.1$edad,na.rm = T,probs = c(0.75)),2),')')))

Edad<-`names<-`(data.frame(matrix(c("Age (Years)",num1),ncol = 2)),columns)

#Area Metropolitana
tab <- table(base1.1$ESTADO_REC,useNA = "always")[c(1:2)]
proptab <- round(prop.table(table(base1.1$ESTADO_REC,useNA = "always")),4)[c(1:2)]*100
Area.Metro<-`names<-`(data.frame(c("Outside Metropolitan Area (%)","Metropolitan Area (%)"),
                                 matrix(c(paste(tab,paste0('(',proptab,')'))),ncol = 1)),
                      columns)

#Empleo
tab <- table(base1.1$EMPLEO_REC,useNA = "always")[c(1:3)]
proptab <- round(prop.table(table(base1.1$EMPLEO_REC,useNA = "always")),4)[c(1:3)]*100
Empleo<-`names<-`(data.frame(c("Unemployed or Retired (%)","Employed (%)","Student (%)"),
                             matrix(c(paste(tab,paste0('(',proptab,')'))),ncol = 1)),
                  columns)

#NYHA
tab <- table(base1.1$nyha,useNA = "always")[c(1:4)]
proptab <- round(prop.table(table(base1.1$nyha,useNA = "always")),4)[c(1:4)]*100
NYHA<-`names<-`(data.frame(c("I (%)","II (%)","III (%)","IV (%)"),
                           matrix(c(paste(tab,paste0('(',proptab,')'))),ncol = 1)),
                columns)


#FEVI
tab <- table(base1.1$ultima_fevi,useNA = "always")[c(2:4)]
proptab <- round(prop.table(table(base1.1$nyha,useNA = "always")),4)[c(2:4)]*100
FEVI<-`names<-`(data.frame(c(">50 (%)","41-49 (%)","<40 (%)"),
                           matrix(c(paste(tab,paste0('(',proptab,')'))),ncol = 1)),
                columns)

#Sx.Genetico
tab <- table(base1.1$asociacion_a_sindrome_genetico==15,useNA = "always")[c(1)]
proptab <- round(prop.table(table(base1.1$asociacion_a_sindrome_genetico==15,useNA = "always")),4)[c(1)]*100
Sx.Genetico<-`names<-`(data.frame(c("Association with Genetic Syndrome (%)"),
                                  matrix(c(paste(tab,paste0('(',proptab,')'))),ncol = 1)),
                       columns)

#HAP o Eisenmenger
tab <- table(base1.1$eisenmenger_o_hap,useNA = "always")[c(2:4)]
proptab <- round(prop.table(table(base1.1$eisenmenger_o_hap,useNA = "always")),4)[c(2:4)]*100
Hap.Eisen<-`names<-`(data.frame(c("High Probability of PH (%)","PH (%)","Eisenmenger (%)"),
                                matrix(c(paste(tab,paste0('(',proptab,')'))),ncol = 1)),
                     columns)

#Arrhythmias
tab <- table(base1.1$arritmias,useNA = "always")[c(2)]
proptab <- round(prop.table(table(base1.1$arritmias,useNA = "always")),4)[c(2)]*100
Arrhythmias<-`names<-`(data.frame(c("Any Arrhythmias (%)"),
                                  matrix(c(paste(tab,paste0('(',proptab,')'))),ncol = 1)),
                       columns)

#Cardio.Isqu
tab <- table(base1.1$cardiopatia_isquemica,useNA = "always")[c(2:3)]
proptab <- round(prop.table(table(base1.1$cardiopatia_isquemica,useNA = "always")),4)[c(2:3)]*100
Cardio.Isqu<-`names<-`(data.frame(c("Ischemic Cardiopathy (%)","Coronary Syndrome (%)"),
                                  matrix(c(paste(tab,paste0('(',proptab,')'))),ncol = 1)),
                       columns)


#Clasificacion AHA
tab <- table(base1.1$CLASIFICACION_AHA,useNA = "always")[c(1:3)]
proptab <- round(prop.table(table(base1.1$CLASIFICACION_AHA,useNA = "always")),4)[c(1:3)]*100
AHA.Clasification<-`names<-`(data.frame(c("Mild (%)",
                                          "Moderate (%)",
                                          "Severe (%)"),
                                        matrix(c(paste(tab,paste0('(',proptab,')'))),ncol = 1)),
                             columns)

#Status Fisiologico
tab <- table(base1.1$estadio_fisiologico,useNA = "always")[c(1:4)]
proptab <- round(prop.table(table(base1.1$estadio_fisiologico,useNA = "always")),4)[c(1:4)]*100
Status.Fisiologico<-`names<-`(data.frame(c("A (%)","B (%)","C (%)","D (%)"),
                                         matrix(c(paste(tab,paste0('(',proptab,')'))),ncol = 1)),
                              columns)

#Status.CHD
tab <- table(base1.1$estatus_de_la_cardiopatia_actualmente,useNA = "always")[c(1:5)]
proptab <- round(prop.table(table(base1.1$estatus_de_la_cardiopatia_actualmente,useNA = "always")),4)[c(1:5)]*100
Status.CHD<-`names<-`(data.frame(c("Non-Repaired and Under Surveillance (%)",
                                   "Non-Repaired and Paliative Care (%)",
                                   "Sumited to Paliative Surgery (%)",
                                   "Total Surgical Repair (%)",
                                   "Cardiac Transplantation (%)"),
                                 matrix(c(paste(tab,paste0('(',proptab,')'))),ncol = 1)),
                      columns)

#Tipo de Procedimiento

tab <- table(base1.1$fisiologia_univentricular,useNA = "always")[c(2:5)]
proptab <- round(prop.table(table(base1.1$fisiologia_univentricular,useNA = "always")),4)[c(2:5)]*100
Intervencion.Realizada<-`names<-`(data.frame(c("Sistemic Pulmonary Fistule (%)",
                                               "Gleen Surgery (%)",
                                               "Fontan Surgery (%)",
                                               "Kawashima  Surgery (%)"),
                                             matrix(c(paste(tab,paste0('(',proptab,')'))),ncol = 1)),
                                  columns)

#Status
tab <- table(base1.1$estado_actual_del_paciente,useNA = "always")[c(1:3)]
proptab <- round(prop.table(table(base1.1$estado_actual_del_paciente,useNA = "always")),4)[c(1:3)]*100
Status.Vivo<-`names<-`(data.frame(c("Alive (%)","Death (%)","Lost at Follow-Up (%)"),
                                  matrix(c(paste(tab,paste0('(',proptab,')'))),ncol = 1)),
                       columns)

#Status
tab <- table(base1.1[base1.1$estado_actual_del_paciente==2,]$muerte_relacionada_a_la_cc,useNA = "always")[c(2)]
proptab <- round(prop.table(table(base1.1[base1.1$estado_actual_del_paciente==2,]$muerte_relacionada_a_la_cc,useNA = "always")),4)[c(2)]*100
Death.CHD<-`names<-`(data.frame(c("Related to CHD (%)"),
                                matrix(c(paste(tab,paste0('(',proptab,')'))),ncol = 1)),
                     columns)


Table1.1<-rbind(Sexo,Edad,Area.Metro,Empleo,NYHA,FEVI,Sx.Genetico,Hap.Eisen,Arrhythmias,Cardio.Isqu,AHA.Clasification,Status.Fisiologico,Status.CHD,Intervencion.Realizada,Status.Vivo,Death.CHD)


#####Moderate Complexity Population - Table 1#####

base1.2<-base%>%dplyr::filter(CLASIFICACION_ESC==2)
columns <- c('Parameter',"Sample of Study (n=3,459)")

#Sexo
tab <- table(base1.2$sexo,useNA = "always")[c(1:2)]
proptab <- round(prop.table(table(base1.2$sexo,useNA = "always")),4)[c(1:2)]*100
Sexo<-`names<-`(data.frame(c("Men (%)","Female (%)"),
                           matrix(c(paste(tab,paste0('(',proptab,')'))),ncol = 1)),
                columns)
#Edad

num1<-c(paste(round(median(base1.2$edad,na.rm = T ),2),
              paste0('(',round(quantile(base1.2$edad,na.rm = T,probs = c(0.25)),2),"-",
                     round(quantile(base1.2$edad,na.rm = T,probs = c(0.75)),2),')')))

Edad<-`names<-`(data.frame(matrix(c("Age (Years)",num1),ncol = 2)),columns)

#Area Metropolitana
tab <- table(base1.2$ESTADO_REC,useNA = "always")[c(1:2)]
proptab <- round(prop.table(table(base1.2$ESTADO_REC,useNA = "always")),4)[c(1:2)]*100
Area.Metro<-`names<-`(data.frame(c("Outside Metropolitan Area (%)","Metropolitan Area (%)"),
                                 matrix(c(paste(tab,paste0('(',proptab,')'))),ncol = 1)),
                      columns)

#Empleo
tab <- table(base1.2$EMPLEO_REC,useNA = "always")[c(1:3)]
proptab <- round(prop.table(table(base1.2$EMPLEO_REC,useNA = "always")),4)[c(1:3)]*100
Empleo<-`names<-`(data.frame(c("Unemployed or Retired (%)","Employed (%)","Student (%)"),
                             matrix(c(paste(tab,paste0('(',proptab,')'))),ncol = 1)),
                  columns)

#NYHA
tab <- table(base1.2$nyha,useNA = "always")[c(1:4)]
proptab <- round(prop.table(table(base1.2$nyha,useNA = "always")),4)[c(1:4)]*100
NYHA<-`names<-`(data.frame(c("I (%)","II (%)","III (%)","IV (%)"),
                           matrix(c(paste(tab,paste0('(',proptab,')'))),ncol = 1)),
                columns)


#FEVI
tab <- table(base1.2$ultima_fevi,useNA = "always")[c(2:4)]
proptab <- round(prop.table(table(base1.2$nyha,useNA = "always")),4)[c(2:4)]*100
FEVI<-`names<-`(data.frame(c(">50 (%)","41-49 (%)","<40 (%)"),
                           matrix(c(paste(tab,paste0('(',proptab,')'))),ncol = 1)),
                columns)

#Sx.Genetico
tab <- table(base1.2$asociacion_a_sindrome_genetico==15,useNA = "always")[c(1)]
proptab <- round(prop.table(table(base1.2$asociacion_a_sindrome_genetico==15,useNA = "always")),4)[c(1)]*100
Sx.Genetico<-`names<-`(data.frame(c("Association with Genetic Syndrome (%)"),
                                  matrix(c(paste(tab,paste0('(',proptab,')'))),ncol = 1)),
                       columns)

#HAP o Eisenmenger
tab <- table(base1.2$eisenmenger_o_hap,useNA = "always")[c(2:4)]
proptab <- round(prop.table(table(base1.2$eisenmenger_o_hap,useNA = "always")),4)[c(2:4)]*100
Hap.Eisen<-`names<-`(data.frame(c("High Probability of PH (%)","PH (%)","Eisenmenger (%)"),
                                matrix(c(paste(tab,paste0('(',proptab,')'))),ncol = 1)),
                     columns)

#Arrhythmias
tab <- table(base1.2$arritmias,useNA = "always")[c(2)]
proptab <- round(prop.table(table(base1.2$arritmias,useNA = "always")),4)[c(2)]*100
Arrhythmias<-`names<-`(data.frame(c("Any Arrhythmias (%)"),
                                  matrix(c(paste(tab,paste0('(',proptab,')'))),ncol = 1)),
                       columns)

#Cardio.Isqu
tab <- table(base1.2$cardiopatia_isquemica,useNA = "always")[c(2:3)]
proptab <- round(prop.table(table(base1.2$cardiopatia_isquemica,useNA = "always")),4)[c(2:3)]*100
Cardio.Isqu<-`names<-`(data.frame(c("Ischemic Cardiopathy (%)","Coronary Syndrome (%)"),
                                  matrix(c(paste(tab,paste0('(',proptab,')'))),ncol = 1)),
                       columns)


#Clasificacion AHA
tab <- table(base1.2$CLASIFICACION_AHA,useNA = "always")[c(1:3)]
proptab <- round(prop.table(table(base1.2$CLASIFICACION_AHA,useNA = "always")),4)[c(1:3)]*100
AHA.Clasification<-`names<-`(data.frame(c("Mild (%)",
                                          "Moderate (%)",
                                          "Severe (%)"),
                                        matrix(c(paste(tab,paste0('(',proptab,')'))),ncol = 1)),
                             columns)

#Status Fisiologico
tab <- table(base1.2$estadio_fisiologico,useNA = "always")[c(1:4)]
proptab <- round(prop.table(table(base1.2$estadio_fisiologico,useNA = "always")),4)[c(1:4)]*100
Status.Fisiologico<-`names<-`(data.frame(c("A (%)","B (%)","C (%)","D (%)"),
                                         matrix(c(paste(tab,paste0('(',proptab,')'))),ncol = 1)),
                              columns)

#Status.CHD
tab <- table(base1.2$estatus_de_la_cardiopatia_actualmente,useNA = "always")[c(1:5)]
proptab <- round(prop.table(table(base1.2$estatus_de_la_cardiopatia_actualmente,useNA = "always")),4)[c(1:5)]*100
Status.CHD<-`names<-`(data.frame(c("Non-Repaired and Under Surveillance (%)",
                                   "Non-Repaired and Paliative Care (%)",
                                   "Sumited to Paliative Surgery (%)",
                                   "Total Surgical Repair (%)",
                                   "Cardiac Transplantation (%)"),
                                 matrix(c(paste(tab,paste0('(',proptab,')'))),ncol = 1)),
                      columns)

#Tipo de Procedimiento

tab <- table(base1.2$fisiologia_univentricular,useNA = "always")[c(2:5)]
proptab <- round(prop.table(table(base1.2$fisiologia_univentricular,useNA = "always")),4)[c(2:5)]*100
Intervencion.Realizada<-`names<-`(data.frame(c("Sistemic Pulmonary Fistule (%)",
                                               "Gleen Surgery (%)",
                                               "Fontan Surgery (%)",
                                               "Kawashima  Surgery (%)"),
                                             matrix(c(paste(tab,paste0('(',proptab,')'))),ncol = 1)),
                                  columns)

#Status
tab <- table(base1.2$estado_actual_del_paciente,useNA = "always")[c(1:3)]
proptab <- round(prop.table(table(base1.2$estado_actual_del_paciente,useNA = "always")),4)[c(1:3)]*100
Status.Vivo<-`names<-`(data.frame(c("Alive (%)","Death (%)","Lost at Follow-Up (%)"),
                                  matrix(c(paste(tab,paste0('(',proptab,')'))),ncol = 1)),
                       columns)

#Status
tab <- table(base1.2[base1.2$estado_actual_del_paciente==2,]$muerte_relacionada_a_la_cc,useNA = "always")[c(2)]
proptab <- round(prop.table(table(base1.2[base1.2$estado_actual_del_paciente==2,]$muerte_relacionada_a_la_cc,useNA = "always")),4)[c(2)]*100
Death.CHD<-`names<-`(data.frame(c("Related to CHD (%)"),
                                matrix(c(paste(tab,paste0('(',proptab,')'))),ncol = 1)),
                     columns)


Table1.2<-rbind(Sexo,Edad,Area.Metro,Empleo,NYHA,FEVI,Sx.Genetico,Hap.Eisen,Arrhythmias,Cardio.Isqu,AHA.Clasification,Status.Fisiologico,Status.CHD,Intervencion.Realizada,Status.Vivo,Death.CHD)

#####Severe Complexity Population - Table 1#####

base1.3<-base%>%dplyr::filter(CLASIFICACION_ESC==3)
columns <- c('Parameter',"Sample of Study (n=3,459)")

#Sexo
tab <- table(base1.3$sexo,useNA = "always")[c(1:2)]
proptab <- round(prop.table(table(base1.3$sexo,useNA = "always")),4)[c(1:2)]*100
Sexo<-`names<-`(data.frame(c("Men (%)","Female (%)"),
                           matrix(c(paste(tab,paste0('(',proptab,')'))),ncol = 1)),
                columns)
#Edad

num1<-c(paste(round(median(base1.3$edad,na.rm = T ),2),
              paste0('(',round(quantile(base1.3$edad,na.rm = T,probs = c(0.25)),2),"-",
                     round(quantile(base1.3$edad,na.rm = T,probs = c(0.75)),2),')')))

Edad<-`names<-`(data.frame(matrix(c("Age (Years)",num1),ncol = 2)),columns)

#Area Metropolitana
tab <- table(base1.3$ESTADO_REC,useNA = "always")[c(1:2)]
proptab <- round(prop.table(table(base1.3$ESTADO_REC,useNA = "always")),4)[c(1:2)]*100
Area.Metro<-`names<-`(data.frame(c("Outside Metropolitan Area (%)","Metropolitan Area (%)"),
                                 matrix(c(paste(tab,paste0('(',proptab,')'))),ncol = 1)),
                      columns)

#Empleo
tab <- table(base1.3$EMPLEO_REC,useNA = "always")[c(1:3)]
proptab <- round(prop.table(table(base1.3$EMPLEO_REC,useNA = "always")),4)[c(1:3)]*100
Empleo<-`names<-`(data.frame(c("Unemployed or Retired (%)","Employed (%)","Student (%)"),
                             matrix(c(paste(tab,paste0('(',proptab,')'))),ncol = 1)),
                  columns)

#NYHA
tab <- table(base1.3$nyha,useNA = "always")[c(1:4)]
proptab <- round(prop.table(table(base1.3$nyha,useNA = "always")),4)[c(1:4)]*100
NYHA<-`names<-`(data.frame(c("I (%)","II (%)","III (%)","IV (%)"),
                           matrix(c(paste(tab,paste0('(',proptab,')'))),ncol = 1)),
                columns)


#FEVI
tab <- table(base1.3$ultima_fevi,useNA = "always")[c(2:4)]
proptab <- round(prop.table(table(base1.3$nyha,useNA = "always")),4)[c(2:4)]*100
FEVI<-`names<-`(data.frame(c(">50 (%)","41-49 (%)","<40 (%)"),
                           matrix(c(paste(tab,paste0('(',proptab,')'))),ncol = 1)),
                columns)

#Sx.Genetico
tab <- table(base1.3$asociacion_a_sindrome_genetico==15,useNA = "always")[c(1)]
proptab <- round(prop.table(table(base1.3$asociacion_a_sindrome_genetico==15,useNA = "always")),4)[c(1)]*100
Sx.Genetico<-`names<-`(data.frame(c("Association with Genetic Syndrome (%)"),
                                  matrix(c(paste(tab,paste0('(',proptab,')'))),ncol = 1)),
                       columns)

#HAP o Eisenmenger
tab <- table(base1.3$eisenmenger_o_hap,useNA = "always")[c(2)]
proptab <- round(prop.table(table(base1.3$eisenmenger_o_hap,useNA = "always")),4)[c(2)]*100
Hap.Eisen<-`names<-`(data.frame(c("High Probability of PH (%)","PH (%)","Eisenmenger (%)"),
                                matrix(c(paste(tab,paste0('(',proptab,')'))),ncol = 1)),
                     columns)

#Arrhythmias
tab <- table(base1.3$arritmias,useNA = "always")[c(2)]
proptab <- round(prop.table(table(base1.3$arritmias,useNA = "always")),4)[c(2)]*100
Arrhythmias<-`names<-`(data.frame(c("Any Arrhythmias (%)"),
                                  matrix(c(paste(tab,paste0('(',proptab,')'))),ncol = 1)),
                       columns)

#Cardio.Isqu
tab <- table(base1.3$cardiopatia_isquemica,useNA = "always")[c(2:3)]
proptab <- round(prop.table(table(base1.3$cardiopatia_isquemica,useNA = "always")),4)[c(2:3)]*100
Cardio.Isqu<-`names<-`(data.frame(c("Ischemic Cardiopathy (%)","Coronary Syndrome (%)"),
                                  matrix(c(paste(tab,paste0('(',proptab,')'))),ncol = 1)),
                       columns)

#Clasificacion AHA
tab <- table(base1.3$CLASIFICACION_AHA,useNA = "always")[c(1:3)]
proptab <- round(prop.table(table(base1.3$CLASIFICACION_AHA,useNA = "always")),4)[c(1:3)]*100
AHA.Clasification<-`names<-`(data.frame(c("Mild (%)",
                                          "Moderate (%)",
                                          "Severe (%)"),
                                        matrix(c(paste(tab,paste0('(',proptab,')'))),ncol = 1)),
                             columns)

#Status Fisiologico
tab <- table(base1.3$estadio_fisiologico,useNA = "always")[c(1:4)]
proptab <- round(prop.table(table(base1.3$estadio_fisiologico,useNA = "always")),4)[c(1:4)]*100
Status.Fisiologico<-`names<-`(data.frame(c("A (%)","B (%)","C (%)","D (%)"),
                                         matrix(c(paste(tab,paste0('(',proptab,')'))),ncol = 1)),
                              columns)

#Status.CHD
tab <- table(base1.3$estatus_de_la_cardiopatia_actualmente,useNA = "always")[c(1:5)]
proptab <- round(prop.table(table(base1.3$estatus_de_la_cardiopatia_actualmente,useNA = "always")),4)[c(1:5)]*100
Status.CHD<-`names<-`(data.frame(c("Non-Repaired and Under Surveillance (%)",
                                   "Non-Repaired and Paliative Care (%)",
                                   "Sumited to Paliative Surgery (%)",
                                   "Total Surgical Repair (%)",
                                   "Cardiac Transplantation (%)"),
                                 matrix(c(paste(tab,paste0('(',proptab,')'))),ncol = 1)),
                      columns)

#Tipo de Procedimiento

tab <- table(base1.3$fisiologia_univentricular,useNA = "always")[c(2:5)]
proptab <- round(prop.table(table(base1.3$fisiologia_univentricular,useNA = "always")),4)[c(2:5)]*100
Intervencion.Realizada<-`names<-`(data.frame(c("Sistemic Pulmonary Fistule (%)",
                                               "Gleen Surgery (%)",
                                               "Fontan Surgery (%)",
                                               "Kawashima  Surgery (%)"),
                                             matrix(c(paste(tab,paste0('(',proptab,')'))),ncol = 1)),
                                  columns)

#Status
tab <- table(base1.3$estado_actual_del_paciente,useNA = "always")[c(1:3)]
proptab <- round(prop.table(table(base1.3$estado_actual_del_paciente,useNA = "always")),4)[c(1:3)]*100
Status.Vivo<-`names<-`(data.frame(c("Alive (%)","Death (%)","Lost at Follow-Up (%)"),
                                  matrix(c(paste(tab,paste0('(',proptab,')'))),ncol = 1)),
                       columns)

#Status
tab <- table(base1.3[base1.3$estado_actual_del_paciente==2,]$muerte_relacionada_a_la_cc,useNA = "always")[c(2)]
proptab <- round(prop.table(table(base1.3[base1.3$estado_actual_del_paciente==2,]$muerte_relacionada_a_la_cc,useNA = "always")),4)[c(2)]*100
Death.CHD<-`names<-`(data.frame(c("Related to CHD (%)"),
                                matrix(c(paste(tab,paste0('(',proptab,')'))),ncol = 1)),
                     columns)


Table1.3<-rbind(Sexo,Edad,Area.Metro,Empleo,NYHA,FEVI,Sx.Genetico,Hap.Eisen,Arrhythmias,Cardio.Isqu,AHA.Clasification,Status.Fisiologico,Status.CHD,Intervencion.Realizada,Status.Vivo,Death.CHD)


#####Merge Table 1#####

Table1<-cbind(Table0,Table1.1[,-c(1)],Table1.2[,-c(1)],Table1.3[,-c(1)])
Table1_Flex<-flextable::align(flextable::flextable(Table1,cwidth=4),align="center",part="all")%>%flextable::autofit()
flextable::save_as_docx(Table1_Flex,path="Table_1.docx")

#####Regional Distribution (Figure 1A)#####

#Load Shapes
geom_df_ent <-  sf::st_read(dsn="shapes", layer="areas_geoestadisticas_estatales")  %>% sf::st_transform(crs = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0",stringsAsFactors=FALSE)
geom_df_ent<-ms_simplify(geom_df_ent, keep = 0.01, keep_shapes = T)
geom_df_ent$id.ent<-as.numeric(geom_df_ent$CVE_ENT)

#Merge Variables

base2<-geom_df_ent%>%left_join(base%>%rename("id.ent"=procedencia)%>%dplyr::select(id.ent,EDO_REC_2)%>%group_by(id.ent,EDO_REC_2)%>%summarise(),by = "id.ent")%>%
  left_join(base%>%rename("id.ent"=procedencia)%>%dplyr::select(id.ent,EDO_REC_2)%>%group_by(EDO_REC_2)%>%summarise(n=n()),by="EDO_REC_2")%>%
  mutate(EDO_REC_2 = factor(EDO_REC_2, labels = c("North Region (n=81)","Central-West Region (n=288)",
                                                  "Central Region (n=2,612)","South-East Region (n=478)")))

base2$EDO_REC_2[is.na(base2$EDO_REC_2)]<-"North Region (n=81)"

Figure1A<-ggplot() +
  geom_sf(data= base2, aes(fill = factor(EDO_REC_2), geometry=geometry), color = "black")  +
  theme_map()+
  ggsci::scale_fill_jama() +
  theme(legend.position = "bottom")+
  theme(plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm"))+
  labs(fill="Number of patients registered")


#####Figura de Clasificacion (Figura 1B)#####

Figure2A.df<-base %>% 
  group_by(cardiopatia) %>% 
  summarise(n=n())%>%
  arrange(desc(n))%>% 
  mutate(perc = paste(n,paste0("(",round(n / nrow(base)*100,2),"%",")")))


Figure2A.df$NAMES<-NULL
Figure2A.df$NAMES[Figure2A.df$cardiopatia==1]<-"Atrial septal defect (ASD) without comorbidities"
Figure2A.df$NAMES[Figure2A.df$cardiopatia==24]<-"Bicuspid aortic valve"
Figure2A.df$NAMES[Figure2A.df$cardiopatia==2]<-"Ventricular septal defect"
Figure2A.df$NAMES[Figure2A.df$cardiopatia==4]<-"Patent ductus arteriosus"
Figure2A.df$NAMES[Figure2A.df$cardiopatia==25]<-"Coarctation of the aorta"
Figure2A.df$NAMES[Figure2A.df$cardiopatia==14]<-"Ebstein's anomaly"
Figure2A.df$NAMES[Figure2A.df$cardiopatia==15]<-"Tetralogy of Fallot"
Figure2A.df$NAMES[Figure2A.df$cardiopatia==22]<-"Subvalvular aortic stenosis"
Figure2A.df$NAMES[Figure2A.df$cardiopatia==20]<-"Pulmonary artery stenosis"
Figure2A.df$NAMES[Figure2A.df$cardiopatia==5]<-"Partial anomalous pulmonary venous return with ASD"
Figure2A.df$NAMES[is.na(Figure2A.df$NAMES)]<-"Others"

Figure2A.df<-Figure2A.df %>% 
  group_by(NAMES) %>% 
  summarise(n=sum(n))%>%
  arrange(desc(n))%>% 
  mutate(perc = n / nrow(base)*100)

Figure1B<-ggplot(Figure2A.df%>%dplyr::filter(NAMES!="Others"), aes(reorder(NAMES, -perc), perc)) +
  geom_col(fill="#FFA500",col="black") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  coord_flip()+ 
  ylab("Percentage, (%)")+
  xlab("CHD Classification")+
  theme_minimal()+
  ggtitle("")+
  theme(axis.text.y = element_text(size = 13),title = element_text(size = 15))+
  geom_text(aes(label=paste0(round(perc,1),"%")), vjust = 0.5,hjust = -0.25, color="black", size=4)+
  scale_y_continuous(limits = c(0,35))+
  annotate("text", x = 9, y = 20,size=10, label = paste("n =",nrow(base)))

#####Merege Figure 1 (Figure 1)#####

Figure1<-ggarrange(Figure1A,Figure1B,ncol = 2,nrow = 1,labels = LETTERS[1:2])

ggsave(Figure1,
       filename = "Figure1.pdf", 
       bg = "white",
       width = 50, 
       height = 15,
       units=c("cm"),
       dpi = 450,
       limitsize = FALSE)

#####Classification Prevalence (Figure 2B)#####

#ESC Classification
#Mild
ncas <- table(base$CLASIFICACION_ESC==1)[2]; npop <- sum(!is.na(base$CLASIFICACION_ESC))
tmp <- as.matrix(cbind(ncas, npop))
Fig2.df.1.1<-round(epi.conf(tmp, ctype = "prevalence", method = "exact", N = 10000, design = 1, 
                            conf.level = 0.95) * 100,2)

#Moderate
ncas <- table(base$CLASIFICACION_ESC==2)[2]; npop <- sum(!is.na(base$CLASIFICACION_ESC))
tmp <- as.matrix(cbind(ncas, npop))
Fig2.df.1.2<-round(epi.conf(tmp, ctype = "prevalence", method = "exact", N = 10000, design = 1, 
                            conf.level = 0.95) * 100,2)

#Mild
ncas <- table(base$CLASIFICACION_ESC==3)[2]; npop <- sum(!is.na(base$CLASIFICACION_ESC))
tmp <- as.matrix(cbind(ncas, npop))
Fig2.df.1.3<-round(epi.conf(tmp, ctype = "prevalence", method = "exact", N = 10000, design = 1, 
                            conf.level = 0.95) * 100,2)

Fig2.df.1<-rbind(Fig2.df.1.1,Fig2.df.1.2,Fig2.df.1.3)
Fig2.df.1$subgroup<-c("Mild","Moderate","Severe")
Fig2.df.1$group<-c("ESC-Functional Classification")

#AHA Anatomical Classification
#Mild
ncas <- table(base$CLASIFICACION_AHA==1)[2]; npop <- sum(!is.na(base$CLASIFICACION_AHA))
tmp <- as.matrix(cbind(ncas, npop))
Fig2.df.2.1<-round(epi.conf(tmp, ctype = "prevalence", method = "exact", N = 10000, design = 1, 
                            conf.level = 0.95) * 100,2)

#Moderate
ncas <- table(base$CLASIFICACION_AHA==2)[2]; npop <- sum(!is.na(base$CLASIFICACION_AHA))
tmp <- as.matrix(cbind(ncas, npop))
Fig2.df.2.2<-round(epi.conf(tmp, ctype = "prevalence", method = "exact", N = 10000, design = 1, 
                            conf.level = 0.95) * 100,2)

#Mild
ncas <- table(base$CLASIFICACION_AHA==3)[2]; npop <- sum(!is.na(base$CLASIFICACION_AHA))
tmp <- as.matrix(cbind(ncas, npop))
Fig2.df.2.3<-round(epi.conf(tmp, ctype = "prevalence", method = "exact", N = 10000, design = 1, 
                            conf.level = 0.95) * 100,2)

Fig2.df.2<-rbind(Fig2.df.2.1,Fig2.df.2.2,Fig2.df.2.3)
Fig2.df.2$subgroup<-c("Mild","Moderate","Severe")
Fig2.df.2$group<-c("AHA-Functional Classification")

Fig2B.df<-rbind(Fig2.df.1,Fig2.df.2)

Figure2B<-Fig2B.df %>%
  ggplot( aes(x=group, y=est,fill=subgroup)) + 
  geom_bar(stat="identity", color="black", 
           position=position_dodge()) +
  geom_errorbar(aes(ymin=lower, ymax=upper), width=.2,
                position=position_dodge(.9)) +
  theme_minimal()+
  xlab("")+
  ylab("Prevalence, (%)")+
  scale_fill_wsj(palette = c("colors6"))+ geom_text(
    aes(label = paste0(est,"%","\n","(",lower,"-",upper,")"), y = est + 8),
    position = position_dodge(0.9),
    vjust = .5,size=3)+
  scale_y_continuous(limits = c(-.1,70))+
  labs(fill="Complexity CHD Classification")+
  ggtitle("")

#####Analisis de Concordancia (Figure 2C)######

DescTools::CohenKappa(table(base$CLASIFICACION_AHA,base$CLASIFICACION_ESC), weight="Unweighted")
epiR::epi.kappa(table(base$CLASIFICACION_AHA,base$CLASIFICACION_ESC))

table <- data.frame(table(base$CLASIFICACION_AHA,base$CLASIFICACION_ESC));plotTable <- table %>%
  mutate(goodbad = ifelse(table$Var1 == table$Var2, "Consistent", "Non-Consistent")) %>%
  rename("AHA"=Var1, "ESC"=Var2)%>%
  group_by(AHA) %>%
  mutate(prop = Freq/sum(Freq)*100,
         AHA = factor(AHA,levels = c(1,2,3), labels = c("Mild", "Moderate", "Severe")))%>%
  mutate(ESC = factor(ESC,levels = c(1,2,3), labels = c("Mild", "Moderate", "Severe")))

Figure2C<-ggplot(data = plotTable, mapping = aes(x = AHA, y = ESC, fill = goodbad, alpha = prop)) +
  geom_tile() +
  geom_text(aes(label = paste0(round(prop,2),"%")), vjust = .5, fontface  = "bold", alpha = 1) +
  scale_fill_manual(values = c("Consistent" = c("#0066cc"), "Non-Consistent" = c("#AD002AFF"))) +
  theme_minimal()+
  xlab("AHA/ACC Anatomical and Functional Classification")+
  ylab("ESC Anatomical Classification")+
  labs(fill="Classification")+
  scale_alpha(guide = 'none')

#####Merge Figure (Figure 2)#####

Figure2<-ggarrange(Figure2B,Figure2C,ncol = 2,nrow = 1,labels = LETTERS[2:3])

ggsave(Figure2,
       filename = "Figure2.pdf", 
       bg = "white",
       width = 40, 
       height = 10,
       units=c("cm"),
       dpi = 450,
       limitsize = FALSE)

#####Modelos de Riesgo#####

base3<-base%>%dplyr::filter(estado_actual_del_paciente!=3)
base3$nyha[base3$nyha==0]<-1
base3$ultima_fevi[base3$ultima_fevi==0]<-1

#Modelo 1

model3<-glm(DEFUNCION~arritmias,data=base3,family = "binomial")
summary(model3); BIC(model3)
jtools::summ(model3,exp=T)
car::vif(model3)


#Logistic Regresion

model3<-glm(DEFUNCION~arritmias+factor(nyha)+factor(REPARADO)+factor(HAP_EINSEN)+factor(ultima_fevi),data=base3,family = "binomial")
summary(model3)
jtools::summ(model3,exp=T)
car::vif(model3)

model3<-glm(DEFUNCION~factor(CLASIFICACION_ESC==1),data=base3)
summary(model3)
jtools::summ(model3,exp=T)
car::vif(model3)

#Multinomial

base$nyha[base$nyha==0]<-1
base$ultima_fevi[base$ultima_fevi==0]<-1

multinom1<-multinom(estado_actual_del_paciente ~ arritmias+factor(nyha)+factor(REPARADO)+factor(HAP_EINSEN), data = base)
summary(multinom1)
Anova(multinom1)
BIC(multinom1)

z <- summary(multinom1)$coefficients/summary(multinom1)$standard.errors
p <- (1 - pnorm(abs(z), 0, 1)) * 2;p
exp(coef(multinom1))
exp(confint(multinom1))

#####Arritmias####

Table2<-base %>% 
  dplyr::select(22:44)%>%
  summarise(across(everything(), list(sum)))%>%
  tidyr::gather(group, value)%>%
  mutate(perc.num=value / nrow(base)*100,
         perc.lab = paste(value,paste0("(",round(value / nrow(base)*100,2),"%",")")))%>%
  dplyr::arrange(-value)

Table2_Flex<-flextable::align(flextable::flextable(Table2,cwidth=4),align="center",part="all")%>%flextable::autofit()
flextable::save_as_docx(Table2_Flex,path="Table_2.docx")


#####Procedimientos####

Table3<-base %>% 
  dplyr::select(46:53)%>%
  summarise(across(everything(), list(sum)))%>%
  tidyr::gather(group, value)%>%
  mutate(perc.num=value / nrow(base)*100,
         perc.lab = paste(value,paste0("(",round(value / nrow(base)*100,2),"%",")")))%>%
  dplyr::arrange(-value)

Table3_Flex<-flextable::align(flextable::flextable(Table3,cwidth=4),align="center",part="all")%>%flextable::autofit()
flextable::save_as_docx(Table3_Flex,path="Table_3.docx")


table(base$nyha,base$CLASIFICACION_ESC)
prop.table(table(base$nyha==4,base$CLASIFICACION_ESC),2)*100

chisq_descriptives(table(base$nyha==4,base$CLASIFICACION_ESC))
?chisq_descriptives



#####Complete CHD#####

Sup.Table3<-base %>% 
  group_by(cardiopatia) %>% 
  summarise(n=n())%>%
  arrange(desc(n))%>% 
  mutate(perc = paste(n,paste0("(",round(n / nrow(base)*100,2),"%",")")))

Sup.Table3.Flex<-flextable::align(flextable::flextable(Sup.Table3,cwidth=4),align="center",part="all")%>%flextable::autofit()
flextable::save_as_docx(Sup.Table3.Flex,path="Sup_Table_3.docx")



base2<-base%>%
  dplyr::filter(actividad_actual!=1)%>%
  dplyr::filter(actividad_actual!=7)%>%
  dplyr::filter(ultima_fevi!=0)%>%
  dplyr::filter(estado_actual_del_paciente!=3)


base2$ACTIVIDAD_REC<-NULL
base2$ACTIVIDAD_REC[base2$actividad_actual==2]<-1
base2$ACTIVIDAD_REC[base2$actividad_actual==3]<-2
base2$ACTIVIDAD_REC[base2$actividad_actual==4]<-3
base2$ACTIVIDAD_REC[base2$actividad_actual==5]<-4
base2$ACTIVIDAD_REC[base2$actividad_actual==7]<-4
base2$ACTIVIDAD_REC[base2$actividad_actual==6]<-5
base2$ACTIVIDAD_REC<-factor(base2$ACTIVIDAD_REC,labels = c("Estudiantes","Trabajadores","Hogar","Incapacidad","Desempleados"))

m1<-glm((nyha>=3)~ACTIVIDAD_REC+edad+sexo+factor(arritmias)+factor(procedimiento_realizado)+factor(eisenmenger_o_hap),family ="binomial",data = base2)
summary(m1)
jtools::summ(m1,exp=T)

m2<-glm((nyha>=3 & ultima_fevi==3)~ACTIVIDAD_REC+edad+sexo,family ="binomial",data = base2)
summary(m2)
jtools::summ(m2,exp=T)

m3<-glm((nyha>=3 & ultima_fevi==3 & estadio_fisiologico>=3)~ACTIVIDAD_REC+sexo+edad+estatus_de_la_cardiopatia_actualmente,family ="binomial",data = base2)
summary(m3)
jtools::summ(m3,exp=T)

base2$arritmias
base2$nyha
base2$procedimiento_realizado
base2$eisenmenger_o_hap

m4<-glm((ultima_fevi>=2)~ACTIVIDAD_REC+factor(arritmias)+factor(nyha)+factor(procedimiento_realizado)+factor(eisenmenger_o_hap),family ="binomial",data = base2)
summary(m4)
jtools::summ(m4,exp=T)


m5<-glm(DEFUNCION~ACTIVIDAD_REC+factor(arritmias)+factor(nyha)+factor(procedimiento_realizado)+factor(eisenmenger_o_hap),family ="binomial",data = base2)
summary(m5)
jtools::summ(m5,exp=T)

DEFUNCION




