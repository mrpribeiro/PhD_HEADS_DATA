setwd("C:/Users/marco/Desktop/R programming course/Group Work DATA") #definir área de trabalho
install.packages("readxl") #instalar pacote para ler excel
#install.packages("skimr") #library para EDA
#install.packages("rmarkdown")


library(readxl)     #carregar o pacote
library(dplyr)
library(ggplot2)

#Importar os dados
gbd_data <- read.csv("IHME-GBD_2021_DATA-11d67504-1.csv")

#Ver o ínicio da base de dados
head(gbd_data)
summary(gbd_data)
names(gbd_data)
gbd_data2 <- read.csv("IHME-GBD_2021_DATA-11d67504-2.csv")
summary(gbd_data2)
names(gbd_data2)

gbd_total <- bind_rows(gbd_data, gbd_data2)#Juntar as duas bases de dados
summary(gbd_total)

# Devolve um vetor com todos os valores únicos (os nomes das categorias)
measure_classes <- unique(gbd_total$measure)
print(measure_classes)

location_classes <- unique(gbd_total$location)
print(location_classes)
cause_classes <- unique(gbd_total$cause)
print(cause_classes)
rei_classes <- unique(gbd_total$rei)
print(rei_classes)
metric_classes <- unique(gbd_total$metric)
print(metric_classes)
age_classes <- unique(gbd_total$age)
print(age_classes)

View(table(gbd_total))
