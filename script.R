rm(list=ls())

library(readxl)
library(ggplot2)

setwd("~/Documents/OnCampusJob/script_r/Analyze-Data-R")
d <- read_excel("Seguimiento a casos.xlsm", sheet = "BD")
head(d)
dim(d)
d2 <- as.data.frame(d)
head(d2)

data.frame(prop.table(table(d2$institucion, d2$rangoedad),2))

#BarPlot por institucion
inst <- data.frame(table(d2$institucion))
colnames(inst) <- c("Institucion","Casos")
ggplot(data=inst, aes(x=Institucion, y=Casos, fill=Institucion)) +
  geom_bar(stat="identity", width=0.7, color="white") + coord_flip()

#Piechart por Tipo de colaborador
t_colab <- data.frame(table(d2$tipo))
colnames(t_colab) <- c("Tipo", "Casos")
ggplot(t_colab, aes(x="", y=Casos, fill=Tipo)) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0) + theme_void()

#BarPlot rango de edad
r_edad <- data.frame(table(d2$rangoedad))
colnames(r_edad) <- c("Rango","Casos")
ggplot(data=r_edad, aes(x=Rango, y=Casos, fill=Rango)) +
  geom_bar(stat="identity", width=0.7, color="white") + coord_flip()

#Piechar por genero
genero <- data.frame(table(d2$genero))
colnames(genero) <- c("Genero", "Casos")
ggplot(genero, aes(x="", y=Casos, fill=Genero)) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0) + theme_void()

#BarPlot por diagnostico
diag <- data.frame(table(d2$diagnostico))
colnames(diag) <- c("Diagnostico", "Casos")
ggplot(data=diag, aes(x=Diagnostico, y=Casos, fill=Diagnostico)) +
  geom_bar(stat="identity", width=0.7, color="white") + coord_flip()


