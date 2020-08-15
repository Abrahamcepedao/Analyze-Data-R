rm(list=ls())

library(readxl)
library(ggplot2)
library(scales)
library(dplyr)

setwd("~/Documents/OnCampusJob/script_r/Analyze-Data-R")
d <- read_excel("Seguimiento_a_casos.xlsm", sheet = "BD")
d2 <- as.data.frame(d)

#BarPlot por institucion
inst <- data.frame(table(d2$institucion))
colnames(inst) <- c("Institucion","Casos")
ggplot(data=inst, aes(x=Institucion, y=Casos, fill=Institucion)) +
  geom_bar(stat="identity", width=0.7, color="white") + coord_flip() + 
  geom_text(aes(label = Casos), hjust = -0.2) +
  ggtitle("Por institución")

#Piechart por Tipo de colaborador
t_colab <- data.frame(table(d2$tipo))
colnames(t_colab) <- c("Tipo", "Casos")
ggplot(t_colab, aes(x="", y=Casos, fill=Tipo)) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0) + theme_void() +
  ggtitle("Por tipo de colaborador")

#BarPlot rango de edad
r_edad <- data.frame(table(d2$rangoedad))
colnames(r_edad) <- c("Rango","Casos")
ggplot(data=r_edad, aes(x=Rango, y=Casos, fill=Rango)) +
  geom_bar(stat="identity", width=0.7, color="white") + coord_flip() + 
  geom_text(aes(label = Casos), hjust = -0.2) +
  ggtitle("Por rango de edad")

#Piechart por genero
genero <- data.frame(table(d2$genero))
colnames(genero) <- c("Genero", "Casos")
ggplot(genero, aes(x="", y=Casos, fill=Genero)) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0) + theme_void() + 
  geom_text(aes(label = paste(round(Casos / sum(Casos) * 100, 1), "%"), x = 0.5),
  position = position_stack(vjust = 0.5)) +
  ggtitle("Por género")

#BarPlot por diagnostico
diag <- data.frame(table(d2$diagnostico))
colnames(diag) <- c("Diagnostico", "Casos")
ggplot(data=diag, aes(x=Diagnostico, y=Casos, fill=Diagnostico)) +
  geom_bar(stat="identity", width=0.7, color="white") + coord_flip() + 
  geom_text(aes(label = Casos), hjust = -0.2) +
  ggtitle("Por diagnóstico")

#Piechart por alta medica
alta <- data.frame(table(d2$alta), data.frame(prop.table(table(d2$alta)))$Freq*100)
colnames(alta) <- c("Alta_Medica", "Casos", "prop")
alta <- alta %>%
  arrange(desc(Alta_Medica)) %>%
  mutate(lab.ypos = cumsum(prop) - 0.5*prop)

ggplot(alta, aes(x="", y=Casos, fill=Alta_Medica)) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0) + theme_void() +
  geom_text(aes(y = lab.ypos, label = prop), color = "white") +
  ggtitle("Por alta medica")

#BarPlot por semana de contagio
semana <- data.frame(table(d2$semanaContagio))
colnames(semana) <- c("Semana", "Casos")
ggplot(data=semana, aes(x=Semana, y=Casos, fill=Semana)) +
  geom_bar(stat="identity", width=0.7, color="white") + coord_flip() + 
  geom_text(aes(label = Casos), hjust = -0.2) +
  ggtitle("Por semana de contagio")


#LinePlot por contagios acumulados
casos <- as.data.frame(read_excel("Seguimiento_a_casos.xlsm", sheet = "Contagios"))
colnames(casos) <- c("Fecha", "Contagios", "Acumulados")
ggplot(casos, aes(x=Fecha, y=Acumulados)) +
  geom_area( fill="#69b3a2", alpha=0.4) +
  geom_line(color="#69b3a2", size=2) +
  geom_point(size=3, color="#69b3a2") +
  ggtitle("Contagios acumulados")


#BarPlot por contagios diarios
ggplot(data=casos, aes(x=as.Date(Fecha, origin="1899-12-30"), y=Contagios, fill=Contagios)) +
  geom_bar(stat = "identity", position="identity") +
  ggtitle("Número de casos Covid-19 por fecha de inicio de síntomas (Nacional)") +
  xlab("Fecha de inicio de síntomas") + ylab("Número de casos") +
  scale_x_date(labels=date_format("%b %d"), breaks=date_breaks("2 weeks")) +
  theme(plot.title = element_text(lineheight=.8, face="bold", size = 20)) +
  theme(text = element_text(size=15)) + stat_smooth(colour="green")


#BarPlot por estado
estado <- data.frame(sort(table(d2$estado), decreasing = TRUE))
colnames(estado) <- c("Estado", "Casos")
ggplot(data=estado, aes(x=Estado, y=Casos, fill=Estado)) +
  geom_bar(stat="identity", width=0.7, color="white") + coord_flip() +
  geom_text(aes(label = Casos), hjust = -0.2) +
  ggtitle("Por estado")

#BarPlot por tipo de contagio
tipo_cont <- data.frame(table(d2$tipoContagio))
colnames(tipo_cont) <- c("Tipo_Contagio", "Casos")
ggplot(data=tipo_cont, aes(x=Tipo_Contagio, y=Casos, fill=Tipo_Contagio)) +
  geom_bar(stat="identity", width=0.7, color="white") +
  geom_text(aes(label = Casos), vjust = -0.2) +
  ggtitle("Por tipo de contagio")


#<-------------------------Cruzadas------------------------------->

#BarPlot tipo de empleado contra el campus
emp_camp <- data.frame(prop.table(table(d2$tipo, d2$campus),2))
colnames(emp_camp) <- c("Empleado", "Campus", "Casos")
ggplot(emp_camp, aes(fill=Empleado, y=Campus, x=Casos)) + 
  geom_bar(position="stack", stat="identity") +
  ggtitle("Tipo de empleado contra campus")

#BarPlot rango de edad contra campus
edad_camp <- data.frame(prop.table(table(d2$rangoedad, d2$campus),2))
colnames(edad_camp) <- c("Rango_Edad", "Campus", "Casos")
ggplot(edad_camp, aes(fill=Rango_Edad, y=Campus, x=Casos)) + 
  geom_bar(position="stack", stat="identity") +
  ggtitle("Rango de edad contra campus")

#BarPlot institucion contra tipo de colaborador
inst_colab <- data.frame(prop.table(table(d2$institucion, d2$tipo),2))
colnames(inst_colab) <- c("Institucion", "Colaborador", "Casos")
ggplot(inst_colab, aes(fill=Colaborador, y=Institucion, x=Casos)) + 
  geom_bar(position="stack", stat="identity") +
  ggtitle("Institución contra tipo de colaborador")

## more


#geom_text(aes(x=fecha[80], label="\nApertura", y=5000), colour="red", angle=90, size=6)
#stat_smooth(colour="green)
#d <- data.frame(table(col1, col 2))
#write.csv(d, ".csv", row.names=FALSE)
#brewer
#virids
