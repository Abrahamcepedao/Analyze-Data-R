rm(list=ls())

library(readxl)
library(ggplot2)
library(scales)
library(dplyr)
library(stringr)

setwd("~/Documents/OnCampusJob/script_r/Analyze-Data-R")
d <- read_excel("Colaboradores_Covid_positivos_13_08_20.xlsm", sheet = "BD")
d2 <- as.data.frame(d)
colnames(d2) <- c("genero", "institucion", "campus", "estado", "posicion", "tipo", "edad", "rangoedad", "inicio", "semanaContagio", "tipoContagio", "asistidoCampus", "tipoAtencion", "diagnostico", "morbilidades", "alta", "fechaAlta", "fechaFallecimiento")

#Clean data
d2$tipoContagio <- str_trim(d2$tipoContagio)
d2$tipoContagio <- ifelse(d2$tipoContagio == "1= Local", "1=Local", d2$tipoContagio)

d2$campus <- str_trim(d2$campus)
d2$campus <- ifelse(d2$campus == "Areas de Apoyo", "Áreas de apoyo", d2$campus)
d2$campus <- ifelse(d2$campus == "C CM", "CCM", d2$campus)
d2$campus <- ifelse(d2$campus == "C. Querétaro", "Querétaro", d2$campus)
d2$campus <- ifelse(d2$campus == "C. Mty", "Monterrey", d2$campus)
d2$campus <- ifelse(d2$campus == "C Sinaloa", "Sinaloa", d2$campus)
d2$campus <- ifelse(d2$campus == "C Veracruz", "Veracruz", d2$campus)
d2$campus <- ifelse(d2$campus == "C Santa Fe", "Santa Fé", d2$campus)
d2$campus <- ifelse(d2$campus == "C Laguna", "Laguna", d2$campus)
d2$campus <- ifelse(d2$campus == "C Ferrería", "Ferrería", d2$campus)
d2$campus <- ifelse(d2$campus == "C Guadalajara", "Guadalajara", d2$campus)
d2$campus <- ifelse(d2$campus == "Prog. en línea", "Prog. En línea", d2$campus)
d2$campus <- ifelse(d2$campus == "Guarderia TEC", "Guardería Tec", d2$campus)
d2$campus <- ifelse(d2$campus == "Guarderia Tec", "Guardería Tec", d2$campus)

d2$estado <- str_trim(d2$estado)
d2$estado <- ifelse(d2$estado == "CDMX", "Ciudad de México", d2$estado)
d2$estado <- ifelse(d2$estado == "Nuevo Léon", "Nuevo León", d2$estado)


#BarPlot por institucion
inst <- data.frame(table(d2$institucion))
colnames(inst) <- c("Institución","Casos")
ggplot(data=inst, aes(x=Institución, y=Casos, fill=Institución)) +
  geom_bar(stat="identity", width=0.7, color="white") + coord_flip() + 
  geom_text(aes(label = Casos), hjust = -0.2) +
  ggtitle("Número de casos Covid-19 por institución") +
  xlab("Número de casos") + ylab("Institución") +
  theme(plot.title = element_text(lineheight=.8, face="bold", size = 18)) +
  theme(text = element_text(size=15)) +
  scale_fill_brewer(direction = -1)

#Piechart por Tipo de colaborador
t_colab <- data.frame(table(d2$tipo), data.frame(prop.table(table(d2$tipo)))$Freq*100)
colnames(t_colab) <- c("Tipo", "Casos", "prop")
t_colab <- t_colab %>%
  arrange(desc(Tipo)) %>%
  mutate(ypos = cumsum(prop) - 0.5*prop)
ggplot(t_colab, aes(x="", y=prop, fill=Tipo)) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0) +
  geom_text(aes(y = ypos, label = paste(format(round(prop, 1), nsmall = 1), "%", sep = "")), color = "black") +
  theme_void() +
  ggtitle("Porcentaje de casos Covid-19 por tipo de colaborador") +
  guides(fill=guide_legend(title="Tipo de colaborador")) +
  theme(plot.title = element_text(lineheight=.8, face="bold", size = 18)) +
  theme(text = element_text(size=15)) +
  scale_fill_brewer(direction = -1)

#BarPlot rango de edad
r_edad <- data.frame(table(d2$rangoedad))
colnames(r_edad) <- c("Rango","Casos")
ggplot(data=r_edad, aes(x=Rango, y=Casos, fill=Rango)) +
  geom_bar(stat="identity", width=0.7, color="white") + coord_flip() + 
  geom_text(aes(label = Casos), hjust = -0.2) +
  xlab("Número de casos") + ylab("Rango de edad") +
  ggtitle("Número de casos Covid-19 por rango de edad") +
  theme(plot.title = element_text(lineheight=.8, face="bold", size = 18)) +
  theme(text = element_text(size=15)) +
  scale_fill_brewer(direction = -1)

#Piechart por genero
genero <- data.frame(table(d2$genero), data.frame(prop.table(table(d2$genero)))$Freq*100)
colnames(genero) <- c("Genero", "Casos", "prop")
genero <- genero %>%
  arrange(desc(Genero)) %>%
  mutate(ypos = cumsum(prop) - 0.5*prop)
ggplot(genero, aes(x="", y=prop, fill=Genero)) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0) +
  geom_text(aes(y = ypos, label = paste(format(round(prop, 1), nsmall = 1), "%", sep = "")), color = "black") +
  theme_void() +
  ggtitle("Porcentaje de casos Covid-19 por género") +
  guides(fill=guide_legend(title="Género")) +
  theme(plot.title = element_text(lineheight=.8, face="bold", size = 18)) +
  theme(text = element_text(size=15)) +
  scale_fill_brewer(direction = -1)

#BarPlot por diagnostico
diag <- data.frame(table(d2$diagnostico))
colnames(diag) <- c("Diagnostico", "Casos")
ggplot(data=diag, aes(x=Diagnostico, y=Casos, fill=Diagnostico)) +
  geom_bar(stat="identity", width=0.7, color="white") + 
  geom_text(aes(label = Casos), vjust = -0.2) +
  xlab("Número de casos") + ylab("Diagnóstico") +
  ggtitle("Número de casos Covid-19 por diagnóstico") +
  guides(fill=guide_legend(title="Diagnóstico")) +
  theme(plot.title = element_text(lineheight=.8, face="bold", size = 18)) +
  theme(text = element_text(size=15)) +
  scale_fill_brewer(direction = -1)

#Piechart por alta medica
alta <- data.frame(table(d2$alta), data.frame(prop.table(table(d2$alta)))$Freq*100)
colnames(alta) <- c("Alta_Médica", "Casos", "prop")
alta <- alta %>%
  arrange(desc(Alta_Médica)) %>%
  mutate(ypos = cumsum(prop) - 0.5*prop)
ggplot(alta, aes(x="", y=prop, fill=Alta_Médica )) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0) +
  geom_text(aes(y = ypos, label = paste(format(round(prop, 1), nsmall = 1), "%", sep = "")), color = "black") +
  theme_void() +
  guides(fill=guide_legend(title="Alta médica")) +
  ggtitle("Porcentaje de casos Covid-19 por alta médica") +
  theme(plot.title = element_text(lineheight=.8, face="bold", size = 18)) +
  theme(text = element_text(size=15)) +
  scale_fill_brewer() 

#BarPlot por semana de contagio
semana <- data.frame(table(d2$semanaContagio))
colnames(semana) <- c("Semana", "Casos")
ggplot(data=semana, aes(x=Semana, y=Casos, fill=Semana)) +
  geom_bar(stat="identity", width=0.7, color="white") + coord_flip() + 
  geom_text(aes(label = Casos), hjust = -0.2) +
  xlab("Número de casos") + ylab("Semana") +
  ggtitle("Número de casos Covid-19 por semana de contagio") +
  theme(plot.title = element_text(lineheight=.8, face="bold", size = 18)) +
  theme(text = element_text(size=15)) +
  scale_fill_viridis_d()


#LinePlot por contagios acumulados
casos <- as.data.frame(read_excel("Colaboradores_Covid_positivos_13_08_20.xlsm", sheet = "Contagios"))
colnames(casos) <- c("Fecha", "Contagios", "Acumulados")
ggplot(casos, aes(x=as.Date(Fecha, origin="1899-12-30"), y=Acumulados)) +
  geom_area( fill="#0a529c", alpha=0.4) +
  geom_line(color="#2669ad", size=2) +
  geom_point(size=3, color="#0a529c") +
  theme(plot.title = element_text(lineheight=.8, face="bold", size = 18)) +
  theme(text = element_text(size=15)) +
  xlab("Fecha") + ylab("Contagios acumulados") +
  ggtitle("Número de casos Covid-19 acumulados")


#BarPlot por contagios diarios
ggplot(data=casos, aes(x=as.Date(Fecha, origin="1899-12-30"), y=Contagios, fill=Contagios)) +
  geom_bar(stat = "identity", position="identity") +
  ggtitle("Número de casos Covid-19 por fecha de inicio de síntomas") +
  xlab("Fecha de inicio de síntomas") + ylab("Número de casos") +
  scale_x_date(labels=date_format("%b %d"), breaks=date_breaks("2 weeks")) +
  theme(plot.title = element_text(lineheight=.8, face="bold", size = 18)) +
  theme(text = element_text(size=15)) + stat_smooth(colour="green")  +
  scale_fill_viridis_b()


#BarPlot por estado
estado <- data.frame(sort(table(d2$estado), decreasing = TRUE))
colnames(estado) <- c("Estado", "Casos")
ggplot(data=estado, aes(x=Estado, y=Casos, fill=Estado)) +
  geom_bar(stat="identity", width=0.7, color="white") + coord_flip() +
  geom_text(aes(label = Casos), hjust = -0.2) +
  xlab("Número de casos") + ylab("Estado") +
  scale_fill_viridis_d() +
  theme(plot.title = element_text(lineheight=.8, face="bold", size = 18)) +
  theme(text = element_text(size=15)) +
  ggtitle("Número de casos Covid-19 por estado")

#BarPlot por tipo de contagio
tipo_cont <- data.frame(table(d2$tipoContagio))
colnames(tipo_cont) <- c("Tipo_Contagio", "Casos")
ggplot(data=tipo_cont, aes(x=Tipo_Contagio, y=Casos, fill=Tipo_Contagio)) +
  geom_bar(stat="identity", width=0.7, color="white") +
  geom_text(aes(label = Casos), vjust = -0.2) +
  xlab("Tipo de contagio") + ylab("Número de casos") +
  scale_fill_brewer(direction = -1) +
  guides(fill=guide_legend(title="Tipo de contagio")) +
  theme(plot.title = element_text(lineheight=.8, face="bold", size = 18)) +
  theme(text = element_text(size=15)) +
  ggtitle("Número de casos Covid-19 por tipo de contagio")


#<-------------------------Cruzadas------------------------------->

#BarPlot tipo de empleado contra el campus
emp_camp <- data.frame(prop.table(table(d2$tipo, d2$campus),2))
colnames(emp_camp) <- c("Empleado", "Campus", "Casos")
ggplot(emp_camp, aes(fill=Empleado, y=Campus, x=Casos)) + 
  geom_bar(position="stack", stat="identity") +
  xlab("Porcentaje de casos") + ylab("Campus") +
  scale_fill_brewer() +
  theme(plot.title = element_text(lineheight=.8, face="bold", size = 18)) +
  theme(text = element_text(size=13)) +
  ggtitle("Porcentaje de casos Covid-19 por tipo de empleado contra campus")

#BarPlot rango de edad contra campus
edad_camp <- data.frame(prop.table(table(d2$rangoedad, d2$campus),2))
colnames(edad_camp) <- c("Rango_Edad", "Campus", "Casos")
ggplot(edad_camp, aes(fill=Rango_Edad, y=Campus, x=Casos)) + 
  geom_bar(position="stack", stat="identity") +
  xlab("Porcentaje de casos") + ylab("Campus") +
  scale_fill_brewer() +
  guides(fill=guide_legend(title="Rango de edad")) +
  theme(plot.title = element_text(lineheight=.8, face="bold", size = 18)) +
  theme(text = element_text(size=13)) +
  ggtitle("Porcentaje de casos Covid-19 por rango de edad contra campus")

#BarPlot institucion contra tipo de colaborador
inst_colab <- data.frame(prop.table(table(d2$tipo, d2$institucion),2))
colnames(inst_colab) <- c("Colaborador", "Institucion", "Casos")
ggplot(inst_colab, aes(fill=Colaborador, y=Institucion, x=Casos)) + 
  geom_bar(position="stack", stat="identity") +
  scale_fill_brewer() +
  xlab("Porcentaje de casos") + ylab("Institución") +
  theme(plot.title = element_text(lineheight=.8, face="bold", size = 18)) +
  theme(text = element_text(size=15)) +
  ggtitle("Porcentaje de casos Covid-19 por institución contra tipo de colaborador")


#Tabla cruzada campus contra tipo de colaborados
#Stargazer R
camp_colab <- table(d2$campus, d2$tipo)
#filter table

write.csv(camp_colab, file = "campus_contra_colaborador.csv")

