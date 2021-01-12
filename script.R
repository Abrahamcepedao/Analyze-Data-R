rm(list=ls())

library(readxl)
library(ggplot2)
library(scales)
library(dplyr)
library(stringr)
library(RColorBrewer)

setwd("~/Documents/OnCampusJob/propuesta-graficos-covid-19/Analyze-Data-R")
d <- read_excel("Colaboradores_Covid_positivos_06_01_21.xlsm", sheet = "BD")
d2 <- as.data.frame(d)

#Nombres de columnas
colnames(d2) <- c("nomina", "nombre", "genero", "institucion", "campus", "estado", "posicion", "corre", "celular", "tipo", "edad", "rangoedad", "inicio", "semanaContagio", "tipoContagio", "asistidoCampus", "tipoAtencion", "diagnostico", "morbilidades", "alta", "fechaAlta", "fechaFallecimiento")

#Limpiar datos tipo de contagio
d2$tipoContagio <- str_trim(d2$tipoContagio)
d2$tipoContagio <- ifelse(d2$tipoContagio == "1= Local", "1=Local", d2$tipoContagio)

#Limpiar datos de campus
d2$campus <- str_trim(d2$campus)
d2$campus <- ifelse(d2$campus == "Areas de Apoyo", "Áreas de apoyo", d2$campus)
d2$campus <- ifelse(d2$campus == "Áreas de Apoyo", "Áreas de apoyo", d2$campus)
d2$campus <- ifelse(d2$campus == "C CM", "Ciudad de México", d2$campus)
d2$campus <- ifelse(d2$campus == "CCM", "Ciudad de México", d2$campus)
d2$campus <- ifelse(d2$campus == "C. Querétaro", "Querétaro", d2$campus)
d2$campus <- ifelse(d2$campus == "C. Mty", "Monterrey", d2$campus)
d2$campus <- ifelse(d2$campus == "C MTY", "Monterrey", d2$campus)
d2$campus <- ifelse(d2$campus == "C Sinaloa", "Sinaloa", d2$campus)
d2$campus <- ifelse(d2$campus == "C Veracruz", "Veracruz", d2$campus)
d2$campus <- ifelse(d2$campus == "C Santa Fe", "Santa Fé", d2$campus)
d2$campus <- ifelse(d2$campus == "C Laguna", "Laguna", d2$campus)
d2$campus <- ifelse(d2$campus == "C Ferrería", "Ferrería", d2$campus)
d2$campus <- ifelse(d2$campus == "C Guadalajara", "Guadalajara", d2$campus)
d2$campus <- ifelse(d2$campus == "CULIACAN", "Culiacán", d2$campus)
d2$campus <- ifelse(d2$campus == "Campus Monterrey", "Monterrey", d2$campus)
d2$campus <- ifelse(d2$campus == "Eugenio Garza Laguera", "EGL", d2$campus)
d2$campus <- ifelse(d2$campus == "Prog. en línea", "Prog. En línea", d2$campus)
d2$campus <- ifelse(d2$campus == "Guarderia TEC", "Guardería Tec", d2$campus)
d2$campus <- ifelse(d2$campus == "Guarderia Tec", "Guardería Tec", d2$campus)
d2$campus <- ifelse(d2$campus == "O Mazatlán", "Mazatlán", d2$campus)
d2$campus <- ifelse(d2$campus == "O México", "Ciudad de  México", d2$campus)
d2$campus <- ifelse(d2$campus == "O Monterrey", "Monterrey", d2$campus)
d2$campus <- ifelse(d2$campus == "R.  Tec. Mty", "R.  Tec Mty", d2$campus)
d2$campus <- ifelse(d2$campus == "Santa Fe", "Santa Fé", d2$campus)
data$Campus <- ifelse(data$Campus == "Central de Veracruz", "Veracruz", data$Campus)
data$Campus <- ifelse(data$Campus == "Santa Fe", "Santa Fé", data$Campus)
data$Campus <- ifelse(data$Campus == "Sonora Norte", "Sonora", data$Campus)

#Limpiar datos de estado
d2$estado <- str_trim(d2$estado)
d2$estado <- ifelse(d2$estado == "CDMX", "Ciudad de México", d2$estado)
d2$estado <- ifelse(d2$estado == "Torreón", "Coahuila", d2$estado)
d2$estado <- ifelse(d2$estado == "jalisco", "Jalisco", d2$estado)
d2$estado <- ifelse(d2$estado == "Mexico", "Estado de México", d2$estado)
d2$estado <- ifelse(d2$estado == "México", "Estado de México", d2$estado)
d2$estado <- ifelse(d2$estado == "Toluca", "Estado de México", d2$estado)
d2$estado <- ifelse(d2$estado == "Monterrey", "Nuevo León", d2$estado)
d2$estado <- ifelse(d2$estado == "Nuevo Léon", "Nuevo León", d2$estado)
d2$estado <- ifelse(d2$estado == "Nuevo Leon", "Nuevo León", d2$estado)
d2$estado <- ifelse(d2$estado == "Nuevo leon", "Nuevo León", d2$estado)
d2$estado <- ifelse(d2$estado == "nuevo León", "Nuevo León", d2$estado)
d2$estado <- ifelse(d2$estado == "SINALOA", "Sinaloa", d2$estado)
d2$estado <- ifelse(d2$estado == "sinaloa", "Sinaloa", d2$estado)
d2$estado <- ifelse(d2$estado == "Veracruz", "Veracrúz", d2$estado)

#Limpiar datos de institucion
d2$institucion <- ifelse(d2$institucion == "TECMILENIO", "Tecmilenio", d2$institucion)

#Limpiar datos de semana de contagio
d2$semanaContagio <- str_trim(d2$semanaContagio)
d2$semanaContagio <- ifelse(d2$semanaContagio == "1er abril", "1ra abril", d2$semanaContagio)
d2$semanaContagio <- ifelse(d2$semanaContagio == "1er marzo", "1ra marzo", d2$semanaContagio)
d2$semanaContagio <- ifelse(d2$semanaContagio == "1er mayo", "1ra mayo", d2$semanaContagio)
d2$semanaContagio <- ifelse(d2$semanaContagio == "1er agosto", "1ra agosto", d2$semanaContagio)
d2$semanaContagio <- ifelse(d2$semanaContagio == "1era julio", "1ra julio", d2$semanaContagio)
d2$semanaContagio <- ifelse(d2$semanaContagio == "1er julio", "1ra julio", d2$semanaContagio)
d2$semanaContagio <- ifelse(d2$semanaContagio == "2da Julio", "2da julio", d2$semanaContagio)
d2$semanaContagio <- ifelse(d2$semanaContagio == "3er marzo", "3ra marzo", d2$semanaContagio)
d2$semanaContagio <- ifelse(d2$semanaContagio == "3er abril", "3ra abril", d2$semanaContagio)
d2$semanaContagio <- ifelse(d2$semanaContagio == "3er mayo", "3ra mayo", d2$semanaContagio)
d2$semanaContagio <- ifelse(d2$semanaContagio == "3er marzo", "3ra marzo", d2$semanaContagio)
d2$semanaContagio <- ifelse(d2$semanaContagio == "3era junio", "3ra junio", d2$semanaContagio)
d2$semanaContagio <- ifelse(d2$semanaContagio == "3er junio", "3ra junio", d2$semanaContagio)
d2$semanaContagio <- ifelse(d2$semanaContagio == "3er julio", "3ra julio", d2$semanaContagio)
d2$semanaContagio <- ifelse(d2$semanaContagio == "3er agosto", "3ra agosto", d2$semanaContagio)
d2$semanaContagio <- ifelse(d2$semanaContagio == "4ta Agosto", "4ta agosto", d2$semanaContagio)
d2$semanaContagio <- ifelse(d2$semanaContagio == "4ta Julio", "4ta julio", d2$semanaContagio)
d2$semanaContagio <- ifelse(d2$semanaContagio == "1er septiembre", "1ra septiembre", d2$semanaContagio)
d2$semanaContagio <- ifelse(d2$semanaContagio == "3er septiembre", "3ra septiembre", d2$semanaContagio)
d2$semanaContagio <- ifelse(d2$semanaContagio == "1er octubre", "1ra octubre", d2$semanaContagio)
d2$semanaContagio <- ifelse(d2$semanaContagio == "3er Octubre", "3ra octubre", d2$semanaContagio)
d2$semanaContagio <- ifelse(d2$semanaContagio == "4ta Octubre", "4ta octubre", d2$semanaContagio)
d2$semanaContagio <- ifelse(d2$semanaContagio == "1er noviembre", "1ra noviembre", d2$semanaContagio)
d2$semanaContagio <- ifelse(d2$semanaContagio == "1er Noviembre", "1ra noviembre", d2$semanaContagio)
d2$semanaContagio <- ifelse(d2$semanaContagio == "3er noviembre", "3ra noviembre", d2$semanaContagio)
d2$semanaContagio <- ifelse(d2$semanaContagio == "1er de Diciembre", "1ra diciembre", d2$semanaContagio)
d2$semanaContagio <- ifelse(d2$semanaContagio == "1er Diciembre", "1ra diciembre", d2$semanaContagio)
d2$semanaContagio <- ifelse(d2$semanaContagio == "3er diciembre", "3ra diciembre", d2$semanaContagio)

#Limpiar datos tipo colaborador
d2$tipo <- ifelse(d2$tipo == "1=Académico", "Académico", d2$tipo)
d2$tipo <- ifelse(d2$tipo == "2=Apoyo", "Apoyo", d2$tipo)
d2$tipo <- ifelse(d2$tipo == "3=Apoyo académico", "Apoyo Académico", d2$tipo)
d2$tipo <- ifelse(d2$tipo == "3=Apoyo Académico", "Apoyo Académico", d2$tipo)
d2$tipo <- ifelse(d2$tipo == "4=Operativo", "Operativo", d2$tipo)
d2$tipo <- ifelse(d2$tipo == "5=Clínico", "Clínico", d2$tipo)

#Limpiar datos tipo de diagnostico
d2$diagnostico <- ifelse(d2$diagnostico == "1=Ambulatorio", "Ambulatorio", d2$diagnostico)
d2$diagnostico <- ifelse(d2$diagnostico == "2=Hospitalizado", "Hospitalizado", d2$diagnostico)

#Limpiar datos genero
d2$genero <- ifelse(d2$genero == "femenino", "Femenino", d2$genero)
d2$genero <- ifelse(d2$genero == "FEMENINO", "Femenino", d2$genero)
d2$genero <- ifelse(d2$genero == "masculino", "Masculino", d2$genero)

#Limpiar datos de tipo de alta
d2$alta <- ifelse(d2$alta == "NO", "No", d2$alta)
d2$alta <- ifelse(d2$alta == "no", "No", d2$alta)

semana_num <- c("1ra", "2da", "3ra", "4ta", "5ta") # numero de semanas
meses <- c("enero", "febrero", "marzo", "abril", "mayo", "junio", "julio", "agosto", "septiembre", "octubre", "noviembre", "diciembre") # meses


#Color palette  config
getPalette = colorRampPalette(brewer.pal(9, "Blues"))

#BarPlot por institucion
#institucion:
#  -> Sorteos tec
#  -> Tecmilenio
#  -> Tecnologico de monterrey
#  -> TecSalud
inst <- data.frame(table(d2$institucion))
colnames(inst) <- c("Institución","Casos")
ggplot(data=inst, aes(x=Institución, y=Casos, fill=Institución)) +
  geom_bar(stat="identity", width=0.7, color="white") + coord_flip() + 
  geom_text(aes(label = Casos), hjust = -0.2, size=5) +
  ggtitle("Número de casos Covid-19 por institución") +
  xlab("Institución") + ylab("Número de casos") +
  theme(plot.title = element_text(lineheight=.8, face="bold", size = 18)) +
  theme(text = element_text(size=15)) + theme(legend.position="none") +
  scale_fill_brewer()

#Piechart por institucion
#institucion:
#  -> Sorteos tec
#  -> Tecmilenio
#  -> Tecnologico de monterrey
#  -> TecSalud
inst2 <- data.frame(table(d2$institucion), data.frame(prop.table(table(d2$institucion)))$Freq*100)
colnames(inst2) <- c("Institución","Casos", "prop")
inst2 <- inst2 %>%
  arrange(desc(Institución)) %>%
  mutate(ypos = cumsum(prop) - 0.5*prop)
ggplot(inst2, aes(x="", y=prop, fill=Institución)) +
  geom_bar(stat="identity") +
  coord_polar("y", start=0) +
  geom_text(aes(y = ypos, label = paste(format(round(prop, 1), nsmall = 1), "%", sep = "")), color = "black", size=6) +
  theme_void() +
  ggtitle("Porcentaje de casos Covid-19 por institución") +
  guides(fill=guide_legend(title="Institución")) +
  theme(plot.title = element_text(lineheight=.8, face="bold", size = 18)) +
  theme(text = element_text(size=20)) +
  scale_fill_brewer()

#Piechart por Tipo de colaborador
#tipo de colaborador:
#  -> Academico
#  -> Apoyo
#  -> Apoyo Academico
#  -> Clinico
#  -> Operativo
t_colab <- data.frame(table(d2$tipo), data.frame(prop.table(table(d2$tipo)))$Freq*100)
colnames(t_colab) <- c("Tipo", "Casos", "prop")
ggplot(data=t_colab, aes(x=Tipo, y=Casos, fill=Tipo)) +
  geom_bar(stat="identity", width=0.7, color="white") + coord_flip() + 
  geom_text(aes(label = Casos), hjust = -0.2, size=5) +
  ggtitle("Número de casos Covid-19 por tipo de colaborador") +
  xlab("Colaborador") + ylab("Número de casos") +
  theme(plot.title = element_text(lineheight=.8, face="bold", size = 18)) +
  theme(text = element_text(size=15)) +
  scale_fill_brewer()


#BarPlot por acumulacion de fallecimiento por fecha
#NOTA: solo toma los que tienen fecha
fallecimiento <- data.frame(table(d2$fechaFallecimiento))
fallecimiento$Acum <- rep(0, times = length(fallecimiento$Var1))
colnames(fallecimiento) <- c("Fecha", "Casos", "Acum")
acum <- 0
for(i in 1:length(fallecimiento$Fecha)){
  acum <- fallecimiento$Casos[i] + acum
  fallecimiento$Acum[i] <- acum
}
ggplot(data=fallecimiento, aes(x=Fecha, y=Acum, fill=Fecha)) +
  geom_bar(stat="identity", width=0.7, color="white", fill = getPalette(length(fallecimiento$Fecha))) +
  geom_text(aes(label = Acum), vjust = -0.2, size=5) +
  ggtitle("Número de fallecimientos Covid-19 por fecha") +
  xlab("Fecha") + ylab("Acumulación de casos") +
  theme(plot.title = element_text(lineheight=.8, face="bold", size = 18)) +
  theme(text = element_text(size=15), axis.text.x = element_text(angle=75, hjust=1)) + theme(legend.position="none")

#BarPlot rango de edad
#rango de edad:
#  -> 0-19
#  -> 20-29
#  -> 30-39
#  -> 40-49
#  -> 50-59
#  -> 60-69
#  -> 70-79
r_edad <- data.frame(table(d2$rangoedad))
colnames(r_edad) <- c("Rango","Casos")
ggplot(data=r_edad, aes(x=Rango, y=Casos, fill=Rango)) +
  geom_bar(stat="identity", width=0.7, color="white") + coord_flip() + 
  geom_text(aes(label = Casos), hjust = -0.05, size=5) +
  xlab("Rango de edad") + ylab("Número de casos") +
  ggtitle("Número de casos Covid-19 por rango de edad") +
  theme(plot.title = element_text(lineheight=.8, face="bold", size = 18)) +
  theme(text = element_text(size=15)) +
  scale_fill_brewer()

#Piechart por genero
#rango de edad:
#  -> Femenino
#  -> Masculino
genero <- data.frame(table(d2$genero), data.frame(prop.table(table(d2$genero)))$Freq*100)
colnames(genero) <- c("Genero", "Casos", "prop")
genero <- genero %>%
  arrange(desc(Genero)) %>%
  mutate(ypos = cumsum(prop) - 0.5*prop)
ggplot(genero, aes(x="", y=prop, fill=Genero)) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0) +
  geom_text(aes(y = ypos, label = paste(format(round(prop, 1), nsmall = 1), "%", sep = "")), color = "black", size=10) +
  theme_void() +
  ggtitle("Porcentaje de casos Covid-19 por género") +
  guides(fill=guide_legend(title="Género")) +
  theme(plot.title = element_text(lineheight=.8, face="bold", size = 18)) +
  theme(text = element_text(size=15)) +
  scale_fill_brewer()

#BarPlot por diagnostico
#rango de edad:
#  -> Ambulatorio
#  -> Hospitalizado
diag <- data.frame(table(d2$diagnostico))
colnames(diag) <- c("Diagnostico", "Casos")
ggplot(data=diag, aes(x=Diagnostico, y=Casos, fill=Diagnostico)) +
  geom_bar(stat="identity", width=0.7, color="white") + 
  geom_text(aes(label = Casos), vjust = -0.2, size=5) +
  xlab("Diagnóstico") + ylab("Número de casos") +
  ggtitle("Número de casos Covid-19 por diagnóstico") +
  guides(fill=guide_legend(title="Diagnóstico")) +
  theme(plot.title = element_text(lineheight=.8, face="bold", size = 18)) +
  theme(text = element_text(size=15)) +
  scale_fill_brewer()

#Piechart por alta medica
#alta médica:
#  -> No
#  -> Sí
alta <- data.frame(table(d2$alta), data.frame(prop.table(table(d2$alta)))$Freq*100)
colnames(alta) <- c("Alta_Médica", "Casos", "prop")
alta <- alta %>%
  arrange(desc(Alta_Médica)) %>%
  mutate(ypos = cumsum(prop) - 0.5*prop)

alta2 <- data.frame(
  Alta_Médica <- c("No", "Sí"),
  Freq <- c(50, 526),
  prop <- c(8.68, 91.31)
)
colnames(alta2) <- c("Alta_Médica", "Casos", "prop")
alta2 <- alta2 %>%
  arrange(desc(Alta_Médica)) %>%
  mutate(ypos = cumsum(prop) - 0.5*prop)

ggplot(alta, aes(x="", y=prop, fill=Alta_Médica )) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0) +
  geom_text(aes(y = ypos, label = paste(format(round(prop, 1), nsmall = 1), "%", sep = "")), color = "black", size=10) +
  theme_void() +
  guides(fill=guide_legend(title="Alta médica")) +
  ggtitle("Porcentaje de casos Covid-19 por alta médica") +
  theme(plot.title = element_text(lineheight=.8, face="bold", size = 18)) +
  theme(text = element_text(size=15)) +
  scale_fill_brewer(direction = -1)

#BarPlot por semana de contagio
#semana de contagio:
#  -> Semanda y cantidad de contagios en la semana
semana <- data.frame(table(d2$semanaContagio))
colnames(semana) <- c("Semana", "Casos")

semana$semana_num <- substr(semana$Semana,1,3)
semana$mes <- str_trim(substring(semana$Semana,4))
semana$semana_num <- factor(semana$semana_num, levels = semana_num)
semana$mes <- factor(semana$mes, levels = meses)
semana <- data.frame(semana[order(semana$mes,semana$semana_num),])
semana$Semana <- factor(semana$Semana, levels = rev(semana$Semana))

#Generar csv con datos historicos de casos acumulados por semana de inicio de síntomas
semanaP <- data.frame(
  Semana <- semana$Semana,
  Casos <- semana$Casos
)
colnames(semanaP) <- c("Semana de Contagio", "Casos")
write.csv(semanaP, file = "campus_contra_colaborador.csv")

ggplot(data=semana, aes(x=Casos, y=Semana, fill=Semana)) +
  geom_bar(stat="identity", width=0.7, color="white", fill = getPalette(length(semana$Semana))) +
  geom_text(aes(label = Casos), hjust = -0.2, size=5) +
  xlab("Número de casos") + ylab("Semana") +
  ggtitle("Número de casos Covid-19 por semana de contagio") +
  theme(plot.title = element_text(lineheight=.8, face="bold", size = 18)) +
  theme(text = element_text(size=15)) + theme(legend.position="none")

#BarPlot por semana de contagio por tipo  de colaborados
#semana de contagio:
#  -> Semanda y cantidad de contagios en la semana
#  -> Tipo de colaborador
semana2 <- data.frame(table(d2$semanaContagio, d2$tipo))
colnames(semana2) <- c("Semana", "Colaborador", "Casos")
semana2$semana_num <- substr(semana2$Semana,1,3)
semana2$mes <- str_trim(substring(semana2$Semana,4))
semana2$semana_num <- factor(semana2$semana_num, levels = semana_num)
semana2$mes <- factor(semana2$mes, levels = meses)
semana2 <- data.frame(semana2[order(semana2$mes,semana2$semana_num),])
semana2$Semana <- factor(semana2$Semana, levels = unique(rev(semana2$Semana)))

ggplot(data = semana2, aes(x = Casos, y = Semana, fill = Colaborador)) + 
  geom_bar(stat = "identity", width=0.7, color="white") +
  xlab("Número de casos") + ylab("Semana") +
  ggtitle("Número de casos Covid-19 por semana de contagio contra tipo de colaborador") +
  geom_text(aes(label = stat(x), group = Semana), stat = 'summary', fun = sum, hjust = -0.2, size=5) +
  theme(plot.title = element_text(lineheight=.8, face="bold", size = 18)) +
  theme(text = element_text(size=15)) +
  scale_fill_brewer()
  
  

#LinePlot por contagios acumulados
#  -> lee una pestaña de excel que cuenta con:
#       ->  Fecha (dia)
#       ->  Numero de contagios en  esa fecha
#       ->  Acumlado hasta la fecha
#  -> Acumulador por cada día
casos <- as.data.frame(read_excel("Colaboradores_Covid_positivos_06_01_21.xlsm", sheet = "Contagios"))
colnames(casos) <- c("Fecha", "Contagios", "Acumulados")
casosTotalAcum <- casos$Acumulados[length(casos$Acumulados)]
ggplot(casos, aes(x=as.Date(Fecha, origin="1899-12-30"), y=Acumulados)) +
  geom_area( fill="#1769a8", alpha=0.5) +
  geom_line(color="#184363", size=2) +
  geom_point(size=3, color="#184363") +
  theme(plot.title = element_text(lineheight=.8, face="bold", size = 18)) +
  theme(text = element_text(size=15)) +
  xlab("Fecha") + ylab("Coantagios acumulados") +
  ggtitle("Número de casos Covid-19 acumulados") +
  coord_cartesian(ylim = c(-10, casosTotalAcum + 50)) +
  scale_color_gradient()


#BarPlot por contagios diarios con línea  de tendencia
ggplot(data=casos, aes(x=as.Date(Fecha, origin="1899-12-30"), y=Contagios, fill=Contagios)) +
  geom_bar(stat = "identity", position="identity") +
  ggtitle("Número de casos Covid-19 por fecha de inicio de síntomas") +
  xlab("Fecha de inicio de síntomas") + ylab("Número de casos") +
  scale_x_date(labels=date_format("%b %d"), breaks=date_breaks("2 weeks")) +
  theme(plot.title = element_text(lineheight=.8, face="bold", size = 18)) +
  theme(text = element_text(size=15)) + stat_smooth(colour="#1769a8")  +
  scale_color_gradient()
 


#BarPlot por estado
#       ->  Contagios por estado

estado <- data.frame(sort(table(d2$estado), decreasing = FALSE))
colnames(estado) <- c("Estado", "Casos")
ggplot(data=estado, aes(x=Estado, y=Casos, fill=Estado)) +
  geom_bar(stat="identity", width=0.7, color="white", fill = getPalette(length(estado$Estado))) + coord_flip() +
  geom_text(aes(label = Casos), hjust = -0.2) +
  xlab("Estado") + ylab("Número de casos") +
  scale_fill_viridis_d() +
  theme(plot.title = element_text(lineheight=.8, face="bold", size = 18)) +
  theme(text = element_text(size=15)) + theme(legend.position="none") +
  ggtitle("Número de casos Covid-19 por estado") 


#<-------------------------Cruzadas------------------------------->

#BarPlot tipo de empleado contra el campus
# -> Cruza el numero de colaboradores de cada tipo por campus
emp_camp2 <- data.frame(table(d2$tipo, d2$campus))
colnames(emp_camp2) <- c("Empleado", "Campus", "Casos")
campuss <- data.frame(table(d2$campus))
campuss <- campuss[campuss$Freq>5,]
emp_camp2 <- emp_camp2[emp_camp2$Campus %in% campuss$Var1,]

ggplot(emp_camp2, aes(fill=Empleado, y=Campus, x=Casos, label = Casos)) + 
  geom_bar(position="stack", stat="identity") +
  xlab("Número de casos") + ylab("Campus") +
  geom_text(data=subset(emp_camp2, Casos>0), size = 5, position = position_stack(vjust = 0.5), check_overlap = FALSE, colour="black", fontface = "bold") +
  scale_fill_viridis_d(begin = 0.1, end = 0.9) +
  guides(fill=guide_legend(title="Colaborador")) +
  theme(plot.title = element_text(lineheight=.8, face="bold", size = 18)) +
  theme(text = element_text(size=15)) +
  ggtitle("Número de casos Covid-19 por tipo de colaborador contra campus (total mayor que 5)") +
  scale_fill_brewer(direction = -1)

#BarPlot rango de edad contra campus
# -> Cruza el numero de colaboradores de cada rango de edad por campus
edad_camp2 <- data.frame(table(d2$rangoedad, d2$campus))
colnames(edad_camp2) <- c("Rango_Edad", "Campus", "Casos")
campus <- data.frame(table(d2$campus))
campus <- campus[campus$Freq>5,]
edad_camp2 <- edad_camp2[edad_camp2$Campus %in% campus$Var1,]
edad_camp2$Colors = ifelse(edad_camp2$Rango_Edad == "0-19", "white", "black")
ggplot(edad_camp2, aes(fill=Rango_Edad, y=Campus, x=Casos, label = Casos)) + 
  geom_bar(position="stack", stat="identity") +
  xlab("Número de casos") + ylab("Campus") +
  geom_text(data=subset(edad_camp2, Casos>0), size = 5, position = position_stack(vjust = 0.5), check_overlap = FALSE, colour="black", fontface = "bold") +
  scale_fill_viridis_d(begin = 0.1, end = 0.9) +
  guides(fill=guide_legend(title="Rango de edad")) +
  theme(plot.title = element_text(lineheight=.8, face="bold", size = 18)) +
  theme(text = element_text(size=15)) +
  ggtitle("Número de casos Covid-19 por rango de edad contra campus (total mayor que 5)") +
  scale_fill_brewer(direction = -1)

  
#BarPlot institucion contra tipo de colaborador
# -> Cruza el numero de colaboradores de cada tipo por institucion
inst_colab <- data.frame(prop.table(table(d2$tipo, d2$institucion),2))
colnames(inst_colab) <- c("Colaborador", "Institucion", "Casos")
ggplot(inst_colab, aes(fill=Colaborador, y=Institucion, x=Casos)) + 
  geom_bar(position="stack", stat="identity") +
  scale_fill_viridis_d() +
  xlab("Porcentaje de casos") + ylab("Institución") +
  theme(plot.title = element_text(lineheight=.8, face="bold", size = 18)) +
  theme(text = element_text(size=15)) +
  ggtitle("Porcentaje de casos Covid-19 por institución contra tipo de colaborador") +
  scale_fill_brewer()



#<-------------------------graficas filtradas------------------------------->

#BarPlot hospitalizados por institución
# -> Numero de colaboradores hospitalizados por cada institucion
inst_hos <- data.frame(table(d2$institucion[d2$diagnostico == "Hospitalizado"]))
colnames(inst_hos) <- c("Institucion", "Casos")
ggplot(inst_hos, aes(fill=Institucion, y=Casos, x=Institucion)) + 
  geom_bar(position="stack", stat="identity", fill = getPalette(length(inst_hos$Institucion))) + coord_flip() + 
  xlab("Institución") + ylab("Número de casos hospitalizados") +
  theme(plot.title = element_text(lineheight=.8, face="bold", size = 18)) +
  theme(text = element_text(size=15)) + theme(legend.position="none") +
  geom_text(aes(label = Casos), hjust = -0.2, size=5) +
  ggtitle("Número de casos Covid-19 hospitalizados por institución")

#BarPlot hospitalizados por campus
# -> Numero de colaboradores hospitalizados por cada campus
campus_hos <- data.frame(table(d2$campus[d2$diagnostico == "Hospitalizado"]))
colnames(campus_hos) <- c("Campus", "Casos")
ggplot(campus_hos, aes(fill=Campus, y=Casos, x=Campus)) + 
  geom_bar(position="stack", stat="identity", fill = getPalette(length(campus_hos$Campus))) + coord_flip() + 
  xlab("Campus") + ylab("Número de casos hospitalizados") +
  theme(plot.title = element_text(lineheight=.8, face="bold", size = 18)) +
  theme(text = element_text(size=15)) + theme(legend.position="none") +
  geom_text(aes(label = Casos), hjust = -0.2, size=5) +
  ggtitle("Número de casos Covid-19 hospitalizados por campus")

#Bar Plot dato generales (casos totales, %de casos hospitalizados, diferencia de con semana anterior, positividad general)
data <- data.frame(read_excel("Tasas_y_Poblacion_Tec_16072020.xlsx", sheet = "Población Tec"))

#BarPlot de porcentage de colaboradores hospitalizados, fallecidos y contagiados con respecto a la poblacion total de colabadores
general_data2 <- data.frame(
  Dato = c("Porcentaje de hospitalización", "Porcentaje de fallecimiento", "Porcentaje de contagios (total)"),
  Casos = c(dim(data.frame(d2[d2$diagnostico == "Hospitalizado",]))[1]/dim(d2)[1]*100, dim(d2[!is.na(d2$fechaFallecimiento),])[1]/dim(d2)[1]*100, dim(d2)[1]/sum(data$TOTAL.Colaboradores)*100),
  stringsAsFactors = FALSE
)

ggplot(general_data2, aes(fill=Dato, y=Casos, x=Dato)) + 
  geom_bar(position="stack", stat="identity", fill = getPalette(3)) + 
  ylab("") + xlab("") +
  theme(plot.title = element_text(lineheight=.8, face="bold", size = 18)) +
  theme(text = element_text(size=17)) + theme(legend.position="none") +
  geom_text(aes(label = paste(round(Casos,2), "%", sep="")), vjust = -0.2, size=8) +
  ggtitle("Porcentajes de datos generales Covid-19")

#BarPlot semanas con menor y mayor cantidad de contagios, semana anterior, semana actual y la diferencia entre las últimas dos
general_data3 <- data.frame(
  Semana = c("Semana min", "Semana max", "Semana anterior", "Semana actual", "Diferencia"),
  Casos = c(min(semana$Casos), max(semana$Casos), semana$Casos[dim(semana)[1]-1], semana$Casos[dim(semana)[1]], semana$Casos[dim(semana)[1]]-semana$Casos[dim(semana)[1]-1]),
  stringsAsFactors = FALSE
)
semanas <-  c("Semana min", "Semana max", "Semana anterior", "Semana actual", "Diferencia")
general_data3$Semana <- factor(general_data3$Semana, levels = semanas)

ggplot(general_data3, aes(fill=Semana, y=Casos, x=Semana)) + 
  geom_bar(position="stack", stat="identity", fill = getPalette(5)) + 
  ylab("") + xlab("") +
  theme(plot.title = element_text(lineheight=.8, face="bold", size = 18)) +
  theme(text = element_text(size=17)) + theme(legend.position="none") +
  geom_text(aes(label = Casos), vjust = ifelse(general_data3$Semana == "Diferencia", -0.2, -0.2), size=8) +
  ggtitle("Datos respecto a contagios semanales Covid-19")

#BarPlot de  casos totales en el campus e instituciones con mayor cantidad de casos, asi como los casos totales
max_campus <- data.frame(table(d2$campus))
max_campus <- max_campus[order(max_campus$Freq),]
max_inst <- data.frame(table(d2$institucion))
max_inst <- max_inst[order(max_inst$Freq),]

general_data4 <- data.frame(
  Dato = c(paste("Campus max (", max_campus$Var1[dim(max_campus)][1], ")", sep=""), paste("Institución max (", max_inst$Var1[dim(max_inst)][1], ")", sep=""), "Casos totales"),
  Casos = c(max_campus$Freq[dim(max_campus)][1], max_inst$Freq[dim(max_inst)][1], dim(d2)[1]),
  stringsAsFactors = FALSE
)
data4_order <- c(paste("Campus max (", max_campus$Var1[dim(max_campus)][1], ")", sep=""), paste("Institución max (", max_inst$Var1[dim(max_inst)][1], ")", sep=""), "Casos totales")
general_data4$Dato <- factor(general_data4$Dato, data4_order)

ggplot(general_data4, aes(fill=Dato, y=Casos, x=Dato)) + 
  geom_bar(position="stack", stat="identity", fill = getPalette(3)) + 
  ylab("") + xlab("") +
  theme(plot.title = element_text(lineheight=.8, face="bold", size = 18)) +
  theme(text = element_text(size=17)) + theme(legend.position="none") +
  geom_text(aes(label = Casos), vjust = -0.2, size=8) +
  ggtitle("Datos de casos totales Covid-19")


#<-------------------------graficas porcentuales------------------------------->

#Barplot de tasa de contagio por cada campus con respecto a su poblacion total de colaboradores
campus_comp <- d2[d2$campus %in% data$Campus,]
campus_comp <- data.frame(table(campus_comp$campus))
campus_totales <- data[data$Campus %in% campus_comp$Var1,]
campus_totales <- data.frame(campus_totales$Campus, campus_totales$TOTAL.Colaboradores)
campus_totales <- campus_totales[order(campus_totales$campus_totales.Campus),]
campus_comp$total <- campus_totales$campus_totales.TOTAL.Colaboradores
colnames(campus_comp) <- c("Campus", "Casos", "Total")

colourCount = length(campus_comp$Campus)
getPalette = colorRampPalette(brewer.pal(9, "Blues"))

ggplot(campus_comp, aes(fill=Campus, y=(Casos/Total*100), x=Campus)) + 
  geom_bar(position="stack", stat="identity", fill = getPalette(colourCount)) + 
  ylab("") + xlab("") + coord_flip() +
  xlab("Campus") + ylab("Porcentaje de tasa de contagio") +
  theme(plot.title = element_text(lineheight=.8, face="bold", size = 18)) +
  theme(text = element_text(size=17)) + theme(legend.position="none") +
  geom_text(aes(label = paste(round(Casos/Total*100,2), "%", sep = "")), hjust = -0.05, size=5) +
  ggtitle("Tasa de contagio por campus Covid-19")

#<-------------------------Tabla csv--------------------------------->
#Genera table con los casos de cada tipo de colaborador por campus
camp_colab <- table(d2$campus, d2$tipo)
#filter table
write.csv(camp_colab, file = "campus_contra_colaborador.csv")

