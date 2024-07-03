library(pacman)
pacman::p_load(dplyr, sjmisc, car, sjlabelled, stargazer, haven, texreg, ggplot2, corrplot)

load("INPUT/proc_data.RData")

head(proc_data)
##se omitne los na
proc_data <- na.omit(proc_data)

proc_data <- proc_data %>%
  mutate(across(starts_with("o"), ~ recode(., "c(-2,-1)=NA; 1=3; 2=2; 3=1; 4=0"))) %>%
  set_na(na = c(-2, -1)) %>%
  rename(busc_trab = o6, razon = o7) %>%
  mutate_at(vars(busc_trab, razon), set_labels, labels = c("Buscó trabajo en el último tiempo" = 1, "No buscó trabajo en el último tiempo" = 2)) %>%
  mutate_at(vars(razon), set_labels, labels = c("Está en espera de una confirmación" = 1:2, "Se dedica a labores de cuidado" = 3:5, "Se dedica al trabajo doméstico" = 10, "Dificultades para conseguir trabajo" = 6:9, "Otro motivo" = 11:19)) %>%
  mutate(edad = cut(edad, c(18, 25, 35, 45, 55, Inf), labels = c("18-24 años", "25-34 años", "35-44 años", "45-54 años", "55 o más años"), include.lowest = TRUE)) %>%
  mutate(sexo = set_labels(sexo, labels = c("Hombre" = 1, "Mujer" = 2)))


proc_data$razon <- set_labels(proc_data$razon,
                              labels=c( "Se dedica a labores de cuidado"=3,4,5,7,8,9,
                                        "Se dedica al trabajo doméstico"=10,
                                        "Está en espera de una confirmación"=1,2,
                                        "Dificultades para conseguir trabajo"=6,12,13,14,
                                        "Otro motivo"=11,15,16,17,18,19))

proc_data <- rename(proc_data, salario = y1)

##regresiones lineales simples
reg1 <- lm(razon ~ sexo, data=proc_data)

knitreg(list(reg1))

reg2 <-  lm(salario ~ sexo, data=proc_data)

knitreg(list(reg2))

reg3 <- lm(busc_trab ~ sexo, data=proc_data)
knitreg(list(reg3))

##regresión múltiple

reg4 <- lm(busc_trab ~ razon + sexo, data=proc_data)
reg5 <- lm(sexo ~ salario + edad, data=proc_data)

knitreg(list(reg5))

knitreg(list(reg4, reg5), 
        custom.model.names = c("Modelo 1",
                               "Modelo 2"),
        custom.note = "*** p < 0.001; ** p < 0.01; * p < 0.05",
        custom.coef.names = c("Intercepto", 
                              "razon",
                              "Mujer <br> <i>(Ref. Hombre)</i>",
                              "salario",
                              "edad25-34 años",
                              "edad35-44 años",
                              "edad45-54 años",
                              "edad55 o más años"),
        caption = "Razones por para la búsqueda de trabajo según edad y sexo",
        caption.above = TRUE)

corrplot.mixed(cor(select(proc_data,  busc_trab, edad, sexo),
                   use = "complete.obs"))

save(proc_data, file="proc_data.RData")

##PRESENTACIÓN QUARTO
graph3 <- proc_data %>% 
  ggplot(aes(x = busc_trab, fill = sexo)) +    
  geom_bar() +  
  xlab("Búsqueda de trabajo") +
  ylab("Cantidad") +
  labs(fill = "Sexo") +
  scale_fill_discrete(labels = c('Hombre', 'Mujer'))

graph3

ruta_grafico <- file.path("output", "graph3.png")


png(filename = ruta_grafico)

