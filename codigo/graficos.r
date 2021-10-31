library(tidyverse)
library(ggplot2)

espectadores = read_delim("../datos/espectadores.csv")
goles = read_delim("../datos/goles.csv")


Goles = goles$Goles
AsisTotal = espectadores$`Asistencia total`
Mundiales = NULL
Mundiales$Sede = espectadores$Sede
Mundiales$Asistotal = espectadores$`Asistencia total`
Mundiales$`Media de asistencia por partido` = espectadores$`Media de asistencia por partido`
Mundiales$Goles = goles$Goles
Mundiales = as.data.frame(Mundiales)

ggplot(Mundiales,aes(x = Asistotal/1000, y = Goles), lwd = 2) +
  geom_point() +
  labs(title = 'Goles en funcion de la Asistencia',
       x = 'Asistencia Total (miles)',
       y = 'Goles')
ggsave("../figuras/dispersion.png")

regresion = lm(Goles ~ AsisTotal)

ggplot(Mundiales,aes(x = Asistotal, y = Goles), lwd = 2) +
  geom_point() +
  labs(title = 'Goles en funcion de la Asistencia',
       x = 'Asistencia Total',
        y = 'Goles') +
  geom_abline(intercept = regresion$coefficients[1] , slope = regresion$coefficients[2], col = 2)
ggsave("../figuras/regresion.png")

