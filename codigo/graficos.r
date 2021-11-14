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
Mundiales$ano = goles$ano
Mundiales = as.data.frame(Mundiales)

ggplot(Mundiales,aes(x = Asistotal/10^6, y = Goles), lwd = 2) +
  geom_point() +
  labs(title = 'Goles en funcion de los espectadores',
       subtitle = 'En todos los mundiales (1930-2018)',
       x = 'Espectadores totales (millones)',
       y = 'Goles totales')
ggsave("../figuras/dispersion.png")

regresion = lm(Goles ~ AsisTotal)

ggplot(Mundiales,aes(x = Asistotal, y = Goles), lwd = 2) +
  geom_point() +
  labs(title = 'Goles en funcion de los espectadores',
       subtitle = 'En todos los mundiales (1930-2018)',
       x = 'Espectadores totales (millones)',
        y = 'Goles Totales') +
  scale_x_continuous(breaks = c(0,10^6,2*10^6,3*10^6,4*10^6),
                     labels = c(0,"1","2","3","4")) +
  geom_abline(intercept = regresion$coefficients[1] , slope = regresion$coefficients[2], col = 2)
ggsave("../figuras/regresion.png")


ggplot(Mundiales,aes(x = ano, y = Asistotal/10^6,, group=1), lwd = 2) +
  labs(title = 'Espectadores por mundial',
       subtitle = 'En todos los mundiales (1930-2018)',
       x = 'Año',
       y = 'Espectadores totales (Millones)') +
  geom_line() + 
  geom_point()

ggsave("../figuras/espectadores.png")


ggplot(Mundiales,aes(x = ano, y = Goles,, group=1), lwd = 2) +
  labs(title = 'Goles por mundial',
       subtitle = 'En todos los mundiales (1930-2018)',
       x = 'Año',
       y = 'Goles totales') +
  geom_line() + 
  geom_point()

ggsave("../figuras/goles.png")
