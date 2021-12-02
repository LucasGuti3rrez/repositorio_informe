library(tidyverse)
library(ggplot2)

espectadores = read.csv("../datos/espectadores.csv")
goles = read.csv("../datos/goles.csv")

Goles = goles$Goles
Asistotal = espectadores$Asistencia.total
Mundiales = NULL
Mundiales$Sede = espectadores$Sede
Mundiales$Asistotal = Asistotal
Mundiales$Goles = goles$Goles
Mundiales$año = goles$año
Mundiales = as.data.frame(Mundiales)

ggplot(Mundiales,aes(x = Asistotal/10^6, y = Goles), lwd = 2) +
  geom_point() +
  labs(title = 'Goles en funcion de los espectadores',
       subtitle = 'En todos los mundiales (1930-2018)',
       x = 'Espectadores totales (millones)',
       y = 'Goles totales')
ggsave("../figuras/dispersion.png")

regresion = lm(Goles[-c(17)] ~ AsisTotal[-c(17)])

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

Mundiales$año

años_reales = seq(1930,2018,4)

ggplot(Mundiales,aes(x = año, y = Asistotal/10^6, group=1), lwd = 2) +
  labs(title = 'Espectadores por mundial',
       subtitle = 'En todos los mundiales (1930-2018)',
       x = 'Año',
       y = 'Espectadores totales (Millones)') +
  scale_x_continuous(breaks = años_reales,
                     labels = años_reales) +
  geom_line() + 
  geom_point()


ggsave("../figuras/espectadores.png")


ggplot(Mundiales,aes(x = año, y = Goles, group=1), lwd = 2) +
  labs(title = 'Goles por mundial',
       subtitle = 'En todos los mundiales (1930-2018)',
       x = 'Año',
       y = 'Goles totales') +
  scale_x_continuous(breaks = años_reales,
                     labels = años_reales) +
  geom_line() + 
  geom_point()

ggsave("../figuras/goles.png")



