library(tidyverse)

espectadores = read_delim("espectadores.csv")
goles = read_delim("goles.csv")


Mundiales = NULL
Mundiales$Sede = espectadores$Sede
Mundiales$`Asistencia total` = espectadores$`Asistencia total`
Mundiales$`Media de asistencia por partido` = espectadores$`Media de asistencia por partido`
Mundiales$Goles = goles$Goles

AsisTotal = Mundiales$`Asistencia total`
AsisMedia = Mundiales$`Media de asistencia por partido`
Goles = Mundiales$Goles

regresion = lm(Goles ~ AsisTotal)
summary(regresion)

plot(AsisTotal, Goles, main = 'Goles en funcion de la Asistencia',xlab = "Asistencia Total", ylab = "Goles", lwd = 2)
abline(regresion, col = 4, lwd = 2)

newx <- seq(min(AsisTotal), max(AsisTotal), by = 100000) 

#Intervalo de confianza recta media
(conf.int.2 <- predict(regresion, newdata = data.frame(AsisTotal = newx), 
                       interval = c("confidence"), level = 0.95))

lines(newx, conf.int.2[ ,2],col = "red", lty = 2, lwd = 2)
lines(newx, conf.int.2[ ,3],col = "red", lty = 2, lwd = 2)


#Intervalo de confianza prediccion
(pred.int <- predict(regresion, newdata = data.frame(AsisTotal = newx), 
                     interval = "prediction", level = 0.95))

lines(newx, pred.int[,2], col = "orange", lty = 2, lwd = 2)
lines(newx, pred.int[,3], col = "orange", lty = 2, lwd = 2)



### Gráfico Residuos vs Valores ajustados
plot(fitted(regresion), resid(regresion),
     xlab = "Valores ajustados",
     ylab = "Residuos",
     main = "Residuos vs Valores ajustados",
     lwd = 2 )
abline(h = 0, lwd = 2)

### Gráfico Residuos vs Predictor
plot(AsisTotal, resid(regresion),
     xlab = "Asistencia total",
     ylab = "Residuos",
     main = "Residuos vs Predictor",
     lwd = 2)
abline(h = 0, lwd = 2)