library(unmarked) 
library(ggplot2)
library(readxl)


detections <- read_excel("C:/.../DetectionHistory.xlsx")
umf <- unmarkedFrameOccu(y=detections)
summary(umf)

######################## Basic Model
umf <- unmarkedFrameOccu(y=detectionsMissing)

basic <- occu(~1 ~1, data = umf)
basic

basic.psi <- predict(basic, newdata=data.frame(site=1), type="state")
basic.p <- predict(basic, newdata=data.frame(site=1), type="det")
basic.psi
basic.p


basic_mb.gof.boot <- mb.gof.test(basic, nsim = 1000)
basic_mb.gof.boot

################ Grafik
outputs.basic <- data.frame(Par=c("psi", "p"),
                         estimates = c(basic.psi$Predicted, basic.p$Predicted),
                         lwr = c(basic.psi$lower, basic.p$lower),
                         upr = c(basic.psi$upper, basic.p$upper)
                        )
outputs.basic

ggplot(outputs.basic, aes(x = Par, y = estimates)) +
  geom_point(size=2) +
  geom_point(aes(x=Par, y=c(0.64,0.46)), color="orange", size = 2)+
  geom_errorbar(aes(ymin = lwr, ymax = upr), size=1, colour = "red", width=.1) +
  geom_point(size=3) +
  geom_point(aes(x=Par, y=c(0.64,0.46)), color="orange", size = 3)+
  labs(x = "", y = "Wert") +
  scale_y_continuous(limits = c(0,1), breaks=seq(0,1,0.2))


################## Kontrollgänge
f1 <- function(x){return( (1-(1-basic.p$Predicted)^x)*100)}
f2 <- function(x){return( (1-(1-basic.p$lower)^x)*100)}
f3 <- function(x){return( (1-(1-basic.p$upper)^x)*100)}

ggplot(data.frame(x=c(0,12)), aes(x=x) ) + stat_function(fun=f1) +
                                          stat_function(fun=f2, color="gray") + 
                                          stat_function(fun=f3, color="gray") +
                                          labs(x = "Anzahl Kontrollgänge", 
                                               y = "Wahrscheinlichkeit, anwesende \nWasseramseln nachzuweisen [%]") +
                                          scale_x_continuous(breaks = 0:12, minor_breaks = NULL) +
                                          scale_y_continuous(breaks = c(0,20,40,50,60,70,80,90,95,99), minor_breaks = 100, labels=c(0,20,40,50,60,70,80,90,95,99))
                                     