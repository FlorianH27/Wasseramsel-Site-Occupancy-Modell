library(unmarked) 
library(ggplot2)
library(readxl)
library(AICcmodavg)


detections <- read_excel("C:/.../DetectionHistory.xlsx")
nb_cov <- read_excel("C:/.../NeighbourCovs.xlsx")
umf_neig <- unmarkedFrameOccu(y=detections, siteCovs = nb_cov)

occu_neig <- occu(~1 ~Neighbour, data = umf_neig)

preds <- data.frame(Neighbour=c(0,1,2))
occu_neig_psi_predict <- predict(occu_neig, type="state", newdata=preds, appendData=TRUE)
occu_neig_psi_predict

ggplot(occu_neig_psi_predict, aes(x = Neighbour, y = Predicted)) +
  geom_point(size=2) +
  geom_errorbar(aes(ymin = lower, ymax = upper), colour = "black", width=.1) +
  labs(x = "Anzahl besetzter Nachbarstandorte", y = "Occupancy (psi)") +
  scale_x_continuous(breaks=c(0,1,2), minor_breaks = c(), expand=c(0.2,0)) +
  scale_y_continuous(limits=c(0,1), breaks=seq(from=0, to=1,by=0.2), minor_breaks = seq(from=0.1,to=0.9,by=0.2))

# Sensitivitätsanalyse
mb.gof <- mb.gof.test(occu_neig, nsim = 1000)
mb.gof

