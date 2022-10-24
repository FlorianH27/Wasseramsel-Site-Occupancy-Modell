library(unmarked) 
library(ggplot2)
library(readxl)
library(AICcmodavg)
library(MuMIn)


detections <- read_excel("C:/.../DetectionHistory.xlsx")
hab_cov <- read_excel("C:/.../BachstrukturenCovs.xlsx")
umf_habitat <- unmarkedFrameOccu(y=detections, siteCovs = hab_cov)


# alle Bachstrukturen
occu_hab <- occu(~1 ~VBachbreite + VWassertiefe + Turbulenzen + Steine + Bachsohle + Ufer + Uferbewuchs + Weg, data = umf_habitat)
occu_hab

occu_hab_dredge <- dredge(global.model = occu_hab, rank = "AIC")
occu_hab_dredge[1:20]

# nur biologisch sinnvolle Bachstrukturen
occu_hab_bio <- occu(~1 ~Turbulenzen + Bachsohle + Ufer + Weg, data = umf_habitat)
occu_hab_bio

occu_hab_bio_dredge <- dredge(global.model = occu_hab, rank = "AIC")
occu_hab_bio_dredge[1:4]


######################## Theoretisch könnte man beste vier Modelle mitteln
occu_hab_bio1 <- occu(~1 ~Ufer, data = UFO_habitat)
occu_hab_bio2 <- occu(~1 ~Ufer + Bachsohle, data = UFO_habitat)
occu_hab_bio3 <- occu(~1 ~Ufer + Weg, data = UFO_habitat)
occu_hab_bio4 <- occu(~1 ~Ufer + Turbulenzen, data = UFO_habitat)

occu_hab_list <- list(occu1 = occu_hab_bio1,
                      occu2 = occu_hab_bio2,
                      occu3 = occu_hab_bio3,
                      occu4 = occu_hab_bio4)

hab_cov_best <- data.frame("Ufer" = hab_cov$Ufer, 
                           "Bachsohle" = hab_cov$Bachsohle,
                           "Weg" = hab_cov$Weg, 
                           "Turbulenzen" = hab_cov$Turbulenzen)
occu_hab_psi_predict <- modavgPred(occu_hab_list,
                                      parm.type = "psi",
                                      newdata = data.frame(hab_cov_best))[c("mod.avg.pred",
                                                                                "lower.CL",
                                                                                "upper.CL")]
occu_hab_psi_predict_df <- data.frame(Predicted = occu_hab_psi_predict$mod.avg.pred,
                                         lower = occu_hab_psi_predict$lower.CL,
                                         upper = occu_hab_psi_predict$upper.CL,
                                         data.frame(hab_best_cov))

# Beispiel eine der vier Bachstrukturen im Average-Modell zu plotten (Turbulenzen)
occu_Turbulenzen_newdata <- data.frame(Turbulenzen = seq(min(hab_cov_best$Turbulenzen), 
                                                         max(hab_cov_best$Turbulenzen), by = 1),
                                       Ufer = mean(hab_cov_best$Ufer), 
                                       Weg = mean(hab_cov_best$Weg), 
                                       Bachsohle = mean(hab_cov_best$Bachsohle))


occu_Turbulenzen_pred <- modavgPred(occu_hab_list,
                                    parm.type = "psi",
                                    newdata = occu_Turbulenzen_newdata)[c("mod.avg.pred",
                                                                          "lower.CL",
                                                                          "upper.CL")]

occu_Turbulenzen_pred_df <- data.frame(Predicted = occu_Turbulenzen_pred$mod.avg.pred,
                                       lower = occu_Turbulenzen_pred$lower.CL,
                                       upper = occu_Turbulenzen_pred$upper.CL,
                                       occu_Turbulenzen_newdata)

ggplot(occu_Turbulenzen_pred_df, aes(x = Turbulenzen, y = Predicted)) +
  geom_point(size=2) +
  geom_errorbar(aes(ymin = lower, ymax = upper), colour = "black", width=.1) +
  labs(x = "Stärke der Turbulenzen (kategorisiert)", y = "Occupancy (psi)") +
  scale_x_continuous(breaks=c(1,2,3), minor_breaks = c(), expand=c(0.3,0)) +
  scale_y_continuous(limits=c(0,1), breaks=seq(from=0, to=1,by=0.2), minor_breaks = seq(from=0.1,to=0.9,by=0.2))



########## Man kann aber auch nur die Naturnähe des Ufers auswählen statt die Modelle zu mitteln
occu_ufer_pred <- predict(occu_hab_bio1, newdata=data.frame("Ufer"=hab_cov_best$Ufer), appendData=TRUE, type="state")

ggplot(occu_ufer_pred_df, aes(x = Ufer, y = Predicted)) +
  geom_point(size=2) +
  geom_errorbar(aes(ymin = lower, ymax = upper), colour = "black", width=.1) +
  labs(x = "Naturnähe des Ufers (kategorisiert)", y = "Occupancy (psi)") +
  scale_x_continuous(breaks=c(1,2,3), minor_breaks = c(), expand=c(0.3,0)) +
  scale_y_continuous(limits=c(0,1), breaks=seq(from=0, to=1,by=0.2), minor_breaks = seq(from=0.1,to=0.9,by=0.2))

# Sensitivitätsanalyse
mb.gof.boot <- mb.gof.test(occu_hab_bio1, nsim = 1000)
mb.gof.boot

# Karte
coords <- read_excel("C:/.../Koordinaten.xlsx")
occu_ufer_co <- cbind(occu_ufer_pred, coords)

ggplot(data = occu_ufer_co, aes(x = x, y = y, fill = Predicted)) +
  geom_point(size = 4, pch = 21, col="white") +
  geom_point(size = 1) +
  theme_classic() +
  scale_x_continuous(breaks = seq(from = 2690000, to=2710000, by=2000), expand = c(0.1,0.1)) +
  scale_y_continuous(limits=c(1242000, 1245000),  breaks = seq(from = 1240000, to=1245000, by=1000), expand = c(0.1,0.1)) +
  scale_fill_gradient(low = "red", high = "green", limits=c(0,1), breaks=c(0,0.22,0.59,0.88,1), name="psi")



