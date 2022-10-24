library(unmarked) 
library(ggplot2)
library(readxl)
library(AICcmodavg)
library(MuMIn)

detections <- read_excel("C:/.../DetectionHistory.xlsx")
minuten_cov <- read_excel("C:/.../MinutenCovs.xlsx")
monat_cov <- read_excel("C:/.../MonatCovs.xlsx")
datum_cov <- read_excel("C:/.../DatumCovs.xlsx")
monat_minuten_cov_pred <- read_excel("C:/.../MonatMinutenCovs.xlsx")

################### Alle Modelle

obs_covs <- list(minuten=minuten_cov, monat=monat_cov, datum=datum_cov)
umf <- unmarkedFrameOccu(y = detections, obsCovs = obs_covs)

det_minuten <- occu(~minuten ~1, data = umf)
det_monat <- occu(~monat ~1, data = umf)
det_datum <- occu(~datum ~1, data = umf)
det_min_dat <- occu(~minuten+datum ~1, data = umf)
det_min_mon <- occu(~minuten+monat ~1, data = umf)
basic <- occu(~1 ~1, data=umf)


################## Vergleichsliste

fl <- fitList("p(.)psi(.)"=basic,
              "p(minuten)"=det_minuten,
              "p(monat)"=det_monat,
              "p(datum)"=det_datum,
              "p(minuten+datum)"=det_min_dat,
              "p(minuten+monat)"=det_min_mon)
ms <- modSel(fl)
ms

det_best_list <- list(det1 = det_min_mon,
                      det2 = det_minuten)
det_p_predict_ls <- modavgPred(det_best_list,
                            parm.type = "detect",
                            newdata = data.frame(monat_minuten_cov_pred))[c("mod.avg.pred","lower.CL","upper.CL")]
det_p_predict_df <- data.frame(Predicted = det_p_predict_ls$mod.avg.pred,
                                      lower = det_p_predict_ls$lower.CL,
                                      upper = det_p_predict_ls$upper.CL,
                                      data.frame(monat_minuten_cov_pred))                                                                     
options(max.print=2000)  # optional um alle Zeilen anzuzeigen
det_p_predict_df

# Sensitivitätsanalyse
m1_mb.gof <- mb.gof.test(det_min_mon, nsim = 1000)
m1_mb.gof

m2_mb.gof <- mb.gof.test(det_minuten, nsim = 1000)
m2_mb.gof



######################################  Plot Minuten in Average-Modell

p_minuten_newdata <- data.frame(minuten=seq(from=-20,to=210,by=1),
                                monat = "Mai") ### Mai fast wie Mittelwert


p_minuten_pred <- modavgPred(det_best_list,
                             parm.type = "detect",
                             newdata = p_minuten_newdata)[c("mod.avg.pred",
                                                            "lower.CL",
                                                            "upper.CL")]

p_minuten_pred_df <- data.frame(Predicted = p_minuten_pred$mod.avg.pred,
                                lower = p_minuten_pred$lower.CL,
                                upper = p_minuten_pred$upper.CL,
                                p_minuten_newdata)

ggplot(data = p_minuten_pred_df, aes(x = minuten, y = Predicted)) +
  theme_classic() +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "gray") +
  stat_smooth(method = "loess", col = "black", se = FALSE) +
  labs(x = "Minuten nach Sonnenaufgang", y = "Detection Probability (p)") +
  scale_x_continuous(limits=c(-20,210), expand=(c(0,0,-0.01,0)), breaks = seq(from=-20, to=200,by=20), labels = seq(from=-20, to=200,by=20) )+
  scale_y_continuous(limits=c(0,1), expand=(c(0,0,0.01,0)), breaks = seq(from=0, to=1,by=0.1))



######################################  Plot Monat in Average-Modell

p_monat_newdata <- data.frame(monat=c("Feb", "Mär", "Apr", "Mai", "Jun"),
                              minuten = 72)

p_monat_pred <- modavgPred(det_best_list,
                           parm.type = "detect",
                           newdata = p_monat_newdata)[c("mod.avg.pred",
                                                        "lower.CL",
                                                        "upper.CL")]

p_monat_pred_df <- data.frame(Predicted = p_monat_pred$mod.avg.pred,
                              lower = p_monat_pred$lower.CL,
                              upper = p_monat_pred$upper.CL,
                              p_monat_newdata)

ggplot(p_monat_pred_df, aes(x = monat, y = Predicted)) +
  geom_point(size=2) +
  geom_errorbar(aes(ymin = lower, ymax = upper), colour = "black", width=.1) +
  labs(x = "Monat", y = "Detection Probability (p)") +
  xlim(c("Feb", "Mär", "Apr", "Mai", "Jun")) +
  scale_y_continuous(limits=c(0,1), breaks=seq(from=0, to=1,by=0.2), minor_breaks = seq(from=0.1,to=0.9,by=0.2))

