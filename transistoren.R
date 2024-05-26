#importiere notwendige Bibliotheken
library(ggplot2)


#import der daten in dataframe
biTrans <- read.table("data/Bipolartransistor.dat", sep = "\t", header = TRUE)

#erstelle U_CE da nicht in Daten vorhanden
biTrans$U_CE <- seq(from = 0, to = 5, by = 0.01)

# Wahl des Arbeitpunktes: I_b = 0.2 mA, U_CE = 2 V
I_b_AP = 0.2
U_CE_AP = 2


#Eingangskennlinienfeld

#ermittelung von U_be im Arbeitspunkt
U_beAP <- subset(biTrans, I.be.mA. == 0.2)$U.be..V.

# Umgebung des Arbeitspunkts definieren (+/- 0.05 mA)
biTrans_Eingang <- biTrans[biTrans$I.be.mA. >= (I_b_AP - 0.05) & biTrans$I.be.mA. <= (I_b_AP + 0.05), ]

# Linearen Fit in der Umgebung des Arbeitspunkts durchführen
fit11 <- lm(I.be.mA. ~ U.be..V. , data = biTrans_Eingang)
#koeffizienten des Fits ermitteln
coefficients11 <- summary(fit11)$coefficients
slope11 <- coefficients11[2, 1]
intercept11 <- coefficients11[1, 1]
slope11_err <- coefficients11[2, 2]
intercept11_err <- coefficients11[1, 2]

#Fitgerade für den Plot erstellen
tangente11 <- data.frame(U = c(U_beAP - 0.05, U_beAP + 0.05),
                          I = c(slope11*(U_beAP - 0.05)+intercept11, slope11*(U_beAP + 0.05)+intercept11))
#Plot es Eingangskennlinienfelds
plot_EingangsKF <- ggplot(biTrans, aes(x = U.be..V.)) +
  theme_bw() +
  geom_line(aes(y = I.be.mA.), color = "darkblue", linewidth = 1.3) +
  geom_line(data = tangente11, aes(x = U, y = I),
            color = "darkorange", linetype = "dashed", size = 1) +
  geom_point(aes(x = U_beAP, y = I_b_AP), color = "red", size = 3) +
  annotate("label", x = 0.3, y = 0.3, size = 6,
          label = paste("U_BE = a*I_B+b\na = ", round(slope11, 3),
          " ± ", round(slope11_err, 3), "\n b = ", round(intercept11, 3),
          " ± ", round(intercept11_err, 3))) +
  labs(title = "Eingangkennlinienfeld",
       x = "U_BE",
       y = "I_B") +
  xlim(0, 0.8) +
  theme(plot.title = element_text(hjust = 0.5), text = element_text(size = 16))
plot_EingangsKF
#Rotation für späteren Plot im Vierquadrantenkennlinienfeld
plot_flip_EingangsKF <- plot_EingangsKF +
  coord_flip() +
  scale_y_reverse() +
  scale_x_reverse()
plot_flip_EingangsKF



#Ausgangskennlinienfeld
#ermittelung von I_C im Arbeitspunkt
I_C_AP <- subset(biTrans, U_CE == 2)$I.Ib.200E.3

#Umgebung
biTrans_22 <- biTrans[biTrans$U_CE >= (U_CE_AP - 0.5) & biTrans$U_CE <= (U_CE_AP + 0.5), ]
# Linearen Fit in der Umgebung des Arbeitspunkts durchführen
fit22 <- lm(I.Ib.200E.3 ~ U_CE , data = biTrans_22)
#koeffizienten des Fits ermitteln
coefficients22 <- summary(fit22)$coefficients
slope22 <- coefficients22[2, 1]
intercept22 <- coefficients22[1, 1]
slope22_err <- coefficients22[2, 2]
intercept22_err <- coefficients22[1, 2]

#tangente berechnen
tangente22 <- data.frame(U = c(U_CE_AP - 1.5, U_CE_AP + 2.5),
                          I = c(slope22*(U_CE_AP - 1.5)+intercept22, slope22*(U_CE_AP + 2.5)+intercept22))


#Plot es Ausgangskennlinienfeld
plot_AusgangsKF <- ggplot(biTrans, aes(x = U_CE)) +
  theme_bw() +
  geom_line(aes(y = I.Ib.100E.3), color = "darkblue",  linewidth = 1.3) +
  geom_line(aes(y = I.Ib.200E.3), color = "darkblue",  linewidth = 1.3) +
  geom_line(aes(y = I.Ib.300E.3), color = "darkblue", linewidth = 1.3) +
  geom_line(aes(y = I.Ib.400E.3), color = "darkblue",  linewidth = 1.3) +
  geom_line(aes(y = I.Ib.500E.3), color = "darkblue", linewidth = 1.3) +
  annotate("label", x = 5.5, y =  biTrans$I.Ib.100E.3[500], size = 5, label = "100 mA") +
  annotate("label", x = 5.5, y =  biTrans$I.Ib.200E.3[500], size = 5, label = "200 mA") +
  annotate("label", x = 5.5, y =  biTrans$I.Ib.300E.3[500], size = 5, label = "300 mA") +
  annotate("label", x = 5.5, y =  biTrans$I.Ib.400E.3[500], size = 5, label = "400 mA") +
  annotate("label", x = 5.5, y =  biTrans$I.Ib.500E.3[500], size = 5, label = "500 mA") +
  annotate("label", x = 5.5, y =  100, size = 5, label = expression(I[B])) +
  annotate("label", x = 2, y = 010, size = 6,
          label = paste("I_CE = a*U_CE+b\na = ", round(slope22, 3),
          " ± ", round(slope22_err, 3), "\n b = ", round(intercept22, 3),
          " ± ", round(intercept22_err, 3))) +
  geom_point(aes(x = U_CE_AP, y = I_C_AP), color = "red", size = 3) +
  geom_line(data = tangente22, aes(x = U, y = I),
            color = "darkorange", linetype = "dashed", size = 1) +
  labs(title = "Ausgangskennlinienfeld",
       x = "U_CE",
       y = "I_C") +
  xlim(0, 6) +
  ylim(0, 100) +
  theme(plot.title = element_text(hjust = 0.5), text = element_text(size = 16))
plot_AusgangsKF


#Stromverstärkungslennlinienfeld
#IC Arbeitspunkt
I_C2_AP <- subset(biTrans, I.be.mA. == 0.2)$I.ce.mA.

#Umgebung um AP
biTrans_21 <- biTrans[biTrans$I.be.mA. >= (I_b_AP - 0.01) & biTrans$I.be.mA. <= (I_b_AP + 0.01), ]

# Linearen Fit in der Umgebung des Arbeitspunkts durchführen
fit21 <- lm(I.ce.mA. ~ I.be.mA., data = biTrans_21)
#koeffizienten des Fits ermitteln
coefficients21 <- summary(fit21)$coefficients
slope21 <- coefficients21[2, 1]
intercept21 <- coefficients21[1, 1]
slope21_err <- coefficients21[2, 2]
intercept21_err <- coefficients21[1, 2]

#tangente berechnen
tangente21 <- data.frame(IB = c(I_b_AP - 0.15, I_b_AP + 0.15),
                        IC = c(slope21*(I_b_AP - 0.15)+intercept21, slope21*(I_b_AP + 0.15)+intercept21))

#Plot es Stromverstärkungslennlinienfeld
plot_StromKF <- ggplot(biTrans, aes(x = I.be.mA.)) +
  theme_bw() +
  geom_line(aes(y = I.ce.mA.), color = "darkblue", linewidth = 1.3) +
  geom_point(aes(x = I_b_AP, y = I_C2_AP), color = "red", size = 3) +
  geom_line(data = tangente21, aes(x = IB, y = IC),
            color = "darkorange", linetype = "dashed", size = 1) +
  annotate("label", x = 0.35, y = 20, size = 6,
          label = paste("I_C = a*I_B+b\na = ", round(slope21, 3),
          " ± ", round(slope21_err, 3), "\n b = ", round(intercept21, 3),
          " ± ", round(intercept21_err, 3))) +
  labs(title = "Stromverstärkungslennlinienfeld",
       x = "I_B",
       y = "I_C") +
  theme(plot.title = element_text(hjust = 0.5), text = element_text(size = 16))
plot_StromKF

#drehe für Viererkennlinie
plot_flip_StromKF <- plot_StromKF +
  scale_x_reverse()

plot_flip_StromKF


#Spannungsrückwirkungskennlinienfeld

#ermittelung von U_BE im Arbeitspunkt
U_BE2_AP <- subset(biTrans, U_CE == 2)$Ube.Ib.200E.3

#Umgebung
biTrans_12 <- biTrans[biTrans$U_CE >= (U_CE_AP - 0.5) & biTrans$U_CE <= (U_CE_AP + 0.5), ]
# Linearen Fit in der Umgebung des Arbeitspunkts durchführen
fit12 <- lm(Ube.Ib.200E.3 ~ U_CE , data = biTrans_12)
#koeffizienten des Fits ermitteln
coefficients12 <- summary(fit12)$coefficients
slope12 <- coefficients12[2, 1]
intercept12 <- coefficients12[1, 1]
slope12_err <- coefficients12[2, 2]
intercept12_err <- coefficients12[1, 2]

#tangente berechnen
tangente12 <- data.frame(UCE = c(U_CE_AP - 2, U_CE_AP + 2.5),
                          UBE = c(slope12*(U_CE_AP - 2)+intercept12, slope12*(U_CE_AP + 2.5)+intercept12))


#plot des Spannungsrückwirkungskennlinienfeld
plot_SpannungKF <- ggplot(biTrans, aes(x = U_CE)) +
  theme_bw() +
  geom_line(aes(y = 0.01*Ube.Ib.100E.3), color = "darkblue",  linewidth = 1.3) +
  geom_line(aes(y = 0.01*Ube.Ib.200E.3), color = "darkblue",  linewidth = 1.3) +
  geom_line(aes(y = 0.01*Ube.Ib.300E.3), color = "darkblue", linewidth = 1.3) +
  geom_line(aes(y = 0.01*Ube.Ib.400E.3), color = "darkblue",  linewidth = 1.3) +
  geom_line(aes(y = 0.01*Ube.Ib.500E.3), color = "darkblue", linewidth = 1.3) +
  annotate("label", x = 5.5, y =  0.56, size = 5, label = "100 mA") +
  annotate("label", x = 5.5, y =  0.62, size = 5, label = "200 mA") +
  annotate("label", x = 5.5, y =  0.68, size = 5, label = "300 mA") +
  annotate("label", x = 5.5, y =  0.74, size = 5, label = "400 mA") +
  annotate("label", x = 5.5, y =  0.8, size = 5, label = "500 mA") +
  annotate("label", x = 5.5, y =  0.48, size = 5, label = expression(I[B])) +
  annotate("label", x = 3, y = 0.4 , size = 6,
          label = paste("U_BE = a*U_CE+b\na = ", round(slope12, 3),
          " ± ", round(slope12_err, 3), "\n b = ", round(0.01*intercept12, 3),
          " ± ", round(0.01*intercept12_err, 3))) +
  geom_point(aes(x = U_CE_AP, y = 0.01*U_BE2_AP), color = "red", size = 3) +
  geom_line(data = tangente12, aes(x = UCE, y = 0.01*UBE),
            color = "darkorange", linetype = "dashed", size = 1) +
  labs(title = "Spannungsrückwirkungskennlinienfeld",
       x = "U_CE",
       y = "U_BE") +
  xlim(0, 6) +
  ylim(0, 0.8) +
  theme(plot.title = element_text(hjust = 0.5), text = element_text(size = 16))
plot_SpannungKF


plot_flip_SpannungKF <- plot_SpannungKF +
  scale_y_reverse()

plot_flip_SpannungKF
