library(ggplot2)
library(patchwork)


#import der daten in dataframe
DataDiode <- read.table("data/Diode.dat", sep = "\t", dec = ",", header = TRUE) #einlesen der Daten

head(DataDiode) # schaue mir den Data frame an
str(DataDiode) # schaue mir die Struktur der Daten an

# wahl des Arbeitspunktes: I = 0.2 mA, U = 0.6 V


# ich geb auf ^^