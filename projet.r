library(readr)
library(ggplot2)



data <- read.csv("open_stats_coronavirus.csv",sep = ";")

etude <- subset(data, data$nom == "maroc")

#data frame, median, moyen, ecart-type, max and min of Dates
dates <- data.frame(date = as.Date(etude$date))
moyDate <- mean.Date(dates$date, na.rm = TRUE)
medDate <- median(dates$date, na.rm = TRUE)
ecartDate <- sd(dates$date, na.rm = TRUE)
minDate <- min(dates$date, na.rm = TRUE)
maxDate <- max(dates$date, na.rm = TRUE)

#data frame, median, moyen, ecart-type, max and min of Cas
cas <- data.frame(cas = etude$cas)
moyCas <- mean(cas$cas,na.rm = TRUE)
medCas <- median(cas$cas, na.rm = TRUE)
ecartCas <- sd(cas$cas, na.rm = TRUE)
minCas <- min(cas$cas, na.rm = TRUE)
maxCas <- max(cas$cas, na.rm = TRUE)

#data frame, median, moyen, ecart-type, max and min of Deces
deces <- data.frame(deces = etude$deces)
moyDeces <- mean(deces$deces,na.rm = TRUE)
medDeces <- median(deces$deces, na.rm = TRUE)
ecartDeces <- sd(deces$deces, na.rm = TRUE)
minDeces <- min(deces$deces, na.rm = TRUE)
maxDeces <- max(deces$deces, na.rm = TRUE)

#data frame, median, moyen, ecart-type, max and min of guerisons
guerisons <- data.frame(guerison = etude$guerisons)
moyGuerisons <- mean(guerisons$guerison,na.rm = TRUE)
medGuerisons <- median(guerisons$guerison, na.rm = TRUE)
ecartGuerisons <- sd(guerisons$guerison, na.rm = TRUE)
minGuerisons <- min(guerisons$guerison, na.rm = TRUE)
maxGuerisons <- max(guerisons$guerison, na.rm = TRUE)

#data frame of each statistic variable we have
moyennes <- data.frame(date = moyDate, cas = moyCas, deces = moyDeces, guerisons = moyGuerisons)
medians <- data.frame(date = medDate, cas = medCas, deces = medDeces, guerisons = medGuerisons)
ecartTyes <- data.frame(date = ecartDate, cas = ecartCas, deces = ecartDeces, guerisons = ecartGuerisons)
maxs <- data.frame(date = maxDate, cas = maxCas, deces = maxDeces, guerisons = maxGuerisons)
mins <- data.frame(date = minDate, cas = minCas, deces = minDeces, guerisons = minGuerisons)


# Calcul de la corrélation linéaire entre les cas et les décès
correlationCasDeces <- cor(etude$cas, etude$deces,use = "pairwise.complete.obs")
print("Corrélation cas deces  :")
print(correlation)
# Calcul de la corrélation linéaire entre les cas et les guerissons
correlationCasGuerrissons <- cor(etude$cas, etude$guerisons,use = "pairwise.complete.obs")
print("Corrélation cas guerissons:")
print(correlationCasGuerrissons)
# Calcul de la corrélation linéaire entre les guerissons  et les décès
correlationDecesGuerissons <- cor(etude$deces, etude$guerisons,use = "pairwise.complete.obs")
print("Corrélation deces guerisons :")
print(correlationDecesGuerissons)



# Régression linéaire (cas ~ décès)
regression <- lm(cas ~ deces, data = etude)
print(regression)

# Visualisation de la régression linéaire
ggplot(etude, aes(x = deces, y = cas)) +
  geom_point(size = 5, shape = 1, color = "black") +
  labs(title = "Répartition des cas au Maroc en fonction des décès") +
  xlab("Décès") +
  ylab("Cas") +
  geom_smooth(method = lm)