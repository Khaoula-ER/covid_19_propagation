#' ********************************************************************************
#' @title 	            data visualisation.R
#' @author              Khaoula Aroui
#' @description
#' xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
#' ********************************************************************************

#Vider l'environement ----
rm(list = ls())
# set the working directory ----
setwd(
  "C:/Users/khaou/Documents/Master SEP/5- Analyse des données et data mining_GUATHERAT/projet individuel"
)
# Librairies requises ----
if (!require(dplyr)) {
  install.packages("dplyr")
  library(dplyr)
}
if (!require(plotly)) {
  install.packages("plotly")
  library(plotly)
}
if (!require(lubridate)) {
  install.packages("lubridate")
  library(lubridate)
}

# sourced scripts ----
source(paste(getwd(), "/PGM/analysis/Importation_donnees.R", sep = ""))

# statistiques univariée:
## taux d'incidence
### Tout le long de l'année  (4 dates)
data_jan = data_overall %>% filter(date == "2021-01-27")
data_sep = data_overall %>% filter(date == "2021-09-15")
data_boxplot = data_jan %>% select(libelle_dep, taux.d.incidence) %>%
  left_join(data_10_av %>% select(libelle_dep, taux.d.incidence),
            by = c("libelle_dep")) %>%
  left_join(data_sep %>% select(libelle_dep, taux.d.incidence),
            by = c("libelle_dep")) %>%
  left_join(data_12_dec %>% select(libelle_dep, taux.d.incidence),
            by = c("libelle_dep"))
colnames(data_boxplot) = c("Département",
                           "21 Janvier 2021",
                           "10 Avril 2021",
                           "15 Septembre 2021",
                           "12 Décembre 2021")


data_boxplot = data_boxplot %>%
  pivot_longer(
    .,
    cols = c(
      "21 Janvier 2021",
      "10 Avril 2021",
      "15 Septembre 2021",
      "12 Décembre 2021"
    ),
    names_to = "Date",
    values_to = "Taux d'incidence"
  )
data_boxplot$Date = factor(
  data_boxplot$Date,
  levels = c(
    "21 Janvier 2021",
    "10 Avril 2021",
    "15 Septembre 2021",
    "12 Décembre 2021"
  ),
  ordered = TRUE
)

data_boxplot %>%
  ggplot(aes(x = Date, y = `Taux d'incidence`)) +
  geom_boxplot(fill = "light blue", width = 0.5) +
  theme(axis.text.x = element_text(size = 12)) +
  theme(axis.text.y = element_text(size = 12)) +
  theme(axis.title.x = element_text(size = 14)) +
  theme(axis.title.y = element_text(size = 14)) +
  theme(panel.background = element_blank(),
        axis.line = element_line(color = "grey")) +
  theme(panel.grid = element_line(color = "grey", linetype = "dotted"))
+ geom_violin(alpha = 0.5, trim = T, fill = "pink")

## taux de positivité
data_boxplot = data_jan %>% select(libelle_dep, tx_pos) %>%
  left_join(data_10_av %>% select(libelle_dep, tx_pos), by = c("libelle_dep")) %>%
  left_join(data_sep %>% select(libelle_dep, tx_pos), by = c("libelle_dep")) %>%
  left_join(data_12_dec %>% select(libelle_dep, tx_pos),
            by = c("libelle_dep"))
colnames(data_boxplot) = c("Département",
                           "21 Janvier 2021",
                           "10 Avril 2021",
                           "15 Septembre 2021",
                           "12 Décembre 2021")


data_boxplot = data_boxplot %>%
  pivot_longer(
    .,
    cols = c(
      "21 Janvier 2021",
      "10 Avril 2021",
      "15 Septembre 2021",
      "12 Décembre 2021"
    ),
    names_to = "Date",
    values_to = "Taux de positivité"
  )
data_boxplot$Date = factor(
  data_boxplot$Date,
  levels = c(
    "21 Janvier 2021",
    "10 Avril 2021",
    "15 Septembre 2021",
    "12 Décembre 2021"
  ),
  ordered = TRUE
)

data_boxplot %>%
  ggplot(aes(x = Date, y = `Taux de positivité`)) +
  geom_boxplot(fill = "light blue", width = 0.5) +
  theme(axis.text.x = element_text(size = 12)) +
  theme(axis.text.y = element_text(size = 12)) +
  theme(panel.background = element_blank(),
        axis.line = element_line(color = "grey")) + theme(panel.grid = element_line(color = "grey", linetype = "dotted")) +
  geom_violin(alpha = 0.5, trim = T, fill = "pink") + geom_violin(alpha = 0.5, trim = T, fill =
                                                                    "pink") +
  theme(axis.title.x = element_text(size = 14)) +
  theme(axis.title.y = element_text(size = 14))


## scatter plot --
###
ggplot(data_12_dec, aes(x = taux.d.incidence, y = couv_complet)) + geom_point(color =
                                                                                "blue") + 
  geom_smooth(method = "lm", color = "dark grey") +
  xlab("Taux d'incidence") + ylab("Pourcentage de couverture vaccinale complète") +
  theme(axis.text.x = element_text(size = 11)) +
  theme(axis.text.y = element_text(size = 11)) +
  theme(axis.title.x = element_text(size = 13)) +
  theme(axis.title.y = element_text(size = 13)) + theme(panel.background = element_blank(),
                                                        axis.line = element_line(color = "grey")) +
  theme(panel.grid = element_line(color = "grey", linetype = "dotted"))


ggplot(data_12_dec, aes(x = tx_pos, y = couv_complet)) + geom_point(color =
                                                                      "blue") + 
  geom_smooth(method = "lm", color = "dark grey") +
  xlab("Taux de positivité") + ylab("Pourcentage de couverture vaccinale complète") +
  theme(axis.text.x = element_text(size = 11)) +
  theme(axis.text.y = element_text(size = 11)) +
  theme(axis.title.x = element_text(size = 13)) +
  theme(axis.title.y = element_text(size = 13)) + theme(panel.background = element_blank(),
                                                        axis.line = element_line(color = "grey")) + 
  theme(panel.grid = element_line(color = "grey", linetype = "dotted"))


ggplot(data_12_dec, aes(x = taux_occupation_sae, y = couv_complet)) + geom_point(color =
                                                                                   "blue") +
  geom_smooth(method = "lm", color = "dark grey") +
  xlab("Taux d'occupation hospitalière") + ylab("Pourcentage de couverture vaccinale complète") +
  theme(axis.text.x = element_text(size = 11)) +
  theme(axis.text.y = element_text(size = 11)) +
  theme(axis.title.x = element_text(size = 13)) +
  theme(axis.title.y = element_text(size = 13)) + theme(panel.background = element_blank(),
                                                        axis.line = element_line(color = "grey")) + 
  theme(panel.grid = element_line(color = "grey", linetype = "dotted"))

## Le 10 avril . correlation  et relation entre indicateurs et les mesures ----
colnames(data_10_av)
data_boxplot = data_10_av %>% select(libelle_dep,
                                     Région,
                                     taux.d.incidence,
                                     Restrictions.sanitaires.renforcées) %>% filter(Restrictions.sanitaires.renforcées ==
                                                                                      1)
data_boxplot %>%
  ggplot(aes(x = libelle_dep, y = taux.d.incidence)) +
  geom_boxplot(fill = "light blue", width = 0.5) +
  theme(axis.text.x = element_text(size = 12)) +
  theme(axis.text.y = element_text(size = 12)) +
  theme(axis.title.x = element_text(size = 14)) +
  theme(axis.title.y = element_text(size = 14)) +
  theme(panel.background = element_blank(),
        axis.line = element_line(color = "grey")) + theme(panel.grid = element_line(color = "grey", linetype = "dotted")) + 
  geom_violin(alpha = 0.5, trim = T, fill ="pink")
