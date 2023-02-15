#' ********************************************************************************
#' @title 	             ACP                                   
#' @author              Khaoula Aroui                                            
#' @description 	      
#' xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
#' ********************************************************************************


#Vider l'environement ----
rm(list = ls())
# set the working directory ----
setwd("C:/Users/khaou/Documents/Master SEP/5- Analyse des données et data mining_GUATHERAT/projet individuel")

#Libraries requises ----
if (!require(readxl)) { install.packages("readxl"); library(readxl) } # pour importer les fichiers excels des bases des données
if (!require(dplyr)) { install.packages("dplyr"); library(dplyr) }    # pour la manipulation des bases des données
if (!require(FactoMineR)) { install.packages("FactoMineR"); library(FactoMineR) }# pour l'ACP
if (!require(factoextra)) { install.packages("factoextra"); library(factoextra) }# pour l'ACP

# sourced scripts ----
source(paste(getwd(),"/PGM/analysis/Importation_donnees.R", sep=""))




#  ACP: Working on quantitative variables only  ----
##corr (couv dose 1 ,couverture )=1, donc seulement une a utiliser 
# les variables sur les demographics de la population sont des combinaisons linéaires ==> on se limite à certaines dans notre ACP
data=data_12_dec%>%select(taux.d.incidence, taux_occupation_sae, tx_pos, couv_complet, couv_rappel,Densité, `Part.des.femmes..en...`, 
                          `Part.des.0.à.24.ans..en...`,  `Part.des.60.ans.ou.plus..en...` )

data=data%>%rename("Part des 0 à 24 ans de la population (en %)"="Part.des.0.à.24.ans..en...", 
                   "Part des femmes de la population (en %)"="Part.des.femmes..en...")

names=c("Taux d'incidence", "Taux d'occupation hospitalière", "Taux de positivité","Couverture complète", "Couverture Rappel",
        "Densité population","% des femmes",
        "% de 0 à 24 ans de la population", "% de 60 ans ou plus de la population")
colnames(data)=names
str(data)

## explorer la relation entre les variables numériques d'interet ----
M<-round(cor(data),2)
cor.mtest <- function(mat, ...) {
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat<- matrix(NA, n, n)
  diag(p.mat) <- 0
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      tmp <- cor.test(mat[, i], mat[, j], ...)
      p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
    }
  }
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
  p.mat
}

p.mat <- cor.mtest(data)

col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
library(corrplot)
corrplot(M, method="color", col=col(100),  
         type="lower", order="original", 
         addCoef.col = "black", # Ajout du coefficient de corrélation
         tl.col="black", tl.srt = 65,#Rotation des etiquettes de textes
         # Combiner avec le niveau de significativité
         p.mat = p.mat, sig.level = 0.05, insig = "blank", 
         # Cacher les coefficients de corrélation sur la diagonale
         diag=FALSE 
)


# PCA ----
res.pca<- prcomp(data, scale. = TRUE)
### Valeurs propres et varoance exprimée
eig.val <- as.data.frame( get_eigenvalue(res.pca) )
eig.val <- cbind(rownames(eig.val), eig.val) 
rownames(eig.val)=NULL
eig.val <- data.frame(lapply(eig.val, function(x) if(is.numeric(x)) round(x, 2) else x))
colnames(eig.val)=c("Composantes principale", "Valeurs propres", 
                    "% de variance", "% de variance cumulée")


# Répartition de la variance totale exprimée ----
fviz_eig(res.pca, addlabels=TRUE)+
  theme(axis.text.x = element_text( size=12))+
  theme(axis.text.y = element_text( size=12))+
  ylab("Pourcentage de variance expliquée")+
  theme(axis.title.x = element_text(size = 14))+
  theme(axis.title.y = element_text(size = 14))+
  theme(panel.background = element_blank(), axis.line = element_line(color = "grey"))+ 
  theme(panel.grid = element_line(color = "grey", linetype = "dotted"))

## correlation des variables avec les composantes 
var <- factoextra::get_pca_var(res.pca)
cor <- var$cor
Table_cor<- as.data.frame(cor)
Table_cor <- cbind(rownames(Table_cor), Table_cor) 
rownames(Table_cor)=NULL
Table_cor <- data.frame(lapply(Table_cor, function(x) if(is.numeric(x)) round(x, 2) else x))
names(Table_cor)[names(Table_cor) == 'rownames.Table_cor.'] <- 'Variable'

library(writexl)
write_xlsx(Table_cor,paste(getwd(), "/results_pgm/results.xlsx", sep="") )


## Matrice des contributions 
contrib <- var$contrib
Table_contrib<- as.data.frame(contrib)
Table_contrib <- cbind(rownames(Table_contrib), Table_contrib) 
rownames(Table_contrib)=NULL
Table_contrib <- data.frame(lapply(Table_contrib, function(x) if(is.numeric(x)) round(x, 2) else x))
names(Table_contrib)[names(Table_contrib) == 'rownames.Table_contrib.'] <- 'Variable'
write_xlsx(Table_contrib,paste(getwd(), "/results_pgm/results2.xlsx", sep="") )

## Qualité de representation des variables 
cos2 <- var$cos2
Table_cos2<- as.data.frame(cos2)
Table_cos2 <- cbind(rownames(Table_cos2), Table_cos2) 
rownames(Table_cos2)=NULL
Table_cos2 <- data.frame(lapply(Table_cos2, function(x) if(is.numeric(x)) round(x, 2) else x))
names(Table_cos2)[names(Table_cos2) == 'rownames.Table_cos2.'] <- 'Variable'
write_xlsx(Table_cos2,paste(getwd(), "/results_pgm/results3.xlsx", sep="") )


# Variables
# correlation circle dim 1 et 2 
fviz_pca_var(res.pca, axes = c(1, 2),
                                    xlab="Composante principale 1", 
                                    ylab="Composante principale 2",
                                    col.var = "contrib", 
                                    gradient.cols = c("#00AFBB", "#E7B800", "#FC4E57"),
                                    repel = FALSE, labelsize=6, legend.size = 6)+
  theme(axis.title.x = element_text(size = 16))+
  theme(axis.title.y = element_text(size = 16))+
  theme(axis.text.x = element_text( size=14))+
  theme(axis.text.y = element_text( size=14))

fviz_pca_ind(res.pca, axes = 1:2,
             geom.ind = "point", # Montre les points seulement (mais pas le "text")
             col.ind = data_12_dec$Région, # colorer by groups
             title="Représentation des individus par région")+scale_shape_manual(values=c(1,2,3,4,5,6,7,8,9,10,11,16,17,18,20,25 ))



# correlation circle dim 2 et 3
fviz_pca_var(res.pca, axes = c(1, 3),
             xlab="Composante principale 1", 
             ylab="Composante principale 3",
             col.var = "contrib", 
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E57"),
             repel = FALSE, labelsize=6, legend.size = 6)+
  theme(axis.title.x = element_text(size = 16))+
  theme(axis.title.y = element_text(size = 16))+
  theme(axis.text.x = element_text( size=14))+
  theme(axis.text.y = element_text( size=14))

fviz_pca_ind(res.pca, axes = 2:3,
             geom.ind = "point", # Montre les points seulement (mais pas le "text")
             col.ind = data_12_dec$Région, # colorer by groups
             title="Représentation des individus par région")+scale_shape_manual(values=c(1,2,3,4,5,6,7,8,9,10,11,16,17,18,20,25 ))



