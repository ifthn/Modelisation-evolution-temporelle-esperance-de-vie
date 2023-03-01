getwd()
library(AER)
library(car)
library(stargazer)
library(sandwich)
library(foreign)
library(lmtest)
library(gmm)
library(systemfit)
library(ggplot2)
library(corrplot)
library(systemfit)
library(nlme)
library("plm")
library(psych)
library(dplyr)
#https://www.rdocumentation.org/packages/plm/versions/2.2-3

#####Statistiques descriptives sur les donnees empilees#######
data=read.table("data_a.csv",head=TRUE,sep=";", dec=",")
head(data)
data$logPIBHabitant <-log(data$PIBHabitant)
summary(data)
data$ID<-as.factor(data$ID)
colnames(data)[1] <- 'Pays'  

plot(data$Periode,data$EsperanceVie,xlab = "year",ylab = "EsperanceVie ")
plot(data$Periode[data$ID==1],data$EsperanceVie[data$ID==1],xlab = "year",ylab = "EsperanceVie ")
data_ind<-groupedData(EsperanceVie ~ Periode|ID, data,   outer = ~ Pays)
plot(data_ind)

par(mfrow=c(1,3))
boxplot(data$EsperanceVie,las=1, names = c("EsperanceVie"), col = c( "red") )
boxplot(data$logPIBHabitant,las=1, names = c("logPIB par habitant"), col = c( "red") )
boxplot(data$DepensesSante,las=1, names = c("Depenses de sante"), col = c( "red") )

par(mfrow=c(1,3))
boxplot(data$EsperanceVie,las=1, names = c("EsperanceVie"), col = c( "red") )
boxplot(data$PIBHabitant,las=1, names = c("PIB par habitant"), col = c( "red") )
boxplot(data$DepensesSante,las=1, names = c("Depenses de sante"), col = c( "red") )




matrixcorr<- cbind(data$PIBHabitant,data$DepensesSante,data$AccesElec,data$EcolePrimaire,data$EnseignSup)
colnames(matrixcorr) <-c("PIBHabitant","DepensesSante","AccesElec","EcolePrimaire","EnseignSup")
matrixcorr
mcor<-cor(matrixcorr)
mcor
#mcor
corrplot(mcor, type="upper", order="hclust", tl.col="black", tl.srt=45)



########donnees identifiees comme un panel

data_p<-pdata.frame(data, index = c("CodePays", "Periode"), drop.index = TRUE, row.names = TRUE)
pdim(data_p, 7)
head(data_p)


## decomposition de la variance 
###EsperanceVie
vartot_EsperanceVie=sum((data_p$EsperanceVie-mean(data_p$EsperanceVie))^2)
vartot_EsperanceVie##variabilité totale

var_withinEsperanceVie = sum(Within(data_p$EsperanceVie)^2)
var_withinEsperanceVie #variabilité within
var_withinEsperanceVie/vartot_EsperanceVie 

var_betweenEsperanceVie = sum((Between(data_p$EsperanceVie)-mean(data_p$EsperanceVie))^2)
var_betweenEsperanceVie
var_betweenEsperanceVie/vartot_EsperanceVie 

###PIBHabitant
vartot_PIBHabitant=sum((data_p$PIBHabitant-mean(data_p$PIBHabitant))^2)
vartot_PIBHabitant##variabilité totale

var_withinPIBHabitant = sum(Within(data_p$PIBHabitant)^2)
var_withinPIBHabitant #variabilité within
var_withinPIBHabitant/vartot_PIBHabitant 

var_betweenPIBHabitant = sum((Between(data_p$PIBHabitant)-mean(data_p$PIBHabitant))^2)
var_betweenPIBHabitant
var_betweenPIBHabitant/vartot_PIBHabitant 


## Esperance de vie
scatterplot(data$EsperanceVie ~ data$Periode|data$Pays,
            boxplots=TRUE,
            smooth=TRUE, 
            legend = TRUE,
            regLine=FALSE,
            xlab = "Annees",
            ylab = "Esperance de vie",
            data=data)


## PIB par habitant
scatterplot(data$PIBHabitant ~ data$Periode|data$Pays,
            boxplots=TRUE,
            smooth=TRUE, 
            legend = TRUE,
            regLine=FALSE,
            xlab = "Annees",
            ylab = "PIB par habitant",
            data=data)


## Depenses de sante
scatterplot(data$DepensesSante ~ data$Periode|data$Pays,
            boxplots=TRUE,
            smooth=TRUE, 
            legend = TRUE,
            regLine=FALSE,
            xlab = "Annees",
            ylab = "Depenses de sante",
            data=data)



## Acces a l'electricite
scatterplot(data$AccesElec ~ data$Periode|data$Pays,
            boxplots=TRUE,
            smooth=TRUE, 
            legend = TRUE,
            regLine=FALSE,
            xlab = "Annees",
            ylab = "Acces a l'electricite",
            data=data)


## Ecole primaire
scatterplot(data$EcolePrimaire ~ data$Periode|data$Pays,
            boxplots=TRUE,
            smooth=TRUE, 
            legend = TRUE,
            regLine=FALSE,
            xlab = "Annees",
            ylab = "Inscription a l'ecole primaire",
            data=data)


## Enseignement superieur
scatterplot(data$EnseignSup ~ data$Periode|data$Pays,
            boxplots=TRUE,
            smooth=TRUE, 
            legend = TRUE,
            regLine=FALSE,
            xlab = "Annees",
            ylab = "Inscription a l'enseignement superieur",
            data=data)

######estimation

###MODELE HOMOGENE POOLED


form <- EsperanceVie~logPIBHabitant+DepensesSante+AccesElec+EcolePrimaire + EnseignSup 
reg_pooled = plm(form ,  model="pooling", data=data_p)
summary(reg_pooled)
#########meme resultat avec LM
reg_pooled1 = lm(EsperanceVie~logPIBHabitant+DepensesSante+AccesElec+EcolePrimaire + EnseignSup  ,  data=data_p)
summary(reg_pooled1)



stargazer(reg_pooled, type = "text", title = "Modele Homogene/Pooled")







###MODELE HETEROGENE
reg_pays = lm(EsperanceVie~Pays + logPIBHabitant:Pays+DepensesSante:Pays+EcolePrimaire:Pays + EnseignSup:Pays , data=data_p)
summary(reg_pays)

stargazer(reg_pays, type = "text", title = "Modele heterogene")

##solution 2 avec le package systemfit (presentation differente des resultats)
#gr_MCO <- systemfit(EsperanceVie~logPIBHabitant+DepensesSante+AccesElec+EcolePrimaire + EnseignSup, method="OLS", data=data_p)
#summary(gr_MCO, residCov = FALSE, equations= FALSE)
#summary(gr_MCO) #presentation differente

###test de Fisher pour le choix entre modele heterogene / modele homogene
anova(reg_pooled1,reg_pays) 
anova(reg_pays,reg_pooled1)


pooltest(form, data = data_p, effect = "individual", model = "pooling") # avec plm

## Interpretation: On refuse l'hypothese nulle et on retient pas modele homogene mais on retient modele heterogene
## Car pvalue < 5% 


####calcul effectue, verif
scr_pays = sum(reg_pays$residuals^2)
scr_pays
ddl_pays = reg_pays$df.residual
ddl_pays

scr_pooled = sum(reg_pooled$residuals^2)
scr_pooled
ddl_pooled = reg_pooled$df.residual
ddl_pooled

F_PP = ((scr_pooled-scr_pays)/(ddl_pooled-ddl_pays))/(scr_pays/ddl_pays)
F_PP
pvalue_PP = pf(F_PP,ddl_pooled-ddl_pays,ddl_pays,lower.tail=FALSE)
pvalue_PP



#####MODELE EFFETS FIXES (MODELE PARTIELLEMENT HOMOGENE)
###estimation LSDV
reg_LSDV = lm(EsperanceVie~logPIBHabitant+DepensesSante+AccesElec+EcolePrimaire + EnseignSup - 1 , data=data_p )
summary(reg_LSDV)
stargazer(reg_LSDV, type = "text", title = "Modele LSDV partiellement homogène")
scr_LSDV = sum(reg_LSDV$residuals^2)
scr_LSDV
ddl_LSDV = reg_LSDV$df.residual
ddl_LSDV

###estimation within
reg_within = plm(EsperanceVie~logPIBHabitant+DepensesSante+AccesElec+EcolePrimaire + EnseignSup ,  effect="individual", model="within", data=data_p)
summary(reg_within)
summary(fixef(reg_within))
scr_within = sum(reg_within$residuals^2)
scr_within #meme scr que LSDV, R2 different
ddl_within = reg_within$df.residual
ddl_within #meme ddl





################## TESTS ############################

# Test Fisher
anova(reg_pooled1,reg_pays) #anova marche avec lm pas avec plm
pooltest(form, data = data_p, effect = "individual", model = "pooling") # avec plm

## Interpretation: On refuse l'hypothese nulle
## Car pvalue < 5% 





# Test présence effets individuels fixes
pFtest(reg_within,reg_pooled1) 

## Interpretation: Pvalue< 5% donc on rejette H0




###Effets fixes ou aleatoires avec test hausmann ? Test H0(4)


# Test entre choix modele heterogene / partiellement homogene

scr_pays = sum(reg_pays$residuals^2)
scr_pays
ddl_pays = reg_pays$df.residual
ddl_pays

scr_LSDV = sum(reg_LSDV$residuals^2)
scr_LSDV
ddl_LSDV = reg_LSDV$df.residual
ddl_LSDV

F_PP3 = ((scr_LSDV-scr_pays)/(ddl_LSDV-ddl_pays))/(scr_pays/ddl_pays)
F_PP3
pvalue_PP3 = pf(F_PP3,ddl_LSDV-ddl_pays,ddl_pays,lower.tail=FALSE)
pvalue_PP3


# Interpretation:  On prefere le modele heterogene puisque la pvalue est inferieure a 5% donc on rejette l'hypothese nulle






#####Modele dynamique

# methode GMM Arrellano et Bond
EsperanceAB = pgmm(EsperanceVie~lag(EsperanceVie, 1:1)+PIBHabitant+DepensesSante+AccesElec+EcolePrimaire+EnseignSup | lag(EsperanceVie,2:99), data = data_p, effect = "individual", model = "twosteps")
summary(EsperanceAB)

# methode SYS_GMM
EsperanceB = pgmm(EsperanceVie~lag(EsperanceVie, 1:1)+PIBHabitant+DepensesSante+AccesElec+EcolePrimaire+EnseignSup | lag(EsperanceVie,2:99), data = data_p, effect = "individual", model = "twosteps", transformation = "ld")
summary(EsperanceB,  robust = TRUE)

