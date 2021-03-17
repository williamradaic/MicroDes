library(PNADcIBGE)
library(gglorenz)
library(ineq)
library(survey)
library(stargazer)

base20201 <- get_pnadc(2020, quarter = 1, vars = c('RM_RIDE', "V2001",'V403312'), defyear = 2020, defperiod = 1, labels = F, design = T)

##peguei os dados do primeiro tri, na rm de PA e na rm de SP. Calculei uma renda per capita familiar

View(base20201$variables)

RM2020PA <- as.data.frame(base20201$variables$RM_RIDE)
tamanho.familia <- as.data.frame(base20201$variables$V2001)
rendimentos2020PA <- as.data.frame(base20201$variables$V403312)
renda2020PA <- cbind(RM2020PA, tamanho.familia, rendimentos2020PA)
renda2020PA <- renda2020PA[renda2020PA$`base20201$variables$RM_RIDE` ==15 , ]

renda2020PA <- renda2020PA[complete.cases(renda2020PA$`base20201$variables$RM_RIDE`),]
renda2020PA <- renda2020PA[complete.cases(renda2020PA$`base20201$variables$V403312`),]
colnames(renda2020PA)<- c("cdg rm","tamanho.familia", "renda")
renda2020PA <- transform(renda2020PA, per.capita.familiar = renda/tamanho.familia)



RM2020SP <- as.data.frame(base20201$variables$RM_RIDE)
tamanho.familia <- as.data.frame(base20201$variables$V2001)
rendimentos2020SP <- as.data.frame(base20201$variables$V403312)
renda2020SP <- cbind(RM2020SP, tamanho.familia, rendimentos2020SP)
renda2020SP <- renda2020SP[renda2020SP$`base20201$variables$RM_RIDE` ==35 , ]

renda2020SP <- renda2020SP[complete.cases(renda2020SP$`base20201$variables$RM_RIDE`),]
renda2020SP <- renda2020SP[complete.cases(renda2020SP$`base20201$variables$V403312`),]
colnames(renda2020SP)<- c("cdg rm","tamanho.familia", "renda")
renda2020SP <- transform(renda2020SP, per.capita.familiar = renda/tamanho.familia)

final2020PA <- as.data.frame(renda2020PA$per.capita.familiar)
final2020SP <- as.data.frame(renda2020SP$per.capita.familiar)

colnames(final2020PA)<- c("renda")
colnames(final2020SP)<- c("renda")

Lc2020PA <- Lc(final2020PA$renda, n=c(1:length(final2020PA$renda)))
plot(Lc2020PA)

Lc2020SP <- Lc(final2020SP$renda, n=c(1:length(final2020SP$renda)))
plot(Lc2020SP)
lines(Lc2020PA, col=2)
legend(0.05,0.9, 
       legend = c("PA", "SP"), col = c(1,2), pch = c(19,19), 
       #bty = "n", 
       pt.cex = 2, 
       cex = 1.2, 
       text.col = "black", 
       horiz = F , 
       inset = c(0.1, 0.1))


plot(Lc2020SP$L.general, type="l")
lines(Lc2020PA$L.general, type="l")



summary(final2020PA)
summary(final2020SP)


### Medidas de desigualdade ############

final2020SP <- final2020SP[order(final2020SP$renda),]
final2020PA <- final2020PA[order(final2020PA$renda),]

Gini(final2020PA)
Gini(final2020SP)

ineq(final2020PA, type = "Theil")
ineq(final2020SP, type = "Theil")

ineq(final2020PA, type = "Atkinson")
ineq(final2020SP, type = "Atkinson")

cumsumSP9 <- sum(final2020SP[(0.9*length(final2020SP)):length(final2020SP)])
cumsumSP9/sum(final2020SP) # renda do 10% mais rico

cumsumSP1 <- sum(final2020SP[1:(0.1*length(final2020SP))])
cumsumSP1/sum(final2020SP) # renda do 10% mais pobre


cumsumPA9 <- sum(final2020PA[(0.9*length(final2020PA)):length(final2020PA)])
cumsumPA9/sum(final2020PA) # renda do 10% mais rico

cumsumPA1 <- sum(final2020PA[1:(0.1*length(final2020PA))])
cumsumPA1/sum(final2020PA) # renda do 10% mais pobre


ineq_matrix <- matrix(NA, nrow = 6, ncol = 3)
ineq_matrix <- rbind(c("Gini", Gini(final2020PA),Gini(final2020SP)), c("Theil", ineq(final2020PA, type = "Theil"), ineq(final2020SP, type = "Theil")), c("Atkinson", ineq(final2020PA, type = "Atkinson"), ineq(final2020SP, type = "Atkinson")), c("Income share, 10% mais rico", cumsumPA9/sum(final2020PA), cumsumSP9/sum(final2020SP)), c("Income share, 10% mais pobre", cumsumPA1/sum(final2020PA), cumsumSP1/sum(final2020SP)), c("90-10 Ratio", quantile(final2020PA,0.9)/quantile(final2020PA,0.1), quantile(final2020SP,0.9)/quantile(final2020SP,0.1)))



ineq_matrix = ineq_matrix[,-1]
colnames(ineq_matrix) = c("PA", "SP")
ineq_matrix <- apply(ineq_matrix, 2 ,as.numeric)
rownames(ineq_matrix) = c("Gini", "Theil", "Atkinson", "Income share, 10% mais rico", "Income share, 10% mais pobre", "90-10 Ratio")

stargazer::stargazer(ineq_matrix,align = T)

## 90-10 Ratio: 

quantile(final2020SP,0.9)/quantile(final2020SP,0.1) # SP
quantile(final2020PA,0.9)/quantile(final2020PA,0.1) # PA



## Linhas de pobreza 

##IBGE U$5.50
### PPP OCDE: 2.247

z = 5.5*2.247*30


pov(as.numeric(unlist(final2020PA)), z,  parameter = NULL, type = "Foster")
pov(as.numeric(unlist(final2020SP)), z,  parameter = NULL, type = "Foster")

pov(as.numeric(unlist(final2020PA)), z,  parameter = NULL, type = "Sen")
pov(as.numeric(unlist(final2020SP)), z,  parameter = NULL, type = "Sen")


##### Lorenz Generalizada


LcG2020PA <- Lc(final2020PA$renda, n=c(1:length(final2020PA$renda)))
plot(LcG2020PA, general = T)

LcG2020SP <- Lc(final2020SP$renda, n=c(1:length(final2020SP$renda)))
plot(LcG2020SP, general = T, main = "Generalized Lorenz curve")
lines(LcG2020PA, col=2, general = T)
legend(0.05,1200000, 
       legend = c("PA", "SP"), col = c(1,2), pch = c(19,19), 
       #bty = "n", 
       pt.cex = 2, 
       cex = 1.2, 
       text.col = "black", 
       horiz = F , 
       inset = c(0.1, 0.1))


#### Dominância pobreza

#pobreza_PA <- as.data.frame(final2020PA[(final2020PA$renda < z),])

#pobreza_SP <- as.data.frame(final2020SP[(final2020SP$renda < z),])


cdfSP <- ecdf(final2020SP$renda)
cdfPA <- ecdf(final2020PA$renda)

plot(cdfPA, main = "Curva de incidência de pobreza",do.points=FALSE,verticals=T, lwd = 2, xlim = c(0, 25000), xlab = 'Renda domiciliar per capita', ylab = 'População acumulada (%)')
lines(cdfSP, col = 2,do.points=FALSE,verticals=T, lwd = 2, xlim = c(0, 25000))
legend(23000,0.4, 
       legend = c("PA", "SP"), col = c(1,2), pch = c(19,19), 
       #bty = "n", 
       pt.cex = 2, 
       cex = 1.2, 
       text.col = "black", 
       horiz = F , 
       inset = c(0.1, 0.1))


### Stargazer
 
list_df <- list(final2020PA, final2020SP)


stargazer::stargazer(list_df, flip = T)





########### PNAD Covid ##############

#library(srvyr) 
#library(Cairo)
#library(dplyr)
#library(tidyverse)#

#PNAD_COVID_112020 <- read_csv("PNAD_COVID_112020.csv", col_types = cols(.default = "d"))

# Renda recebida em todos os trabalhos: PNAD_COVID_112020$C011A12

#RMCOVIDPA <- PNAD_COVID_112020

#RMCOVIDPA %>% filter(RMCOVIDPA$RM_RIDE == 15)
#RMCOVIDPA = RMCOVIDPA[complete.cases(RMCOVIDPA$C011A12),]
#tamanho.familia <- as.data.frame(RMCOVIDPA$V2001)
#rendimentos2020PA <- as.data.frame(base20201$variables$V403312)
#renda2020PA <- cbind(RM2020PA, tamanho.familia, rendimentos2020PA)
#renda2020PA <- renda2020PA[renda2020PA$`base20201$variables$RM_RIDE` ==15 , ]##

#renda2020PA <- renda2020PA[complete.cases(renda2020PA$`base20201$variables$RM_RIDE`),]
#renda2020PA <- renda2020PA[complete.cases(renda2020PA$`base20201$variables$V403312`),]
#colnames(renda2020PA)<- c("cdg rm","tamanho.familia", "renda")
#renda2020PA <- transform(renda2020PA, per.capita.familiar = renda/tamanho.familia)

#RMCOVIDPA$F006









