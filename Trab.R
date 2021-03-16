library(PNADcIBGE)
library(gglorenz)
library(ineq)

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


 plot(Lc2020SP$L.general, type="l")
lines(Lc2020PA$L.general, type="l")

summary(final2020PA)
summary(final2020SP)

Gini(final2020PA$renda)
Gini(final2020SP$renda)



