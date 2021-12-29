setwd('/home/luanmugarte/Dissertacao/Scripts')

#### Carregando os dados ####

# IPCA
ipca <- read.csv("Dados/ipcacompleto.csv")

# IPCA índice
ipcaindice <- read.csv("Dados/ipcaindice.csv")

# IGP-10 índice
igp <- read.csv("Dados/igp10.csv")

# IPA-10 índice
ipa <- read.csv("Dados/ipa10.csv")

# IPCA decomposto
ipcadecomp <- read.csv('Dados/IPCAdecomposto.csv')

# Cambio 
cambiocompleto <- read.csv("Dados/cambiocompleto.csv")

# Capacidade
capacidade <- read.csv("Dados/capacidade.csv")

# Commodities
comm <- read.csv("Dados/comm.csv")

# Juros
selic <- read.csv('Dados/selic.csv')

# PIM-PF
pimpf <- read.csv("Dados/pimpf.csv")

# PIM-PF antiga
pimpfantiga <- read.csv("Dados/pimpfantiga.csv")

# PIM-PF
pimpf_saz <- read.csv("Dados/pimpf_sazo.csv")

# PIM-PF antiga
pimpfantiga_saz <- read.csv("Dados/pimpfantiga_sazo.csv")
pimpfantiga_saz <- pimpfantiga_saz[289:nrow(pimpfantiga_saz),]


# PIB mensal
pibmensal <- read.csv('Dados/pibmensal(BCB).csv', skip = 1, header = F)

# Desemprego (Retropolada)
desemprego <- read.csv('Dados/PNADc Retropolada (Carvalho, 2016) - Taxa de Desemprego.csv', header = F, dec = ",")

#### Manipulando as séries ####

# Ajustando nomes das colunas
colnames(capacidade) <- cbind('index','data','valores')

# Vendo as séries
ipca[1,]
cambiocompleto[1,]
comm[1,]
capacidade
selic[1,]
desemprego[1,]

# Compatilizando PIM-PF ####

# Convertendo séries em objetos ts
pimpf <- ts(pimpf[1:nrow(pimpf),2], start = c(2002,1), end = c(2020,02), frequency = 12)

pimpfantiga <- ts(pimpfantiga[1:nrow(pimpfantiga),2], start = c(1999,1), frequency = 12)

# Valor de proporção
pimpf[1]/pimpfantiga[37]

# Compatibilizando a PIM-PF antiga com o valor de proporção
pimpfantiga <- ts(pimpfantiga*(pimpf[1]/pimpfantiga[37]), start = c(1999,1), frequency = 12)

# Ano de 2002
pimpfantiga[37:48]

# Criando série unificada
pimpf <- ts(c(pimpfantiga[7:36],pimpf[1:length(pimpf)]), start = c(1999,7), end = c(2020,2), frequency = 12)


# Compatilizando PIM-PF sazonalizada ####

# Convertendo séries em objetos ts
pimpf_saz <- ts(pimpf_saz[1:nrow(pimpf_saz),2], start = c(2002,1), end = c(2020,02), frequency = 12)

pimpfantiga_saz <- ts(pimpfantiga_saz[1:nrow(pimpfantiga_saz),2], start = c(1999,1), frequency = 12)

# Valor de proporção
pimpf_saz[1]/pimpfantiga_saz[37]

# Compatibilizando a PIM-PF antiga com o valor de proporção
pimpfantiga_saz <- ts(pimpfantiga_saz*(pimpf_saz[1]/pimpfantiga_saz[37]), start = c(1999,1), frequency = 12)

# Ano de 2002
pimpfantiga_saz[37:48]

# Criando série unificada
pimpf_saz <- ts(c(pimpfantiga_saz[13:36],pimpf_saz[1:length(pimpf_saz)]), start = c(1999,7), end = c(2020,2), frequency = 12)
pimpf_saz

# Convertendo em objetos ts - Período: 07/1999 - 02/2020 ####

ipca <- ts((ipca[7:nrow(ipca),2]), start = c(1999,7), end = c(2020,2), frequency = 12)
ipcaindice <- ts((ipcaindice[7:nrow(ipcaindice),2]), start = c(1999,7), end = c(2020,2), frequency = 12)
cambio <- (ts(cambiocompleto[7:nrow(cambiocompleto),2], start = c(1999,7),  end = c(2020,2), frequency = 12))
comm <- ts((comm[7:nrow(comm),2]),  start = c(1999,7), end = c(2020,2), frequency = 12)
capacidade <- ts((capacidade[91:nrow(capacidade),2]),  start = c(1999,7), end = c(2020,2), frequency = 12)
desemprego <- ts(desemprego[19:nrow(desemprego),2], start = c(1999,7), end = c(2020,2), frequency = 12)
pibmensal <- ts(pibmensal[236:nrow(pibmensal),2], start = c(1999,7), end = c(2020,2), frequency = 12)

# IGP
igp
igp <- ts((igp[71:nrow(igp),2]), start = c(1999,7), end = c(2020,2), frequency = 12)

# IPA
ipa
ipa <- ts((ipa[71:nrow(ipa),2]), start = c(1999,7), end = c(2020,2), frequency = 12)

# IPCA tradables
tradables <- ts((ipcadecomp[8:nrow(ipcadecomp),4]), start = c(1999,7), end = c(2020,2), frequency = 12)

# IPCA non tradables
nontradables <- ts((ipcadecomp[8:nrow(ipcadecomp),5]), start = c(1999,7), end = c(2020,2), frequency = 12)

# IPCA monitorados
monitorados <- ts((ipcadecomp[8:nrow(ipcadecomp),6]), start = c(1999,7), end = c(2020,2), frequency = 12)

# Vendo as séries
ipca
cambio
comm
capacidade
desemprego
ipa
igp

# Verificando numero de obs
length(ipca)
length(cambio)
length(comm)
length(capacidade)
length(desemprego)

# Plotando as śeries
par(mfrow=c(1,1))
plot(ipca)
plot(comm)
plot(capacidade)
plot(cambio)


#### Dessazonalizando as séries ####

# IPCA
ipca
monthplot(ipca)
seas_ipca <- seas(x = ipca)
summary(seas_ipca)

seasonal::qs(seas_ipca)

ipcadessaz <- series(seas_ipca, "s11")
plot(ipca)
plot(seas_ipca)
ipca <- ipcadessaz

# COMM

monthplot(comm)
seas_comm <- seas(x = comm)
summary(seas_comm)

seasonal::qs(seas_comm)

# desemprego
desemprego
monthplot(desemprego)
seas_desemprego <- seas(x = desemprego)
summary(seas_desemprego)

seasonal::qs(seas_desemprego)

desempregodessaz <- series(seas_desemprego, "s11")
plot(desemprego)
plot(seas_desemprego)
desemprego <- desempregodessaz

# PIM-PF Sazonalizada
pimpf_saz
monthplot(pimpf_saz)
seas_pimpf_saz <- seas(x = pimpf_saz)
summary(seas_pimpf_saz)

seasonal::qs(seas_pimpf_saz)

pimpf_sazdessaz <- series(seas_pimpf_saz, "s11")
plot(pimpf_saz)
plot(seas_pimpf_saz)
pimpf_saz_dessaz <- pimpf_sazdessaz
plot(pimpf)
plot(pimpf_saz)
plot(pimpf_saz_dessaz)


# PIMPF D
pimpf
monthplot(pimpf)
seas_pimpf <- seas(x = pimpf)
summary(seas_pimpf)

seasonal::qs(seas_pimpf)

pimpfdessaz <- series(seas_pimpf, "s11")
plot(pimpf)
plot(seas_pimpf)
pimpf <- pimpfdessaz

# Séries de hiato ####

# Obtendo tendencia da PIMPF via filtro HP e diferença #

pimpf.hp <- hpfilter((pimpf), freq = 12, type = 'frequency')

par(mfrow=c(3,1))
plot(pimpf.hp$trend)
plot(pimpf)
plot((pimpf)-pimpf.hp$trend)
pimpf.hiato <- ((pimpf)-pimpf.hp$trend)
pimpf.hiato

lpimpf.hp <- hpfilter(log(pimpf), freq = 12, type = 'frequency')

par(mfrow=c(3,1))
plot(pimpf.hp$trend)
plot(pimpf)
plot(log(pimpf)-lpimpf.hp$trend)
lpimpf.hiato <- (log(pimpf)-lpimpf.hp$trend)
lpimpf.hiato

# plot(pimpf.hiato)
# plot(lpimpf.hiato)
# plot(pimpf.hiato)
# 
# # Obtendo tendencia do desemprego via filtro HP e diferença #
# desemprego.hp <- hpfilter(desemprego, freq = 12, type = 'frequency')
# 
# par(mfrow=c(3,1))
# plot(desemprego.hp$trend)
# plot(desemprego)
# plot((desemprego-desemprego.hp$trend))
# desemprego.hiato <- (desemprego-desemprego.hp$trend)
# desemprego.hiato
# 
# 
# # Obtendo tendencia da capacidade via filtro HP e diferença #
# capacidade.hp <- hpfilter((capacidade), freq = 12, type = 'frequency')
# 
# par(mfrow=c(3,1))
# plot(capacidade.hp$trend)
# plot(capacidade)
# plot((capacidade)-capacidade.hp$trend)
# capacidade.hiato <- ((capacidade)-capacidade.hp$trend)
# capacidade.hiato
# 
# lcapacidade.hp <- hpfilter(log(capacidade), freq = 12, type = 'frequency')
# 
# par(mfrow=c(3,1))
# plot(lcapacidade.hp$trend)
# plot(log(capacidade))
# plot(log(capacidade)-lcapacidade.hp$trend)
# lcapacidade.hiato <- (log(capacidade)-lcapacidade.hp$trend)
# lcapacidade.hiato
# 
# capacidade

# Obtendo tendencia do pib via filtro HP e diferença #
pibmensal.hp <- hpfilter((pibmensal), freq = 12, type = 'frequency')

par(mfrow=c(3,1))
plot(pibmensal.hp$trend)
plot(pibmensal)
plot((pibmensal)-pibmensal.hp$trend)
pibmensal.hiato <- ((pibmensal)-pibmensal.hp$trend)
pibmensal.hiato



#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$##
#                                                                     #
#                     DADOS MENSAIS - 1999-2020                       #
#                                                                     #
#######################################################################

# Verificando tamanho dos dados (Deve ser igual a 242 obs)
length(ipca)
length(cambio)
length(comm)
length(capacidade)
length(ipcaindice)

# Exportando os dados

# Coluna de data

date = seq(from = as.Date("1999-07-01"), to = as.Date("2020-02-01"), by = 'month')

length(date)

#######################################################################
#                                                                     #
#                     DADOS TRIMESTRAIS                               #
#                                                                     #
#######################################################################

# Série do PIB
pib <- read_excel('Dados/pibtrimreal.xlsx', col_names = c('data','Taxa12meses','Taxatrimestral'))
pib_real <- read_excel('Dados/pib_real.xlsx', col_names = c('data','pib'))
pib_real <- ts(pib_real[4:nrow(pib_real),2], start = c(1999,4), end = c(2019,4), frequency = 4)
pib_real

pib.trim <- ts(pib[15:nrow(pib),3], start = c(1999,4), end = c(2019,4), frequency = 4)
pib.trim12 <- ts(pib[14:nrow(pib),2], start = c(1999,3), end = c(2019,4), frequency = 4)


pib.hiato <- read_excel('Dados/hiatodoproduto.xlsx', col_names = c('data','hiato'))
pib.hiato <- ts(pib.hiato[29:nrow(pib.hiato),2],  start = c(1999,4), end = c(2019,4), frequency = 4) 

# SÉRIE DE CALCULO PARA LIMIAR DE CICLO #### 
pib.trimMA <- ts(pib[1:nrow(pib),3], start = c(1996,2), end = c(2019,4), frequency = 4)
pib.trim12 <- ts(pib[1:nrow(pib),2], start = c(1996,2), end = c(2019,4), frequency = 4)

pib.trim12.3 <- SMA(pib.trim12,3)
pib.trim12.5 <- SMA(pib.trimMA,7)
pib.trim12.7 <- SMA(pib.trim12,7)
pib.trim12.7

length(pib.trim)
length(pib.trim12.3)
length(pib.trim12.5)
length(pib.trim12.7)

pib.trim12

par(mfrow=c(4,1))
plot.ts(pib.trim)
plot.ts(pib.trim12.5)
plot.ts(pib.trim12.3)
plot.ts(pib.trim12.7)

# Escolhida
pibma7 <- pib.trim12.7[15:length(pib.trim12.7)]
pibma7

# Transformando em objetos xts ####
ipca <- as.xts(ipca)
igp <- as.xts(igp)
ipa <- as.xts(ipa)
tradables <- as.xts(tradables)
nontradables <- as.xts(nontradables)
monitorados <- as.xts(monitorados)
cambio <- as.xts(cambio)
capacidade <- as.xts(capacidade)
comm <- as.xts(comm)
pimpf <- as.xts(pimpf)
desemprego <- as.xts(desemprego)
date <- as.xts(date)
pimpf.hiato <- as.xts(pimpf.hiato)
lpimpf.hiato <- as.xts(lpimpf.hiato)
ipcaindice <- as.xts(ipcaindice)
# cambioMA3 <- as.xts(ts(cambioMA3,start = c(1999,7), end = c(2020,2), frequency = 12))
# cambioMA6 <- as.xts(ts(cambioMA6,start = c(1999,7), end = c(2020,2), frequency = 12))
pib.hiato <- as.xts(pib.hiato)



# Transformando em dados trimestrais
ipca.trim <- apply.quarterly(ipca, mean)
igp.trim <- apply.quarterly(igp, mean)
ipa.trim <- apply.quarterly(ipa, mean)
tradables.trim <- apply.quarterly(tradables, mean)
nontradables.trim <- apply.quarterly(nontradables, mean)
monitorados.trim <- apply.quarterly(monitorados, mean)
cambio.trim <- apply.quarterly(cambio, mean)
comm.trim <- apply.quarterly(comm, mean)
capacidade.trim <- apply.quarterly(capacidade, mean)
pimpf.trim <- apply.quarterly(pimpf, mean)
date.trim <- seq(from = as.Date("1999-10-01"), to = as.Date("2019-12-01"), by = 'quarter')
desemprego.trim <- apply.quarterly(desemprego, mean)
# capacidade.hiato.trim <- apply.quarterly(capacidade.hiato, mean)
# pimpf.hiato.trim <- apply.quarterly(pimpf.hiato, mean)
lpimpf.hiato.trim <- apply.quarterly(lpimpf.hiato, mean)
ipcaindice.trim <- apply.quarterly(ipcaindice, mean)
# tcambio.trim <- apply.quarterly(tcambio,mean)
# cambioMA3.trim <- apply.quarterly(cambioMA3,mean)
# cambioMA6.trim <- apply.quarterly(cambioMA6,mean)
par(mfrow=c(1,1))

# Exportando os dados ####

# Ajustando tamanho de obs (Retirar 3T/1999 e 1T/2020 por causa de possível quebra estrutural)

ipca.trim <- ipca.trim[2:(length(ipca.trim)-1)]
igp.trim <- igp.trim[2:(length(igp.trim)-1)]
ipa.trim <- ipa.trim[2:(length(ipa.trim)-1)]
monitorados.trim <- monitorados.trim[2:(length(monitorados.trim)-1)]
tradables.trim <- tradables.trim[2:(length(tradables.trim)-1)]
nontradables.trim <- nontradables.trim[2:(length(nontradables.trim)-1)]
capacidade.trim <- capacidade.trim[2:(length(capacidade.trim)-1)]
pimpf.trim <- pimpf.trim[2:(length(pimpf.trim)-1)]
desemprego.trim <- desemprego.trim[2:(length(desemprego.trim)-1)]
comm.trim <- comm.trim[2:(length(comm.trim)-1)]
cambio.trim <- cambio.trim[2:(length(cambio.trim)-1)]
# pimpf.hiato.trim <- pimpf.hiato.trim[2:(length(pimpf.hiato.trim)-1)]
lpimpf.hiato.trim <- lpimpf.hiato.trim[2:(length(lpimpf.hiato.trim)-1)]
ipcaindice.trim <- ipcaindice.trim[2:(length(ipcaindice.trim)-1)]
# cambioMA3.trim <- cambioMA3.trim[2:(length(cambioMA3.trim)-1)]
# cambioMA6.trim <- cambioMA6.trim[2:(length(cambioMA6.trim)-1)]

# Verificando nº de obs
length(ipca.trim)
length(capacidade.trim)
length(desemprego.trim)
length(comm.trim)
length(cambio.trim)
length(pib.trim)
length(pib.hiato)

dadosbrutos.trim <- tibble(date.trim,
                           ipca.trim,
                           igp.trim,
                           ipa.trim,
                           tradables.trim,
                           nontradables.trim,
                           monitorados.trim,
                           capacidade.trim,
                           pib.trim,
                           desemprego.trim,
                           comm.trim,
                           cambio.trim[,1],
                           pimpf.trim,
                           lpimpf.hiato.trim,
                           ipcaindice.trim,
                           pib.hiato,
                           pibma7,
                           pib_real,
                           .name_repair = ~  c("date",
                                               "ipca",
                                               'igp',
                                               'ipa',
                                               'tradables',
                                               'nontradables',
                                               'monitorados',
                                               "capacidade",
                                               "pib_g",
                                               "desemprego",
                                               "comm",
                                               "cambio",
                                               "pimpf",
                                               "lpimpf.hiato",
                                               "ipcaindice",
                                               "pib.hiato",
                                               "pibma7",
                                               'pib'))



