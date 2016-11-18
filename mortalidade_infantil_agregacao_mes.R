# --------------------------------------------- #
# Denisson Silva - denissonsilva@ufmg.br        #
# Dalson Britto  - dalsonbritto@yahoo.com.br    #
# Lucas Silva    - lukasemanoel@gmail.com       #
# _____________________________________________ #
# 1. baixar dados mortalidade infantil          #
# 2. baixar dados nascidos vivos                #
# 3. merge das bases de dados - Agregacao ANO   #
# --------------------------------------------- #

# carregando o pacote 
library(datasus)
# carregando a função sinasc.dn
source("sinasc.dn.R")

# iniciando um diretorio temporario via ftp.datasus
datasus.init()

# baixando dados mortalidade infantil
mort_infantil <- sim.doinf(c(2000:2014),  language =  "pt")
mort_infantil$Ano <- substr(mort_infantil$DTOBITO, 5, 8)
mort_infantil[mort_infantil$Ano == "", 41] <- 2000 

saveRDS(mort_infantil, "mortalidade_infantil2000_2014.Rda")

# baixando nascidos vivos 
# foi feita em duas etapas porque as duas bases juntas tem quase 10GB
# e tambem tem codigos do ibge diferente
nascidosVivos <- sinasc.dn(c(2000:2005))
saveRDS(nascidosVivos, "nascidosVivos_2000_2005.Rda")

# apagando objeto
rm("nascidosVivos")

nascidosVivos <- sinasc.dn(c(2006:2014))
saveRDS(nascidosVivos, "nascidosVivos_2006_2014.Rda")

### --------------------------------------------------------------------------------
# trantando os dados

# carregando base com os dois codigos IBGE6 e IBGE7
mun <- read.csv2("mun.csv")

mort_infantil <- readRDS("mortalidade_infantil2000_2014.Rda")
mort_infantil$Total_mort_inf <- 1
mort_infantil$mes <- substr(mort_infantil$DTOBITO, 3, 4)

# agregando por Municipio e Ano
mort_infantil <- aggregate(Total_mort_inf ~ Ano + mes + CODMUNRES, data = mort_infantil, sum)

mort_infantil1 <- mort_infantil[mort_infantil < 2006, ] 
mort_infantil1 <- merge(mort_infantil1, mun[, 1:2], by.x = "CODMUNRES", by.y = "IBGE7", all.x = T)
names(mort_infantil1)[1] <- "IBGE7"

mort_infantil2 <- mort_infantil[mort_infantil > 2005, ] 
mort_infantil2 <- merge(mort_infantil2, mun[, 1:2], by.x = "CODMUNRES", by.y = "IBGE6", all.x = T)
names(mort_infantil2)[1] <- "IBGE6"

mort_infantil1 <- mort_infantil1[, c(5, 1:4)]
mort_infantil2 <- mort_infantil2[, c(1, 5, 2:4)]

mort_infantil <- rbind(mort_infantil1, mort_infantil2)
mort_infantil <- mort_infantil[!is.na(mort_infantil$Total_mort_inf), ]

rm("mort_infantil1", "mort_infantil2")

nascido0005 <- readRDS("nascidosVivos_2000_2005.Rda")
nascido0005$Ano <- substr(nascido0005$DTNASC, 5, 8)
nascido0005$Total_NascidoVivo <- 1
nascido0005$mes <- substr(nascido0005$DTNASC, 3, 4)
nascido0005 <- aggregate(Total_NascidoVivo ~ Ano + mes + CODMUNRES, data = nascido0005, sum)
nascido0005 <- merge(nascido0005, mun, by.x = "CODMUNRES", by.y = "IBGE7", all.x = T)
names(nascido0005)[1] <- "IBGE7"

nascido0614 <- readRDS("nascidosVivos_2006_2014.Rda")
nascido0614$Ano <- substr(nascido0614$DTNASC, 5, 8)
nascido0614$Total_NascidoVivo <- 1
nascido0614$mes <- substr(nascido0614$DTNASC, 3, 4)
nascido0614 <- aggregate(Total_NascidoVivo ~ Ano + mes + CODMUNRES, data = nascido0614, sum)
nascido0614 <- merge(nascido0614, mun, by.x = "CODMUNRES", by.y = "IBGE6", all.x = T)
names(nascido0614)[1] <- "IBGE6"

nascido0005 <- nascido0005[, c(5, 1:4, 6:15)]
nascido0614 <- nascido0614[, c(1, 5, 2:4, 6:15)]
nascidosVivos <- rbind(nascido0005, nascido0614)

rm("nascido0005", "nascido0614")

# merge das bases nascidos vivos e mortalidade infantil municipio ano
base <- merge(nascidosVivos, mort_infantil, by = c("Ano","IBGE6" , "IBGE7"), all = T)

base[is.na(base$Total_NascidoVivo), 4] <- 0
base[is.na(base$Total_mort_inf), 16] <- 0

base$Taxa_mort_infantil  <- round((base$Total_mort_inf / base$Total_NascidoVivo) * 1000, 2)

# incluindo a variavel regiao
regiao <- read.csv2("reg.csv")

base <- merge(base, regiao, by = "UFCOD", all.x = T)
base <- base[!duplicated(base),]

# salvando a base com aggregacao municipio ano
write.csv2(base, "indicadores_mortalidade_infantil_mes.csv")
saveRDS(base, "indicadores_mortalidade_infantil_mes.Rda")
