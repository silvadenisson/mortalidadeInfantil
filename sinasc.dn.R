# sinasc.dn.R 
# 2016 by Denisson Silva
# objetivo da função fazer download do dados de nascidos vivos disponibilizados pelo DATASUS.BR
# a função foi desenolvida a partir da função sim.doinf do pacote datasus https://github.com/danicat/datasus

sinasc.dn <- function(years) {
  
  # cria um data.frame vazio
  sinasc <- data.frame()
  
  for(i in years) {
    for (y  in c('AC', 'AL', 'AP', 'AM',
                 'BA', 'CE', 'DF', 'ES',
                 'GO', 'MA', 'MT', 'MS',
                 'MG', 'PA', 'PB', 'PR',
                 'PE', 'PI', 'RJ', 'RN',
                 'RS', 'RO', 'RR', 'SC',
                 'SP', 'SE', 'TO')) {
      
    # constroi o nome do arquivo
    filenames  <- paste0("DN", y , i, ".dbc")
    
    # cria um diretório temporario
    localnames <- file.path(tempdir(), filenames)
    
    # endereço de coleta
    url.base   <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SINASC/NOV/DNRES"
    
    # conticional para se o arquivo já foi feito o download, na mesma secao do R não fazer novamente
    if( !file.exists(localnames) ) {
      
      url <- paste(url.base, filenames, sep = "/" )
      
      if( download.file(url, destfile = localnames) ) {
        
        url <- paste(url.base, toupper(filenames), sep = "/" )
        download.file(url,  destfile = localnames)
      }
    }
    
    # vetor de variaveis desejadas
    fields     <- c("NUMERODN",
                    "LOCNASC",
                    "CODMUNNASC",
                    "IDADEMAE",
                    "ESTCIVMAE",
                    "ESCMAE",
                    "QTDFILVIVO",
                    "QTDFILMORT",
                    "CODMUNRES",
                    "GESTACAO",
                    "GRAVIDEZ",
                    "PARTO",
                    "CONSULTAS",
                    "DTNASC",
                    "SEXO",
                    "APGAR1",
                    "APGAR5",
                    "RACACOR",
                    "PESO",
                    "CODANOMAL"
    )
    
      # ler o banco de dados com a funcao read.dbc
      df <- read.dbc::read.dbc(localnames, as.is = TRUE)
      
      # selecionando as variaveis 
      df <- df[, fields]
      
      # adcionando o nome do arquivo na base de dados
      df$dataset <- filenames
      
      # juntando as base de dados quando requisitado mais de ano
      sinasc <- rbind(df, sinasc, make.row.names = FALSE)
    }
  }
     
  sinasc
}  