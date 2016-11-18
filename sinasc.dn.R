# sinasc.dn.R 
# 2016 Denisson Silva
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU Affero General Public License as published
# by the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU Affero General Public License for more details.
#
# You should have received a copy of the GNU Affero General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

#' Provides Access to the Declarations of Death (Children) Dataset from the Mortality Information System (SIM).
#'
#' \code{sim.domat} returns a data.frame with a subset of the Declarations of Death - Maternal (DOMAT) dataset
#' 
#' @details
#' The Mortality Information System (SIM) offers health managers, researchers and institutions highly relevant information for defining priorities for disease prevention and control programmes, based on death statement information collected by the State Health Departments. The national Database generated from this information is administered by the Health Surveillance Secretariat in cooperation with DATASUS.
#'
#' The system works through the filling in and collection of a standard document, the Death Statement (Declaration of Death), which is entered onto the system in the states and municipalities. The data collected is of great importance for health surveillance and epidemiological analysis, as well as health and demographical statistics.
#'
#' This system contains data divided in the following categories:
#' 
#' \itemize{
#'  \item DO: Declarations of death  
#'  \item DOFET: Declarations of death (Fetal)  
#'  \item DOEXT: Declarations of death (External Causes)  
#'  \item DOINF: Declarations of death (Children)  
#'  \item DOMAT: Declarations of death (Maternal)
#' }
#' 
#' The \code{sim.*} functions are provided for each available subsystem, for example, \code{sim.dofet}, \code{sim.doext} and so on.
#' 
#' @note
#' DATASUS is the name of the Department of Informatics of the Brazilian Unified Health System (SUS) and is resposible for publishing public healthcare data. Besides the DATASUS, the Brazilian National Agency for Supplementary Health (ANS) also uses this file format for its public data. The name DATASUS is also often used to represent the public datasets they provide.
#'
#' Neither this project, nor its author, has any association with the brazilian government.
#' @param years  numeric; one or more years representing the data to be read
#' @param english logical; translates column names from Portuguese (Brazil) to English. Default is TRUE.
#' @return a data.frame with Brazil's mortality data
#' @keywords datasus
#' @export
#' @author Denisson Silva, \email{denissonsilva@ufmg.br}
#' @seealso \code{\link{datasus.init}} \code{\link{read.dbc}}
#' @examples
#'
#' pr10 <- sinasc.dn (2010)
sinasc.dn <- function(years) {
  
  sinasc <- data.frame()
  
  for(i in years) {
    for (y  in c('AC', 'AL', 'AP', 'AM',
                 'BA', 'CE', 'DF', 'ES',
                 'GO', 'MA', 'MT', 'MS',
                 'MG', 'PA', 'PB', 'PR',
                 'PE', 'PI', 'RJ', 'RN',
                 'RS', 'RO', 'RR', 'SC',
                 'SP', 'SE', 'TO')) {
      
    
    # Prepare local file names and remote download paths
    
    filenames  <- paste0("DN", y , i, ".dbc")
    
    localnames <- file.path(tempdir(), filenames)
    
    url.base   <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SINASC/NOV/DNRES"
    
    if( !file.exists(localnames) ) {
      # Try default path
      url <- paste(url.base, filenames, sep = "/" )
      
      if( download.file(url, destfile = localnames) ) {
        # Some of the files are postfixed as .DBC (uppercase), try again on error
        url <- paste(url.base, toupper(filenames), sep = "/" )
        download.file(url,  destfile = localnames)
      }
    }
    
    # Select which fields of the files will be loaded considerando nao 2000 como referencia
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
    
    
    
    # Load downloaded files into a data.frame
      df <- read.dbc::read.dbc(localnames, as.is = TRUE)
      
      # Select only applicable fields
      df <- df[, fields]
      
      # This column is used as an indicator of which file originated the data
      df$dataset <- filenames
      
      sinasc <- rbind(df, sinasc, make.row.names = FALSE)
    }
  }
  # Returns data.frame
     
  sinasc
}  