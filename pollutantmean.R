directory <- 'specdata'
pollutant <- 'nitrate'
id <- 1:10

'''   EXEMPLO
source("pollutantmean.R")
pollutantmean("specdata", "sulfate", 1:10)
## [1] 4.064
pollutantmean("specdata", "nitrate", 70:72)
## [1] 1.706
pollutantmean("specdata", "nitrate", 23)
## [1] 1.281  
'''
   
## Part 1 
pollutantmean <- function(directory, pollutant, id = 1:332) {
   ## 'directory' is a character vector of length 1 indicating
   ## the location of the CSV files
   ## 'pollutant' is a character vector of length 1 indicating
   ## the name of the pollutant for which we will calculate the
   ## mean; either "sulfate" or "nitrate".
   ## 'id' is an integer vector indicating the monitor ID numbers
   ## to be used
   ## Return the mean of the pollutant across all monitors list
   ## in the 'id' vector (ignoring NA values)
   ## NOTE: Do not round the result!
   ## Verifica existencia do diretório 'specdata'
   if (length(directory) == 1 || class(directory) == "character") {
      
      if(file.exists(directory) == TRUE) {
         
         print("'directory' specdata encontrado")
         ## Verifica se o valor de 'pollutant' é sulfate ou nitrate
         
         if(pollutant == "sulfate" || pollutant == "nitrate"){
            
            print("'pollutant' informado corretamente")
            
            ###  ----------- manutenção da primeira parte inicia aqui
            
            ###  ----- tratar os datasets removendo os NA
            
            pollutant_mean <- c()
            
            for(i in id) {
            
               id <- 2
               current_file <- read.csv(paste0("./",directory,"/",(formatC(id, width = 3, flag = "0")),".csv"), header=T, sep=",");head(current_file)
               
               ###  teste utilizando 'nitrate'
               na_removed <- current_file[!is.na(current_file[, "nitrate"]), "nitrate"]
               ###  teste utilizando 'nitrate'
               
               pollutant_mean <- c(pollutant_mean, na_removed);pollutant_mean
            }
            
            result <- mean(pollutant_mean)

            ###  ----------- manutenção da primeira parte finaliza aqui
            
         } else {
            
            print("'pollutant' não informado corretamente")
         }
                  
      } else {
         
         print("'directory' specdata não encontrado")
      }
   }
   else {
      
      print("'directory' informado de forma errada.")
   }
   
   return(round(result, 3))
}