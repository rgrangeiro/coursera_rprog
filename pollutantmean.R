
###   EXEMPLO
### source("pollutantmean.R")
### pollutantmean("specdata", "sulfate", 1:10)
## [1] 4.064
### pollutantmean("specdata", "nitrate", 70:72)
## [1] 1.706
### pollutantmean("specdata", "nitrate", 23)
## [1] 1.281  

   
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
         
         ## Verifica se o valor de 'pollutant' é sulfate ou nitrate
         
         if(pollutant == "sulfate" || pollutant == "nitrate"){
            
				pollutant_mean <- c()
         	
            for(i in id) {

            	## lê os arquivos, formatando o id para um numeral de tres caracteres
            	
            	current_file <- read.csv(paste0("./",directory,"/",(formatC(i, width = 3, flag = "0")),".csv"), header=T, sep=",");head(current_file)
            	
            	## remove os valores NA
            	
            	na_removed <- current_file[!is.na(current_file[, pollutant]), pollutant]
            	
            	## adiciona ao dataset do id anterior o dataset com os valores deste id
            	
            	pollutant_mean <- c(pollutant_mean, na_removed)
            	
            	## aplica mean ao dataset final

            	result <- mean(pollutant_mean)
            	
            	}
            
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
   
   ## informa o resultado
   
   return(round(result, 3))
}