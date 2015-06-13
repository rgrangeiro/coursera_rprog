### source("complete.R")
### complete("specdata", 1)
##   id nobs
## 1  1  117
### complete("specdata", c(2, 4, 8, 10, 12))
##   id nobs
## 1  2 1041
## 2  4  474
## 3  8  192
## 4 10  148
## 5 12   96
### complete("specdata", 30:25)
##   id nobs
## 1 30  932
## 2 29  711
## 3 28  475
## 4 27  338
## 5 26  586
## 6 25  463
### complete("specdata", 3)
##   id nobs
## 1  3  243

complete <- function(directory, id = 1:332) {
	## 'directory' is a character vector of length 1 indicating
	## the location of the CSV files
	
	## 'id' is an integer vector indicating the monitor ID numbers
	## to be used
	
	## Return a data frame of the form:
	## id nobs
	## 1  117
	## 2  1041
	## ...
	## where 'id' is the monitor ID number and 'nobs' is the
	## number of complete cases
	if (length(directory) == 1 || class(directory) == "character") {
		
		if(file.exists(directory) == TRUE) {
			
			## inicia variavel
			
			result <- c()	
			
			## loop para ler todos os arquivos dos ids correspondentes ao informado
			
			for(i in id) {
				
				## lê os arquivos, formatando o id para um numeral de tres caracteres
				
				current_file <- read.csv(paste0("./",directory,"/",(formatC(i, width = 3, flag = "0")),".csv"), header=T, sep=",")
				
				## remove os valores NA
				
				result <- data.frame(rbind(result,c(i,(sum(complete.cases(current_file))))))
				
			}
			
		} else {
			
			print("'directory' specdata não encontrado")
		}
	}
	else {
		
		print("'directory' informado de forma errada.")
	}
	
	## adiciona rotulos às colunas de result
		
	names(result) <- c("id", "nobs")
	
	## informa o resultado
	
	return(result)
}