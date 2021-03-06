### source("corr.R")
### source("complete.R")
### cr <- corr("specdata", 150)
### head(cr)
## [1] -0.01896 -0.14051 -0.04390 -0.06816 -0.12351 -0.07589
### summary(cr)
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
## -0.2110 -0.0500  0.0946  0.1250  0.2680  0.7630
### cr <- corr("specdata", 400)
### head(cr)
## [1] -0.01896 -0.04390 -0.06816 -0.07589  0.76313 -0.15783
### summary(cr)
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
## -0.1760 -0.0311  0.1000  0.1400  0.2680  0.7630
### cr <- corr("specdata", 5000)
### summary(cr)
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
## 
### length(cr)
## [1] 0
### cr <- corr("specdata")
### summary(cr)
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
## -1.0000 -0.0528  0.1070  0.1370  0.2780  1.0000
### length(cr)
## [1] 323

corr <- function(directory, threshold = 0) {
	## 'directory' is a character vector of length 1 indicating
	## the location of the CSV files
	
	## 'threshold' is a numeric vector of length 1 indicating the
	## number of completely observed observations (on all
	## variables) required to compute the correlation between
	## nitrate and sulfate; the default is 0
	
	## Return a numeric vector of correlations
	## NOTE: Do not round the result!
	if (length(directory) == 1 || class(directory) == "character") {
		
		if(file.exists(directory) == TRUE) {
			
			## Lista os arquivos dentro do diretorio
			
			files <- dir(directory)
			
			## inicia variavel
			
			result <- c()
			
			## loop listando os arquivos
			
			for(i in files) {
				
				## lê os arquivos, formatando o id para um numeral de tres caracteres
				
				current_file <- read.csv(paste0("./",directory,"/",(formatC(i, width = 3, flag = "0"))), header=T, sep=",")
				
				## remove os valores NA
				
				clean_data <- na.omit(current_file)
				
				## verifica threshold
				
				if (nrow(clean_data) > threshold ) {
					
					result <- c(result,cor(clean_data$sulfate,clean_data$nitrate))
					
				}
				
			}
			
		} else {
			
			print("'directory' specdata não encontrado")
		}
	}
	
	else {
		
		print("'directory' informado de forma errada.")
	}
	
	## informa o resultado
	
	return(result)
	
}