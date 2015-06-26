## source("best.R")
## best("TX", "heart attack")

## Write a function called best that take two arguments: the 2-character abbreviated name of a state and an
## outcome name. The function reads the outcome-of-care-measures.csv file and returns a character vector
## with the name of the hospital that has the best (i.e. lowest) 30-day mortality for the specified outcome
## in that state. The hospital name is the name provided in the Hospital.Name variable. The outcomes can
## be one of “heart attack”, “heart failure”, or “pneumonia”. Hospitals that do not have data on a particular
## outcome should be excluded from the set of hospitals when deciding the rankings.

## Handling ties. If there is a tie for the best hospital for a given outcome, then the hospital names should
## be sorted in alphabetical order and the first hospital in that set should be chosen (i.e. if hospitals “b”, “c”,
## and “f” are tied for best, then hospital “b” should be returned).

## The function should check the validity of its arguments. If an invalid state value is passed to best, the
## function should throw an error via the stop function with the exact message “invalid state”. If an invalid
## outcome value is passed to best, the function should throw an error via the stop function with the exact
## message “invalid outcome”.

best <- function(state, outcome) {
	
	## Read outcome data
	outcome_data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
	outcome_data[outcome_data == "Not Available"] <- NA
	
	#hosp_30days_mort <- (outcome[,c(2,7,11,17,23)])
	#names(hosp_30days_mort) <- c("Hospital.Name","State","Rates.Heart_Attack","Rates.Heart_Failure","Rates.Pneumonia")
	
	## Heart Attack dataframe
	dt_heart.attack <- na.omit(outcome_data[,c(2,7,11)])
	names(dt_heart.attack) <- c("Hospital.Name","State","Rates.Heart_Attack")
	
	## Heart Failure dataframe
	dt_heart.failure <- na.omit(outcome_data[,c(2,7,17)])
	names(dt_heart.failure) <- c("Hospital.Name","State","Rates.Heart_Failure")
	
	## Pneumonia dataframe
	dt_pneumonia <- na.omit(outcome_data[,c(2,7,23)])
	names(dt_pneumonia) <- c("Hospital.Name","State","Rates.Pneumonia")
	
	## Valid State and outcome lists
	valid_states <- unique((outcome_data[,c(7)]))
	valid_outcome <- c("heart attack","heart failure","pneumonia")
	
	## Check that state and outcome are valid
	ifelse(state %in% valid_states, flag_state <- "ok", flag_state <- "nok")
	ifelse(outcome %in% valid_outcome, flag_outcome <- "ok", flag_outcome <- "nok")
	
	if (flag_state == "ok" ) {
		
		if (flag_outcome == "ok" ) {
			
			
			if ( outcome == "heart attack" ) {
				
				## check best hospital - heart attack
				## print("h-att")
				dt_state <- dt_heart.attack[which(dt_heart.attack$State == state),]
				sort_dt_state <- dt_state[order(as.numeric(dt_state$Rates.Heart_Attack),dt_state$Hospital.Name),]
				
				if(!is.na(sort_dt_state[1,3]==sort_dt_state[2,3] || dim(dt_state)[1] != 1)) {
					
					sort_hosp <- rbind(sort_dt_state[1,c(1,3)],sort_dt_state[2,c(1,3)])
					result <- na.omit(sort_hosp[order(sort_hosp$Hospital.Name),])
					print(result[1,1])
					
				} else {
					
					result <- sort_dt_state[1,1]
					print(result)
					
				}
				
			} else {
				
				if ( outcome == "heart failure") {
					
					## check best hospital - heart failure
					## print("h-fai")
					dt_state <- dt_heart.failure[which(dt_heart.failure$State == state),]
					sort_dt_state <- dt_state[order(as.numeric(dt_state$Rates.Heart_Failure),dt_state$Hospital.Name),]
					
					if(!is.na(sort_dt_state[1,3]==sort_dt_state[2,3] || dim(dt_state)[1] != "1")) {
						
						sort_hosp <- rbind(sort_dt_state[1,c(1,3)],sort_dt_state[2,c(1,3)])
						result <- na.omit(sort_hosp[order(sort_hosp$Hospital.Name),])
						print(result[1,1])
						
					} else {
						
						result <- sort_dt_state[1,1]
						print(result)
						
					}
										
				} else {
					
					if ( outcome == "pneumonia") {
						
						## check best hospital - heart pneumonia
						## print("pneu")
						dt_state <- dt_pneumonia[which(dt_pneumonia$State == state),]
						sort_dt_state <- dt_state[order(as.numeric(dt_state$Rates.Pneumonia),dt_state$Hospital.Name),]
						
						if(!is.na(sort_dt_state[1,3]==sort_dt_state[2,3] || dim(dt_state)[1] != "1")) {
							
							sort_hosp <- rbind(sort_dt_state[1,c(1,3)],sort_dt_state[2,c(1,3)])
							result <- na.omit(sort_hosp[order(sort_hosp$Hospital.Name),])
							print(result[1,1])
							
						} else {
							
							result <- sort_dt_state[1,1]
							print(result)
							
						}
					}
				}
			}
			
		} else {
			
			stop('invalid outcome') 
			
		}
		
	} else {
		
		stop('invalid state') 
		
	}
	
}