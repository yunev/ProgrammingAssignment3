best <- function(state, outcome) {
	# Read outcome data
	d <- read.csv("outcome-of-care-measures.csv",colClasses="character")
	# Check that state and outcome are valid
	if (!(state %in% d$State)) stop("invalid state")
	if (outcome == "heart attack") {n <- 11
	}else if (outcome == "heart failure") {n <- 17
	}else if (outcome == "pneumonia") {n <- 23
	}else {stop("invalid outcome")}
	# Return hospital in the state with lowest 30-day rate
	f <- d[d$State==state,c(2,n)] # filter by state
	f[,2] <- as.numeric(f[,2]) # translate to numbers
	f <- f[!is.na(f[,2]),] # filter NAs
	s <- f[order(f[,2], f[,1]),]
	return(s[1,1])
}
