rankhospital <- function(state, outcome, num = "best") {
	## Read outcome data
	d <- read.csv("outcome-of-care-measures.csv",colClasses="character")
	## Check that state and outcome are valid
	if (!(state %in% d$State)) stop("invalid state")
        if (outcome == "heart attack") {n <- 11
        }else if (outcome == "heart failure") {n <- 17
        }else if (outcome == "pneumonia") {n <- 23
        }else {stop("invalid outcome")}
	## Return hospital name in that state with the given rank 30-day death rate
	f <- d[d$State==state,c(2,n)] # filter by state
        f[,2] <- as.numeric(f[,2]) # translate to numbers
        f <- f[!is.na(f[,2]),] # filter NAs
        s <- f[order(f[,2], f[,1]),]
	if (num == "best") {i <- 1
	}else if (num == "worst") {i <- nrow(s)
	}else {try(i <- as.numeric(num))}
	if (i > nrow(s)) return(NA) 
        return(s[i,1])
}
