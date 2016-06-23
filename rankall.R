rankall <- function(outcome, num = "best") {
	## Read outcome data
	d <- read.csv("outcome-of-care-measures.csv",colClasses="character")
	## Check that state and outcome are valid
	if (outcome == "heart attack") {n <- 11
        }else if (outcome == "heart failure") {n <- 17
        }else if (outcome == "pneumonia") {n <- 23
        }else {stop("invalid outcome")}
	
	## For each state, find the hospital of the given rank
	t <- d[,c(2,7,n)]
	t[,3] <- as.numeric(t[,3])
	t <- t[!is.na(t[,3]),]
	s <- split(t, t[,2])	
	rv <- sapply(s, function(le,n){le[order(le[,3],le[,1]),][ifelse(n=="best",1,ifelse(n=="worst", nrow(le), as.numeric(n))),1]},num)
	## Return a data frame with the hospital names and the
	## (abbreviated) state name
	rd <- data.frame(hospital=rv,state=names(rv))
        return(rd)
}
