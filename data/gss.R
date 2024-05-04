# Data from GSS

ex <- read.csv("./data/gss.txt", na.string = c("0", "8", "9"))
year <- ex$YEAR
ex$SATJOB[ex$SATJOB == 4] <- 3
ex <- (ex[c(3, 4, 5, 6, 7, 8)]) # CAPPUN(D),GUNLAW(G),SEX(S),ABRAPE(A),CONFINAN(F),SATJOB(J)
names(ex) <- c("D", "G", "S", "A", "B", "J")
data <- ex
tab <- table(ex)
cat("Loaded `data` and `tab`.\n")
