p <- c( 21, 2, 5, 13, 4, 11, 16, 1)
lienert <- cbind(expand.grid(X1 = c(0,1), X2 = c(0,1), X3 = c(0,1) ), Freq = p)
lienert_data <- xtabs(Freq ~ ., lienert)