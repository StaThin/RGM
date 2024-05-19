#' makeP
#'
#' @param B a matrix of parameters
#'
#' @return P a matrix such that P %*% beta = vec(B)
#' @export
#'
#' @examples
`makeP` <- function(B) {
    vb <- as.vector(B)
    pos <- which(vb != 0)
    beta <- vb[pos]
    P <- matrix(0, prod(dim(B)), length(pos))
    P[pos, ] <- diag(length(pos))
    list(P = P, beta = beta)
}

`summaryRCG` <- function(out, n, dig = 3) {
    S <- out$Shat
    I_B <- out$Bhat
    B <- diag(nrow(I_B)) - I_B
    O <- out$Ohat
    P <- makeP(B)
    beta <- P$beta
    P <- P$P
    Q <- makeP(O)
    omega <- Q$beta
    Q <- Q$P

    V <- solve(t(P) %*% (S %x% solve(O)) %*% P) / n
    se_beta <- sqrt(diag(V))

    W <- solve((1 / 2) * t(Q) %*% (solve(O) %x% solve(O)) %*% Q) / n
    se_omega <- sqrt(diag(W))
    arr <- data.frame(beta,
        se = se_beta, LB = beta - 2.58 * se_beta,
        UB = beta + 2.58 * se_beta
    )
    x <- which(B != 0, arr.ind = TRUE)
    arr_names <- paste(rownames(x), rownames(B)[x[, 2]], sep = "|")
    rownames(arr) <- arr_names
    print(round(arr, dig))
    arcs <- data.frame(omega,
        se = se_omega, LB = omega - 2.58 * se_omega,
        UB = omega + 2.58 * se_omega
    )
    x <- which(O != 0, arr.ind = TRUE)
    arc_names <- paste(rownames(x), rownames(O)[x[, 2]], sep = "~")
    rownames(arcs) <- arc_names
    print(round(arcs, dig))
    invisible()
}
