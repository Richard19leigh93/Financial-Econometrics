# Selecting appropriate ARMA model

ARMA_sel_func <- function(x) {cl = makePSOCKcluster(10)

AC = autoarfima(as.numeric(x), ar.max = 2, ma.max = 2, 
                criterion = "AIC", method = "partial", arfima = FALSE, include.mean = NULL, 
                distribution.model = "norm", solver = "solnp", cluster = cl)

show(head(AC$rank.matrix))}