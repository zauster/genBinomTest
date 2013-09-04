
## print.genbinom
## a function to display the output of the generalized binomial test

## Proposal: (STATA-like)
##    N   Observed k   Expected k   Assumed p   Observed p
## -------------------------------------------------------
##   10          7            4       0.40000      0.70000

print.genbinom <- function(x, digits = 5, prefix = "", ...)
    {
    cat("\n")
    cat(strwrap(x$method, prefix = "\t"), sep="\n")
    cat("\n")
    cat("data: ")#, x$data.name, "\n")
    cat("\n")
    cat("N\tObserved k\tExpected k\n")
    cat(x$n, "\t", x$x, "\t\t", format(x$n * x$p, digits = 5),
        "\n", sep = "")
    cat("\tAssumed p\tObserved p\n")
    cat("\t", format(x$p, digits = digits), "\t\t",
        format(x$x / x$n, digits = digits), "\n", sep = "")

    ## cat("alternative:", x$alternative, "\n")
    cat("\nP Values:\n")
    cat("\tP(X >= ", x$x, "):\t\t",
        format(x$pvalues[2], digits = digits), "\n", sep = "")
    cat("\tP(X <= ", x$x, "):\t\t",
        format(x$pvalues[1], digits = digits), "\n", sep = "")
    cat("\tP(X < ", x$twosidedbounds[1],
        ", X > ", x$twosidedbounds[2], "):\t",
        format(x$pvalues[3], digits = digits), "\n", sep = "")

    if(!is.null(x$conf.int))
        {
        cat(format(100 * attr(x$conf.int, "conf.level")),
            "percent confidence interval:\n",
            format(c(x$conf.int[1L], x$conf.int[2L])), "\n")
    }

    ## if(!is.null(x$estimate)) {
    ##     cat("\n\nsample estimates:\n")
    ##     print(x$estimate, ...)
    ## }
    cat("\n")
    invisible(x)
    }
