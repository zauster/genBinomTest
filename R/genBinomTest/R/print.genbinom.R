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
    cat("\tAssumed p\tThreshold p\n")
    cat("\t", format(x$p, digits = digits), "\t\t",
        format(x$x / x$n, digits = digits), "\n", sep = "")

    ## cat("alternative:", x$alternative, "\n")
    cat("\np-Value of H_0:\t\t\t\tConfidence Intervals\n")
    cat("\tp <= ", x$p, ":\t",
        format(x$pvalues["pval.upper"], digits = digits,
               width = 8), sep = "")
    cat("\t", format(c(x$ci.upper[1], x$ci.upper[2]),
                     digits = digits, width = 8), "\n")

    cat("\tp >= ", x$p, ":\t",
        format(x$pvalues["pval.lower"], digits = digits,
               width = 8), sep = "")
    cat("\t", format(c(x$ci.lower[1], x$ci.lower[2]),
                     digits = digits, width = 8), "\n")

    cat("\tp = ", x$p, ":\t",
        format(x$pvalues["pval.twosided"], digits = digits,
               width = 8), sep = "")
    cat("\t", format(c(x$ci.twosided[1], x$ci.twosided[2]),
                     digits = digits, width = 8), "\n")

    if(!is.null(x$conf.int))
        {
            cat("\n")
            cat(format(100 * (1 - x$alpha)),
                "percent confidence interval:\n",
                format(c(x$conf.int[1L], x$conf.int[2L]),
                       digits = digits), "\n")
    }

    ## if(!is.null(x$estimate)) {
    ##     cat("\n\nsample estimates:\n")
    ##     print(x$estimate, ...)
    ## }
    cat("\n")
    invisible(x)
    }
