
// generalized binomial test
capture program drop genbinomtest
program define genbinomtest

/* version 12.0 */
    args N succ p
/* syntax N(integer) succ(integer) p(real 0.5) */

    display as text  _newline _column(15) "Generalized Binomial test" _newline

    display as text _column(8) "N   Observed k   Expected k   Assumed p   Observed p"
    display as text "{hline 59}"
    display as result %8.0g `N' "   " %9.0g `succ' "    " %9.0g `N'*`p' "   " %9.0g `p' "   " %9.0g `succ'/`N'


    /* local res */
if `succ' <= (`N' * `p' - 1) {
    display binomial(`N', `succ', `p')
}
else if `succ' < `N' * `p' {
    display `succ'
}
else {
    display `p'
}
end


/* . bitesti 10 3 0.8 */

/*         N   Observed k   Expected k   Assumed p   Observed p */
/* ------------------------------------------------------------ */
/*        10          3            8       0.80000      0.30000 */

/*   Pr(k >= 3) = 0.999922  (one-sided test) */
/*   Pr(k <= 3) = 0.000864  (one-sided test) */
/*   Pr(k <= 3) = 0.000864  (two-sided test) */
