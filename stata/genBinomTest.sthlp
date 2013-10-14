{smcl}
{* *! version 1.0.0  13oct2013}{...}
{vieweralsosee "" "--"}{...}
{vieweralsosee "[R] help" "help help"}{...}
{viewerjumpto "Syntax" "genBinomTest##syntax"}{...}
{viewerjumpto "Description" "genBinomTest##description"}{...}
{viewerjumpto "Remarks" "genBinomTest##remarks"}{...}
{viewerjumpto "Examples" "genBinomTest##examples"}{...}
{viewerjumpto "Saved Results" "genBinomTest##saved_results"}{...}
{title:Title}

{phang}
{bf:genBinomTest} {hline 2} Generalized binomial test for independently (though not identically) distributed variables.


{marker syntax}{...}
{title:Syntax}

{pstd}
Generalized binomial probability test

{p 8 16 2}
{cmd:genBinomTest} {it:#N} {it:#succ} {it:#p}

{marker description}{...}
{title:Description}

{pstd}
{cmd:genBinomTest} calculates exact hypothesis tests for independent binomial random variables, based on the inequalities given in Hoeffding (1956). The null hypothesis is that the probability of a success on a trial is {it:#p}. {it:#N} is the total number of observations, whereas {it:#succ} refers to the number of successes observed.

{marker remarks}{...}
{title:Remarks}

{pstd}
For detailed information on the generalized binomial test, see Schlag (2013).

{pstd}
Hoeffding, W. (1956), "On the distribution of the number of successes in independent trails." The Annals of Mathematical Statistics, 27, 713-721

{pstd}
Schlag, K. (2013), "Doing Hoeffding's Homework - Tests and Confidence Intervals for the mean of finitely many independently distributed Bernoulli random variables."

{marker examples}{...}
{title:Examples}

{pstd}Test if probability of success = 0.5, given 8 successes in 10
trials{p_end}
{phang}{cmd:. genBinomTest 10 8 .5}{p_end}

{marker saved_results}{...}
{title:Saved results}

{pstd}
{cmd:genBinomTest} saves the following in {cmd:r()}:

{synoptset 20 tabbed}{...}
{p2col 5 25 25 2: Scalars}{p_end}
{synopt:{cmd:r(pvallower)}} lower one-sided p-value {p_end}
{synopt:{cmd:r(pvalupper)}} upper one-sided p-value {p_end}
{synopt:{cmd:r(pvaltwosided)}} two-sided p-value {p_end}
{p2colreset}{...}


