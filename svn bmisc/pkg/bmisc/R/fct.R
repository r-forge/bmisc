fct <-function(){

    cat("\n\n------------------------------------------------\n",sep="")
    cat("READ HELP DOCUMENTS FOR MORE OPTIONS AND DETAILS\n",sep="")
    cat("------------------------------------------------\n\n\n",sep="")
    
    cat("Descriptive Statistics:\n",sep="")
    cat("--------------------------------------\n",sep="")
    cat("n(x) - Sample size\n",sep="")
    cat("cv(x, na.rm=TRUE) - Coefficient of Variation\n",sep="")
    cat("se(x, na.rm=TRUE) - Standard Error\n\n",sep="")
    cat("runmean(x,window) - Running average of x with window, returns same length as x, with smoothed end points\n",sep="")
    cat("runmax(x,window) - Running max of x with window, returns same length as x, with smoothed end points\n",sep="")
    cat("runmin(x,window) - Running min of x with window, returns same length as x, with smoothed end points\n",sep="")
    cat("rollmin(x,window) - Running min of x with window, edited version of rollmax from zoo package\n\n\n",sep="")


    cat("Statistics:\n",sep="")
    cat("-----------\n",sep="")
    
    cat("reject.z(x,index=NULL,threshold=2) - Rejects x's, split over index, with z > threshold\n",sep="")
    cat("make.z(x,index=NULL) - Gets z scores, split over index\n",sep="")
    cat("replace.z(x,index=NULL,threshold=2) - Replaces x's with threshold, split over index, with z > threshold\n\n",sep="")
    
    cat("lev(y,  group,  data, trim.alpha = 0.25, type='abs') - Levene type tests\n",sep="")
    cat("mse(x) - Mean squarre error\n",sep="")
    cat("norm.test(x, title=NULL) - Normality tests\n\n",sep="")
    cat("corr.perm(x,y,nperm=999) - Pearson Correlation test by Permutation\n",sep="")
    cat("ttest.perm(vec1, vec2, nperm=999, alternative = 'two.sided', var.equal = T, silent=FALSE, type='i') - Student's t-tests by Permutation\n",sep="")
    cat("pair.diff(x, g) - Creates two lower triangle matrix: The mean differences and their standard error\n",sep="")
    cat("mc.long( y, group, data=NULL, p.adjust.method='sidak', num=NULL, silent=FALSE,...) - Pairwise T tests long format\n",sep="")
    cat("P.adjust(p, method = p.adjust.methods, n = length(p)) - Adjusts P-values for Multiple Comparisons\n",sep="")
    cat("P.adjust.methods = c('holm', 'hochberg', 'hommel', 'sidak', 'bonferroni', 'BH', 'BY',  'fdr', 'none') - Methods for adjusting p values\n\n\n",sep="")


    cat("Graphs:\n",sep="")
    cat("------\n",sep="")
    cat("Errbar(x, y, xinf=NULL, xsup=NULL, yinf=NULL, ysup=NULL, yCI=NULL, xCI=NULL, cap=0.05, ...) - Adds error bars on plot\n",sep="")
    cat("histplot(dat, breaks='Sturges', ...) - Histogram with normal density curve overlayed\n",sep="")
    cat("QQplot(dat, quant=TRUE) - QQplot with quantiles 25, 50, 75\n\n",sep="")
    cat("gam.Check(b,...) - Some diagnostics for a fitted gam model - Modified version of gam.check\n\n",sep="")
    cat("r.colors(file) - Creates pdf of all 657 standard built-in character colors of R\n\n\n",sep="")

    cat("Misc.:\n",sep="")
    cat("------\n",sep="")
    cat("att.strp(data) - Strips a object of its attributes\n",sep="")
    cat("ceiling.lg(x) - Ceiling to largest digit, i.e., 54 -> 60\n",sep="")
    cat("clean(data= x,  col.start =1,  min.val=NULL) - Cleans a data.frame from a starting point with a defined threshold  \n",sep="")
    cat("day(x) - Day of year as decimal number (001-366) \n",sep="")
    cat("format.hms(sec) - Returns hrs:min:sec \n",sep="")
    cat("is.even(x) - Returns TRUE if roundup(x) is an even number\n",sep="")
    cat("is.odd(x) - Returns TRUE if roundup(x) is an odd number\n",sep="")
    cat("last(x) - Gets last element of vector, list, data.frame, etc.\n",sep="")
    cat("performance(expr, samples=1, gcFirst=TRUE) - Does samples of function 'expr' to test its performance\n\n\n",sep="")
    cat("rm.levels(factor) - Removes unused levels from factor\n",sep="")
    cat("roundup(x, numdigits=0) - The 'conventional' rounding of 5 to the higher value\n",sep="")
    cat("sort.vdf(x, by, increasing=TRUE) - Sort Data Frames and Vectors\n",sep="")
    cat("week.1(x) - Week of the year starting on the first of January (01-53) \n",sep="")
    cat("week.num(x, day=c('sunday', 'monday')) - Week of the year as decimal number (00-53) using Sunday o Monday as the first day 1 of the week\n\n\n",sep="")
    
    cat("------------------------------------------------\n",sep="")
    cat("READ HELP DOCUMENTS FOR MORE OPTIONS AND DETAILS\n",sep="")
    cat("------------------------------------------------\n",sep="")
}
