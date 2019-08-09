filterTSeriesSSA <- function(
### Decompose a vector (i.e. time series) into spectral bands
series                      ##<< numeric vector: Input time series (no gaps!)
, borders.wl                ##<< list of numeric vectors: Borders of the different periodicity
                            ## bands to extract. Units are the sampling frequency of the series
                            ## (needs one  vector per step (see details)).
, M = rep(floor(length(series) / 3), times = n.steps)  ##<< integer (vector): Window length
                            ## or embedding dimension (see details and ?ssa) (in ssa() this parameter
                            ## is called L).
, n.comp = rep(40, times = n.steps)##<< integer (vector): Amount of SSA components to compute.
                            ## See the help of ssa (package Rssa) for details.
, harmonics=rep(0, times = n.steps)##<< integer (vector): How many harmonics to include in each
                            ## component (see details). No harmonics are used if set to 0 (default).
, tolerance.harmonics = 0.05##<< numeric fraction (0-1): Tolerance to use to determine the
                            ## width of the bands the harmonics are looked for in. The actual
                            ## width is calculated by multiplying the frequency of the "main"
                            ## oscillation with this ratio. Use higher values for oscillations
                            ## with few repetitions (and, hence, wider peaks in a spectrum) and lower
                            ## ones with those with many repetitions (and thus sharper peaks).
, var.thresh.ratio = 0.005  ##<< numeric fraction(0-1): Variance threshold below which eigentriples are
                            ## treated as "noise" and will not be included in the groups. The actual
                            ## threshold is calculated by multiplying the total variance of
                            ## the time series with this fraction.
, grouping = c('grouping.auto', 'groupSSANearestNeighbour')[1]##<< character string: Method to use for grouping
                            ##  the individual SSA eigentriples. 'grouping.auto' uses the function
                            ## of that name in package Rssa, 'groupSSANearestNeighbour' employs a rather crude scheme
                            ## based on finding pairs (or larger groups) in an euclidian
                            ## distance matrix of the reconstructions of all extracted SSA eigentriples.
                            ## See ?grouping.auto or ?groupSSANearestNeighbour for details.
, groupingMethod = 'wcor'   ## character string: Method to use for grouping with the grouping.auto
                            ## function.
, repeat.extr = rep(1, times = length(borders.wl))##<< integer value/vector: How often to repeat the
                            ## extraction. If the respective value is > 1 than the result of the extraction
                            ## is again subject to spectral decomposition/filtering for n times and only
                            ## the (filtered) result is treated as the actual band (see details).
, recstr.type = 'subtraction' ##<< string: How to determine the high frequency residuals.
                            ## If == 'subtraction', the high frequency signal is computed by subtracting
                            ## all other signals from the original series (see details). For all other
                            ## values only extracted eigentriples with high frequencies are grouped
                            ## in this band.
, pad.series = c(0, 0)      ##<< integer vector (length 2): Length of the part of the series to use for
                            ## padding at the start (first value) and at the end of the series. Values
                            ## of zero (default) cause no padding.
, SSA.methods = c('nutrlan', 'propack', 'eigen', 'svd')  ##<< character vector: Methods to use for the
                            ## SSA computation. First the first method is tried, when convergence
                            ## fails, the second is used and so on. See the help of ssa() in
                            ## package Rssa for details on the methods. It is preferable to use more
                            ## than one method as some methods (especially nutrlan) in some cases do not
                            ## converge. The last two methods are relatively slow.
, center.series = TRUE      ##<< logical: Whether to center the series around zero prior to the computation.
                            ## The (subtracted) mean will be added to the long term 'trend' component,
                            ## e.g. to the step containing an Inf value in  borders.wl. Not centering
                            ## of the series may cause erroneous trend extraction in some cases!
, call.freq = quote(calcFrequency(series.t)) ##<< 'quoted' function call : call to a function to compute
                            ## the frequency of the 'major' oscillation present in some time series.
                            ## This is used to compute the frequency of the (grouped) SSA eigentriples.
                            ## See the help for 'calcFrequency' for details of the default mechanism.
, n.steps = switch(class(borders.wl),list=length(borders.wl), dim(borders.wl)[2]) ##<< integer:
                            ##  Amount of steps in the process. This argument is only kept
                            ## for backwards compatibility. Do not supply or change any values!
, plot.spectra = FALSE      ##<< logical: Whether to plot pseudo spectra for the different steps.
, second.axis = TRUE        ##<< logical: Whether to plot a second axis with period units
, open.plot = TRUE          ##<< logical: Whether to open a new plotting window for the plots. Set this to
                            ##   FALSE and open and set up a device prior to running the function to specify
                            ##   or change the device options.
, print.stat = TRUE         ##<< logical: whether to print status information during the calculations.
, xlim = c()                ##<< numeric vector: x-limits for the plotted spectra. If not supplied
                            ## it goes from 1 / n....0,5.
, debugging = FALSE         ##<< logical: if TRUE, workspaces are saved that can be used for debugging
                            ##   non convergence cases that do not cause R errors but may indicate
                            ##   a possible error in the settings, data or code.
, ...                       ##<< miscellaneous: further arguments passed to the plotting routines.
) {
  5
### Numeric 5.
}

.result <-
 list(filterTSeriesSSA = list(definition = "filterTSeriesSSA <- function(\n### Decompose a vector (i.e. time series) into spectral bands\nseries                      ##<< numeric vector: Input time series (no gaps!)\n, borders.wl                ##<< list of numeric vectors: Borders of the different periodicity\n                            ## bands to extract. Units are the sampling frequency of the series\n                            ## (needs one  vector per step (see details)).\n, M = rep(floor(length(series) / 3), times = n.steps)  ##<< integer (vector): Window length\n                            ## or embedding dimension (see details and ?ssa) (in ssa() this parameter\n                            ## is called L).\n, n.comp = rep(40, times = n.steps)##<< integer (vector): Amount of SSA components to compute.\n                            ## See the help of ssa (package Rssa) for details.\n, harmonics=rep(0, times = n.steps)##<< integer (vector): How many harmonics to include in each\n                            ## component (see details). No harmonics are used if set to 0 (default).\n, tolerance.harmonics = 0.05##<< numeric fraction (0-1): Tolerance to use to determine the\n                            ## width of the bands the harmonics are looked for in. The actual\n                            ## width is calculated by multiplying the frequency of the \"main\"\n                            ## oscillation with this ratio. Use higher values for oscillations\n                            ## with few repetitions (and, hence, wider peaks in a spectrum) and lower\n                            ## ones with those with many repetitions (and thus sharper peaks).\n, var.thresh.ratio = 0.005  ##<< numeric fraction(0-1): Variance threshold below which eigentriples are\n                            ## treated as \"noise\" and will not be included in the groups. The actual\n                            ## threshold is calculated by multiplying the total variance of\n                            ## the time series with this fraction.\n, grouping = c('grouping.auto', 'groupSSANearestNeighbour')[1]##<< character string: Method to use for grouping\n                            ##  the individual SSA eigentriples. 'grouping.auto' uses the function\n                            ## of that name in package Rssa, 'groupSSANearestNeighbour' employs a rather crude scheme\n                            ## based on finding pairs (or larger groups) in an euclidian\n                            ## distance matrix of the reconstructions of all extracted SSA eigentriples.\n                            ## See ?grouping.auto or ?groupSSANearestNeighbour for details.\n, groupingMethod = 'wcor'   ## character string: Method to use for grouping with the grouping.auto\n                            ## function.\n, repeat.extr = rep(1, times = length(borders.wl))##<< integer value/vector: How often to repeat the\n                            ## extraction. If the respective value is > 1 than the result of the extraction\n                            ## is again subject to spectral decomposition/filtering for n times and only\n                            ## the (filtered) result is treated as the actual band (see details).\n, recstr.type = 'subtraction' ##<< string: How to determine the high frequency residuals.\n                            ## If == 'subtraction', the high frequency signal is computed by subtracting\n                            ## all other signals from the original series (see details). For all other\n                            ## values only extracted eigentriples with high frequencies are grouped\n                            ## in this band.\n, pad.series = c(0, 0)      ##<< integer vector (length 2): Length of the part of the series to use for\n                            ## padding at the start (first value) and at the end of the series. Values\n                            ## of zero (default) cause no padding.\n, SSA.methods = c('nutrlan', 'propack', 'eigen', 'svd')  ##<< character vector: Methods to use for the\n                            ## SSA computation. First the first method is tried, when convergence\n                            ## fails, the second is used and so on. See the help of ssa() in\n                            ## package Rssa for details on the methods. It is preferable to use more\n                            ## than one method as some methods (especially nutrlan) in some cases do not\n                            ## converge. The last two methods are relatively slow.\n, center.series = TRUE      ##<< logical: Whether to center the series around zero prior to the computation.\n                            ## The (subtracted) mean will be added to the long term 'trend' component,\n                            ## e.g. to the step containing an Inf value in  borders.wl. Not centering\n                            ## of the series may cause erroneous trend extraction in some cases!\n, call.freq = quote(calcFrequency(series.t)) ##<< 'quoted' function call : call to a function to compute\n                            ## the frequency of the 'major' oscillation present in some time series.\n                            ## This is used to compute the frequency of the (grouped) SSA eigentriples.\n                            ## See the help for 'calcFrequency' for details of the default mechanism.\n, n.steps = switch(class(borders.wl),list=length(borders.wl), dim(borders.wl)[2]) ##<< integer:\n                            ##  Amount of steps in the process. This argument is only kept\n                            ## for backwards compatibility. Do not supply or change any values!\n, plot.spectra = FALSE      ##<< logical: Whether to plot pseudo spectra for the different steps.\n, second.axis = TRUE        ##<< logical: Whether to plot a second axis with period units\n, open.plot = TRUE          ##<< logical: Whether to open a new plotting window for the plots. Set this to\n                            ##   FALSE and open and set up a device prior to running the function to specify\n                            ##   or change the device options.\n, print.stat = TRUE         ##<< logical: whether to print status information during the calculations.\n, xlim = c()                ##<< numeric vector: x-limits for the plotted spectra. If not supplied\n                            ## it goes from 1 / n....0,5.\n, debugging = FALSE         ##<< logical: if TRUE, workspaces are saved that can be used for debugging\n                            ##   non convergence cases that do not cause R errors but may indicate\n                            ##   a possible error in the settings, data or code.\n, ...                       ##<< miscellaneous: further arguments passed to the plotting routines.\n) {\n  5\n### Numeric 5.\n}",
     description = "Decompose a vector (i.e. time series) into spectral bands",
     value = "Numeric 5.", `item{series}` = "numeric vector: Input time series (no gaps!)",
     `item{borders.wl}` = "list of numeric vectors: Borders of the different periodicity\nbands to extract. Units are the sampling frequency of the series\n(needs one  vector per step (see details)).",
     `item{M}` = "integer (vector): Window length\nor embedding dimension (see details and ?ssa) (in ssa() this parameter\nis called L).",
     `item{n.comp}` = "integer (vector): Amount of SSA components to compute.\nSee the help of ssa (package Rssa) for details.",
     `item{harmonics}` = "integer (vector): How many harmonics to include in each\ncomponent (see details). No harmonics are used if set to 0 (default).",
     `item{tolerance.harmonics}` = "numeric fraction (0-1): Tolerance to use to determine the\nwidth of the bands the harmonics are looked for in. The actual\nwidth is calculated by multiplying the frequency of the \"main\"\noscillation with this ratio. Use higher values for oscillations\nwith few repetitions (and, hence, wider peaks in a spectrum) and lower\nones with those with many repetitions (and thus sharper peaks).",
     `item{var.thresh.ratio}` = "numeric fraction(0-1): Variance threshold below which eigentriples are\ntreated as \"noise\" and will not be included in the groups. The actual\nthreshold is calculated by multiplying the total variance of\nthe time series with this fraction.",
     `item{grouping}` = "character string: Method to use for grouping\nthe individual SSA eigentriples. 'grouping.auto' uses the function\nof that name in package Rssa, 'groupSSANearestNeighbour' employs a rather crude scheme\nbased on finding pairs (or larger groups) in an euclidian\ndistance matrix of the reconstructions of all extracted SSA eigentriples.\nSee ?grouping.auto or ?groupSSANearestNeighbour for details.",
     `item{repeat.extr}` = "integer value/vector: How often to repeat the\nextraction. If the respective value is > 1 than the result of the extraction\nis again subject to spectral decomposition/filtering for n times and only\nthe (filtered) result is treated as the actual band (see details).",
     `item{recstr.type}` = "string: How to determine the high frequency residuals.\nIf == 'subtraction', the high frequency signal is computed by subtracting\nall other signals from the original series (see details). For all other\nvalues only extracted eigentriples with high frequencies are grouped\nin this band.",
     `item{pad.series}` = "integer vector (length 2): Length of the part of the series to use for\npadding at the start (first value) and at the end of the series. Values\nof zero (default) cause no padding.",
     `item{SSA.methods}` = "character vector: Methods to use for the\nSSA computation. First the first method is tried, when convergence\nfails, the second is used and so on. See the help of ssa() in\npackage Rssa for details on the methods. It is preferable to use more\nthan one method as some methods (especially nutrlan) in some cases do not\nconverge. The last two methods are relatively slow.",
     `item{center.series}` = "logical: Whether to center the series around zero prior to the computation.\nThe (subtracted) mean will be added to the long term 'trend' component,\ne.g. to the step containing an Inf value in  borders.wl. Not centering\nof the series may cause erroneous trend extraction in some cases!",
     `item{call.freq}` = "'quoted' function call : call to a function to compute\nthe frequency of the 'major' oscillation present in some time series.\nThis is used to compute the frequency of the (grouped) SSA eigentriples.\nSee the help for 'calcFrequency' for details of the default mechanism.",
     `item{n.steps}` = "integer:\nAmount of steps in the process. This argument is only kept\nfor backwards compatibility. Do not supply or change any values!",
     `item{plot.spectra}` = "logical: Whether to plot pseudo spectra for the different steps.",
     `item{second.axis}` = "logical: Whether to plot a second axis with period units",
     `item{open.plot}` = "logical: Whether to open a new plotting window for the plots. Set this to\nFALSE and open and set up a device prior to running the function to specify\nor change the device options.",
     `item{print.stat}` = "logical: whether to print status information during the calculations.",
     `item{xlim}` = "numeric vector: x-limits for the plotted spectra. If not supplied\nit goes from 1 / n....0,5.",
     `item{debugging}` = "logical: if TRUE, workspaces are saved that can be used for debugging\nnon convergence cases that do not cause R errors but may indicate\na possible error in the settings, data or code.",
     `item{\\dots}` = "miscellaneous: further arguments passed to the plotting routines.",
     format = "", title = "filterTSeriesSSA"))

.parsers <- inlinedocs::test.parsers
