# RUtilLibrary
Utility functions for descriptive statistics, plots and database usage

## DATABASE CONNECTION
`db.init(host, user, password, dbname)`
- connect to a database

`db.query(queryString)`
- executes a select query

## DESCRIPTIVE STATISTICS
`statistics.descriptive.frequency(data, columns.independent)`
- counts the occurrences (frequency) of the independent variables in a dataset

`statistics.descriptive.nonparametric(data, column.dependent, columns.independent)`
- provides N, Sum 25th, 50th (median), and 75th percentile, min, max

`statistics.descriptive.parametric(data, column.dependent, columns.independent, conf.interval = 0.95, conf.accurate = TRUE)`
- provides N, Sum, Mean, SD, SE, confidence interval, min, max

## STATISTIC PLOTS
`statistics.histogram(data, column.dependent, columns.independent, column.fill = NULL, bin.width = NULL, bins = 30, title = TRUE)`
- creates a histrogram based on the dependent column. For each combination of the independent columns, a separate graph will be created. column.fill can be used to color different values in another color inside each frequency bin

`statistics.plot.frequency(data, columns.independent, xlab = NULL, ylab = NULL, legend.lab = NULL, xlim = NULL, ylim = NULL, title = NULL)`
- creates a barchart plot of frequency data using the statistics.descriptive.frequency function.

`statistics.plot.nonparametric(data, column.dependent, columns.independent, xlab = NULL, ylab = NULL, legend.lab = NULL, xlim = NULL, ylim = NULL, title = NULL)`
- creates a boxplot (for non parametric data)

`statistics.plot.parametric(data, column.dependent, columns.independent, xlab = NULL, ylab = NULL, legend.lab = NULL, xlim = NULL, ylim = NULL, title = NULL, conf.interval = 0.95, conf.accurate = TRUE)`
- creates a barchart with confidence intervals (the error bars are based on the exact sample size!)

# UTIL FUNCTIONS
`effect.size.string(effect.size)`
- converts a effect size (e.g. r = 0.35) into a string (e.g. “medium effect”)

`p.value.string(p.value)`
- converts a p-value into a string: p < 0.1 = .,p < 0.05 = *, p < 0.01 = **, p < 0.001 = ***

`save.plot(p, filename, ppi = 300, paper = "a4r", width = 11.69,  height = 8.27, print.default.graphicdevice = TRUE)`
- saves a ggplot (as from all the statistics.plot and histogram functions) in a file and also displays it in the RStudio window

`reset.output()`
- closes all graphic devices and open sinks

`copy.conditional.column(x, y, columns.copy = names(y), columns.copy.x = columns.copy, columns.copy.y = columns.copy, by = intersect(names(x), names(y))[1], by.x = by, by.y = by)`
- copies column content from one data frame to another based on a common column
