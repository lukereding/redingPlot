# redingPlot

redingPlot contains some useful plotting functions. Most of the functions aren't as flexible as your plain vanilla base R plotting functions (e.g. many of the functions only accept the data as a list), but you'll find some better ways to plot things not easily implemented in R. The focus is on plotting the relationships between quantitative and categorical variables while showing the data.

To make things as confusing as possible, the development version of the code is in a [different repo](https://github.com/lukereding/graphics) so that I'm not constantly updating this one. See that repo for some examples of what the plots look like.

----------------

## how to install **redingPlot**
* For everything that `looks like this`, copy and paste into R
* `install.packages("devtools")`
* `library(devtools)`
* `install_github("lukereding/redingPlot")`
* then restart R
* then you load **redingPlot** as you would any other package: `library(redingPlot)`
* because the packages that this package depends on don't always load when loading the package, run `library(viridis); library(beeswarm); library(magrittr)`
  * if you get an error that you haven't installed these packages, run the following line of code: `if (!"magrittr" %in% installed.packages()) install.packages("magrittr"); library(magrittr); if (!"viridis" %in% installed.packages()) install.packages("viridis"); library(viridis); if (!"beeswarm" %in% installed.packages()) install.packages("beeswarm"); library(beeswarm)`
* try `?scatter` and make sure the help file loads currently. If so, you're all set!

----------------

Functions available at this time:
* bar() -- barplot with data jittered on top
* beeStrip() -- plot the data for each group as a histogram, draw line at the mean for each group
* beeStripBox() -- same as beeStrip, but plot a boxplot alongside each histogram
* simple() -- plots the mean for each group as a line along the individual data points jittered
* strip() -- plots the mean for each group as a point with along the individual data points jittered
* scatter() -- provides good defaults for a scatterplot. Also automatically performs a linear regression and prints relevant stats to the plot

---------------
#### these plots are designed with the following philosophy in mind:

* every part of the graph should have a clear purpose
* show the data, some summary of the data or parameter estimate of interest, and some measure of variability about that estimate
* clearly label sample sizes
* state some measure of effect size
* state the the test used to assess statistical significance and the resulting p-value
* don't extrapolate
* use color thoughtfully. Or just use [viridis](https://bids.github.io/colormap/). Why? (a) It's perceptually uniform. (b) It looks good. (c) It works just as well printed in black and white. (d) It's accessible to people with the most common forms of colorblindness. (e) It works for representing categories or for representing a quantitative variable. (f) You'll never need to justify your color scheme ever again. Note that this is also true of the other colormaps provided in the viridis package (plasma, magma, and inferno), but they don't look as nice.

Some of these things are not implemented (yet) in the graphics in the package.
