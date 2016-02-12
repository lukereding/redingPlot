# redingPlot

redingPlot contains some useful plotting functions. Most of the functions aren't as flexible as your plain vanilla base R plotting functions (e.g. many of the functions only accept the data as a list), but you'll find some better ways to plot things not easily implemented in R. The focus is on plotting the relationships between quantitative and categorical variables while showing the data.

----------------

## what's here
  * [installing redingPlot](https://github.com/lukereding/redingPlot#how-to-install-redingplot)
  * [available functions](https://github.com/lukereding/redingPlot#available-functions)
  * [philosophy](https://github.com/lukereding/redingPlot#these-plots-are-designed-with-the-following-philosophy-in-mind)

###how to install **redingPlot**

Copy and paste everything that looks `like this` into R

* `if (!"devtools" %in% installed.packages()) install.packages("devtools"); library(devtools)`
* `install_github("lukereding/redingPlot")`
*  restart R--you only have to do this when you first install redingPlot
*  load redingPlot as you would any other package: `library(redingPlot)`
* try `?scatter` and make sure the help file loads currently, and maybe try one of the examples: `scatter(trees[,1],trees[,2],xlab="tree girth (in.)",ylab="tree height (ft.)",main="scatter() example")`. If it looks good, you're all set!

----------------

###available functions:

To find the code to generate these plots and other examples, check the help files (i.e. `?bar`).

* **cats_meow()** -- barplot + mean with 95% confidence interval + data jittered      
![meow!](https://github.com/lukereding/redingPlot/raw/master/examples/cats_meow.png)
* **bar()** -- barplot with data jittered on top     
![bar](https://github.com/lukereding/redingPlot/raw/master/examples/bar.png)
* **beeStrip()** -- plot the data for each group as a histogram, draw line at the mean for each group      
![beeStrip](https://github.com/lukereding/redingPlot/raw/master/examples/beeStrip.png)
* **beeStripBox()** -- same as beeStrip, but plot a boxplot alongside each histogram     
![beeStripBox](https://github.com/lukereding/redingPlot/raw/master/examples/beeStripBox.png)
* **simple()** -- plots the mean for each group as a line along the individual data points jittered        
![simple](https://github.com/lukereding/redingPlot/raw/master/examples/simple.png)
* **strip()** -- plots the mean for each group as a point with along the individual data points jittered      
![strip](https://github.com/lukereding/redingPlot/raw/master/examples/strip.png) 
* **scatter()** -- provides good defaults for a scatterplot. Also automatically performs a linear regression and prints relevant stats to the plot     
![scatter](https://github.com/lukereding/redingPlot/raw/master/examples/scatter.png)

---------------
####these plots are designed with the following philosophy in mind:

* every part of the graph should have a clear purpose
* show the data, some summary of the data or parameter estimate of interest, and some measure of variability about that estimate
* clearly label sample sizes
* state some measure of effect size
* state the the test used to assess statistical significance and the resulting p-value
* don't extrapolate
* use color thoughtfully. Or just use [viridis](https://bids.github.io/colormap/). Why? (a) It's perceptually uniform. (b) It looks good. (c) It works just as well printed in black and white. (d) It's accessible to people with the most common forms of colorblindness. (e) It works for representing categories or for representing a quantitative variable. (f) You'll never need to justify your color scheme ever again. Note that this is also true of the other colormaps provided in the viridis package (plasma, magma, and inferno), but they don't look as nice.

Some of these things are not implemented (yet) in the graphics in the package.


--------------

Your graphics should not stop here. Good graphics should tell the whole story without requiring the reader to dive into the figure caption. We put axis labels on our graphs so that our readers don't have to sift through text to make connections and extract meaning. Most graphics are easily improved by reminding the reader what they're looking at. The graphs produced by these plotting functions should provide a template for further tweaking either within R (for greater reproducibility when your advisor or a reviewer demands a change) or within a graphics editor like Inkscape.
