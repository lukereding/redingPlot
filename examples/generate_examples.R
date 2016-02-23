# run to generate example graphs on the README

source("../R/plotting_functions.R")

setwd("~/Documents/redingPlot/examples/")

# define height and width
h = 800*.7
w = 700*.8

## cat'smeow
png("./cats_meow.png", height = h, width = w)
data(iris); cats_meow(iris$Sepal.Length,iris$Species, ylab="sepal length", xlab = "species", main="cat's meow example")
dev.off()

## beeStripBox
png("./beeStripBox.png", height = h, width = w)
data(iris); beeStripBox(iris$Sepal.Length,iris$Species,xlab="species",ylab="sepal length",main="beeStripBox() example")
dev.off()

## bar
png("./bar.png", height = h*0.7, width= w/2)
bar(list(iris %>% filter(Species=="setosa") %>% .$Sepal.Length, iris %>% filter(Species=="versicolor") %>% .$Sepal.Length, iris %>% filter(Species=="virginica") %>% .$Sepal.Length),median=T,CI=T,lab=c("setosa","versicolor","virginica"),ylab="sepal length",main="bar() example")
dev.off()

## beeStrip
png("./beeStrip.png", height = h, width = w)
beeStrip(list(iris %>% filter(Species=="setosa") %>% .$Sepal.Length, iris %>% filter(Species=="versicolor") %>% .$Sepal.Length, iris %>% filter(Species=="virginica") %>% .$Sepal.Length),lab=c("setosa","versicolor","virginica"),ylab="sepal length",main="beeStrip()",xlab="species")
dev.off()

## simple
png("./simple.png", height = h, width = w)
simple(list(iris %>% filter(Species=="setosa") %>% .$Sepal.Length, iris %>% filter(Species=="versicolor") %>% .$Sepal.Length, iris %>% filter(Species=="virginica") %>% .$Sepal.Length),lab=c("setosa","versicolor","virginica"),ylab="sepal length",main="simple()",xlab="species")
dev.off()

## strip
png("./strip.png", height = h, width = w)
strip(list(iris %>% filter(Species=="setosa") %>% .$Sepal.Length, iris %>% filter(Species=="versicolor") %>% .$Sepal.Length, iris %>% filter(Species=="virginica") %>% .$Sepal.Length),lab=c("setosa","versicolor","virginica"),ylab="sepal length",main="strip()",xlab="species",mean_col="black",point_size=1.4,type="ci")
dev.off()

## scatter 
png("./scatter.png", height = 500*.8, width = 600*.8)
scatter(trees[,1],trees[,2],xlab="tree girth (in.)",ylab="tree height (ft.)",main="scatter() example")
dev.off()

## scatter hist
png("./scatter_hist.png", height = 500*.8, width = 600*.8)
scatter_hist(trees[,1],trees[,2],xlab="tree girth (in.)",ylab="tree height (ft.)",title="scatter_hist() example")
dev.off()
