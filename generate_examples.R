# run to generate example graphs on the README

library(redingPlot)

# define height and width
h = 6
w = 5

## beeStripBox
pdf("./beeStripBox.pdf", height = h, width = w)
data(iris); beeStripBox(iris$Sepal.Length,iris$Species,xlab="species",ylab="sepal length",main="beeStripBox() example")
dev.off()

## bar
pdf("./bar.pdf", height = h, width= w)
bar(list(iris %>% filter(Species=="setosa") %>% .$Sepal.Length, iris %>% filter(Species=="versicolor") %>% .$Sepal.Length, iris %>% filter(Species=="virginica") %>% .$Sepal.Length),median=T,CI=T,lab=c("setosa","versicolor","virginica"),ylab="sepal length",main="bar() example")
dev.off()

## beeStrip
pdf("./beeStrip.pdf", height = h, width = 6)
beeStrip(list(iris %>% filter(Species=="setosa") %>% .$Sepal.Length, iris %>% filter(Species=="versicolor") %>% .$Sepal.Length, iris %>% filter(Species=="virginica") %>% .$Sepal.Length),lab=c("setosa","versicolor","virginica"),ylab="sepal length",main="beeStrip()",xlab="species") 
dev.off()

## simple
pdf("./simple.pdf", height = h, width = w)
simple(list(iris %>% filter(Species=="setosa") %>% .$Sepal.Length, iris %>% filter(Species=="versicolor") %>% .$Sepal.Length, iris %>% filter(Species=="virginica") %>% .$Sepal.Length),lab=c("setosa","versicolor","virginica"),ylab="sepal length",main="simple()",xlab="species")
dev.off()

## strip
pdf("./strip.pdf", height = h, width = w)
strip(list(iris %>% filter(Species=="setosa") %>% .$Sepal.Length, iris %>% filter(Species=="versicolor") %>% .$Sepal.Length, iris %>% filter(Species=="virginica") %>% .$Sepal.Length),lab=c("setosa","versicolor","virginica"),ylab="sepal length",main="strip()",xlab="species",mean_col="black",point_size=1.4,type="ci") dev.off()
dev.off()

## scatter
pdf("./scatter.pdf", height = h, width = w)
scatter(trees[,1],trees[,2],xlab="tree girth (in.)",ylab="tree height (ft.)",main="scatter() example")
dev.off()
