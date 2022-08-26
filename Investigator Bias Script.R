#Load Phylogenetic Packages
library(ape)
library(treeio)
library(treebalance)
library(ggtree)
#read tree nexus file
tetrapodtree<-read.nexus('tetrapodtree.txt')
#Resolve tree
tetrapodtree<-bifurcatr(tetrapodtree, runs = 1)
#Generate subtrees
tetrapodsubtrees<-subtrees(tetrapodtree)
#Calculate ewColless I
for (i in tetrapodsubtrees){print(ewCollessI(i))}
#Calculate values for Sampling Index
for (i in tetrapodsubtrees){print((i$edge.length/tetrapodtree$edge.length))
for (i in tetrapodsubtrees){print(length(i$tip.label)/length(tetrapodsubtrees$tip.label)
#Convert tree to tibble
tetrapodtreetibble<-as_tibble(tetrapodtree)
#Load imbalance and bias data
read.csv('tetrapoddata.csv')
#Attach SES values to tibble
tetrapodtreetibble<-full_join(tetrapodtreetibble, tetrapoddata$SES, by='node')
#Plot heatmap
tettreeheatmap<-ggtree(tetapodtree)%<+%tetrapodtreetibble+geom_tiplab(size=2.5, color='black')+geom_tree(size=2)+aes(color=Correlation)+scale_colour_gradient()
#Perform multiple regression
tetrapodmultipleregression<-with(tetrapoddata, lm(Imbalance~Index+SESmcd))
summary(tetrapodmultipleregression)
#Perform Spearman's rank test
cor.test(tetrapoddata$Index,tetrapoddata$SESmcd, method = 'spearman', exact = FALSE)
