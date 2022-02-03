#proportion similarity index
library(RInSp)

data<-read.csv("gecko microhabitat.csv", header=TRUE)
data<-read.csv("gecko activity.csv", header=TRUE)
data<-read.csv("gecko orientation.csv", header=TRUE)
data<-read.csv("gecko tree spp.csv", header=TRUE)

# Select a single spatial sampling group (release group SR)
multinomdata = import.RInSp(data, row.names = 1,
                             info.cols = 2, subset.rows = c("site", "1"))
similarity = overlap(multinomdata)

#--------add a loop to test significance---------#

data1<-read.csv("indv microhabitat.csv", header=TRUE)
dat<-subset(data1,select = c("site","branch", "branchlettes","trunk","foliage","foam.cover","long.grass","ground","flax.leaf"))#extract only the microhabitat columns from dataset

data1<-read.csv("indv activity.csv", header=TRUE)
dat<-subset(data1,select = c("site","not.moving","moving","basking","eating"))

data1<-read.csv("indv orientation.csv", header=TRUE)
dat<-subset(data1,select = c("Site","up","down","horizontal"))

data1<-read.csv("indv tree spp.csv", header=TRUE)
dat<-subset(data1,select = c("site","kanuka","Ngaio","grass","COProb","lancewood", "PITten","PITcra","PITeug","OLEpan","PSEarb","NZ.flax","KNIexc"))

means <- aggregate(dat, list(data1$release_type), mean)

randoverlap <- as.numeric(similarity$meanoverlap)
for (i in 2:10000) {
  randgroup <- sample(data1$release_type)
  randmeans <-aggregate(dat, list(randgroup), mean)
  randmeans1 = import.RInSp(randmeans, row.names = 1,
                              info.cols = 2, subset.rows = c("site", "1"))
  randoverlap[i] <- as.numeric(overlap(randmeans1)$meanoverlap[1])#assigns the value to position or column i in the empty variable
}
hist(randoverlap, breaks = 20)
abline(v = randoverlap[1], col = "red")

2*sum(randoverlap <= randoverlap[1])/length(randoverlap)#calculates p-value
