dat <- read.table("calgrad_140909.txt",header=TRUE)

freq <- apply(sign(dat[,5:dim(dat)[2]]),2,sum)

hist(freq)

doms <- names(dat[,5:dim(dat)[2]]) [freq>8]

dat1 <- dat[, c(2,match(doms,names(dat)))]

par(mfrow=c(ceiling(length(doms)/4),4), mar= c(5,4,1,1), mgp= c(2, 1, 0), oma=c(0,0,1,1),bty="l")
for(i in 2:length(doms)){
	#plot(dat1$elevation,dat1[,i], main=names(dat1)[i])
	scatter.smooth(dat1$elevation,dat1[,i],col=2 , main=names(dat1)[i], xlab="Elevation", ylab="percent cover")
}


#make 1 helialpe column
dat$Helianthemum_alpestre <- rowSums(dat[,c(81,82)])
dat <- dat[,-82]


scatter.smooth(dat$elevation,dat$Helianthemum_nummularium,col=2 , main="Helianthemum_nummularium", xlab="Elevation", ylab="percent cover")
scatter.smooth(dat$elevation,dat$Helianthemum_alpestre,col=2 , main="Helianthemum_alpestre", xlab="Elevation", ylab="percent cover")


#check for species that are locally abundant
freq <- apply(dat[,5:dim(dat)[2]],2,max)
doms2 <- names(dat[,5:dim(dat)[2]]) [freq>35]
doms2 <- doms2[-c(8,11)]

dat1 <- dat[, c(2,match(doms2,names(dat)))]

par(mfrow=c(4,4), mar= c(5,4,1,1), mgp= c(2, 1, 0), oma=c(0,0,1,1),bty="l")
for(i in 2:length(doms)){
	#plot(dat1$elevation,dat1[,i], main=names(dat1)[i])
	scatter.smooth(dat1$elevation,dat1[,i],col=2 , main=names(dat1)[i], xlab="Elevation", ylab="percent cover")
}
