fna.data <- "WisconsinCancer.csv"
wisc.df <- read.csv(file = "WisconsinCancer.csv" )


wisc.data <- as.matrix(wisc.df[, 3:32])
row.names(wisc.data) <- wisc.df$id

diagnosis <- as.numeric(wisc.df$diagnosis == "M" )
nrow(wisc.data)
length(grep("_mean", colnames(wisc.data)))


table (wisc.df$diagnosis)

colMeans(wisc.data)

apply(wisc.data,2,sd)
wisc.pr <- prcomp((wisc.data), scale = TRUE  )
summary(wisc.pr)
biplot(wisc.pr)



plot(wisc.pr$x[, c(1, 3)], col = (diagnosis + 1), 
     xlab = "PC1", ylab = "PC3")

variance <-wisc.pr$sdev ^2
pve<- variance/sum(variance)*100

barplot(pve, ylab = "Precent of Variance Explained",
        names.arg=paste0("PC",1:length(pve)), las=2, axes = FALSE)
axis(2, at=pve, labels=round(pve,2)*100 )

par(mfcol=c(1,2))
plot(cumsum(pve) , xlab = "Principal Component", 
      ylab= "precent of variance explained",
      ylim= c(0, 100), type="o")
plot(cumsum(pve), xlab = "Principal component",    
       ylab = "Cumulative Proportion of Variance Explained", 
     ylim = c(0, 1), type = "o")

data.scaled <- scale(wisc.data)

data.dist <- dist(data.scaled)
wisc.hclust <- hclust(data.dist, method="complete")
plot(wisc.hclust)
abline(h=19, col="red", lty=2)



table(wisc.hclust.clusters, diagnosis)

wisc.pca.hclust<-hclust(dist(wisc.pr$x[,1:7]), method="ward.D2")
plot(wisc.pca.hclust)

grps<- cutree(wisc.pca.hclust, k=2)
table(grps)
table(grps, diagnosis)
plot(wisc.pr$x[,1], col=grps)
library(rgl)



url <- "https://tinyurl.com/new-samples-CSV"


new <- read.csv(url)

npc <- predict(wisc.pr, newdata=new)
npc

plot(wisc.pr$x[,1:2], col=grps)
points(npc[,1], npc[,2], col="blue", pch=16, cex=3)
text(npc[,1], npc[,2], col="white")

