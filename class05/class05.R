# class 5 R graphics

# 2A. Line Project
weight <- read.table("weight_chart.txt", header= TRUE)

plot (weight$Age, weight$Weight, xlab= "Age (months)", ylab="Weight (kg)", pch=18,
      typ= "b", main= "Baby Weight With Age")

#2B Bar Plot

feat <- read.table("feature_counts.txt", sep="\t", header=TRUE)
barplot(feat$Count, names.arg = feat$Feature, horiz=TRUE, las=1)

#section 3
counts<- read.table("male_female_counts.txt", sep="\t", header=TRUE)
barplot(counts$Count, names.arg= counts$Sample, las=2, col=c("red","blue", "pink"))
