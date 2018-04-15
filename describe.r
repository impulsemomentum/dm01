library(car)
library(DAAG)
library(DMwR)
library(lattice)
data <- read.csv("NFL Play by Play 2009-2017 (v4).csv")
#qqPlot(data$yrdln,main=paste("yrdln-qqPlot"," ",sep =""))
#boxplot(data$yrdln,main=paste("yrdln-box"," ",sep =""), na.rm = TRUE)
draw <- function(list_a,list_b,file,title)
{
	ppath = file
	png(file=ppath,width=1200,height=1000)
	split.screen(c(3,2))
	par(pin=c(4,4))
	screen(1) 
	hist(list_a,main = paste(title," ",sep =""))
	screen(3)
	qqPlot(list_a,main=paste(title," ",sep =""))
	screen(5)
	boxplot(list_a, main=paste(title," ",sep =""), na.rm = TRUE)
	
	screen(2) 
	hist(list_b,main = paste(title," original",sep =""))
	screen(4)
	qqPlot(list_b,main=paste(title," original",sep =""))
	screen(6)
	boxplot(list_b, main=paste(title," original",sep =""), na.rm = TRUE)
	dev.off()
	
}
clean_NA <-function(data)
{
	data[!complete.cases(data)]
	data <- na.omit(data)
	data
}

central_NA <- function(data)
{
	data <- data[-manyNAs(data)]
	data <- centralImputation(data)
	data
}

knn_NA <- function(data)
{
	data <- knnImputation(data,10)
}

relation_NA <- function(data)
{
	symnum(cor(data[,4:18],use='complete.obs'))
	lm(formula=PO4~oPO4, data=data)
	data = data[-manyNAs(data),]
	data
}

list_a = data$yrdln
clean_NA_data = clean_NA(list_a)
central_NA_data = central_NA(list_a)
knn_NA_data = knn_NA(list_a)
relation_NA_data = relation_NA(list_a)
draw(list_a,clean_NA_data,paste("clean","yrdln.png",sep =""),"clean")
draw(list_a,central_NA_data,paste("central","yrdln.png",sep =""),"central")
draw(list_a,knn_NA_data,paste("knn_NA","yrdln.png",sep =""),"knn_NA")
draw(list_a,relation_NA_data,paste("relation_NA","yrdln.png",sep =""),"relation_NA")