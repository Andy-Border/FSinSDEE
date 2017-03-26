# Distribution, use the function to get the 
# distribution of the interested column of a 
# data frame

# Examples:
# getDistribution(ISBSG,16)
# drawDistribution(ISBSG,16)
# drawDistribution(iris,3)

getDistribution<- function(data,colNum =16) {
  # browser()
library("dplyr")
library("entropy")
data <- data[complete.cases(data[,colNum]),]
groupByCategory <- group_by(data,data[[colNum]])
groupByCategory1 <- summarise(groupByCategory,UsedTimes=n())
groupByCategory1 <- arrange(groupByCategory1,desc(UsedTimes))
groupByCategory1 <- mutate(groupByCategory1,mean=mean(UsedTimes),Standard_Deviation=sd(UsedTimes))
groupByCategory1$LevelNames <- factor(groupByCategory1$`data[[colNum]]`, levels=unique(groupByCategory1$`data[[colNum]]`))
names(groupByCategory1)[1]<-paste(colnames(data[colNum]))
return(groupByCategory1)
}
drawDistribution<- function(data,colNum =16) {
  library("ggplot2")
  library("ggthemes")
  groupByCategory1 <- getDistribution(data,colNum)
  drawPic <- ggplot(groupByCategory1,aes(x=LevelNames,y=UsedTimes))
  drawPic+ geom_bar(stat= 'identity', width = 0.5)+ 
    theme_economist()+
    ggtitle(paste("Distribution of ",colnames(data[colNum])))+
    theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
  #return(groupByCategory1)
}