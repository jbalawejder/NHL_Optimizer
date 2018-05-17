### Part 1: Total Analysis ###

nh1 <- read.csv("/Users/jbala/OneDrive/Documents/NHL Data/myNHLData15-16.csv", header=TRUE)
attach(nhl)
summary(nhl)		#View Summary Stats


#Pie chart of player nationalities

Player_Country <- table(nhl$Country)
Player_Country
slices <- c(487,13,38,37,34)
lbls <- c("CAN", "CHE", "CZE", "FIN", "RUS", "USA", "Other")
pct <- round(slices/sum(slices)*100)
lbls <-paste(lbls,pct)
lbls <- paste(lbls, "%", sep="")
pie(slices,labels-lbls, col=c("red", "gray", "royalblue", "navy", "darkred", "goldenrod1", "blue", "darkgreen"), main="NH: Player Nationality")

## Pie chart of player positions
table(nhl$Pos)
slices <- c(259, 306, 92, 174, 159)
lbls <- c("Center", "Defense", "GOalie", "Left Wing", "Right Wing")
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls,pct)
lbls <- paste(lbls, "%", sep="")
pie(slices,labels-lbls, col=c("darkblue", "tomato", "royalblue", "seagreen1", "seagreen4"), main="NH: Player Positions")

## Barplot of number of players by age
install.packages("ggplot2")
library(ggplot)
library(plyr)
Players_Age <- count(nhl, 'Age')
plot <- ggplot(data=Players_Age, aes(x=Age, y = freq)) + geom_bar(stat="identity", fill="steelblue")
plot + scale_x_discrete(limits=c(18-43), position = c("right")) + xlab("Player Age") + ylab("Count") + ggtitle("Number of NHL Players by Age") + geom_text(aes(label=freq), vjust=-0.3, size=3.5) + theme_minimal()

## Boxplot of salary by position
boxplot(Salary ~ as.factor(Pos), main="Salary by Position", xlab= "Position", ylab="Salary (in millions)", col="steelblue")


### Part Two: Assembling the Team ###
## Analyzing Forwards
forwards <- read.csv("/Users/jbala/OneDrive/Documents/NHL Data/myNHLData15-16_Forwards.csv", header=TRUE)
attach(forwards)
sorted_forwards <- forwards [order(-PTS),]    ## Sort forwards in descending order by Points
head(sorted_forwards, 30)                     ## Look at top 50 forwards in NHL based on pints
top_forwards <- head(sorted_forwards, 30)     ## Store top 50 forwards
attach(top_forwards)
top_forwards

ptssalary <- PTS/Salary
ptssalary

## Analyzing Defencemen
defence <- read.csv("/Users/jbala/OneDrive/Documents/NHL Data/myNHLData15-16_Defence.csv", header=TRUE)
attach(defence)
sorted_defence <- defence [order(-BkS),]    
head(sorted_defence, 30)                     
top_defence <- head(sorted_defence, 30)     
attach(top_defence)
top_defence

bkssalary <- BkS/Salary
bkssalary

## Analyzing Goalies
goalies <- read.csv("/Users/jbala/OneDrive/Documents/NHL Data/myNHLData15-16_Goalies.csv", header=TRUE)
attach(goalies)
sorted_goalies <- goalies [order(-GP),]    
head(sorted_goalies, 15)                     
top_goalies <- head(sorted_goalies, 15)     
attach(top_goalies)
top_goalies

svsalary <- Sv/Salary
svsalary
