# Number 1

#Part A
###Download data
dat.white<-read.csv("https://cipolli.com/students/data/WhiteParticipants.txt",
                    header=T,sep=",")[-1,]
dat.black<-read.csv("https://cipolli.com/students/data/BlackParticipants.txt",
                    header=T,sep=",")[-1,]
dat.matched<-read.csv("https://cipolli.com/students/data/matchedsample.txt",
                      header=T,sep=",")
colnames(dat.matched)<-c("aid","id","MatchedSample")

#Part B
#remove obs where dat.white does not equal 1
library(dplyr) #download library for data manipulation
dat.white1 = data.frame(filter(dat.white, Race==1)) #create a new dataframe where Race != 1
dat.black1 = data.frame(filter(dat.black, Race==3)) #create a new dataframe where Race != 0

#Part C
#merge data of white and black participants into new object
dat.wb <- rbind(dat.white1,dat.black1)

#Other attempts
# dat.wb <- c(dat.white, dat.black, dat.matched) doesn't combine variables

# column.names <- colnames(dat.white)
# dat.wb <- merge(dat.white, dat.black, by = column.names) this merges horizontally- we want vertically
# https://www.statmethods.net/management/merging.html

#Part D
#remove observations not in the representative sample
dat.matched1 = data.frame(filter(dat.matched, MatchedSample==1)) #representative sample 

#Other attempts
# originally used aid as column to filter on- needed MatchedSample 

#Part E
#remove flagged users as a false match for demographics, filter them out
dat.wb1 = data.frame(filter(dat.wb, 
                           aid!="5c9963bd-8f12-6d35-bafc-cdeee22209ed" & 
                           aid!="5c996fcc-1f89-13c3-50e7-3f7c554aad3c" &
                           aid!="5c9a3bd8-1cbc-854b-a193-620f5eaee500" &
                           aid!="5cbcaf2e-3233-62d3-7aa3-ad1c9f4ab5a0" &
                           aid!="5c9a68ff-1fd6-4ff1-6bf5-b8a1504075b8"))

#Other attempts
# originally used && when & is correct

#Part F
#create new variable subtracting LadderSelf from LadderGroup
dat.wb1$LadderGroup <- as.numeric(dat.wb1$LadderGroup) #changes the var from factor to numeric
dat.wb1$LadderSelf <- as.numeric(dat.wb1$LadderSelf) #changes the var from factor to numeric
dat.wb1$LadderDiff <- dat.wb1$LadderGroup - dat.wb1$LadderSelf

#Other attempts
#‘-’ not meaningful for factors error
# dat.wb$LadderDiff <- (dat.wb$LadderGroup - dat.wb$LadderSelf) needs to be numeric

#Part G
#adds all of the values and divides by the number of values w/out the mean function
dat.wb1$PosEmo <- (as.numeric(dat.wb1$Amused) + as.numeric(dat.wb1$Awe) + as.numeric(dat.wb1$Grateful) + as.numeric(dat.wb1$Hopeful) + as.numeric(dat.wb1$Inspired) + as.numeric(dat.wb1$Interested) + as.numeric(dat.wb1$Joy) + as.numeric(dat.wb1$Love) + as.numeric(dat.wb1$Proud) + as.numeric(dat.wb1$Serene))/10

#Other attempts
#create a variable called PosEmo
#dat.wb1$PosEmo <- mean("Amused", "Awe", "Grateful", "Hopeful", "Inspired", "Interested",
#                       "Joy", "Love", "Proud", "Serene") #didn't account for the column names properly
# dat.wb$PosEmo <- mean(dat.wb$Amused, dat.wb$Awe, dat.wb$Grateful, dat.wb$Hopeful, dat.wb$Inspired, dat.wb$Interested, dat.wb$Joy, dat.wb$Love, dat.wb$Proud, dat.wb$Serene)
#R didn't seem to like when we used the mean function
#dat.wb1$PosEmo <- mean(as.numeric(dat.wb1$Amused), as.numeric(dat.wb1$Awe), as.numeri(dat.wb1$Grateful), as.numeric(dat.wb1$Hopeful), as.numeric(dat.wb1$Inspired), as.numeric(dat.wb1$Interested), as.numeric(dat.wb1$Joy), as.numeric(dat.wb1$Love), as.numeric(dat.wb1$Proud), as.numeric(dat.wb1$Serene))

#oops this was inefficient, changing emotions to numerics from factors individually
dat.wb1$Amused <- as.numeric(dat.wb1$Amused)
dat.wb1$Awe <- as.numeric(dat.wb1$Awe)
dat.wb1$Grateful <- as.numeric(dat.wb1$Grateful)
dat.wb1$Hopeful <- as.numeric(dat.wb1$Hopeful)
dat.wb1$Inspired <- as.numeric(dat.wb1$Inspired)
dat.wb1$Interested <- as.numeric(dat.wb1$Interested)
dat.wb1$Joy <- as.numeric(dat.wb1$Joy)
dat.wb1$Love <- as.numeric(dat.wb1$Love)
dat.wb1$Proud <- as.numeric(dat.wb1$Proud)
dat.wb1$Serene <- as.numeric(dat.wb1$Serene)


#Part H
#plot LadderDiff v. PosEmo for white and black participants
#download necessary packages
library(ggplot2)
library(graphics)
library(gridExtra)

#specify data to use

#How people feel relative to their group for white and black people
ggdat<-data.frame(x=dat.wb1$LadderDiff[which(dat.wb1$Race==1)]) #Ladderdiff for white people
g1<-ggplot(data=ggdat, aes(x=x,y=..count..))+ #specifying x and y axis
  geom_bar(fill="purple", #color
           #trim=FALSE,
           alpha=0.5,
           show.legend = FALSE) +
  xlab("Feeling of Self within Group") +
  ylab("Count") +
  ggtitle("How White Individuals feel Relative to their Group") +
  theme_bw() +
  geom_hline(yintercept = 0)

ggdat<-data.frame(x=dat.wb1$LadderDiff[which(dat.wb1$Race==3)]) #Ladderdiff for black people
g2<-ggplot(data=ggdat, aes(x=x,y=..count..))+ #specify x and y axis
  geom_bar(fill="magenta",
           #trim=FALSE,
           alpha=0.5,
           show.legend = FALSE) +
  xlab("Feeling of Self within Group") +
  ylab("Count") +
  ggtitle("How Black Individuals feel Relative to their Group") +
  theme_bw() +
  geom_hline(yintercept = 0)
grid.arrange(g1,g2,ncol=2) #put graphs side by side

#sentiment of individual taking the survey
ggdat<-data.frame(y=dat.wb1$PosEmo[which(dat.wb1$Race==1)]) #sentiment of white people
p1<-ggplot(data=ggdat,aes(x='',y=y))+
  geom_violin(fill="green",
              #trim=FALSE,
              alpha=0.5,
              show.legend = FALSE) +
  geom_boxplot(width=.25, fill="white") +
  xlab('') +
  ylab("Emotion Level") +
  ggtitle("Sentiment of White Surveyors") +
  theme_bw()

ggdat<-data.frame(y=dat.wb1$PosEmo[which(dat.wb1$Race==3)]) #sentiment of black people
p2<-ggplot(data=ggdat,aes(x='',y=y))+
  geom_violin(fill="lightgreen",
              #trim=FALSE,
              alpha=0.5,
              show.legend = FALSE) +
  geom_boxplot(width=.25, fill="white") +
  xlab('') +
  ylab("Emotion Level") +
  ggtitle("Sentiment of Black Surveyors") +
  theme_bw()
grid.arrange(p1,p2,ncol=2)

#comparing white and black ladderdiff and posemo
ggdat<-data.frame(x=dat.wb1$LadderDiff[which(dat.wb1$Race==1)], y=dat.wb1$PosEmo[which(dat.wb1$Race==1)]) #which specifies white
t1<-ggplot(data=ggdat, aes(x=x,y=y)) +
  geom_point(size=2,shape=18,color='violet') + #shape, size and color of points
  geom_smooth(method = lm, fill="lightblue") + #adds regression line, confidence interval color
  xlab("Ladder Difference") +
  ylab("Sentiment") +
  ggtitle("White Ladder Difference versus Individual Sentiment")
ggdat<-data.frame(x=dat.wb1$LadderDiff[which(dat.wb1$Race==3)], y=dat.wb1$PosEmo[which(dat.wb1$Race==3)])
t2<-ggplot(data=ggdat, aes(x=x,y=y)) +
  geom_point(size=2,shape=18,color="purple") + #shape, size and color of points
  geom_smooth(method = lm, fill="lightblue") + #adds regression line, CI color
  xlab("Ladder Difference") +
  ylab("Sentiment") +
  ggtitle("Black Ladder Difference versus Individual Sentiment")
grid.arrange(t1,t2,ncol=2)

install.packages("wesanderson") #install colors
library(wesanderson) #load colors
#black and white comparison on one graph
ggdat<-data.frame(x=dat.wb1$LadderDiff, y=dat.wb1$PosEmo, Race=dat.wb1$Race)
m<-ggplot(data=ggdat, aes(x=x,y=y,
                          color=Race,shape=Race)) + #color and shape of points differ by race
  geom_point() + #twoway point plot
  geom_smooth(method = lm, #regression line
              fullrange=TRUE, #extend regression line fully
              aes(fill=Race)) + #fill color of confidence interval by Race
  xlab("Ladder Difference") +
  ylab("Sentiment") + 
  ggtitle("Comparing White(1) and Black(3) Ladder Difference & Sentiment") +
  theme_minimal() #minimal theme w/ white background
m+scale_color_manual(values=wes_palette(n=2,name="Moonrise3")) #one color option
#or
m+scale_color_brewer(palette="Accent") #another color option

# race and PosEmo

ggdat<-data.frame(LD=dat.wb1$LadderDiff,race=dat.wb1$Race)
ggplot(data=dat.wb1, aes(y=PosEmo,x=Race) ) + #tell ggplot which data to use
  geom_violin(fill="magenta",           
              trim=FALSE,             #bar outline color
              alpha = 0.5,
              show.legend = FALSE)+
  geom_boxplot(width = .25, fill = "white")+
xlab("Race")              + #x axis label
  ylab("Frequencies of PosEmo")                 + #y axis label
  ggtitle("Frequencies of PosEmo Across White and Black Race") + #add title to plot
  theme_bw()   # +                 #removes grey background

# race and LadderDiff
ggdat<-data.frame(LadderDifference=dat.wb1$LadderDiff,race=dat.wb1$Race)
ggplot(data=ggdat,aes(x=LadderDifference,fill=race))+
  geom_bar(position = position_dodge())+#don't leave it stacked- put side by side
  ggtitle("Ladder Difference by Race") +
  theme_bw() +
  geom_hline(yintercept = 0)




#what do ladderdiff and posemo mean
#laddergroup - where would you place your group of people
#ladderself - where would put yourself
#posemo - how people feel when taking the survey
  #relationship between how people feel when taking the survey

#what we got from office hours
ggdat<-data.frame(x=dat.wb1$LadderDiff[which(dat.wb1$Race==3)])
ggplot(data=ggdat, aes(x=x,y=..count..))+
  geom_bar()

ggdat<-data.frame(LD=dat.wb1$LadderDiff,race=dat.wb1$Race)
ggplot(data=ggdat,aes(x=LD,fill=race))+
  geom_bar(position=position_dodge()) #we unstacked

ggdat<-data.frame(PE=dat.wb1$PosEmo,race=dat.wb1$Race)

#failed attempt number 2 where I didn't know what data to extract
ggdat<-data.frame((dat.wb1$LadderDiff))
colnames(ggdat)=c("LadderDiff", "PosEmo")
p1<-ggplot(data=ggdat, aes(x=LadderDiff)) +
  geom_bar(stat = "identity",
    color= "black",
           fill= "lightblue") +
  xlab("LadderDiff") +
  ylab("Frequency") +
  ggtitle("Frequency of LadderDiff for Black and White Surveyors") +
  geom_hline(yintercept=0) +
  theme_bw()
p1

#ggdat2<-data.frame(PosEmo=dat.wb$PosEmo,race=dat.wb$Race)
#ggplot(data=ggdat2,aes(x=PosEmo,fill=race))+
#  geom_bar(position = position_dodge()) #don't leave it stacked- put side by side

#ggdat_white<-data.frame(table(data.frame(filter(dat.wb$LadderDiff, Race == 1))))
#colnames(ggdat_white)=c("LadderDiff","PosEmo")
#p_white<-ggplot(data=ggdat_white,aes(x=LadderDiff,y=PosEmo)) + #tell ggplot which data to use
#  geom_bar(stat="identity",           #plot the count (no transformation needed)
#           color="black",             #bar outline color
#           fill="lightblue")        + #bar colors
#  xlab("Ladder Difference")              + #x axis label
#  ylab("Positive Emotion")                 + #y axis label
#  ggtitle("Positive Emotion vs Ladder Difference") + #add title to plot
#  geom_hline(yintercept=0)          + #adds a line for the x-axis
#  theme_bw()                     #removes grey background
#p_white

# tried to plot all of the information in one graph
# not the prettiest way to do that
# colnames(ggdat)=c("LadderDiff", "PosEmo")
# p1<-ggplot(ggdat, aes(x=LadderDiff)) +
#   #  geom_histogram(bins=10, #how many bins to use
#   #                 fill = "lightblue",
#   #                 color="black") +
#   geom_bar()+
#   xlab("Difference between the Group and Individual") +
#   ylab("Emotion") +
#   ggtitle("Emotions with respect to Ladder Difference") +
#   theme_bw() +
#   geom_hline(yintercept=0)
# p1