###CHAPTER 2 -- CRAN R     ##Descriptive Title
###CIPOLLI                 ##Author

####################################################################################
####################################################################################
# Section: Data Frame
####################################################################################
####################################################################################

#############################################################
###Creating a Data Frame#####################################
#############################################################
r<-data.frame(iNKT=c(1.34,1.19,0.89,0.88,0.98,0.85,0.93,0.87,1.02,0.96,
                     0.77,1.17,1.12,1.61,1.18,1.00,0.88,1.08,1.12,0.69,
                     1.29,1.16,0.94,0.94,0.58,0.87,1.11,1.08,0.92,1.23,
                     0.94,0.41,0.57,0.19,1.13,0.33,0.60,0.16,0.33,0.77,
                     1.44,0.62,0.70,0.77,0.47,0.44,0.28,0.22),
              Ship1=c("+/+","+/+","+/+","+/+","+/+","+/+","+/+","+/+",
                      "+/+","+/+","+/+","+/+","+/+","+/+","+/-","+/-",
                      "+/-","+/-","+/-","+/-","+/-","+/-","+/-","+/-",
                      "+/-","+/-","+/-","+/-","+/-","+/-","+/-","+/-",
                      "-/-","-/-","-/-","-/-","-/-","-/-","-/-","-/-",
                      "-/-","-/-","-/-","-/-","-/-","-/-","-/-","-/-")
)
#See the first 6 rows of data frame we created
head(r)
#See the last 6 rows of data frame we created
tail(r)
#Ask for the number of rows
nrow(r)
#Ask for the number of columns
ncol(r)
#Ask for the column names -- what data is available
names(r)
#Ask for the number of observations
length(r)

summary(r) #Summarize data vectors
#############################################################
###Subsets of Data Frames####################################
#############################################################
#Ask for the iNKT observations for the 48 mice
r$iNKT
r[,1]
r[,"iNKT"]

#Ask for the Ship1 status observations for the 48 mice
r$Ship1
r[,2]
r[,"Ship1"]

#Ask for the observations for mouse #1
r[1,]
#Ask for the iNKT observation for mouse #1
r$iNKT[1]
r[1,1]
r[1,"iNKT"]
#Ask for the Ship1 observation for mouse #1
r$Ship1[1]
r[1,2]
r[1,"Ship1"]
#Grabs the first three rows of the ship1dat dataset
r[c(1,2,3),]


#############################################################
###    Subsetting Data Frames with which#####################
#############################################################
#Grabs frequencies of iNKT cell populations for -/- SHIP1 status
r$iNKT[which(r$Ship1=="-/-")]
#subset(x=r$iNKT,subset=r$Ship1=="-/-") equivalently

#Grabs frequencies of iNKT cell populations for +/- SHIP1 status
r$iNKT[which(r$Ship1=="+/-")]
#subset(x=r$iNKT,subset=r$Ship1=="+/-") equivalently

#Grabs frequencies of iNKT cell populations for +/+ SHIP1 status
r$iNKT[which(r$Ship1=="+/+")]
#subset(x=r$iNKT,subset=r$Ship1=="+/+") equivalently

#Ask for observations (rows) where iNKT is greater than 1
r[which(r$iNKT>1),] #leaving the second index blank returns all columns
#subset(x=r,subset=r$iNKT>1) equivalently
  
#Ask for observations with Ship1 status +/+
#and iNKT is greater than 1
r[which(r$iNKT>1 & r$Ship1=="+/+"),]
#subset(x=r,subset=r$iNKT>1 & r$Ship1=="+/+") equivalently
#############################################################
###    Read data from file   ################################
#############################################################
#Reads in data from the internet
#file gives the location of the file
#sep tells us how each observation is separated.
#header indicates that there are column names to the data
ship1dat<-read.table(file = "https://cipolli.com/students/data/ship1.txt",
                     sep=",",header=TRUE)

#Wrong web address
#ship1dat<-read.table(file = "https://cipolli.com/students/data/ship12.txt",
#                   sep=",",header=TRUE)

#Allows us to peek at the data to ensure that the data was read correctly
head(ship1dat)


#############################################################
###    Subsetting Data Frames ###############################
#############################################################
#Grabs the column of frequencies of iNKT cell populations
ship1dat$iNKT
ship1dat[,1]

#Grabs the column of SHIP1 status
ship1dat$Ship1
ship1dat[,2]

#Grabs the first row of the ship1dat data
ship1dat[1,]
#Grabs the second row of the ship1dat data
ship1dat[2,]
#Grabs the three row of the ship1dat data
ship1dat[c(1,2,3),]

#############################################################
###    Subsetting Data Frames with which#####################
#############################################################
#Grabs frequencies of iNKT cell populations for -/- SHIP1 status
ship1dat$iNKT[which(ship1dat$Ship1=="-/-")]
#Grabs frequencies of iNKT cell populations for +/- SHIP1 status
ship1dat$iNKT[which(ship1dat$Ship1=="+/-")]
#Grabs frequencies of iNKT cell populations for +/+ SHIP1 status
ship1dat$iNKT[which(ship1dat$Ship1=="+/+")]

#############################################################
###    Spotting Errors in R #################################
#############################################################
#Doesn't return data -- column name is iNKT, not inkt
ship1dat$inkt

#Doesn't return data -- there is not SHIP1 status of "-/+"
ship1dat$iNKT[which(ship1dat$Ship1=="-/+")]

#############################################################
###    Write data to file   #################################
#############################################################
write.table(x=ship1dat,file="ship1.csv",
                      sep=",",col.names=TRUE) #col.names=TRUE prints column names too


#############################################################
###    Lists   ##############################################
#############################################################
stats.professors<-list(names=c("Will","Roy","Josh"),
                       degrees=list(c("Math","Stat","Computer Science"),
                                    c("Math","Stat","Bio Stat","Applied Math"),
                                    c("Math","Stat")),
                       ratings=c(4.6,4.8,3.7),
                       difficulty=c(3.6,2.7,3.2),
                       tags=list(c("Accessible","Inspirational","Good Feedback"),
                                 c("Accessible","Hilarious","Don't Skip","Good Feedback"),
                                 c("Tough Grader","Don't Skip","Test Heavy",
                                   "Graded by Few Things","Accessible",
                                   "Lecture Heavy","Clear Grading Criteria"))
)
head(stats.professors)
length(stats.professors)

#Ask for the professors' names
stats.professors$names
stats.professors[["names"]]
stats.professors[[1]]
#Ask for the data about Will -- the first entries
stats.professors[["degrees"]][1]
stats.professors[["ratings"]][1]
stats.professors[["difficulty"]][1]
stats.professors[["tags"]][1]

#Spotting an error:
stats.professors["names"]
stats.professors["names"][1]
#correct:
stats.professors[["names"]][1]

#unlist
unlist(stats.professors,recursive=FALSE)

unlist(stats.professors)

#############################################################
###    Data Types of Data Frames#############################
#############################################################
#See the classes of individual objects
c(class(ship1dat),mode(ship1dat),typeof(ship1dat))
c(class(ship1dat$iNKT),mode(ship1dat$iNKT),typeof(ship1dat$iNKT))
c(class(ship1dat$Ship1),mode(ship1dat$Ship1),typeof(ship1dat$Ship1))
#See a summary of the data frame we created
str(ship1dat)

ship1dat$Ship1
as.numeric(ship1dat$Ship1)

ship1dat$Ship1.refactored<-factor(ship1dat$Ship1,levels = c("+/+","-/-","+/-"))
ship1dat$Ship1.refactored
as.numeric(ship1dat$Ship1.refactored)


#droping unused levels
ship1dat.plusplus<-ship1dat[which(ship1dat$Ship1=="+/+"),]
ship1dat.plusplus$Ship1

ship1dat$Ship1<-as.character(ship1dat$Ship1)
ship1dat[which(ship1dat$Ship1=="+/+"),]$Ship1

ship1dat.plusplus$Ship1<-droplevels(ship1dat.plusplus$Ship1)
ship1dat.plusplus$Ship1

x <- stats.professors # recall stats.professors is a list of data
c(class(x),mode(x),typeof(x))
x <- stats.professors[1]
c(class(x),mode(x),typeof(x))
x <- stats.professors[[1]]
c(class(x),mode(x),typeof(x))
A<-matrix(data = c(2,0,1,8),nrow = 2,ncol = 2)
x <- A #recall A is a matrix
c(class(x),mode(x),typeof(x))
x <- ls #recall ls() lists all objects in the current session
c(class(x),mode(x),typeof(x))

####################################################################################
####################################################################################
# Section: Matrix
####################################################################################
####################################################################################

#############################################################
###Creating a Matrices#######################################
#############################################################
#Matrices are created by columns by default
A<-matrix(data = c(2,0,1,8),nrow = 2,ncol = 2)
A
#We can ask R to build Matrices by row
A<-matrix(data = c(2,1,0,8),nrow = 2,ncol = 2,byrow=TRUE)
A
#Matrices are created by columns by default
B<-matrix(data = c(11,5,2,9),nrow = 2,ncol = 2)
B
#We can ask R to build Matrices by row
B<-matrix(data = c(11,2,5,9),nrow = 2,ncol = 2,byrow=TRUE)
B

#What are the number of entries in the matrix?
length(A) 
#Summarize the columns of the matrix
summary(A)
#What are the number of rows in the matrix?
nrow(A)
#What are the number of columns in the matrix?
ncol(A)
#What are the dimensions of the matrix?
dim(A) #Gives the number of rows and columns
#Previewing a matrix
head(A)  #Shows full matrix because it is small
tail(A)  #Shows full matrix because it is small
#What are the diagnoal entries of A?
diag(A)
#What is the transpose of A?
t(A)

#############################################################
###Spotting an Error in R ###################################
#############################################################
#A<-matrix(data = c(2,1,0,8),nrow = 2,ncol = 2)
#A

#############################################################
###Stock Functions on Matrices ##############################
#############################################################
#Treats like a vector of size length(A)
as.vector(A)
#What is the minimum element in g?
min(A)
#What is the maximum element in g?
max(A)
#What is the sum of all elements in g?
sum(A)
#Calculate a cumulative or "running" total.
cumsum(A)
#What is the average element in g?
mean(A)
#What is the standard deviation of elements in g?
sd(A)
#What are the quantiles of elements in g?
quantile(A)
#What is the 90th percentile of elements in g?
quantile(A,probs= c(0.90))

rowSums(A)
colSums(B)

#############################################################
###Matrix Algebra ###########################################
#############################################################
A+2
A-2
A*2
A/2

A+B
A-B
A*B
A/B

#Matrix multiplication of A and B
A %*% B
#What is the determinant of A?
det(A)
#What is the inverse of A?
solve(A)
#What are the eigen values and vectors for A?
eigen(A)

#############################################################
###Combining Matrices########################################
#############################################################
#Join matrices at the rows
C<-rbind(A,B)
C
#Join matrices at the columns
D<-cbind(A,B)
D

#############################################################
###Spotting an Error in R####################################
#############################################################
A%*%C
A%*%D