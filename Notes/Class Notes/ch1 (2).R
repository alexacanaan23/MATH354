###CHAPTER 1 -- CRAN R     ##Descriptive Title
###CIPOLLI                 ##Author

####################################################################################
####################################################################################
# Section: Creating Objects
####################################################################################
####################################################################################

#############################################################
###Variable Assignment#######################################
#############################################################

#Adding in R is easy
a<-4+5 #Ask to add using +

#Subtracting in R is easy, too
b<-10-15 #Ask to subtract using -

#Multiplying in R is easy, too
c<-3*5 #Ask to multiply using *

#Dividing in R is easy, too
d<-27/3 #Ask to divide using /

#Square Roots in R are easy, too
e<-sqrt(69) #Ask for a sqrt with sqrt()

#We can do several calculations at once but we want to ensure that we're 
#careful with order of operations.
f<-(3+5)*3/3^2 #Just squares the 3 according to pEmdas
g<-((3+5)*3/3)^2  #Squares the result of the expression in parenthesis according to Pemdas
#These give very different answers showing this is important!

a
b
c
d
e
f
g

(a<-4+5) #print an assigned variable by placing the line in parentheses

#############################################################
###Algebraic Calculations with Variables#####################
#############################################################
a+b
c-d
a*f
e/g
sqrt(f)

#############################################################
###Errors in R###############################################
#############################################################
#A

#############################################################
###Errors in R###############################################
#############################################################
#2a<-2*a

#############################################################
###Errors in R###############################################
#############################################################
#af

####################################################################################
####################################################################################
# Section: Removing Objects from the Environment
####################################################################################
####################################################################################
#############################################################
###Objects in the session####################################
#############################################################
ls()
rm("a")
rm(list=ls())

####################################################################################
####################################################################################
# Section: Data Types
####################################################################################
####################################################################################
#############################################################
###converting data types to numbers##########################
#############################################################
string<-"45"                   #starting with a character object
as.numeric(string)
is.character(string)
is.numeric(as.numeric(string))
is.double(as.numeric(string))
is.integer(as.numeric(string))

logical <-TRUE                 #starting with a logical object
as.numeric(logical) #TRUE maps to 1
as.numeric(FALSE)   #FALSE maps to 0
is.logical(logical)
is.numeric(as.numeric(logical))
is.double(as.numeric(logical))
is.integer(as.numeric(logical))

complex<-4+5i                 #starting with a complex object
is.numeric(complex)
is.complex(complex)

int<- 7L                      #starting with an integer object
is.numeric(int)
is.integer(int)
is.double(int)
is.integer(as.numeric(int))
is.double(as.numeric(int))
as.character(int)
as.complex(int)

#############################################################
###Errors in R###############################################
#############################################################
string<-"Changing Data Types in R" #string (non-number)
as.numeric(string)
is.character(string)

#############################################################
###Asking for Data Types     ################################
#############################################################
x<-1
c(class(x),mode(x),typeof(x))
x<-"Hello"
c(class(x),mode(x),typeof(x))
x<-FALSE
c(class(x),mode(x),typeof(x))
x<-7L
c(class(x),mode(x),typeof(x))
x<-4+3i
c(class(x),mode(x),typeof(x))

####################################################################################
####################################################################################
# Section: Vectors
####################################################################################
####################################################################################
#############################################################
###Create Vectors############################################
#############################################################
#c() allows us to define a vector of numbers in R
c(1,4,3,2,5,2,6,7,8)
#c() allows us to define a vector of words in R
c("Yes","No","No", "No", "Yes", "Yes", "No")
#seq() creates vector of an ordered sequence -- this saves us time
seq(from=0,to=10,by=2)
#rep() creates a vector of repeated values
rep(3,5)

#############################################################
###Errors in R###############################################
#############################################################
#c(1,4,3,2,5,2,6,,8)#If truly missing it should be c(1,4,3,2,5,2,6,NA,8)

#############################################################
###Errors in R###############################################
#############################################################
#c("Yes","No",No, "No", "Yes", "Yes", "No") #No looks for an object saved to No, we should use "No" here.

#############################################################
###Variable Assignment w/ Vectors############################
#############################################################
#c() allows us to define a vector of numbers in R
g<-c(1,4,3,2,5,2,6,7,8)
#c() allows us to define a vector of words in R
h<-c("Yes","No","No", "No", "Yes", "Yes", "No")
#seq() creates vector of an ordered sequence -- this saves us time
i<-seq(from=0,to=10,by=2)
#rep() creates a vector of repeated values
j<-rep(3,5)

g
h
i
j

#We can ask for the 3rd value in vector h
h[3]
#We can ask for the first three values in vector i
i[c(1,2,3)]
i[seq(from=1,to=3,by=1)]
i[1:3]
#We can ask for all but the first value in vector g
g[-1]
#We can ask for all but the first 3 values in vector i
i[-c(1,2,3)]
i[-seq(from=1,to=3,by=1)]
i[-(1:3)]
#We can ask for the elements of i in ascending order
i[order(i)]
#We can ask for the elements of i in decending order
i[order(i,decreasing=TRUE)]
#We can combine elements of g and i
k<-c(g,i)
k

#############################################################
###Errors in R###############################################
#############################################################
k
k[0]
k[1]

#############################################################
###Errors in R###############################################
#############################################################
k
k[15]
k[16]


#############################################################
### Calculation with Vectors#################################
#############################################################
g  #Starting Vector
#+4 adds 4 to each item in the vector
g+4
#-3 subtracts 3 from each item in the vector
g-3
#*10 multiplies each item in the vector by 10
g*10
#/2 divides each item in the vector by 2
g/2
#We can add, subtract, multiple and divide using vectors too,
#but to do so we usually want to ensure that they are of the same length.
#We can ask for the length of the vector by simply asking for it.
length(g) #returns the length of g
m<-c(1,2,3,4,5,6,7,8,9) #could use 1:9 or seq(from=1,to=9,by=1)
g+m #Adds corresponding items in g and m
g-m #Subtracts corresponding items in g and m
g*m #Multiplies corresponding items in g and m
g/m #Divides corresponding items in g and m
g%*%m #Dot product of two vectors

#############################################################
###Errors in R###############################################
#############################################################
g #Starting Vector
length(g) #length of g
n<-c(1,2,3) #Note the length of n (3) is a multiple of 9,
p<-c(1,2,3,4) #the lenghth of p (4) is not.
g+n #this works by repeating n three times
#The result above is g+(1,2,3,1,2,3,1,2,3).
#Note that because it works doesn't mean we necessarily wanted
#to ask for such a calculation. In fact, we rarely want to complete
#such a calculation. Errors without error messages are the worst kind.
g+p #this gives a warning
#The result above is g+c(1,2,3,4,1,2,3,4,1).

#############################################################
###Questions with Vectors####################################
#############################################################
#See the first few observations
head(g)
#How many elements are in the vector?
length(g)
#What are the unique elements in g?
unique(g)
#How many unique elements are in the vector?
length(unique(g))
#Provide a summary of (numerical) observations
summary(g)
#Provide a summary of (categorical) observations
table(h)
#Table also works for numerical vectors with a handful of unique observations
table(g)

####################################################################################
####################################################################################
# Section: The which function
####################################################################################
####################################################################################
#############################################################
###The which function########################################
#############################################################
g
#Which elements in g are less than 3?
which(g<3)
#Which elements in g are less than or equal to 3?
which(g<=3)
#Which elements in g are greater than 3?
which(g>3)
#Which elements in g are greater than or equal to 3?
which(g>=3)
#Which elements in g are equal to 2?
which(g==2)
#Which elements in g are not equal to 2?
which(g!=2)
#Which elements in g are in the set (1,2,3,4,5)?
which(g %in% c(1,2,3,4,5))
#Which elements in g are less than or equal to 6 AND less greater than 2?
which(g<=6 & g>2)  #Think 2<g<=6
#Which elements in g are less than or equal to 6 OR less greater than 2?
which(g<=6 | g>2)  #At least one of these is true: 2<g or g<=6 or 2<g<=6
#Which elements in g are not in the set (1,2,3,4,5)
which(!(g %in% c(1,2,3,4,5)))


#Which elements in g are equal to 2?
which(g==2)
#How many elements are in the vector are equal to 2?
length(which(g==2))
#This returns the two 2 elements
g[which(g==2)]
#This also returns the two 2 elements
subset(x=g,subset=g==2)

#Create a blank object called q
q<-NULL
#Is q an empty object?
is.null(q)
#Save a vector to object q
q<-c(0,2,3,5,7,11,NA,13)
#Is q an empty object?
is.null(q)
#Are there missing observations?
which(is.na(q))
#is.na also catches nan
q/0
which(is.na(q/0))
which(is.nan(q/0))

#############################################################
###Errors in R###############################################
#############################################################
#Which elements in g are equal to 2?
g[which(g==22)] 
subset(x=g,subset=g==22)

#############################################################
###Describing Vectors########################################
#############################################################
#What is the minimum element in g?
min(g)
#What is the maximum element in g?
max(g)
#What is the sum of all elements in g?
sum(g)
#Calculate a cumulative or "running" total.
cumsum(g)
#What is the average element in g?
mean(g)
#What is the standard deviation of elements in g?
sd(g)
#What are the quantiles of elements in g?
quantile(g)
#What is the 90th percentile of elements in g?
quantile(g,probs= c(0.90))

#############################################################
###Spotting an error in R####################################
#############################################################
q #Starting Vector
#What is the minimum element in q?
min(q)
#What is the maximum element in q?
max(q)
#What is the sum of all elements in q?
sum(q)
#Calculate a cumulative or "running" total.
cumsum(q)
#What is the average element in q?
mean(q)
#What is the standard deviation of elements in q?
sd(q)
#What are the quantiles of elements in q?
quantile(q)
#What is the 90th percentile of elements in q?
quantile(q,probs= c(0.90))


#What is the minimum element in q?
min(q,na.rm=TRUE)
#What is the maximum element in q?
max(q,na.rm=TRUE)
#What is the sum of all elements in q?
sum(q,na.rm=TRUE)
#Calculate a cumulative or "running" total.
cumsum(q,na.rm=TRUE)
#What is the average element in q?
mean(q,na.rm=TRUE)
#What is the standard deviation of elements in q?
sd(q,na.rm=TRUE)
#What are the quantiles of elements in q?
quantile(q,na.rm=TRUE)
#What is the 90th percentile of elements in q?
quantile(q,probs= c(0.90),na.rm=TRUE)

q.obs<-q[which(!is.na(q))] #keep observations that are not missing
#q.obs<-subset(x=q,subset=!is.na(q)) is equivalent
q.obs
q.obs<-q[-which(is.na(q))] #remove observations that are missing
q.obs
cumsum(q.obs)

#############################################################
###Comparing with Vectors####################################
#############################################################
intersect(g,i)#Which elements do g and i share?
union(g,i)#Which elements do g and i have combined?
setdiff(g,i)#Which elements from g are not in i?
setdiff(i,g)#Which elements from i are not in g?

g%in%i
any(g%in%i)#Are any elements of g also in i?
all(g%in%i)#Are all elements of g in i? Is g a subset of i?
which(g%in%i)#What are the positions of elements of g which are also in i?
g[which(g%in%i)]#What are the elements of g which are also in i?
subset(x=g,subset=g%in%i)#What are the elements of g which are also in i?

#############################################################
###Data Types of Vectors#####################################
#############################################################
(x<-c(1,3+1i))
c(class(x),mode(x),typeof(x))
(x<-c(1,"hi"))
c(class(x),mode(x),typeof(x))
(x<-c(1,TRUE))
c(class(x),mode(x),typeof(x))
(x<-c(3+1i,TRUE))
c(class(x),mode(x),typeof(x))
(x<-c(TRUE,"hi"))
c(class(x),mode(x),typeof(x))
(x<-c(3+1i,"hi"))
c(class(x),mode(x),typeof(x))
(x<-c(1,3+1i,"hi"))
c(class(x),mode(x),typeof(x))
(x<-c(1,3+1i,TRUE))
c(class(x),mode(x),typeof(x))
(x<-c(1,TRUE,"hi"))
c(class(x),mode(x),typeof(x))
(x<-c(3+1i,TRUE,"hi"))
c(class(x),mode(x),typeof(x))
(x<-c(1,3+1i,"hi",TRUE))
c(class(x),mode(x),typeof(x))

#############################################################
###Factor Vectors############################################
#############################################################
###Character Vector
h
(h.factor<-as.factor(h))
nlevels(h.factor)
summary(h)
summary(h.factor)

###Numeric Vector
k
(k.factor<-as.factor(k))
summary(k)
summary(k.factor)
sum(k)
sum(k.factor)
sum(as.numeric(k.factor))

k
as.numeric(k.factor)
as.numeric(as.character(k.factor))

####################################################################################
####################################################################################
# Section: Lists
####################################################################################
####################################################################################

#############################################################
###Data Types of List#####################################
#############################################################
(x<-list(1,3+1i,"hi",TRUE))
c(class(x),mode(x),typeof(x))
c(class(x[[1]]),mode(x[[1]]),typeof(x[[1]]))
c(class(x[[2]]),mode(x[[2]]),typeof(x[[2]]))
c(class(x[[3]]),mode(x[[3]]),typeof(x[[3]]))
c(class(x[[4]]),mode(x[[4]]),typeof(x[[4]]))

is.list(x)
is.complex(x)
is.complex(x[[2]])
#############################################################
###List to Vector ###########################################
#############################################################
(y<-unlist(x))
c(class(y),mode(y),typeof(y))
is.list(y)
is.vector(y)