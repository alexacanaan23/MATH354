###CHAPTER 3 -- CRAN R     ##Descriptive Title
###CIPOLLI                 ##Author

####################################################################################
####################################################################################
# Section: Conditional Statments
####################################################################################
####################################################################################

#############################################################
###Recalling logical statements##############################
#############################################################
x=5
x<3 #"less than three"
x<=3 #"at most three"
x>3 #"greater than three"
x>=3 #"at least three"
x==3 #"equal to three" note the double equals sign"
x%%2==0 #"is even" note this says the remainder is zero when divided by 2"
is.numeric(x) #"is x of numeric type?"
is.character(x) #"Is x of character type?"

#############################################################
###Conditional Shorthand ####################################
#############################################################
library("carData")
data("Vocab")
head(Vocab)
#Using the which() function -- starting vector of 0s
pass1<-rep(0,nrow(Vocab))
pass1[which(Vocab$vocabulary>=6)]<-1
pass2<-rep(0,nrow(Vocab))
pass2[which(Vocab$vocabulary>5)]<-1
#Using the which() function -- starting vector of 1s
pass3<-rep(1,nrow(Vocab))
pass3[which(Vocab$vocabulary<6)]<-0
pass4<-rep(1,nrow(Vocab))
pass4[which(Vocab$vocabulary<=5)]<-0
#Using the ifelse() function
pass5<-ifelse(Vocab$vocabulary>=6,1,0)
pass6<-ifelse(Vocab$vocabulary>5,1,0)

#Tabulate frequencies
table(pass1)
#Tabulate proportions
prop.table(table(pass1))

#############################################################
###Conditional Blocks #######################################
#############################################################
bmi<-23.4
if(bmi<0){
  category<-NA
}else if(bmi<18.5){
  category<-"Underweight"
}else if(bmi<25){
  category<-"Normal"
}else if(bmi<30){
  category<-"Overweight"
}else if(bmi>=30){
  category<-"Obese"
}
category

#############################################################
###Loops and Conditional Statements##########################
#############################################################
library("MASS") #Loads Pima Indian Data
length(Pima.te$bmi)#332 sample size
category<-rep(NA,length(Pima.te$bmi)) #a blank vector for filling
for(i in 1:length(Pima.te$bmi)){#for each BMI observation
  bmi=Pima.te$bmi[i] #set bmi to observation i
  if(bmi<0){
    cat<-NA #negative bmi values are not possible
  }else if(bmi<18.5){
    cat<-"Underweight"
  }else if(bmi<25){
    cat<-"Normal"
  }else if(bmi<30){
    cat<-"Overweight"
  }else{ #otherwise bmi>=30
    cat<-"Obese"
  } #end else if block
  category[i]=cat #save the BMI category for observation i
} #end for loop
table(category,Pima.te$type)
#calculates proportions conditional on bmi category
prop.table(x=table(category,Pima.te$type),margin=1)


####################################################################################
####################################################################################
# Section:  Loops
####################################################################################
####################################################################################

#############################################################
###Repeat loop ##############################################
#############################################################
nTrial<-0 #A object for counting trials
nOutlier<-0 #A object for counting outliers
repeat{
  nTrial<-nTrial+1 #count the trial at the beginning of each iteration
  x<-rnorm(n=1,mean=0,sd=1) #generate random normal data
  if(x < -3){ #first outlier condition
    nOutlier<-nOutlier+1
  }else if(x>3){ #second outlier condition
    nOutlier<-nOutlier+1
  }else{ #otherwise -3<=x<=3 (not an outlier)
    #do nothing, the observation is not outlying
  }
  if(nOutlier==100){ #stopping condition
    break
  }
}
nOutlier
nTrial
nOutlier/nTrial #True proportion is about 0.0027

#############################################################
###While Loop ###############################################
#############################################################
nTrial<-0 #A object for counting trials
nOutlier<-0 #A object for counting outliers
while(nOutlier<100){ #complete while the count of outliers is less than 100
  nTrial<-nTrial+1 #count the trial at the beginning of each iteration
  x<-rnorm(1,0,1) #generate random normal data
  if(x < -3){ #first outlier condition
    nOutlier<-nOutlier+1
  }else if(x>3){ #second outlier condition
    nOutlier<-nOutlier+1
  }else{ #otherwise -3<=x<=3 (not an outlier)
    #do nothing, the observation is not outlying
  }
}
nOutlier
nTrial
nOutlier/nTrial #True proportion is about 0.0027

#############################################################
###Power Sum example#########################################
#############################################################
library("stringr") #This package has a tool for separating digits into a vector
max.digit.sum<-0
max.k<-0
for(k in 1:20){
  current.power<-2^k
  #Split the power into a vector of digits
  #simplify=TRUE ensures a vector, not a list, is returned
  digits<-as.numeric(str_split(string=current.power,pattern="",simplify=TRUE))
  current.digit.sum<-sum(digits)
  if(current.digit.sum>=25){
    next #skips to next loop because restriction is transgressed
  }
  if(current.digit.sum>max.digit.sum){
    max.digit.sum<-current.digit.sum
    max.k<-k
  }
}
max.k
max.digit.sum


max.digit.sum<-0
max.k<-0
for(k in 1:20){
  current.power<-2^k
  #Split the power into a vector of digits
  #simplify=TRUE ensures a vector, not a list, is returned
  digits<-as.numeric(str_split(string=current.power,pattern="",simplify=TRUE))
  current.digit.sum<-sum(digits)
  if(current.digit.sum>max.digit.sum&current.digit.sum<25){
    max.digit.sum<-current.digit.sum
    max.k<-k
  }
}
max.k
max.digit.sum

####################################################################################
####################################################################################
# Section:  Functions
####################################################################################
####################################################################################

sqrt(16)

#############################################################
###Negative root function ###################################
#############################################################
nsqrt<-function(x){ #nsqrt will give the negative square root of positive real x
  if(x>=0){ #the square root does not exist for negative values
    return(-sqrt(x)) #return the negative root to the user
  }else{ #the user has asked for the square root of an inappropriate in
    stop("The square root of a negative number produces non-real solutions.")
  }
}
nsqrt(16) #nsqrt() is a function, x=16 is input
nsqrt(-1)

#############################################################
###Both root function #######################################
#############################################################
#bothsqrst will give both the negative and positive square root of positive real x
bothsqrst<-function(x){
  if(x>0){
    c(-sqrt(x),sqrt(x))
  }else{
    stop("The square root of a negative number produces non-real solutions.")
  }
}
bothsqrts(16) #bothsqrts() is a function, x=16 is input
bothsqrst(-1)

#############################################################
###generalized root function ################################
#############################################################
#gensqrt will give the negative and positive square root of any input x
gensqrt<-function(x){
  if(is.complex(x)){
    return(c(-sqrt(x),sqrt(x))) #sqrt works for complex input
  }
  if(is.numeric(x)){
    if(x>0){
      return(c(-sqrt(x),sqrt(x))) #sqrt works for positive real input
    } else{
      warning("The square root of a negative number yields a complex solution.")
      #we must convert numeric to complex for negative real input 
      return(c(-sqrt(as.complex(x)),sqrt(as.complex(x))))
    }
  }
  if(!is.numeric(x)&!is.complex(x)){
    stop("The gensqrt function requires complex or real input.")
  }
}
gensqrt(16)
gensqrt(-1)
gensqrt("hello")

x<-3+1i
is.numeric(x)
x<0
is.numeric(x) & x<0
is.numeric(x) && x<0

#############################################################
###generalized root function ################################
#############################################################
gensqrt2<-function(x,type="positive"){
  if(!is.numeric(x)&!is.complex(x)){
    stop("The gensqrt function requires complex or real input.")
  }else{
    if(is.numeric(x) && x<0){
      x<-as.complex(x)
      warning("The square root of a negative number yields a complex solution.")
    }
    if(type=="positive"){
      return(sqrt(x))   
    }else if(type=="negative"){
      return(-sqrt(x))
    }else if(type=="both"){
      return(c(-sqrt(x),sqrt(x)))
    }else{
      stop("The type argument must be `positive', 'negative', or 'both'.")
    } 
  }
}
gensqrt2(16,type="both")
gensqrt2(25,type="negative")
gensqrt2(-1,type="positive")
gensqrt2("hello")

gensqrt2(-1)

#############################################################
###applying functions to vectors#############################
#############################################################
sqrt(c(1,4,9,16,25))
nsqrt(c(1,4,9,16,25))
bothsqrst(c(1,4,9,16,25))
gensqrt(c(1,4,9,16,25))

#############################################################
###vectorized negative root function#########################
#############################################################
nsqrt.vectorized<-function(x){
  output<-rep(NA,length(x)) #a place to save output
  for(i in 1:length(x)){ #loop through each observation
    if(x[i]>=0){ #change all x to x[i] so that each x is run
      output[i]<-(-sqrt(x[i])) #save the output for the current x value
    }else{
      stop("The square root of a negative number produces non-real solutions.")
    }
  }
  return(output) #return the output for all x
}
nsqrt.vectorized(16)
nsqrt.vectorized(c(1,4,9,16,25))

#############################################################
###vectorized generalized root function######################
#############################################################
gensqrt.vectorized<-function(x){
  output<-data.frame(input=rep(NA,length(x)),negative.root=rep(NA,length(x)),
                     positive.root=rep(NA,length(x)))
  for(i in 1:length(x)){ #loop through each observation
    if(is.complex(x[i])){ #change all x to x[i] so that each x is run
      #save the output for the current x value
      output[i,]<-c(x[i],-sqrt(x[i]),sqrt(x[i]))
    }
    if(is.numeric(x[i])){
      if(x[i]>0){
        #save the output for the current x value
        output[i,]<-c(x[i],-sqrt(x[i]),sqrt(x[i]))
      } else{
        warning("The square root of a negative number yields a complex solution.")
        #save the output for the current x value
        output[i,]<-c(x[i],-sqrt(as.complex(x[i])),sqrt(as.complex(x[i])))
      }
    }
    if(!is.numeric(x[i])&!is.complex(x[i])){
      stop("The gensqrt function requires complex or real input.")
    }
  }
  return(output) #return the output for all x
}
gensqrt.vectorized(16)
gensqrt.vectorized(c(1,4,9,16,25))

####################################################################################
####################################################################################
# Section:  Apply Functions
####################################################################################
####################################################################################
A<-matrix(data = c(2,0,1,8),nrow = 2,ncol = 2)
A
apply(X=A,MARGIN=1,FUN=mean) #margin=1 indicates to apply summary() to the rows
apply(X=A,MARGIN=2,FUN=mean) #margin=2 indicates to apply summary() to the columns

#load data
ship1dat<-read.table(file = "https://cipolli.com/students/data/ship1.txt",
                     sep=",",header=TRUE)


head(ship1dat)
class(ship1dat$iNKT)
class(ship1dat$Ship1)
apply(X=ship1dat,MARGIN=2,FUN=class) #margin=2 indicates to apply summary() to the columns

head(as.matrix(ship1dat))


gensqrt.vectorized(c(1,4,9,16,25))
lapply(X=c(1,4,9,16,25),FUN=gensqrt)
sapply(X=c(1,4,9,16,25),FUN=gensqrt)

mean(ship1dat$iNKT[which(ship1dat$Ship1=="-/-")])
mean(ship1dat$iNKT[which(ship1dat$Ship1=="+/-")])
mean(ship1dat$iNKT[which(ship1dat$Ship1=="+/+")])
tapply(X=ship1dat$iNKT,INDEX=ship1dat$Ship1,FUN=mean)





bmi<-function(bmi){
  category<-NULL
  if(bmi<0){
    category<-NA
  }else if(bmi<18.5){
    category<-"Underweight"
  }else if(bmi<25){
    category<-"Normal"
  }else if(bmi<30){
    category<-"Overweight"
  }else{
    category<-"Obese"
  }
  return(category)
}
bmi(23.4)

#apply bmi() function to all BMI observations
category<-sapply(X=Pima.te$bmi,FUN=bmi)
table(category,Pima.te$type) #create the 
#calculates proportions conditional on bmi category
prop.table(x=table(category,Pima.te$type),margin=1)