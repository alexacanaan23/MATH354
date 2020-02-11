###CHAPTER 0 -- CRAN R     ##Descriptive Title
###CIPOLLI                 ##Author

#############################################################
###Commenting Code in R######################################
#############################################################
# This is a comment -- nothing after a # on the same line will be run.
# If I go to the next line and I'm still commenting, I need to put another #
# at the front of that line.

#############################################################
###Algebraic Calculations####################################
#############################################################
#Adding in R is easy
4+5 #Ask to add using +

#Subtracting in R is easy, too
10-15 #Ask to subtract using -

#Multiplying in R is easy, too
3*5 #Ask to multiply using *

#Dividing in R is easy, too
27/3 #Ask to divide using /

#Square Roots in R are easy, too
sqrt(69) #Ask for a sqrt with sqrt()

#We can do several calculations at once but we want to ensure that we're 
#careful with order of operations.
(3+5)*3/3^2 #Just squares the 3 according to pEmdas
((3+5)*3/3)^2 #Squares the result of the expression in parenthesis according to Pemdas
#These give very different answers showing this is important!

#############################################################
###Errors in R###############################################
#############################################################
#((3+5)*3/3^2   ##Should be ((3+5)*3/3)^2
#Hit ESC to let R know you've made a mistake
#This will change the still waiting (+) to a new command (>)

#############################################################
###Errors in R###############################################
#############################################################
#((3+5)3/3)^2  ##Should be ((3+5)*3/3)^2


#############################################################
###Downloading and Using Libraries###########################
#############################################################

#Download and installs flea data
install.packages("tourr")
#Loads flea data
library("tourr")
#Download and installs plotting features
#Loads plotting features
install.packages("plotly")
library("plotly")
#Demonstrates the plotting ability of R further motivating us to learn it!
demo("animation-tour-basic",package = "plotly")