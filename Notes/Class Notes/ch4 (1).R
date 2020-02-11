#see page 40-41 in Chapter 2 notes to upload the file.
dat.csv<-read.csv("C:/Users/wcipolli/OneDrive/Teaching/2019FA-354/Notes/code/TurkData.csv",
                  header=TRUE,stringsAsFactors=FALSE)[-1,] #removes questions in second row

####Add common elements first--we comment about parts of the data that aren't added yet 
dat.clean<-with(data.frame(#Columns A-J
                               sub=Subject, IP=V6, Status=V7, StartDate=V8, EndDate=V9,
                               turkID=turkerId, rstatus=relationshipstatus, int.dating= interest_dating,	
                               attention.check=Att_Check, treatment=Cond1HS2LS,
                               ###Columns K-P are filled in based on the treatment (see below)
                               #Column Q
                               int.gender=Pref_1W2M,
                               #Columns R-BG Are Blank
                               #The comments below delineate the possible flows of the 
                               #survey. Each participant only takes one; we'll have to 
                               #work this in later.
                               #####################################################################
                               #Columns BH (Q359) denotes participants with SURVEY 1
                               #####################################################################
                               ####Columns BI-CQ result from PERSON 1 Asian Woman
                               ####Columns CT-ED result from PERSON 5 Black Woman
                               ####Columns EG-FO result from PERSON 4 White Woman
                               #####################################################################
                               #Columns FQ (Q426) denotes participants with SURVEY 2
                               #####################################################################
                               ####Columns FR-GZ result from PERSON 2
                               ####Columns HC-IK result from PERSON 1
                               ####Columns IN-JX result from PERSON 5
                               #####################################################################
                               #Columns JZ (Q492) denotes participants with SURVEY 3
                               #####################################################################
                               ####Columns KA-LI result from PERSON 3
                               ####Columns LL-MT result from PERSON 2
                               ####Columns MW-OE result from PERSON 1
                               #####################################################################
                               #Columns OG (Q558) denotes participants with SURVEY 4
                               #####################################################################
                               ####Columns OH-PP result from PERSON 4
                               ####Columns PS-RA result from PERSON 3
                               ####Columns RD-SL result from PERSON 2
                               #####################################################################
                               #Columns SN (Q624) denotes participants with SURVEY 5
                               #####################################################################
                               ####Columns SO-TY result from PERSON 5
                               ####Columns UB-VJ result from PERSON 4
                               ####Columns VM-WU result from PERSON 3
                               #####################################################################
                               #Columns WW (Q82) denotes participants with SURVEY 6
                               #####################################################################
                               ####Columns WX-YF result from PERSON 1
                               ####Columns YI-ZS result from PERSON 5
                               ####Columns ZV-ABD result from PERSON 4
                               #####################################################################
                               #Columns ABF (Q415) denotes participants with SURVEY 7
                               #####################################################################
                               ####Columns ABG-ACO result from PERSON 2
                               ####Columns ACR-ADZ result from PERSON 1
                               ####Columns AEC-AFM result from PERSON 5
                               #####################################################################
                               #Columns AFO (Q481) denotes participants with SURVEY 8
                               #####################################################################
                               ####Columns AFP-AGX result from PERSON 3
                               ####Columns AHA-AII result from PERSON 2
                               ####Columns AIL-AJT result from PERSON 1
                               #####################################################################
                               #Columns AJV (Q547) denotes participants with SURVEY 9
                               #####################################################################
                               ####Columns AJW-ALE result from PERSON 4
                               ####Columns ALH-AMP result from PERSON 3
                               ####Columns AMS-AOA result from PERSON 2
                               #####################################################################
                               #Columns AOC (Q613) denotes participants with SURVEY 10
                               #####################################################################
                               ####Columns AOD-APN result from PERSON 5
                               ####Columns APQ-AQY result from PERSON 4
                               ####Columns ARB-ASJ result from PERSON 3
                               #####################################################################
                               #Columns ASK-AUS
                               imp.relig=Q65_1,	imp.polit=Q65_2, imp.race=Q65_4, imp.hobbies=Q65_5,
                               most.relig=Q69_1,long.relig=Q69_2,most.polit=Q69_3,long.polit=Q69_4,
                               most.race=Q69_5, long.race=Q69_6,most.hobbies=Q69_7,long.hobbies=Q69_8,
                               #Q43	is 1 for all participants -- prompt questions about race
                               FT.white=FT1_1, FT.hispanic=FT1_2, FT.black=FT1_3, FT.asian=FT1_4,
                               Egal.equity=EgalMotivation_1, Egal.harmony=EgalMotivation_2,
                               #Demographic is 1 for all participants -- prompt questions about self
                               #Q47 is 1 for all participants -- prompt continues about motivations (black)
                               black.non_pred_import=IMS1_1,black.stereotypes_values_ok=IMS2R_1,black.non_pred_belief=IMS3_1,
                               black.stereotypes_values_wrong=IMS4_1,	black.non_pred_concept=IMS5_1,
                               black.PC=EMS1_1,	black.neg_react=EMS2_1,	black.anger=EMS3_1,	black.disapprove=EMS4_1,
                               black.pressure=EMS5_1,
                               #Q697 is 1 for all participants -- prompt continues about motivations (asian)
                               asian.non_pred_import=Q698_1,asian.stereotypes_values_ok=Q699_1,asian.non_pred_belief=Q700_1,
                               asian.stereotypes_values_wrong=Q701_1,	asian.non_pred_concept=Q702_1,
                               asian.PC=Q703_1,	asian.neg_react=Q704_1,	asian.anger=Q705_1,	asian.disapprove=Q706_1,
                               asian.pressure=Q707_1,	                            
                               #Q49	is 1 for all participants -- prompt continues about demographic information
                               age=age, gender=gender1m,	hispanic=hisp, white=race_1, black=race_2, native.pacific=race_3,
                               asian=race_4, other.race=race_5, other.race.text=Q399, sexual.orientation=sexo,
                               sexual.orientation.other.text=other, ladder.self=Q76, household.income=Q78,
                               highest.edu=Q80, device=Q393, purpose=Q377, connections.bool=Q379, connections.text=Q381),
data=dat.csv)

####################################################
### Treament Data from columns K-P
####################################################
dat.clean$graph.hobbies<-ifelse(dat.csv$Cond1HS2LS==1,dat.csv$simhob,dat.csv$simhob2)
dat.clean$graph.educ<-ifelse(dat.csv$Cond1HS2LS==1,dat.csv$simed,dat.csv$simed2)
dat.clean$graph.int<-ifelse(dat.csv$Cond1HS2LS==1,dat.csv$mixedrace,dat.csv$samerace)

####################################################
### Save survey type
####################################################
dat.clean$survey=rep(NA,nrow(dat.clean))
dat.clean$survey[which(dat.csv$Q359==1)]=1
dat.clean$survey[which(dat.csv$Q426==1)]=2
dat.clean$survey[which(dat.csv$Q492==1)]=3
dat.clean$survey[which(dat.csv$Q558==1)]=4
dat.clean$survey[which(dat.csv$Q624==1)]=5
dat.clean$survey[which(dat.csv$Q82==1)]=6
dat.clean$survey[which(dat.csv$Q415==1)]=7
dat.clean$survey[which(dat.csv$Q481==1)]=8
dat.clean$survey[which(dat.csv$Q547==1)]=9
dat.clean$survey[which(dat.csv$Q613==1)]=10


####################################################
### Save corresponding survey data across surveys
####################################################
#######################
###Asian Profile
#######################
dat.clean$a.p.appeal<-rep(NA,nrow(dat.clean))
dat.clean$a.p.not.appeal<-rep(NA,nrow(dat.clean))
dat.clean$a.p.romantic.int<-rep(NA,nrow(dat.clean))
dat.clean$a.p.date<-rep(NA,nrow(dat.clean))
####################################################
dat.clean$a.f.appeal<-rep(NA,nrow(dat.clean))
dat.clean$a.f.not.appeal<-rep(NA,nrow(dat.clean))
dat.clean$a.f.romantic.int<-rep(NA,nrow(dat.clean))
dat.clean$a.f.date<-rep(NA,nrow(dat.clean))
####################################################
###Ignore turn ons/offs outside of study (there are also varying numbers of these)
dat.clean$a.onoff.gender.group<-rep(NA,nrow(dat.clean))
dat.clean$a.onoff.gender.rank<-rep(NA,nrow(dat.clean))
dat.clean$a.onoff.race.group<-rep(NA,nrow(dat.clean))
dat.clean$a.onoff.race.rank<-rep(NA,nrow(dat.clean))
####################################################
dat.clean$a.rank.about<-rep(NA,nrow(dat.clean))
dat.clean$a.rank.hobbie<-rep(NA,nrow(dat.clean))
dat.clean$a.rank.food<-rep(NA,nrow(dat.clean))
dat.clean$a.rank.friends<-rep(NA,nrow(dat.clean))
dat.clean$a.rank.race<-rep(NA,nrow(dat.clean))
dat.clean$a.rank.gender<-rep(NA,nrow(dat.clean))
#######################
###Black Profile
#######################
dat.clean$b.p.appeal<-rep(NA,nrow(dat.clean))
dat.clean$b.p.not.appeal<-rep(NA,nrow(dat.clean))
dat.clean$b.p.romantic.int<-rep(NA,nrow(dat.clean))
dat.clean$b.p.date<-rep(NA,nrow(dat.clean))
####################################################
dat.clean$b.f.appeal<-rep(NA,nrow(dat.clean))
dat.clean$b.f.not.appeal<-rep(NA,nrow(dat.clean))
dat.clean$b.f.romantic.int<-rep(NA,nrow(dat.clean))
dat.clean$b.f.date<-rep(NA,nrow(dat.clean))
####################################################
###Ignore turn ons/offs outside of study (there are also varying numbers of these)
dat.clean$b.onoff.gender.group<-rep(NA,nrow(dat.clean))
dat.clean$b.onoff.gender.rank<-rep(NA,nrow(dat.clean))
dat.clean$b.onoff.race.group<-rep(NA,nrow(dat.clean))
dat.clean$b.onoff.race.rank<-rep(NA,nrow(dat.clean))
####################################################
dat.clean$b.rank.about<-rep(NA,nrow(dat.clean))
dat.clean$b.rank.hobbie<-rep(NA,nrow(dat.clean))
dat.clean$b.rank.food<-rep(NA,nrow(dat.clean))
dat.clean$b.rank.friends<-rep(NA,nrow(dat.clean))
dat.clean$b.rank.race<-rep(NA,nrow(dat.clean))
dat.clean$b.rank.gender<-rep(NA,nrow(dat.clean))
#######################
###White Profile
#######################
dat.clean$w.p.appeal<-rep(NA,nrow(dat.clean))
dat.clean$w.p.not.appeal<-rep(NA,nrow(dat.clean))
dat.clean$w.p.romantic.int<-rep(NA,nrow(dat.clean))
dat.clean$w.p.date<-rep(NA,nrow(dat.clean))
####################################################
dat.clean$w.f.appeal<-rep(NA,nrow(dat.clean))
dat.clean$w.f.not.appeal<-rep(NA,nrow(dat.clean))
dat.clean$w.f.romantic.int<-rep(NA,nrow(dat.clean))
dat.clean$w.f.date<-rep(NA,nrow(dat.clean))
####################################################
###Ignore turn ons/offs outside of study (there are also varying numbers of these)
dat.clean$w.onoff.gender.group<-rep(NA,nrow(dat.clean))
dat.clean$w.onoff.gender.rank<-rep(NA,nrow(dat.clean))
dat.clean$w.onoff.race.group<-rep(NA,nrow(dat.clean))
dat.clean$w.onoff.race.rank<-rep(NA,nrow(dat.clean))
####################################################
dat.clean$w.rank.about<-rep(NA,nrow(dat.clean))
dat.clean$w.rank.hobbie<-rep(NA,nrow(dat.clean))
dat.clean$w.rank.food<-rep(NA,nrow(dat.clean))
dat.clean$w.rank.friends<-rep(NA,nrow(dat.clean))
dat.clean$w.rank.race<-rep(NA,nrow(dat.clean))
dat.clean$w.rank.gender<-rep(NA,nrow(dat.clean))

######################################################
######################################################
### This function collects the data for users that
### took each surevy. See commented code below to
### see how the function was created.
######################################################
######################################################
extract.data<-function(d,d.csv,survey,aperson,bperson,wperson,sex){
  ####################################################
  ### Survey k
  ####################################################
  obs<-which(d$survey==survey)
  
  #######################
  ###Asian
  #######################
  d$a.p.appeal[obs]<-d.csv[obs,paste("PP",aperson,"_A",sex,"_like_1",sep="")]
  d$a.p.not.appeal[obs]<-d.csv[obs,paste("PP",aperson,"_A",sex,"_like_2",sep="")]
  d$a.p.romantic.int[obs]<-d.csv[obs,paste("PP",aperson,"_A",sex,"_like_3",sep="")]
  d$a.p.date[obs]<-d.csv[obs,paste("PP",aperson,"_A",sex,"_date_2",sep="")]
  ####################################################
  d$a.f.appeal[obs]<-d.csv[obs,paste("FP",aperson,"_A",sex,"_like_1",sep="")]          
  d$a.f.not.appeal[obs]<-d.csv[obs,paste("FP",aperson,"_A",sex,"_like_2",sep="")]      
  d$a.f.romantic.int[obs]<-d.csv[obs,paste("FP",aperson,"_A",sex,"_like_3",sep="")]
  d$a.f.date[obs]<-d.csv[obs,paste("FP",aperson,"_A",sex,"_date_2",sep="")]
  ####################################################
  ###Ignore turn ons/offs outside of study
  d$a.onoff.gender.group[obs]<-d.csv[obs,paste("FP",aperson,"_A",sex,"_sort_14_Group",sep="")]
  d$a.onoff.gender.rank[obs]<-d.csv[obs,paste("FP",aperson,"_A",sex,"_sort_14_Rank",sep="")]
  d$a.onoff.race.group[obs]<-d.csv[obs,paste("FP",aperson,"_A",sex,"_sort_15_Group",sep="")]
  d$a.onoff.race.rank[obs]<-d.csv[obs,paste("FP",aperson,"_A",sex,"_sort_15_Rank",sep="")]
  ####################################################
  d$a.rank.about[obs]<-d.csv[obs,paste("FP",aperson,"_A",sex,"_rank_1",sep="")]
  d$a.rank.hobbie[obs]<-d.csv[obs,paste("FP",aperson,"_A",sex,"_rank_2",sep="")]
  d$a.rank.food[obs]<-d.csv[obs,paste("FP",aperson,"_A",sex,"_rank_4",sep="")]
  d$a.rank.friends[obs]<-d.csv[obs,paste("FP",aperson,"_A",sex,"_rank_7",sep="")]
  d$a.rank.race[obs]<-d.csv[obs,paste("FP",aperson,"_A",sex,"_rank_5",sep="")]
  d$a.rank.gender[obs]<-d.csv[obs,paste("FP",aperson,"_A",sex,"_rank_6",sep="")]
  
  #######################
  ###Black
  #######################
  d$b.p.appeal[obs]<-d.csv[obs,paste("PP",bperson,"_B",sex,"_like_1",sep="")]
  d$b.p.not.appeal[obs]<-d.csv[obs,paste("PP",bperson,"_B",sex,"_like_2",sep="")]
  d$b.p.romantic.int[obs]<-d.csv[obs,paste("PP",bperson,"_B",sex,"_like_3",sep="")]
  d$b.p.date[obs]<-d.csv[obs,paste("PP",bperson,"_B",sex,"_date_2",sep="")]
  ####################################################
  d$b.f.appeal[obs]<-d.csv[obs,paste("FP",bperson,"_B",sex,"_like_1",sep="")]          
  d$b.f.not.appeal[obs]<-d.csv[obs,paste("FP",bperson,"_B",sex,"_like_2",sep="")]      
  d$b.f.romantic.int[obs]<-d.csv[obs,paste("FP",bperson,"_B",sex,"_like_3",sep="")]
  d$b.f.date[obs]<-d.csv[obs,paste("FP",bperson,"_B",sex,"_date_2",sep="")]
  ####################################################
  ###Ignore turn ons/offs outside of study
  d$b.onoff.gender.group[obs]<-d.csv[obs,paste("FP",bperson,"_B",sex,"_sort_14_Group",sep="")]
  d$b.onoff.gender.rank[obs]<-d.csv[obs,paste("FP",bperson,"_B",sex,"_sort_14_Rank",sep="")]
  d$b.onoff.race.group[obs]<-d.csv[obs,paste("FP",bperson,"_B",sex,"_sort_15_Group",sep="")]
  d$b.onoff.race.rank[obs]<-d.csv[obs,paste("FP",bperson,"_B",sex,"_sort_15_Rank",sep="")]
  ####################################################
  d$b.rank.about[obs]<-d.csv[obs,paste("FP",bperson,"_B",sex,"_rank_1",sep="")]
  d$b.rank.hobbie[obs]<-d.csv[obs,paste("FP",bperson,"_B",sex,"_rank_2",sep="")]
  d$b.rank.food[obs]<-d.csv[obs,paste("FP",bperson,"_B",sex,"_rank_4",sep="")]
  d$b.rank.friends[obs]<-d.csv[obs,paste("FP",bperson,"_B",sex,"_rank_7",sep="")]
  d$b.rank.race[obs]<-d.csv[obs,paste("FP",bperson,"_B",sex,"_rank_5",sep="")]
  d$b.rank.gender[obs]<-d.csv[obs,paste("FP",bperson,"_B",sex,"_rank_6",sep="")]
  
  #######################
  ###White
  #######################
  d$w.p.appeal[obs]<-d.csv[obs,paste("PP",wperson,"_W",sex,"_like_1",sep="")]
  d$w.p.not.appeal[obs]<-d.csv[obs,paste("PP",wperson,"_W",sex,"_like_2",sep="")]
  d$w.p.romantic.int[obs]<-d.csv[obs,paste("PP",wperson,"_W",sex,"_like_3",sep="")]
  d$w.p.date[obs]<-d.csv[obs,paste("PP",wperson,"_W",sex,"_date_2",sep="")]
  ####################################################
  d$w.f.appeal[obs]<-d.csv[obs,paste("FP",wperson,"_W",sex,"_like_1",sep="")]          
  d$w.f.not.appeal[obs]<-d.csv[obs,paste("FP",wperson,"_W",sex,"_like_2",sep="")]      
  d$w.f.romantic.int[obs]<-d.csv[obs,paste("FP",wperson,"_W",sex,"_like_3",sep="")]
  d$w.f.date[obs]<-d.csv[obs,paste("FP",wperson,"_W",sex,"_date_2",sep="")]
  ####################################################
  ###Ignore turn ons/offs outside of study
  d$w.onoff.gender.group[obs]<-d.csv[obs,paste("FP",wperson,"_W",sex,"_sort_14_Group",sep="")]
  d$w.onoff.gender.rank[obs]<-d.csv[obs,paste("FP",wperson,"_W",sex,"_sort_14_Rank",sep="")]
  d$w.onoff.race.group[obs]<-d.csv[obs,paste("FP",wperson,"_W",sex,"_sort_15_Group",sep="")]
  d$w.onoff.race.rank[obs]<-d.csv[obs,paste("FP",wperson,"_W",sex,"_sort_15_Rank",sep="")]
  ####################################################
  d$w.rank.about[obs]<-d.csv[obs,paste("FP",wperson,"_W",sex,"_rank_1",sep="")]
  d$w.rank.hobbie[obs]<-d.csv[obs,paste("FP",wperson,"_W",sex,"_rank_2",sep="")]
  d$w.rank.food[obs]<-d.csv[obs,paste("FP",wperson,"_W",sex,"_rank_4",sep="")]
  d$w.rank.friends[obs]<-d.csv[obs,paste("FP",wperson,"_W",sex,"_rank_7",sep="")]
  d$w.rank.race[obs]<-d.csv[obs,paste("FP",wperson,"_W",sex,"_rank_5",sep="")]
  d$w.rank.gender[obs]<-d.csv[obs,paste("FP",wperson,"_W",sex,"_rank_6",sep="")]
  
  return(d)
}
#####################################################################
#Columns BH (Q359) denotes participants with SURVEY 1
#####################################################################
####Columns BI-CQ result from PERSON 1 Asian Woman
####Columns CT-ED result from PERSON 5 Black Woman
####Columns EG-FO result from PERSON 4 White Woman
dat.clean<-extract.data(d=dat.clean,d.csv=dat.csv,survey=1,
                        aperson=1,bperson=5,wperson=4,sex="W")

#####################################################################
#Columns FQ (Q426) denotes participants with SURVEY 2
#####################################################################
####Columns FR-GZ result from PERSON 2 Asian Woman
####Columns HC-IK result from PERSON 1 Black Woman
####Columns IN-JX result from PERSON 5 White Woman
dat.clean<-extract.data(d=dat.clean,d.csv=dat.csv,survey=2,
                        aperson=2,bperson=1,wperson=5,sex="W")

#####################################################################
#Columns JZ (Q492) denotes participants with SURVEY 3
#####################################################################
####Columns KA-LI result from PERSON 3 Asian Woman
####Columns LL-MT result from PERSON 2 Black Woman
####Columns MW-OE result from PERSON 1 White Woman
dat.clean<-extract.data(d=dat.clean,d.csv=dat.csv,survey=3,
                        aperson=3,bperson=2,wperson=1,sex="W")

#####################################################################
#Columns OG (Q558) denotes participants with SURVEY 4
#####################################################################
####Columns OH-PP result from PERSON 4 Asian Woman
####Columns PS-RA result from PERSON 3 Black Woman
####Columns RD-SL result from PERSON 2 White Woman
dat.clean<-extract.data(d=dat.clean,d.csv=dat.csv,survey=4,
                        aperson=4,bperson=3,wperson=2,sex="W")

#####################################################################
#Columns SN (Q624) denotes participants with SURVEY 5
#####################################################################
####Columns SO-TY result from PERSON 5 Asian Woman
####Columns UB-VJ result from PERSON 4 Black Woman
####Columns VM-WU result from PERSON 3 White Woman
dat.clean<-extract.data(d=dat.clean,d.csv=dat.csv,survey=5,
                        aperson=5,bperson=4,wperson=3,sex="W")

#####################################################################
#Columns WW (Q82) denotes participants with SURVEY 6
#####################################################################
####Columns WX-YF result from PERSON 1 Asian Man
####Columns YI-ZS result from PERSON 5 Black Man
####Columns ZV-ABD result from PERSON 4 White Man
dat.clean<-extract.data(d=dat.clean,d.csv=dat.csv,survey=6,
                        aperson=1,bperson=5,wperson=4,sex="M")

#####################################################################
#Columns ABF (Q415) denotes participants with SURVEY 7
#####################################################################
####Columns ABG-ACO result from PERSON 2 Asian Man
####Columns ACR-ADZ result from PERSON 1 Black Man
####Columns AEC-AFM result from PERSON 5 White Man
dat.clean<-extract.data(d=dat.clean,d.csv=dat.csv,survey=7,
                        aperson=2,bperson=1,wperson=5,sex="M")

#####################################################################
#Columns AFO (Q481) denotes participants with SURVEY 8
#####################################################################
####Columns AFP-AGX result from PERSON 3 Asian Man
####Columns AHA-AII result from PERSON 2 Black Man
####Columns AIL-AJT result from PERSON 1 White Man
dat.clean<-extract.data(d=dat.clean,d.csv=dat.csv,survey=8,
                        aperson=3,bperson=2,wperson=1,sex="M")

#####################################################################
#Columns AJV (Q547) denotes participants with SURVEY 9
#####################################################################
####Columns AJW-ALE result from PERSON 4 Asian Man
####Columns ALH-AMP result from PERSON 3 Black Man
####Columns AMS-AOA result from PERSON 2 White Man
dat.clean<-extract.data(d=dat.clean,d.csv=dat.csv,survey=9,
                        aperson=4,bperson=3,wperson=2,sex="M")

#####################################################################
#Columns AOC (Q613) denotes participants with SURVEY 10
#####################################################################
####Columns AOD-APN result from PERSON 5 Asian Man
####Columns APQ-AQY result from PERSON 4 Black Man
####Columns ARB-ASJ result from PERSON 3 White Man
#####################################################################
dat.clean<-extract.data(d=dat.clean,d.csv=dat.csv,survey=10,
                        aperson=5,bperson=4,wperson=3,sex="M")

####################################################
### Rework Data Types
####################################################
#All columns are treated as character vectors:
table(apply(X=dat.clean,MARGIN=2,FUN=typeof))

character.col<-c("sub","IP","Status","StartDate","EndDate","turkID",
"other.race.text","sexual.orientation.other.text","purpose",
"connections.text")

numeric.col<-c("imp.relig", "imp.polit", "imp.race", "imp.hobbies",
               "most.relig","long.relig","most.polit","long.polit",
               "most.race", "long.race","most.hobbies","long.hobbies",
               "FT.white", "FT.hispanic", "FT.black", "FT.asian",
               "Egal.equity", "Egal.harmony", "black.non_pred_import",
               "black.stereotypes_values_ok","black.non_pred_belief",
               "black.stereotypes_values_wrong",	"black.non_pred_concept",
               "black.PC",	"black.neg_react", "black.anger",	"black.disapprove",
               "black.pressure","asian.non_pred_import","asian.stereotypes_values_ok",
               "asian.non_pred_belief","asian.stereotypes_values_wrong",
               "asian.non_pred_concept","asian.PC","asian.neg_react",	"asian.anger",
               "asian.disapprove","asian.pressure","age","ladder.self", "household.income",
               "a.p.appeal","a.p.not.appeal","a.p.romantic.int","a.p.date",
               "a.f.appeal","a.f.not.appeal","a.f.romantic.int","a.f.date",
               "a.onoff.gender.rank","a.onoff.race.rank","a.rank.about",
               "a.rank.hobbie","a.rank.food","a.rank.friends","a.rank.race",
               "a.rank.gender",
               "b.p.appeal","b.p.not.appeal","b.p.romantic.int","b.p.date",
               "b.f.appeal","b.f.not.appeal","b.f.romantic.int","b.f.date",
               "b.onoff.gender.rank","b.onoff.race.rank","b.rank.about",
               "b.rank.hobbie","b.rank.food","b.rank.friends","b.rank.race",
               "b.rank.gender",
               "w.p.appeal","w.p.not.appeal","w.p.romantic.int","w.p.date",
               "w.f.appeal","w.f.not.appeal","w.f.romantic.int","w.f.date",
               "w.onoff.gender.rank","w.onoff.race.rank","w.rank.about",
               "w.rank.hobbie","w.rank.food","w.rank.friends","w.rank.race",
               "w.rank.gender")

factor.col<-c("rstatus","int.dating","attention.check","treatment",
              "graph.hobbies","graph.educ","graph.int","int.gender",
              "gender",	"hispanic", "white", "black", "native.pacific",
              "asian", "other.race", "sexual.orientation", "highest.edu",
              "device","connections.bool","a.onoff.gender.group","a.onoff.race.group",
              "b.onoff.gender.group","b.onoff.race.group","w.onoff.gender.group",
              "w.onoff.race.group","survey")

dat.clean[,numeric.col]<-apply(X=dat.clean[,numeric.col],MARGIN=2,FUN=as.numeric)
dat.clean[,factor.col]<-apply(X=dat.clean[,factor.col],MARGIN=2,FUN=as.factor)

