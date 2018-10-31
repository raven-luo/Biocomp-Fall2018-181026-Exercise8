setwd("C:/Users/raven/Desktop/biocomputing/exercise8/Biocomp-Fall2018-181026-Exercise8")

#Task1
UWvMSU=read.table(file="UWvMSU_1-22-13.txt",header=TRUE,stringsAsFactors=FALSE) #read table will recognize columns. csv only recognizes comma
UW_ins=UWvMSU[UWvMSU[,2]=="UW",c(1,3)] #instantaneous times and scores of UW are picked out
MSU_ins=UWvMSU[UWvMSU[,2]=="MSU",c(1,3)] #I repeated the code because there are only two teams. Otherwise I can create a function and feed a vector of team names into it
#make a new dataframe with cumulative scores added in
cumu_get=function(ins){
  cumu=data.frame(ins,cumulative=vector(mode="numeric",length=nrow(ins)))
  cumu[1,3]=cumu[1,2]
  i=2
  while(i<=nrow(ins)) {
    cumu[i,3]=cumu[i-1,3]+cumu[i,2]
    i=i+1
  }
  return(cumu)
}
UW_cumu=cumu_get(UW_ins)
MSU_cumu=cumu_get(MSU_ins)
UWvMSU_graph=plot(UW_cumu[,1],UW_cumu[,3],type="l",main="UW vs MSU Cumulative Scores over Time",xlab="Time (min)",ylab="Cumulative Score",col="red")
lines(MSU_cumu[,1],MSU_cumu[,3],col="blue")
legend("topleft",c("UW","MSU"),pch=16,col=c("red","blue")) #Legend doesn't know data. pch is shape. Color is for shape.

#Task2
#So you need to make it into a function after all
#We want to prevent R from taking code as Guess input
game=function(){
  print("I'm thinking of a number 1-100...")
  answer=sample(1:100,1)
  attempt=0 #I set attempt to 0 to prevent R from taking code to be user Guess input
  #you will go into loop because 0!=answer
  while (attempt!=answer) {
    attempt=readline(prompt="Guess:") #take input first
    if (attempt>answer) {print("Lower")} else if (attempt<answer) {print("Higher")} else {print("Correct!")}
  }
  }
    #in this way, once attempt==answer, print Correct, and then because attempt!=answer is FALSE, you don't go into the while loop again
game()