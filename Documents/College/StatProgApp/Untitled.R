
#create table
Pr <- matrix(c(0.17, NA, 0.08, 0.18,0.24,.20), byrow=FALSE, 2,3 )

colnames(Pr)<-(c("A","B","C"))
rownames(Pr)<-(c("D", "E"))

Pr

#count number of instances missing 
MissingDataPoints <- length(which(is.na(Pr)))
MissingDataPoints
#get location of missing instances
Location <- which(is.na(Pr), arr.ind=TRUE)

Location


#calculate value of existing data
existing <- 0
for(row in 1:nrow(Pr)) {
  for(col in 1:ncol(Pr)) {
    entry <- Pr[row, col]
    #print(entry)
    if (!is.na(entry)){
      existing <- existing + entry
    }
  }
}


#use summation rule to find sum of missing data

ValMissingDP<- 1-existing



# fill in the missing data points
missingVal <- ValMissingDP
#print the new table
Pr[Location]<-ValMissingDP
Pr[Location]
Pr

#calculate location of missing data points using the marginals
A <-sum(Pr[,1])
B <-sum(Pr[,2])
C <-sum(Pr[,3])
D <-sum(Pr[1,])
E <-sum(Pr[2,])



side_marginals<-A+B+C
bottom_marginals<-D+E

side_marginals
bottom_marginals

#to check for indepdance of A and D
AnD <- Pr[1,1] 

AtimesD <- A*D

AnD
AtimesD


if (AnD==A*D){
  print(" A and D are independant")
 
}else{
  print (" A and D are not independant")
}
#conditionals
BnE=Pr[2,2]
BgivenE=BnE/E
BgivenE

EgivenB= BnE/B

EgivenB

EgivenBBayes= (BgivenE*E)/B
EgivenBBayes

#union
EUB=E+B-BnE

EUB

