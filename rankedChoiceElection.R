# install.packages("igraph")
# 
# library(igraph)
# 
# library(aod)
# library(ggplot2)
# library(tidyverse)
# library(readxl)
###################################################################################
###################################################################################
# ETL

# Load the file (UPDATE with right file name/location)
election = read.csv("~/Documents/COMP/events/COMP Bad Movie Night! (Holiday Themed!).csv")

# Clean column names
names(election)[2:4] = c('FirstChoice','SecondChoice','ThirdChoice')

# Create name list (to be referenced throughout -- UPDATE for different election)
nameList = c('Eight Crazy Nights',
            'Silent Night, Deadly Night Part 2',
            'The Star Wars Holiday Special',
            'Santa Claus Conquers the Martians',
            'Santa and the Ice Cream Bunny',
            'Santa with Muscles')

colStart = ncol(election)
numCandidates = length(nameList)

# Convert all preferences to numerical ranks: 1,2,3, and everything else is 4
for (i  in 1:length(nameList)){
  election[,ncol(election)+1] = ifelse(election$FirstChoice == nameList[i],1,
                                ifelse(election$SecondChoice == nameList[i],2,
                                ifelse(election$ThirdChoice == nameList[i],3,4)))
}
names(election)[(ncol(election)-length(nameList)+1):ncol(election)] = nameList

# Make an overall preferences matrix (column > row)
preferences = matrix(0L, nrow = numCandidates, ncol = numCandidates, dimnames = list(nameList,nameList))

for(i in 1:numCandidates){
  for(j in 1:numCandidates){
    # column over row favored
    preferences[i,j] = sum(election[,i+colStart] > election[,j+colStart])
    }
}

###################################################################################
###################################################################################
# DETERMINE THE CONDORCET WINNER USING THE TIDEMAN ALGORITHM

# Create TALLY data frame to determine majorities and ranking
tally <- data.frame(More=integer(),
                 More_name=character(), 
                 Less=integer(),
                 Less_name = character(),
                 Margin=integer(),
                 Minority=integer(),
                 TotalVotesMore=integer(),
                 TotalVotesLess=integer(),
                 stringsAsFactors=FALSE) 

for(i in 2:numCandidates){
  for(j in 1:(i-1)){
    Margin = abs(preferences[i,j] - preferences[j,i])
    
    if (Margin > 0){
     if (preferences[i,j] > preferences[j,i]){
       More = j
       Less = i
       Minority = preferences[j,i]
     } else {
       More = i
       Less = j
       Minority = preferences[i,j]
     }
      More_name = nameList[More]
      Less_name = nameList[Less]
      TotalVotesMore = sum(election[,colStart+More]<4)
      TotalVotesLess = sum(election[,colStart+Less]<4)
      tally = rbind(tally,data.frame(More,More_name,
                                     Less,Less_name,
                                     Margin,Minority,
                                     TotalVotesMore,
                                     TotalVotesLess))
    }
  }
}

# SORT step
# descending sort by margin, smallest minority, and total votes for the winner and loser
tally = tally[order(-tally$Margin,tally$Minority,-tally$TotalVotesMore,tally$TotalVotesLess),] 

# LOCK step, add each edge as long as don't introduce cycle
xedges = NULL
for (i in 1:nrow(tally)){
  x = tally[i,1]
  y = tally[i,3]
  g = graph(edges = xedges,n=numCandidates)
  if (length(all_simple_paths(g,y,x,"out")) == 0){ # only add edge if not creating a cycle
    xedges = c(xedges,x,y)
  }
}

# Determine the Condorcet winner:
# The Condorcet winner is the person who would win a two-candidate election 
# against each of the other candidates in a plurality vote.
# In terms of the associated graph, it would be the lone source, as in
# the only vertex with internal degree 0.

# Compute overall Condorcet rankings
Condorcet_rank=NULL
allSet = 1:numCandidates
Rd = 1
yedges = xedges
while (any(allSet) | Rd < numCandidates+1) {
  # determine all vertices with no outgoing edges
  Condorcet_new = setdiff(allSet,yedges[seq(2,length(yedges),2)])
  
  if (length(Condorcet_new) > 1) {
    Condorcet_new_ranked = data.frame(Condorcet_new = integer(),
                                      rankSum = integer()
                                      )
    for (i in Condorcet_new) {
      Condorcet_new_ranked[i,] = c(i,sum(election[,colStart+i]<4))
    }
    Condorcet_new_ranked = Condorcet_new_ranked[order(-Condorcet_new_ranked$rankSum),]
    Condorcet_new = Condorcet_new_ranked[,1]
    Condorcet_new = Condorcet_new[!is.na(Condorcet_new)]
  }
  
  Condorcet_rank = c(Condorcet_rank,Condorcet_new)

  allSet = setdiff(allSet,Condorcet_new)
  
  for (i in Condorcet_new){
    yedges[c(which(yedges == i),which(yedges == i)+1)] = 0
  }
  
  Rd = Rd + 1
}

Condorcet_rank_list = 1:numCandidates
Condorcet_rank_list[Condorcet_rank] = 1:numCandidates

g = graph(edges = nameList[xedges])
plot(g)

###################################################################################
# DETERMINE THE INSTANT-RUNOFF WINNER

Tot = nrow(election)
Rd = 1
compare = election[,c((ncol(election)-numCandidates+1):ncol(election))]
Round = NULL
for (i in 1:numCandidates){
  Round[length(Round)+1] = length(which(compare[,i]==1))
}
output = data.frame(id = 1:numCandidates, 
                    nameList,
                    CondorcetRank = Condorcet_rank_list,
                    Round1 = Round
                    )
output = output[order(-output[,ncol(output)],output$CondorcetRank),]

Max = output[1,ncol(output)]

while (Max < 0.5*Tot) {
  Min = output[numCandidates+1-Rd,1]
  for (i in 1:numCandidates){
    compare[,i] = ifelse(compare[,i]>compare[,Min] & compare[,i] < numCandidates+2-Rd,compare[,i]-1,compare[,i])
  }
  compare[,Min] = numCandidates+1-Rd
  
  Round = NULL
  for (i in 1:numCandidates){
    Round[length(Round)+1] = length(which(compare[,i]==1))
  }
  
  update_output = data.frame(id = 1:numCandidates,Round)
  output = merge(x = output, y = update_output, by = 'id', all = TRUE)
  Rd = Rd + 1
  names(output)[ncol(output)] = paste0('Round', Rd)
  
  output = output[order(-output[,ncol(output)],output$CondorcetRank),]
  Max = output[1,ncol(output)]
}

output[,4:ncol(output)] = output[,4:ncol(output)] / Tot
