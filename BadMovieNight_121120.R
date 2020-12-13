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

# Load the file (pull results and unzip with same file name)
BadMovies <- read.csv("~/Documents/COMP/events/COMP Bad Movie Night! (Holiday Themed!).csv")

# Clean column names
BadMovies$FirstChoice = BadMovies$Which.of.the.following.bad.movies.would.you.most.like.to.see...First.choice.
BadMovies$SecondChoice = BadMovies$Which.of.the.following.bad.movies.would.you.most.like.to.see...Second.choice.
BadMovies$ThirdChoice = BadMovies$Which.of.the.following.bad.movies.would.you.most.like.to.see...Third.choice.

# Create name list (to be referenced throughout)
vEightCrazy = 'Eight Crazy Nights'
vSilentNight = 'Silent Night, Deadly Night Part 2'
vStarWars = 'The Star Wars Holiday Special'
vSantaMartian = 'Santa Claus Conquers the Martians'
vSantaBunny = 'Santa and the Ice Cream Bunny'
vSantaMuscles = 'Santa with Muscles'
nameList = c(vEightCrazy,vSilentNight,vStarWars,vSantaMartian,vSantaBunny,vSantaMuscles)

# Convert all preferences to numerical ranks: 1,2,3,4,4,4
BadMovies$EightCrazy = ifelse(BadMovies$FirstChoice == vEightCrazy,1,
                                    ifelse(BadMovies$SecondChoice == vEightCrazy,2,
                                           ifelse(BadMovies$ThirdChoice == vEightCrazy,3,4)))
BadMovies$SilentNight = ifelse(BadMovies$FirstChoice == vSilentNight,1,
                              ifelse(BadMovies$SecondChoice == vSilentNight,2,
                                     ifelse(BadMovies$ThirdChoice == vSilentNight,3,4)))
BadMovies$StarWars = ifelse(BadMovies$FirstChoice == vStarWars,1,
                               ifelse(BadMovies$SecondChoice == vStarWars,2,
                                      ifelse(BadMovies$ThirdChoice == vStarWars,3,4)))
BadMovies$SantaMartian = ifelse(BadMovies$FirstChoice == vSantaMartian,1,
                            ifelse(BadMovies$SecondChoice == vSantaMartian,2,
                                   ifelse(BadMovies$ThirdChoice == vSantaMartian,3,4)))
BadMovies$SantaBunny = ifelse(BadMovies$FirstChoice == vSantaBunny,1,
                            ifelse(BadMovies$SecondChoice == vSantaBunny,2,
                                   ifelse(BadMovies$ThirdChoice == vSantaBunny,3,4)))
BadMovies$SantaMuscles = ifelse(BadMovies$FirstChoice == vSantaMuscles,1,
                            ifelse(BadMovies$SecondChoice == vSantaMuscles,2,
                                   ifelse(BadMovies$ThirdChoice == vSantaMuscles,3,4)))

# Make an overall preferences matrix (column > row)
preferences = matrix(0L, nrow = 6, ncol = 6, dimnames = list(nameList,nameList))

for(i in 1:6){
  for(j in 1:6){
    # column over row favored -- also, add 7 to account for number of columns
    preferences[i,j] = sum(BadMovies[,i+7] > BadMovies[,j+7])
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

for(i in 2:6){
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
      TotalVotesMore = sum(BadMovies[,7+More]<4)
      TotalVotesLess = sum(BadMovies[,7+Less]<4)
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
  g = graph(edges = xedges,n=6)
  if (length(all_simple_paths(g,y,x,"out")) == 0){ # only add edge if not creating a cycle
    xedges = c(xedges,x,y)
  }
}

# Note did not add (1) Martians to Eight Crazy nor (2) Martians to Muscles (otherwise cycle)

# Determine the Condorcet winner:
# The Condorcet winner is the person who would win a two-candidate election 
# against each of the other candidates in a plurality vote.
# In terms of the associated graph, it would be the lone source, as in
# the only vertex with internal degree 0.

# Compute overall Condorcet rankings
Condorcet_rank=NULL
allSet = 1:6
Rd = 1
yedges = xedges
while (length(allSet)>0) {
  Condorcet_new = setdiff(allSet,yedges[seq(2,length(yedges),2)])
  
  if (length(Condorcet_new) > 1) {
    Condorcet_new_ranked = data.frame(Condorcet_new = integer(),
                                      rankSum = integer()
                                      )
    for (i in Condorcet_new) {
      Condorcet_new_ranked[i,] = c(i,sum(BadMovies[,7+i]<4))
    }
    Condorcet_new_ranked = Condorcet_new_ranked[order(-Condorcet_new_ranked$rankSum),]
    Condorcet_new = Condorcet_new_ranked[,1]
    Condorcet_new = Condorcet_new[!is.na(Condorcet_new)]
  }
  # Add on step to disambiguate ranks based on total votes (tie-breaker)
  # if (length(Condorcet_new) > 1) {
  #   Cordorcet_new_ranked = NULL
  #   for (i in Condorcet_new){
  #     Cordorcet_new_ranked[nrow(Cordorcet_new_ranked)+1]=sum(BadMovies[,7+i] < 4)
  #   }
  #   Cordorcet_new = Condorcet_new[order(-Condorcet_new_ranked),]
  # }
  
  
  Condorcet_rank = c(Condorcet_rank,Condorcet_new)

  allSet = setdiff(allSet,Condorcet_new)
  
  for (i in Condorcet_new){
    yedges[c(which(yedges == i),which(yedges == i)+1)] = 0
  }
  
  Rd = Rd + 1
}

Condorcet_rank_list = 1:6
Condorcet_rank_list[Condorcet_rank] = 1:6

nameList[Condorcet_rank]

g = graph(edges = nameList[c(xedges)])
plot(g)

###################################################################################
# DETERMINE THE INSTANT-RUNOFF WINNER

Tot = length(BadMovies[,1])
Rd = 1
compare = BadMovies[,c((ncol(BadMovies)-5):ncol(BadMovies))]
output = data.frame(id = 1:6, 
                    nameList,
                    CondorcetRank = Condorcet_rank_list,
                    Round1 = c(length(which(compare[,1]==1)),
                               length(which(compare[,2]==1)),
                               length(which(compare[,3]==1)),
                               length(which(compare[,4]==1)),
                               length(which(compare[,5]==1)),
                               length(which(compare[,6]==1)))
                    )
output = output[order(-output[,ncol(output)],output$CondorcetRank),]

Max = output[1,ncol(output)]

while (Max < 0.5*Tot) {
  Min = output[7-Rd,1]
  for (i in 1:6){
    compare[,i] = ifelse(compare[,i]>compare[,Min] & compare[,i] < 8-Rd,compare[,i]-1,compare[,i])
  }
  compare[,Min] = 7-Rd
  
  update_output = data.frame(id = 1:6,
                             Round = c(length(which(compare[,1]==1)),
                                        length(which(compare[,2]==1)),
                                        length(which(compare[,3]==1)),
                                        length(which(compare[,4]==1)),
                                        length(which(compare[,5]==1)),
                                        length(which(compare[,6]==1)))
                             )
  output = merge(x = output, y = update_output, by = 'id', all = TRUE)
  Rd = Rd + 1
  names(output)[ncol(output)] <- paste0('Round', Rd)
  
  output = output[order(-output[,ncol(output)],output$CondorcetRank),]
  Max = output[1,ncol(output)]
}

output[,4:ncol(output)] = output[,4:ncol(output)] / Tot

# Final output table with both rankings included
#output = output[order(-output[,9],-output[,8],-output[,7],-output[,6],-output[,5],-output[,4]),]
