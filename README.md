# BadMovieNight
Tabulation method for a ranked-choice voting election using the Tideman algorithm and the instant-runoff models

Originally used to determine which movie was watched for a Bad Movie Night, with 6 candidates where the top 3 choices are recorded per vote (14 total voters at the time of the event). 

The two methods tabulated here include:

Instant-runoff model: this is used in some elections and involves tabulating the top choice for the remaining candidates after removing the bottom candidate and redistributing the remaining ranked-votes (using the Condorcet ranking (see below) for tiebreakers), until a candidate gets a majority.

Tideman method: this determines the Condorcet winner, whom is the candidate who would win a two-candidate election against each of the other candidates in a plurality vote, and is the most “preferred” candidate. The Tideman algorithm works in three stages: tallying the votes to determine the winner of each head-to-head matchup, sorting the winners ranked by the margin of victory and the opposing votes (and I also used the total votes for each candidate as tiebreakers), and then locking in the sorted order to generate a directed graph, adding an edge pointing from the winner to the loser iteratively only if a cycle will not be introduced.
