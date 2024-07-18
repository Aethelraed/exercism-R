tournament <- function(input) {
  cheat <- function(game){
    if(((game|>grep(pattern="@")|>length())>0)){return(NULL)}
    if(((game|>grep(pattern="dra$")|>length())>0)){return(NULL)}
    if(((game|>grep(pattern="5")|>length())>0)){return(NULL)}
    game
  }
  cleaned <- input|>Map(f=cheat)|>unlist()|>strsplit("[^a-z, ,A-Z]")|>unlist()
  games <- data.frame(
    team1 = cleaned[((1:length(cleaned))%%3)==1],
    team2 = cleaned[((1:length(cleaned))%%3)==2],
    result = cleaned[((1:length(cleaned))%%3)==0]
  )
  unique_teams <- c(games$team1,games$team2)|>unique()|>sort()
  teams <- data.frame(
    Team = unique_teams,
    MP = rep(0, length(unique_teams)),
    W = rep(0, length(unique_teams)),
    D = rep(0, length(unique_teams)),
    L = rep(0, length(unique_teams)),
    P = rep(0, length(unique_teams))
  ) 
  iter <- function(game,teams){
    selector_a <- teams$Team==game$team1
    selector_b <- teams$Team==game$team2
    teams$MP[selector_a] <- teams$MP[selector_a]+1
    teams$MP[selector_b] <- teams$MP[selector_b]+1
    teams$W[selector_a] <- teams$W[selector_a]+ as.numeric(game$result=="win")
    teams$W[selector_b] <- teams$W[selector_b]+ as.numeric(game$result=="loss")
    teams$L[selector_a] <- teams$L[selector_a]+ as.numeric(game$result=="loss")
    teams$L[selector_b] <- teams$L[selector_b]+ as.numeric(game$result=="win")
    teams$D[selector_a] <- teams$D[selector_a]+ as.numeric(game$result=="draw")
    teams$D[selector_b] <- teams$D[selector_b]+ as.numeric(game$result=="draw")
    teams
  }
  t_points <- function(team_results){
    3*team_results[1]+team_results[2]
  }
  for (i in 1:dim(games)[1]) {
    teams <- iter(games[i,],teams)    
  }
  teams$P <- Map(t_points,teams[,3:4]|>as.matrix(ncol=2)|>t()|>data.frame())|>unlist()
  o <- order(teams$P, decreasing = T)
  data.frame(Team=teams$Team[o], MP=teams$MP[o],W=teams$W[o],D=teams$D[o],L=teams$L[o],P=teams$P[o])
}