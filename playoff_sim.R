##############################################################
# Thomas Bassine
# Date: 3/15/2020
# Purpose: Create a function that allows the user to easily 
# simulate the outcome of the 2020 NBA playoffs. We don't 
# know if these playoffs will actually be played because the
# season has been delayed due to the Corona virus outbreak.
##############################################################

# Read in the 2003-2018 playoffs. This dataset will be used 
# to build my simple model of an NBA playoff series based on 
# the point differentials of the teams playing:

y <- read.csv('https://raw.githubusercontent.com/tvbassine/NBA_Playoff_Simulator_2020/master/playoffs_2003_2018.csv',
              stringsAsFactors = F)
y$year <- 0
for(i in 1:nrow(y)){
  y$year[i] <- as.numeric(unlist(strsplit(y$series_code[i], split = '_'))[3])
}

# Build Logistic Regression model of playoff series outcome.
# Net is the difference in regular season point differential 
# of the teams playing, so point differntial and which team 
# has homecourt are the two variables.
model <- glm(home_win ~ net,
             family=binomial(link='logit'),
             data=y)

# Read in current season standings (as of suspension of NBA season).
# Get these standings in a format where they can easily be fed to my
# simulator.
east <- read.csv('https://raw.githubusercontent.com/tvbassine/NBA_Playoff_Simulator_2020/master/east_seeds_3_15_20.csv',
                  stringsAsFactors = F)
east$win_pct <- east$wins / (east$wins + east$losses)
east$win_pct[east$Team == 'Indiana Pacers'] <- east$win_pct[east$Team == 'Indiana Pacers'] + 0.001
east <- east[order(east$win_pct, decreasing = T),]
east$seed <- 1:nrow(east)
east$net <- east$Point.Differential
east$net[east$Team == 'Milwaukee Bucks'] <- 9

west <- read.csv('https://raw.githubusercontent.com/tvbassine/NBA_Playoff_Simulator_2020/master/west_seeds_3_15_20.csv',
                 stringsAsFactors = F)
west$win_pct <- west$wins / (west$wins + west$losses)
west$win_pct[west$Team == 'Oklahoma City Thunder'] <- west$win_pct[west$Team == 'Oklahoma City Thunder'] + 0.001
west <- west[order(west$win_pct, decreasing = T),]
west$seed <- 1:nrow(west)
west$net <- west$Point.Differential

# Function that simulates series and get winning team:
sim <- function(tms){
  # Get home team
  hm <- which.max(tms$win_pct)
  aw <- which.min(tms$win_pct)
  # Get difference in net ratings
  net = data.frame(net = tms$net[hm] - tms$net[aw])
  # Get probability of home winning 
  win_prob <- predict(model, newdata = net, type = 'response')
  # simulate series and spit out winning team
  rand <- runif(1, 0, 1)
  if(rand < win_prob){
    out <- tms[hm,]
  } else { out <- tms[aw,] }
  return(out)
}

playoff_sim <- function(east, west){
  # Eastern Conference Playoffs:
  east_1st_rd = east[c(1,8,4,5,3,6,2,7), c('Team','net', 'win_pct')]
  east_2nd_rd = east_1st_rd[1:4,]
  east_3rd_rd = east_1st_rd[1:2,]
  east_4th_rd = east_1st_rd[1,]
  
  # Get first round winners:
  east_2nd_rd[1,] <- sim(east_1st_rd[1:2,])
  east_2nd_rd[2,] <- sim(east_1st_rd[3:4,])
  east_2nd_rd[3,] <- sim(east_1st_rd[5:6,])
  east_2nd_rd[4,] <- sim(east_1st_rd[7:8,])
  
  # Get teams that advanced to conference finals:
  east_3rd_rd[1,] = sim(east_2nd_rd[1:2,])
  east_3rd_rd[2,] = sim(east_2nd_rd[3:4,])
  
  # Get conference winner:
  east_4th_rd[1,]= sim(east_3rd_rd[1:2,])
  
  # Western Conference Playoffs:
  west_1st_rd = west[c(1,8,4,5,3,6,2,7), c('Team','net', 'win_pct')]
  west_2nd_rd = west_1st_rd[1:4,]
  west_3rd_rd = west_1st_rd[1:2,]
  west_4th_rd = west_1st_rd[1,]
  
  # Get first round winners:
  west_2nd_rd[1,] <- sim(west_1st_rd[1:2,])
  west_2nd_rd[2,] <- sim(west_1st_rd[3:4,])
  west_2nd_rd[3,] <- sim(west_1st_rd[5:6,])
  west_2nd_rd[4,] <- sim(west_1st_rd[7:8,])
  
  # Get teams that advanced to conference finals:
  west_3rd_rd[1,] = sim(west_2nd_rd[1:2,])
  west_3rd_rd[2,] = sim(west_2nd_rd[3:4,])
  
  # Get conference winner:
  west_4th_rd[1,]= sim(west_3rd_rd[1:2,])
  
  # Get champion:
  finals <- rbind(east_4th_rd, west_4th_rd)
  champ = sim(finals)
  
  return(list(  east_1st_rd = east_1st_rd,
                east_2nd_rd = east_2nd_rd,
                east_3rd_rd = east_3rd_rd,
                east_4th_rd = east_4th_rd,
                west_1st_rd = west_1st_rd,
                west_2nd_rd = west_2nd_rd,
                west_3rd_rd = west_3rd_rd,
                west_4th_rd = west_4th_rd,
                champ = champ
              ))
  
  
}

set.seed(43954)

out <- playoff_sim(east, west)
out$east_3rd_rd
out$west_3rd_rd
out$east_4th_rd
out$west_4th_rd
out$champ

# set.seed(100)
# out <- rep(0, 50000)
# for(i in 1:length(out)){
#   temp <- playoff_sim(east, west)
#   out[i] <- temp$champ[1,1]
#   print(i)
# }
# 
# 100 * table(out) / length(out)
# sv <- out
# table(sv)
