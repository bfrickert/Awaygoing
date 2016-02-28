library(pacman)
p_load(tourr,dplyr,caret, RSocrata, readr)

random.forest <- read.table('data/random.forest.tsv', sep='\t', stringsAsFactors = T, header = T)

######################################
# Create Data Set with Clusters
######################################
place.names <- read.table('../data/place.names.tsv',sep='\t',stringsAsFactors = F, header=T)
names(place.names)[1] <- 'casenum'
joind <-inner_join(place.names, places, by='casenum')
rm(place.names)
joind$is.awaygoing <- 0

fit <- kmeans(joind[,4:12], 15, nstart = 1, algorithm='Lloyd', iter.max = 100)

joind$cluster <- fit$cluster 

awaygoing <- filter(joind, city %in% c('Pittsburgh', 'Philadelphia', 'Portland', 'Providence', 'Milwaukee','Minneapolis-St. Paul', 'Buffalo', 'Detroit', 'Memphis', 'Cleveland', 'Kansas City', 'Omaha', 'Louisville', 'Columbus') & state != 'GA-AL' & state != 'OR' & state != 'KS')
awaygoing$is.awaygoing <- 1
not.awaygoing <- subset(joind,!casenum %in% awaygoing$casenum)

joind <- rbind(awaygoing, not.awaygoing)
rm(awaygoing, not.awaygoing)

######################################
# Ranking with Logistic Regression
######################################
earth.dist <- function (long1, lat1, long2, lat2)
{
  rad <- pi/180
  a1 <- lat1 * rad
  a2 <- long1 * rad
  b1 <- lat2 * rad
  b2 <- long2 * rad
  dlon <- b2 - a2
  dlat <- b1 - a1
  a <- (sin(dlat/2))^2 + cos(a1) * cos(b1) * (sin(dlon/2))^2
  c <- 2 * atan2(sqrt(a), sqrt(1 - a))
  R <- 6378.145
  d <- R * c
  return(d)
}

joind.train <- joind
joind.newdata <- filter(joind, is.awaygoing == 0)



model <- glm(formula= is.awaygoing ~ ., data=joind.train[,c(4:12,17)], family=binomial)
p <- predict(model, joind, type="response")
summary(model)

joind$prob <- p
joind.not.awaygoing <- arrange(joind[15:329,], desc(prob))
joind.not.awaygoing$rank <- c(1:nrow(joind.not.awaygoing))

#########################################################

#lottery <- read.socrata('https://data.ny.gov/Government-Finance/Lottery-Mega-Millions-Winning-Numbers-Beginning-20/5xaw-6ayf')
hate <- read.socrata('https://splc.demo.socrata.com/dataset/Active-Hate-Groups-in-2014/hzr8-i6je')
states <- data.frame(State = state.name, code = state.abb)
states <- rbind(states, data.frame(State='District of Columbia', code ="DC"))
hate$State <- as.character(hate$State)
states$State <- as.character(states$State)
hate <-inner_join(hate, states, by='State')
