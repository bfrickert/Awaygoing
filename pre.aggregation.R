library(pacman)
p_load(tourr,dplyr,caret, ggplot2, ggmap,plyr)

# Get Places Rated data and join it to cities and states
place.names <- read.table('data/place.names.tsv',sep='\t',stringsAsFactors = F, header=T)
names(place.names)[1] <- 'casenum'
joind <-inner_join(place.names, places, by='casenum')
rm(place.names)
joind$is.awaygoing <- 0

# cluster the cities by ratings using kmeans
fit <- kmeans(joind[,4:12], 15, nstart = 1, algorithm='Lloyd', iter.max = 100)
joind$cluster <- fit$cluster

# identify cities Awaygoing has already visited
awaygoing <- filter(joind, city %in% c('Pittsburgh', 'Philadelphia', 'Portland', 'Providence', 'Milwaukee','Minneapolis-St. Paul', 'Buffalo', 'Detroit', 'Memphis', 'Cleveland', 'Kansas City', 'Omaha', 'Louisville', 'Columbus') & state != 'GA-AL' & state != 'OR' & state != 'KS')
awaygoing$is.awaygoing <- 1
not.awaygoing <- subset(joind,!casenum %in% awaygoing$casenum)

# rbind Awaygoing cities and non-Awaygoing cities into one dataset
joind <- rbind(awaygoing, not.awaygoing)

# use a random forest algorithm to predict what cities Awaygoing will visit
train <- rbind(awaygoing, not.awaygoing[1:14,])
test <- not.awaygoing[15:nrow(not.awaygoing),]
fit <- train(train[,c(4:12,15,18)], factor(train$is.awaygoing), method='rf')
rm(awaygoing, not.awaygoing)
pred <- predict(fit, test[,c(4:12,15,18)])
test$pred <- pred
# Output the predictions to a file
write.table(select(filter(test, pred ==1), city, state), 'shiny/data/random.forest.tsv', 
            sep='\t', row.names = F)


# Use a logistical regression to determine the probability that a city will
# be chosen as an Awaygoing destination, then assign a rank to each city
# according to that probability
joind.train <- joind # joind[1:328,]
joind.newdata <- joind[15:329,]
model <- glm(formula= is.awaygoing ~ ., data=joind.train[,c(4:12,17)], family=binomial)
p <- predict(model, joind, type="response")
joind$prob <- p
joind.not.awaygoing <- arrange(joind[15:329,], desc(prob))
joind.not.awaygoing$rank <- c(1:nrow(joind.not.awaygoing))

# Prepare US maps for each city, showing distance from SF and DC
create.map <- function(c, s) {
  print(c)
  print(s)
  file.name <-paste('shiny/viz/maps/',c, '.',s,'.png', sep='')
  ggmap(map)
  sf <- filter(joind, city=='San Francisco' | (city==c & state==s))
  dc <- filter(joind, city=='Washington' | (city==c & state==s))
  mp <- ggmap(map)+geom_point(data=joind.not.awaygoing,alpha = .7, aes(x=long, y=lat,size =pop, fill=rank))+
    ggtitle("Unspoiled Potential Awaygoing Cities") +
    geom_point(data=sf, aes(x=long, y=lat),color="black") + geom_line(data=sf, aes(x=long, y=lat), color="green", size=2)+
    geom_point(data=dc, aes(x=long, y=lat),color="black") + geom_line(data=dc, aes(x=long, y=lat), color="cyan", size=2) +
    theme(axis.line=element_blank(),axis.text.x=element_blank(),
          axis.text.y=element_blank(),axis.ticks=element_blank(),
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),legend.position="none",
          panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),plot.background=element_blank())
  
  
    ggsave(file.name, plot=mp)
  }

# Only needs to be run if you are re-creating all the maps
########################
# map<- get_map(location = 'US', zoom = 4)
# apply(select(joind.not.awaygoing, city, state), 1, function(x){
#   create.map(x[1], x[2])
# })

# We want to know the distance of any city from Washington, DC
# and San Francisco
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
dist.to.sf <- function(c, s){
  sf.1 <- filter(joind, city=='San Francisco')
  dul.1 <- filter(joind, city==c & state==s)
  earth.dist(sf.1$long, sf.1$lat, dul.1$long, dul.1$lat)
}
dist.to.dc <- function(c,s){  
  dul.1 <- filter(joind, city==c & state==s)
  dc.1 <- filter(joind, city=='Washington')
  earth.dist(dc.1$long, dc.1$lat, dul.1$long, dul.1$lat)
}

joind.not.awaygoing$sf.dists <- apply(select(joind.not.awaygoing, city, state), 1, function(x){
  dist.to.sf(x[1], x[2])
})
joind.not.awaygoing$dc.dists <- apply(select(joind.not.awaygoing, city, state), 1, function(x){
  dist.to.dc(x[1], x[2])
})

# Tabulate rankings by Places Rated data
joind.not.awaygoing <- arrange(joind.not.awaygoing, desc(climate))
joind.not.awaygoing$climate.rank <- 1:nrow(joind.not.awaygoing)

joind.not.awaygoing <- arrange(joind.not.awaygoing, desc(econ))
joind.not.awaygoing$econ.rank <- 1:nrow(joind.not.awaygoing)

joind.not.awaygoing <- arrange(joind.not.awaygoing, crime)
joind.not.awaygoing$crime.rank <- 1:nrow(joind.not.awaygoing)

joind.not.awaygoing <- arrange(joind.not.awaygoing, housingcost)
joind.not.awaygoing$housingcost.rank <- 1:nrow(joind.not.awaygoing)

joind.not.awaygoing <- arrange(joind.not.awaygoing, desc(hlthcare))
joind.not.awaygoing$hlthcare.rank <- 1:nrow(joind.not.awaygoing)

joind.not.awaygoing <- arrange(joind.not.awaygoing, desc(transp))
joind.not.awaygoing$transp.rank <- 1:nrow(joind.not.awaygoing)

joind.not.awaygoing <- arrange(joind.not.awaygoing, desc(recreat))
joind.not.awaygoing$recreat.rank <- 1:nrow(joind.not.awaygoing)

joind.not.awaygoing <- arrange(joind.not.awaygoing, desc(educ))
joind.not.awaygoing$educ.rank <- 1:nrow(joind.not.awaygoing)

joind.not.awaygoing <- arrange(joind.not.awaygoing, desc(arts))
joind.not.awaygoing$arts.rank <- 1:nrow(joind.not.awaygoing)

joind.not.awaygoing <- arrange(joind.not.awaygoing, desc(pop))
joind.not.awaygoing$pop.rank <- 1:nrow(joind.not.awaygoing)

# Use a Logit probability to order Potential Awaygoing Cities by "suitability"
joind.train <- joind
joind.newdata <- filter(joind, is.awaygoing == 0)

model <- glm(formula= is.awaygoing ~ ., data=joind.train[,c(4:12,17)], family=binomial)
p <- predict(model, joind.not.awaygoing, type="response")
summary(model)

joind.not.awaygoing$prob <- p
joind.not.awaygoing <- arrange(joind.not.awaygoing, desc(prob))
joind.not.awaygoing$rank <- c(1:nrow(joind.not.awaygoing))

# Output finished dataset
write.table(joind.not.awaygoing, 'shiny/data/joind.not.awaygoing.tsv',sep='\t',
            row.names = F)
  

# Figure out what hate groups are operating within a 100-mile radius
# of each city using data from the Southern Law Poverty Center
hate <- read.socrata('https://splc.demo.socrata.com/dataset/Active-Hate-Groups-in-2014/hzr8-i6je')

find.hate.groups <- function(city, state, lat, long){
  hate.subset <- select(hate, Latitude, Longitude)
  hate.subset$lat <- lat
  hate.subset$long <- long

  hate.groups <- apply(hate.subset,1, function(x){
    return(earth.dist(x[3], x[4], x[1], x[2]) <=100)
  })
  hate.df <- hate[hate.groups,]
  if (nrow(hate.df) > 0) {
    hate.df$city <- paste(city, state, sep=', ')
    return(hate.df)}
}

stl<-filter(joind.not.awaygoing, city=='Billings')
find.hate.groups('Chicago', 'IL', stl$lat, stl$long)

hate.per.city <- ldply(apply(select(joind.not.awaygoing, city, state, lat, long), 1, function(x){
  find.hate.groups(x[1],x[2],as.numeric(x[3]),as.numeric(x[4]))
}))

write.table(hate.per.city, 'shiny/data/hate.per.city.tsv', sep='\t', row.names = F)
