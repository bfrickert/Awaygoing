library(pacman)
p_load(tourr,dplyr,caret)

place.names <- read.table('data/place.names.tsv',sep='\t',stringsAsFactors = F, header=T)
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
#rm(awaygoing, not.awaygoing)

hist(awaygoing$cluster, breaks=15)
hist(not.awaygoing$cluster, breaks=15)
select(awaygoing, city, cluster)
arrange(select(filter(awaygoing, cluster %in% c(1,4,10)), city, state, cluster), cluster)

train <- rbind(awaygoing, not.awaygoing[1:14,])
test <- not.awaygoing[15:nrow(not.awaygoing),]
fit <- train(train[,c(4:12,15,18)], train$is.awaygoing, method='rf')

pred <- predict(fit, test[,c(4:12,15,18)])
test$pred <- round(pred)
select(filter(test, pred ==1), city, state)


d <- dist(joind[,c(4:12,15)], method = "euclidean") # distance matrix
fit <- hclust(d, method="ward.D2")
plot(fit) # display dendogram
groups <- cutree(fit, k=5) # cut tree into 5 clusters
# draw dendogram with red borders around the 5 clusters
rect.hclust(fit, k=5, border="red")


n <- names(train)
f <- as.formula(paste("is.awaygoing ~", paste(n[!n %in% "is.awaygoing"], collapse = " + ")))
nn <- neuralnet(f,data=train,hidden=c(5,3), linear.output=T)

plot(nn)

pr.nn <- compute(nn,test_[,1:13])

joind.train <- joind # joind[1:328,]
joind.newdata <- joind[15:329,]
model <- glm(formula= is.awaygoing ~ ., data=joind.train[,c(4:12,17)], family=binomial)
p <- predict(model, joind, type="response")
summary(model)

joind$prob <- p
joind.not.awaygoing <- arrange(joind[15:329,], desc(prob))
joind.not.awaygoing$rank <- c(1:nrow(joind.not.awaygoing))


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


map<- get_map(location = 'US', zoom = 4)
ggmap(map)
sf <- filter(joind, city=='San Francisco' | city=='Oklahoma City')
dc <- filter(joind, city=='Washington' | city=='Oklahoma City')
ggmap(map)+geom_point(data=joind.not.awaygoing,alpha = .7, aes(x=long, y=lat,size =pop, fill=rank))+ggtitle("Unspoiled Cities") +
  geom_point(data=sf, aes(x=long, y=lat),color="black") + geom_line(data=sf, aes(x=long, y=lat), color="black")+
  geom_point(data=dc, aes(x=long, y=lat),color="black") + geom_line(data=dc, aes(x=long, y=lat), color="orange")

sf.1 <- filter(joind, city=='San Francisco')
dc.1 <- filter(joind, city=='Washington')
dul.1 <- filter(joind, city=='Oklahoma City')
earth.dist(sf.1$long, sf.1$lat, dul.1$long, dul.1$lat)
earth.dist(dc.1$long, dc.1$lat, dul.1$long, dul.1$lat)
