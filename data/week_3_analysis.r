

#Jukka Kontto
#23th November 2021

#Clustering and classification

library(MASS)

data("Boston")
str(Boston)
dim(Boston)

summary(Boston)

library(GGally)
library(ggplot2)
p <- ggpairs(Boston, mapping = aes(), lower = list(combo = wrap("facethist", bins = 20)))
p

pairs(Boston)

cor_matrix<-cor(Boston) %>% round(digits = 2)
library(corrplot)
corrplot(cor_matrix, method="circle", type="upper", cl.pos="b", tl.pos="d", tl.cex = 0.6)

boston_scaled <- data.frame(scale(Boston))
summary(boston_scaled)

tmp.mean <- Boston$crim-mean(Boston$crim)
summary(tmp.mean)
85.36268/sd(tmp.mean)
sd(boston_scaled$crim)

p <- ggpairs(data.frame(boston_scaled), mapping = aes(), lower = list(combo = wrap("facethist", bins = 20)))
p

#New variable `crime` is derived:
bins <- quantile(boston_scaled$crim)
crime <- cut(boston_scaled$crim, breaks = bins, include.lowest = TRUE, label = c("low","med_low","med_high","high"))
boston_scaled$crim <- NULL
boston_scaled$crime <- crime

#Dividing the data to train and test datasets:

ind <- sample(nrow(boston_scaled),  size = nrow(Boston) * 0.8)
train <- boston_scaled[ind,] #train
test <- boston_scaled[-ind,] #test

# linear discriminant analysis
lda.fit <- lda(crime~., data = train)



# print the lda.fit object
lda.fit

# the function for lda biplot arrows
lda.arrows <- function(x, myscale = 1, arrow_heads = 0.1, color = "red", tex = 0.75, choices = c(1,2)){
  heads <- coef(x)
  arrows(x0 = 0, y0 = 0, 
         x1 = myscale * heads[,choices[1]], 
         y1 = myscale * heads[,choices[2]], col=color, length = arrow_heads)
  text(myscale * heads[,choices], labels = row.names(heads), 
       cex = tex, col=color, pos=3)
}

# target classes as numeric
classes <- as.numeric(train$crime)

# plot the lda results
plot(lda.fit, dimen = 2, col=classes,pch=classes)
lda.arrows(lda.fit, myscale = 2)


correct_classes <- test$crime #saving crime categories
test$crime <- NULL #removing crime from test set

lda.pred <- predict(lda.fit, newdata = test)

table(correct = correct_classes, predicted = lda.pred$class)

data(Boston)

boston_scaled <- scale(Boston)

dist_eu <- dist(boston_scaled)

set.seed(544356)

# calculate the total within sum of squares
twcss <- sapply(1:10, function(k){kmeans(boston_scaled, k)$tot.withinss})

# visualize the results
qplot(x = 1:10, y = twcss, geom = 'line') + scale_x_continuous(breaks=seq(1,10,by=1)) + xlab('number of clusters') 

km <-kmeans(boston_scaled, centers = 2)

# plot the Boston dataset with clusters
pairs(boston_scaled, col = km$cluster)

class(km$cluster)

p <- ggpairs(data.frame(boston_scaled), mapping = aes(col = factor(km$cluster), alpha=0.3), lower = list(combo = wrap("facethist", bins = 20)))
p
warnings()

#from DataCamp:


# plot matrix of the variables
pairs(Boston)

#######

# calculate the correlation matrix and round it
cor_matrix<-cor(Boston) %>% round(digits = 2)

# print the correlation matrix
cor_matrix

# visualize the correlation matrix
corrplot(cor_matrix, method="circle", type="upper", cl.pos="b", tl.pos="d", tl.cex = 0.6)

########

# center and standardize variables
boston_scaled <- scale(Boston)

# summaries of the scaled variables
summary(boston_scaled)

# class of the boston_scaled object
class(boston_scaled)

# change the object to data frame
boston_scaled <- as.data.frame(boston_scaled)

########

# create a quantile vector of crim and print it
bins <- quantile(boston_scaled$crim)
bins

# create a categorical variable 'crime'
crime <- cut(boston_scaled$crim, breaks = bins, include.lowest = TRUE, label = c("low","med_low","med_high","high"))

##########


# linear discriminant analysis
lda.fit <- lda(crime~., data = train)

# print the lda.fit object
lda.fit

# the function for lda biplot arrows
lda.arrows <- function(x, myscale = 1, arrow_heads = 0.1, color = "red", tex = 0.75, choices = c(1,2)){
  heads <- coef(x)
  arrows(x0 = 0, y0 = 0, 
         x1 = myscale * heads[,choices[1]], 
         y1 = myscale * heads[,choices[2]], col=color, length = arrow_heads)
  text(myscale * heads[,choices], labels = row.names(heads), 
       cex = tex, col=color, pos=3)
}

# target classes as numeric
classes <- as.numeric(train$crime)

# plot the lda results
plot(lda.fit, dimen = 2, col=classes,pch=classes)
lda.arrows(lda.fit, myscale = 3)


#########################


# predict classes with test data
lda.pred <- predict(lda.fit, newdata = test)

# cross tabulate the results
table(correct = correct_classes, predicted = lda.pred$class)


##########################

# euclidean distance matrix
dist_eu <- dist(Boston)

# look at the summary of the distances
summary(dist_eu)

# manhattan distance matrix
dist_man <- dist(Boston,method="manhattan")

# look at the summary of the distances
summary(dist_man)


###############################

# k-means clustering
km <-kmeans(Boston, centers = 3)

# plot the Boston dataset with clusters
pairs(Boston, col = km$cluster)

###############################


# MASS, ggplot2 and Boston dataset are available
set.seed(123)

# determine the number of clusters
k_max <- 10

# calculate the total within sum of squares
twcss <- sapply(1:k_max, function(k){kmeans(Boston, k)$tot.withinss})

# visualize the results
qplot(x = 1:k_max, y = twcss, geom = 'line')

# k-means clustering
km <-kmeans(Boston, centers = 2)

# plot the Boston dataset with clusters
pairs(Boston, col = km$cluster)


#################################


