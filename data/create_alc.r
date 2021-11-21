
#Jukka Kontto
#16th November 2021

#Logistic regression


#Useful links:
#https://drive.google.com/file/d/1c_2YYl9omnVhZl4uTnWagsjCFV0w0JXQ/view?usp=sharing
#http://www.sthda.com/english/articles/36-classification-methods-essentials/151-logistic-regression-essentials-in-r/
#http://www.sthda.com/english/articles/36-classification-methods-essentials/150-stepwise-logistic-regression-essentials-in-r/#:~:text=The%2520stepwise%2520logistic%2520regression%2520can,ref(stepwise%252Dregression)).
#https://www.r-bloggers.com/2015/08/evaluating-logistic-regression-models/

#Data file is downloaded from
#"http://archive.ics.uci.edu/ml/machine-learning-databases/00320/student.zip"
#and saved in the 'data' folder

#Unzipping data file:
unzip("/home/jkox/Git/proj/IODS-project/data/student.zip",exdir="/home/jkox/Git/proj/IODS-project/data/student")

#Reading datasets:

math <- read.csv2(file='/home/jkox/Git/proj/IODS-project/data/student/student-mat.csv')
por <- read.csv2(file='/home/jkox/Git/proj/IODS-project/data/student/student-por.csv')
dim(math) #395 x 33
names(math) #column names
str(math)

dim(por) #649 x 33
names(por)
str(por)

table(names(math)==names(por)) #same column names

#Following Reijo Sund's instructions to combine the two datasets and to create new variables:

# Define own id for both datasets
library(dplyr)
packageVersion('dplyr') #1.0.7
por_id <- por %>% mutate(id=1000+row_number()) 
math_id <- math %>% mutate(id=2000+row_number())

# Which columns vary in datasets
free_cols <- c("id","failures","paid","absences","G1","G2","G3")

# The rest of the columns are common identifiers used for joining the datasets
join_cols <- setdiff(colnames(por_id),free_cols)

pormath_free <- por_id %>% bind_rows(math_id) %>% select(one_of(free_cols))

# Combine datasets to one long data
#   NOTE! There are NO 382 but 370 students that belong to both datasets
#         Original joining/merging example is erroneous!
pormath <- por_id %>% 
  bind_rows(math_id) %>%
  # Aggregate data (more joining variables than in the example)  
  group_by(.dots=join_cols) %>%  
  # Calculating required variables from two obs  
  summarise(                                                           
    n=n(),
    id.p=min(id),
    id.m=max(id),
    failures=round(mean(failures)),     #  Rounded mean for numerical
    paid=first(paid),                   #    and first for chars
    absences=round(mean(absences)),
    G1=round(mean(G1)),
    G2=round(mean(G2)),
    G3=round(mean(G3))    
  ) %>%
  # Remove lines that do not have exactly one obs from both datasets
  #   There must be exactly 2 observations found in order to joining be succesful
  #   In addition, 2 obs to be joined must be 1 from por and 1 from math
  #     (id:s differ more than max within one dataset (649 here))
  filter(n==2, id.m-id.p>650) %>%  
  # Join original free fields, because rounded means or first values may not be relevant
  inner_join(pormath_free,by=c("id.p"="id"),suffix=c("",".p")) %>%
  inner_join(pormath_free,by=c("id.m"="id"),suffix=c("",".m")) %>%
  # Calculate other required variables  
  ungroup %>% mutate(
    alc_use = (Dalc + Walc) / 2,
    high_use = alc_use > 2,
    cid=3000+row_number()
  )

glimpse(pormath)
dim(pormath) #370 rows (OK) and 51 variables
head(data.frame(pormath)) #first 6 rows
str(pormath)
names(pormath) #names of the columns (alc_use and high_use are included)

write.csv(pormath,file="/home/jkox/Git/proj/IODS-project/data/pormath.csv",row.names = FALSE)


############################
### Analysis part (testing):

dim(pormath)
str(pormath)
head(subset(pormath,select=c('absences','absences.p','absences.m')),10)

summary(subset(pormath, select=c('high_use','absences','health','sex','age')))
table(pormath$sex)
with(pormath,table(health,high_use))



table(pormath$high_use)

library(GGally)
library(ggplot2)
p <- ggpairs(subset(pormath, select=c('high_use','absences','health','sex','age')), mapping = aes(col=high_use,alpha=0.3), lower = list(combo = wrap("facethist", bins = 20)))
p

with(pormath,table(sex,high_use))

m <- glm(high_use ~ absences + health + sex + age, data = pormath, family = "binomial")
summary(m)
coef(m)

Coeff <- coef(m) %>% exp
CI <- confint(m) %>% exp
cbind(Coeff,CI)

m2 <- glm(high_use ~ absences + sex, data = pormath, family = "binomial")

probabilities <- predict(m2, type = "response")
pormath <- mutate(pormath, probability = probabilities)
pormath <- mutate(pormath, prediction = probability>0.5)

# tabulate the target variable versus the predictions
table(high_use = pormath$high_use, prediction = pormath$prediction)
94/(251+8+86+25)

table(high_use = pormath$high_use, prediction = pormath$prediction) %>% prop.table %>% addmargins()

# plot
g <- ggplot(pormath, aes(x = probability, y = high_use, col=prediction)) + geom_point()
g



# define a loss function (mean prediction error)
loss_func <- function(class, prob) {
  n_wrong <- abs(class - prob) > 0.5
  mean(n_wrong)
}

# call loss_func to compute the average number of wrong predictions in the (training) data
loss_func(class = pormath$high_use, prob = pormath$probability)

head(select(pormath,select=c('high_use','probability')),30)

loss_func(class = pormath$high_use, prob = 0)
loss_func(class = pormath$high_use, prob = 1)

###R-code from datacamp:

# glimpse at the data
glimpse(math_por)

# draw a bar plot of high_use by sex
g2 + geom_bar() +
facet_wrap("sex")

# use gather() to gather columns into key-value pairs and then glimpse() at the resulting data
gather(alc) %>% glimpse

# produce summary statistics by group
alc %>% group_by(sex, high_use) %>% summarise(count = n(), mean_grade = mean(G3))

# find the model with glm()
m <- glm(high_use ~ failures + absences, data = alc, family = "binomial")

# print out the coefficients of the model
coef(m)

# compute confidence intervals (CI)
CI <- confint(m) %>% exp

# fit the model
m <- glm(high_use ~ failures + absences + sex, data = alc, family = "binomial")

# predict() the probability of high_use
probabilities <- predict(m, type = "response")

# add the predicted probabilities to 'alc'
alc <- mutate(alc, probability = probabilities)

# use the probabilities to make a prediction of high_use
alc <- mutate(alc, prediction = probability>0.5)

# see the last ten original classes, predicted probabilities, and class predictions
select(alc, failures, absences, sex, high_use, probability, prediction) %>% tail(10)

# tabulate the target variable versus the predictions
table(high_use = alc$high_use, prediction = alc$prediction)

# initialize a plot of 'high_use' versus 'probability' in 'alc'
g <- ggplot(alc, aes(x = probability, y = high_use, col=prediction))

# define the geom as points and draw the plot
g + geom_point()

# tabulate the target variable versus the predictions
table(high_use = alc$high_use, prediction = alc$prediction) %>% prop.table %>% addmargins()

# the logistic regression model m and dataset alc with predictions are available

# define a loss function (mean prediction error)
loss_func <- function(class, prob) {
  n_wrong <- abs(class - prob) > 0.5
  mean(n_wrong)
}

# call loss_func to compute the average number of wrong predictions in the (training) data
loss_func(class = alc$high_use, prob = alc$probability)

# the logistic regression model m and dataset alc (with predictions) are available

# define a loss function (average prediction error)
loss_func <- function(class, prob) {
  n_wrong <- abs(class - prob) > 0.5
  mean(n_wrong)
}

# compute the average number of wrong predictions in the (training) data
loss_func(alc$high_use, alc$probability)

# K-fold cross-validation
library(boot)
cv <- cv.glm(data = alc, cost = loss_func, glmfit = m, K = 10)

# average number of wrong predictions in the cross validation
cv$delta[1]




