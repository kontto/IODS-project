
#Jukka Kontto
#9th November 2021

#Regression and model validation

#Useful link: http://www.sthda.com/english/articles/40-regression-analysis/167-simple-linear-regression-in-r/

#Reading data:
mydata <- read.table(url("http://www.helsinki.fi/~kvehkala/JYTmooc/JYTOPKYS3-data.txt"),sep="\t",header=TRUE)

#Checking data:
dim(mydata) #183 rows and 60 variables
names(mydata) #names of the variables
str(mydata) #all are numeric variables except gender
summary(mydata)
#56/60 variables are numeric variables with integer values 1-5
#no missing values

#Creating analysis dataset:

library(dplyr)

#Three new variables:
deep_questions <- c("D03", "D11", "D19", "D27", "D07", "D14", "D22", "D30","D06",  "D15", "D23", "D31")
surface_questions <- c("SU02","SU10","SU18","SU26", "SU05","SU13","SU21","SU29","SU08","SU16","SU24","SU32")
strategic_questions <- c("ST01","ST09","ST17","ST25","ST04","ST12","ST20","ST28")

deep_columns <- select(mydata, one_of(deep_questions))
mydata$deep <- rowSums(deep_columns)

surface_columns <- select(mydata, one_of(surface_questions))
mydata$surf <- rowSums(surface_columns)

strategic_columns <- select(mydata, one_of(strategic_questions))
mydata$stra <- rowSums(strategic_columns)

#Variables selected:
keep_columns <- c("gender","Age","Attitude", "deep", "stra", "surf", "Points")

analysis_data <- select(mydata,one_of(keep_columns))

#All variable names to lower case:
names(analysis_data) <- tolower(names(analysis_data))

#Scaling combination variables:
analysis_data$deep <- analysis_data$deep/12
analysis_data$stra <- analysis_data$stra/8
analysis_data$surf <- analysis_data$surf/12
analysis_data$attitude <- analysis_data$attitude/10

#Only those with points>0
analysis_data <- filter(analysis_data, points>0)
analysis_data$gender <- factor(analysis_data$gender)
dim(analysis_data) #166 x 7, OK
str(analysis_data)
head(analysis_data)
summary(analysis_data)

#Checking working directory:
getwd() #"/home/jkox/Git/proj/IODS-project" (working directory OK)

#saving data:
write.csv(analysis_data,file='data/learning2014.csv',row.names = FALSE)

#Test: Reading data
test.analysis_data <- read.csv(file='data/learning2014.csv')
str(test.analysis_data) #OK
head(test.analysis_data) #OK
rm(test.analysis_data) #removing test data

#Graphical summary

library(GGally)
library(ggplot2)

p <- ggpairs(analysis_data, mapping = aes(), lower = list(combo = wrap("facethist", bins = 20)))
p


#Linear model

my_model <- lm(points ~ attitude+age+stra, data = analysis_data)
summary(my_model)

#Removing age:
my_model2 <- lm(points ~ attitude+stra, data = analysis_data)
summary(my_model2)

#Removing stra:
my_model3 <- lm(points ~ attitude, data = analysis_data)
summary(my_model3)

qplot(attitude, points, data = analysis_data) + geom_smooth(method = "lm") #plot

par(mfrow=c(2,2))
plot(my_model3,1)
plot(my_model3,2)
#plot(my_model3,4)
plot(my_model3,5)


###
#Additional examples from DataCamp:

########################

# learning2014 is available

# Access the gglot2 library
library(ggplot2)

# initialize plot with data and aesthetic mapping
p1 <- ggplot(analysis_data, aes(x = attitude, y = points, col=gender))

# define the visualization type (points)
p2 <- p1 + geom_point()

# draw the plot
p2

# add a regression line
p3 <- p2 + geom_smooth(method = "lm")

# add a main title and draw the plot
p4 <- p3 + ggtitle("Main title")
p4

#########

# learning2014 is available

# draw a scatter plot matrix of the variables in learning2014.
# [-1] excludes the first column (gender)
pairs(learning2014[-1], col=learning2014$gender)

# access the GGally and ggplot2 libraries
library(GGally)
library(ggplot2)

# create a more advanced plot matrix with ggpairs()
p <- ggpairs(learning2014, mapping = aes(col=gender,alpha=0.3), lower = list(combo = wrap("facethist", bins = 20)))

# draw the plot
p

#######################
