
#Jukka Kontto
#30th November 2021

#Dimensionality reduction techniques

#links to interpret PCA results:
#https://blog.bioturing.com/2018/06/18/how-to-read-pca-biplots-and-scree-plots/
#https://www.geo.fu-berlin.de/en/v/soga/Geodata-analysis/Principal-Component-Analysis/principal-components-basics/Interpretation-and-visualization/index.html#:~:text=Interpreting%20Biplots,in%20a%20single%20biplot%20display.&text=The%20plot%20shows%20the%20observations,principal%20components%20(synthetic%20variables).

human <- read.csv(file="/home/jkox/Git/proj/IODS-project/data/human.csv",row.names = 1)

str(human) #structure

dim(human) #dimensions

summary(human)

library(GGally)
p <- ggpairs(human, mapping = aes(), lower = list(combo = wrap("facethist", bins = 20)))
p

#PCA 1

pca_human <- prcomp(human)
s <- summary(pca_human)
round(100*s$importance[2, ], digits = 1)
biplot(pca_human, choices = 1:2, cex=c(0.8,1), col=c("grey40","deeppink2"))

#PCA 2

human_std <- scale(human)
pca_human_std <- prcomp(human_std)
s_std <- summary(pca_human_std)
pca_pr <- round(100*s_std$importance[2, ], digits = 1)
pca_pr
#pc_lab <- paste0(names(pca_pr), " (", pca_pr, "%)") # create object pc_lab to be used as axis labels
pc_lab <- paste0(c('Human development','Equality'), " (", pca_pr, "%)") # create object pc_lab to be used as axis labels
biplot(pca_human_std, choices = 1:2, cex=c(0.8,1), col=c("grey40","deeppink2"), xlab = pc_lab[1], ylab = pc_lab[2])

#MCA

library(FactoMineR)
library(tidyr)
data(tea)

str(tea)
dim(tea)

gather(tea) %>% ggplot(aes(value)) + facet_wrap("key", scales = "free")  + geom_bar() + theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8))

names(tea)

mca <- MCA(tea[,-19], graph = FALSE)
summary(mca)
plot(mca, invisible=c("ind"), habillage = "quali", grapth.type = "classic")

biplot(mca, choices=1:2)



#From DataCamp:

# remove the commas from GNI and print out a numeric version of it
str_replace(human$GNI, pattern=",", replace ="")

# human with modified GNI and dplyr are available

# columns to keep
keep <- c("Country", "Edu2.FM", "Labo.FM", "Life.Exp", "Edu.Exp", "GNI", "Mat.Mor", "Ado.Birth", "Parli.F")

# select the 'keep' columns
human <- select(human, one_of(keep))

# print out a completeness indicator of the 'human' data
complete.cases(human)

# print out the data along with a completeness indicator as the last column
data.frame(human[-1], comp = complete.cases(human))

# filter out all rows with NA values
human_ <- filter(human, complete.cases(human))

# modified human is available

# standardize the variables
human_std <- scale(human)

# print out summaries of the standardized variables
summary(human_std)

# perform principal component analysis (with the SVD method)
pca_human <- prcomp(human_std)

# draw a biplot of the principal component representation and the original variables
biplot(pca_human, choices = 1:2, cex=c(0.8,1), col=c("grey40","deeppink2"))

# pca_human, dplyr are available

# create and print out a summary of pca_human
s <- summary(pca_human)


# rounded percetanges of variance captured by each PC
pca_pr <- round(100*s$importance[2, ], digits = 1)

# print out the percentages of variance
pca_pr

# create object pc_lab to be used as axis labels
pc_lab <- paste0(names(pca_pr), " (", pca_pr, "%)")

# draw a biplot
biplot(pca_human, cex = c(0.8, 1), col = c("grey40", "deeppink2"), xlab = pc_lab[1], ylab = pc_lab[2])

# visualize the dataset
gather(tea_time) %>% ggplot(aes(value)) + facet_wrap("key", scales = "free")  + geom_bar() + theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8))

# tea_time is available

# multiple correspondence analysis
mca <- MCA(tea_time, graph = FALSE)

# summary of the model
summary(mca)

# visualize MCA
plot(mca, invisible=c("ind"), habillage = "quali", grapth.type = "classic")




