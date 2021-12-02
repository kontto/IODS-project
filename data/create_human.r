

#Jukka Kontto
#24th November 2021

#Week 4 data wrangling



#Reading datsets:

hd <- read.csv("http://s3.amazonaws.com/assets.datacamp.com/production/course_2218/datasets/human_development.csv", stringsAsFactors = F)

str(hd)
dim(hd) #195 x 8
summary(hd)
#   HDI.Rank        Country          Human.Development.Index..HDI. Life.Expectancy.at.Birth Expected.Years.of.Education Mean.Years.of.Education Gross.National.Income..GNI..per.Capita GNI.per.Capita.Rank.Minus.HDI.Rank
#Min.   :  1.00   Length:195         Min.   :0.3480                Min.   :49.00            Min.   : 4.10               Min.   : 1.400          Length:195                             Min.   :-84.0000                  
#1st Qu.: 47.75   Class :character   1st Qu.:0.5770                1st Qu.:65.75            1st Qu.:11.10               1st Qu.: 5.550          Class :character                       1st Qu.: -9.0000                  
#Median : 94.00   Mode  :character   Median :0.7210                Median :73.10            Median :13.10               Median : 8.400          Mode  :character                       Median :  1.5000                  
#Mean   : 94.31                      Mean   :0.6918                Mean   :71.07            Mean   :12.86               Mean   : 8.079                                                 Mean   :  0.1862                  
#3rd Qu.:141.25                      3rd Qu.:0.8000                3rd Qu.:76.80            3rd Qu.:14.90               3rd Qu.:10.600                                                 3rd Qu.: 11.0000                  
#Max.   :188.00                      Max.   :0.9440                Max.   :84.00            Max.   :20.20               Max.   :13.100                                                 Max.   : 47.0000                  
#NA's   :7                                                                                                                                                                             NA's   :7             

gii <- read.csv("http://s3.amazonaws.com/assets.datacamp.com/production/course_2218/datasets/gender_inequality.csv", stringsAsFactors = F, na.strings = "..")

str(gii)
dim(gii) #195 x 10
summary(gii)
#   GII.Rank        Country          Gender.Inequality.Index..GII. Maternal.Mortality.Ratio Adolescent.Birth.Rate Percent.Representation.in.Parliament Population.with.Secondary.Education..Female.
#Min.   :  1.00   Length:195         Min.   :0.0160                Min.   :   1.0           Min.   :  0.60        Min.   : 0.00                        Min.   :  0.9                               
#1st Qu.: 47.75   Class :character   1st Qu.:0.2030                1st Qu.:  16.0           1st Qu.: 15.45        1st Qu.:12.47                        1st Qu.: 27.8                               
#Median : 94.00   Mode  :character   Median :0.3935                Median :  69.0           Median : 40.95        Median :19.50                        Median : 55.7                               
#Mean   : 94.31                      Mean   :0.3695                Mean   : 163.2           Mean   : 49.55        Mean   :20.60                        Mean   : 54.8                               
#3rd Qu.:141.25                      3rd Qu.:0.5272                3rd Qu.: 230.0           3rd Qu.: 71.78        3rd Qu.:27.02                        3rd Qu.: 81.8                               
#Max.   :188.00                      Max.   :0.7440                Max.   :1100.0           Max.   :204.80        Max.   :57.50                        Max.   :100.0                               
#NA's   :7                           NA's   :33                    NA's   :10               NA's   :5             NA's   :3                            NA's   :26                                  
#Population.with.Secondary.Education..Male. Labour.Force.Participation.Rate..Female. Labour.Force.Participation.Rate..Male.
#Min.   :  3.20                             Min.   :13.50                            Min.   :44.20                         
#1st Qu.: 38.30                             1st Qu.:44.50                            1st Qu.:68.88                         
#Median : 60.00                             Median :53.30                            Median :75.55                         
#Mean   : 60.29                             Mean   :52.61                            Mean   :74.74                         
#3rd Qu.: 85.80                             3rd Qu.:62.62                            3rd Qu.:80.15                         
#Max.   :100.00                             Max.   :88.10                            Max.   :95.50                         
#NA's   :26                                 NA's   :11                               NA's   :11


#renaming the columns:

names(hd) <- c('hdi_rank','country','hdi','life_exp','exp_edu','mean_edu','GNI_cap','GNI_HDI_diff')
names(gii) <- c('gii_rank','country','gii','mm_ratio','a_birth_rate','parlrep_p','edu2F','edu2M','labF','labM')

#New variables:

library(dplyr)
gii <- gii %>% mutate(edu2R = edu2F / edu2M) %>% mutate(labR = labF / labM)

#joining datasets:
human <- inner_join(hd,gii,by='country')

dim(human) #195 x 19, OK

write.csv(human,file="/home/jkox/Git/proj/IODS-project/data/human.csv",row.names = FALSE)


#Week 5 data wrangling

library(dplyr)
library(stringr)

human <- read.csv(file="/home/jkox/Git/proj/IODS-project/data/human.csv")

str(human)
# 'data.frame':	195 obs. of  19 variables:
#   $ hdi_rank    : int  1 2 3 4 5 6 6 8 9 9 ...
# $ country     : chr  "Norway" "Australia" "Switzerland" "Denmark" ...
# $ hdi         : num  0.944 0.935 0.93 0.923 0.922 0.916 0.916 0.915 0.913 0.913 ...
# $ life_exp    : num  81.6 82.4 83 80.2 81.6 80.9 80.9 79.1 82 81.8 ...
# $ exp_edu     : num  17.5 20.2 15.8 18.7 17.9 16.5 18.6 16.5 15.9 19.2 ...
# $ mean_edu    : num  12.6 13 12.8 12.7 11.9 13.1 12.2 12.9 13 12.5 ...
# $ GNI_cap     : chr  "64,992" "42,261" "56,431" "44,025" ...
# $ GNI_HDI_diff: int  5 17 6 11 9 11 16 3 11 23 ...
# $ gii_rank    : int  1 2 3 4 5 6 6 8 9 9 ...
# $ gii         : num  0.067 0.11 0.028 0.048 0.062 0.041 0.113 0.28 0.129 0.157 ...
# $ mm_ratio    : int  4 6 6 5 6 7 9 28 11 8 ...
# $ a_birth_rate: num  7.8 12.1 1.9 5.1 6.2 3.8 8.2 31 14.5 25.3 ...
# $ parlrep_p   : num  39.6 30.5 28.5 38 36.9 36.9 19.9 19.4 28.2 31.4 ...
# $ edu2F       : num  97.4 94.3 95 95.5 87.7 96.3 80.5 95.1 100 95 ...
# $ edu2M       : num  96.7 94.6 96.6 96.6 90.5 97 78.6 94.8 100 95.3 ...
# $ labF        : num  61.2 58.8 61.8 58.7 58.5 53.6 53.1 56.3 61.6 62 ...
# $ labM        : num  68.7 71.8 74.9 66.4 70.6 66.4 68.1 68.9 71 73.8 ...
# $ edu2R       : num  1.007 0.997 0.983 0.989 0.969 ...
# $ labR        : num  0.891 0.819 0.825 0.884 0.829 ...

dim(human) #195 x 19

#Dataset has 19 variables concerning country-specific information about people capabilities, health, education, economics, and gender inequality.
#The dataset originates from the United Nations Development Programme.
#The variables are:
#hdi_rank = the ranking of countries in terms of Human Development Index (HDI)
#hdi = Human development Index
#life_exp = life expentance at birth
#exp_edu = expected years of education
#mean_edu = mean years of education
#GNI_cap = gross national income per capita
#GNI_HDI_diff = The difference between GNI and HDI rankings
#gii_rank = the ranking of countries in terms of Gender Inequality Index (GII)
#gii = Gender Inequality Index
#mm_ratio = maternal mortality ratio
#a_birth_rate = adolescent birth rate
#parlrep_p = representation in parliament, percent
#edu2F = population with secondary education among female
#edu2M = population with secondary education among male
#labF = labour force participation rate among female
#labM = labour force participation rate among male
#edu2R = ratio between edu2F and edu2M
#labR = ratio between labF and labM


#mutating GNI_cap:
human$GNI_cap <- str_replace(human$GNI_cap, pattern=",", replace ="") %>% as.numeric

#keeping relevant variables:
keep_columns <- c("country","edu2F","labF","exp_edu","life_exp","GNI_cap","mm_ratio","a_birth_rate","parlrep_p")
human <- dplyr::select(human, one_of(keep_columns))

#excluding rows with missing data:
human <- filter(human, complete.cases(human))

#excluding regions (and keeping countries):
tail(human$country,10) #last 7 are regions
human <- human[1:(nrow(human)-7),]

#defining countries as row names and excluding variable country
rownames(human) <- human$country
human <- dplyr::select(human,-country)

dim(human) #155 x 8, OK

#saving data:
write.csv(human,file="/home/jkox/Git/proj/IODS-project/data/human.csv",row.names = TRUE)


