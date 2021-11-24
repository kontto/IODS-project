

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


