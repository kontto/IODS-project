
#Jukka Kontto
#7th December 2021

#Week 6 data wrangling

library(dplyr)
library(tidyr)

BPRS <- read.table("https://raw.githubusercontent.com/KimmoVehkalahti/MABS/master/Examples/data/BPRS.txt", header = TRUE)
RATS <- read.table("https://raw.githubusercontent.com/KimmoVehkalahti/MABS/master/Examples/data/rats.txt", header = TRUE)

#dimensions:

dim(BPRS) #40 x 11
dim(RATS) #16 x 13

#names of the variables

names(BPRS)
#[1] "treatment" "subject"   "week0"     "week1"     "week2"     "week3"     "week4"     "week5"     "week6"     "week7"     "week8"

names(RATS)
#[1] "ID"    "Group" "WD1"   "WD8"   "WD15"  "WD22"  "WD29"  "WD36"  "WD43"  "WD44"  "WD50"  "WD57"  "WD64"

#summaries

summary(BPRS)
# treatment      subject          week0           week1           week2          week3           week4           week5           week6           week7          week8      
# Min.   :1.0   Min.   : 1.00   Min.   :24.00   Min.   :23.00   Min.   :26.0   Min.   :24.00   Min.   :20.00   Min.   :20.00   Min.   :19.00   Min.   :18.0   Min.   :20.00  
# 1st Qu.:1.0   1st Qu.: 5.75   1st Qu.:38.00   1st Qu.:35.00   1st Qu.:32.0   1st Qu.:29.75   1st Qu.:28.00   1st Qu.:26.00   1st Qu.:22.75   1st Qu.:23.0   1st Qu.:22.75  
# Median :1.5   Median :10.50   Median :46.00   Median :41.00   Median :38.0   Median :36.50   Median :34.50   Median :30.50   Median :28.50   Median :30.0   Median :28.00  
# Mean   :1.5   Mean   :10.50   Mean   :48.00   Mean   :46.33   Mean   :41.7   Mean   :39.15   Mean   :36.35   Mean   :32.55   Mean   :31.23   Mean   :32.2   Mean   :31.43  
# 3rd Qu.:2.0   3rd Qu.:15.25   3rd Qu.:58.25   3rd Qu.:54.25   3rd Qu.:49.0   3rd Qu.:44.50   3rd Qu.:43.00   3rd Qu.:38.00   3rd Qu.:37.00   3rd Qu.:38.0   3rd Qu.:35.25  
# Max.   :2.0   Max.   :20.00   Max.   :78.00   Max.   :95.00   Max.   :75.0   Max.   :76.00   Max.   :66.00   Max.   :64.00   Max.   :64.00   Max.   :62.0   Max.   :75.00

summary(RATS)
# ID            Group           WD1             WD8             WD15            WD22            WD29            WD36            WD43            WD44            WD50            WD57      
# Min.   : 1.00   Min.   :1.00   Min.   :225.0   Min.   :230.0   Min.   :230.0   Min.   :232.0   Min.   :240.0   Min.   :240.0   Min.   :243.0   Min.   :244.0   Min.   :238.0   Min.   :247.0  
# 1st Qu.: 4.75   1st Qu.:1.00   1st Qu.:252.5   1st Qu.:255.0   1st Qu.:255.0   1st Qu.:267.2   1st Qu.:268.8   1st Qu.:267.2   1st Qu.:269.2   1st Qu.:270.0   1st Qu.:273.8   1st Qu.:273.8  
# Median : 8.50   Median :1.50   Median :340.0   Median :345.0   Median :347.5   Median :351.5   Median :356.5   Median :360.0   Median :360.0   Median :362.0   Median :370.0   Median :373.5  
# Mean   : 8.50   Mean   :1.75   Mean   :365.9   Mean   :369.1   Mean   :372.5   Mean   :379.2   Mean   :383.9   Mean   :387.0   Mean   :386.0   Mean   :388.3   Mean   :394.6   Mean   :398.6  
# 3rd Qu.:12.25   3rd Qu.:2.25   3rd Qu.:480.0   3rd Qu.:476.2   3rd Qu.:486.2   3rd Qu.:492.5   3rd Qu.:497.8   3rd Qu.:504.2   3rd Qu.:501.0   3rd Qu.:510.5   3rd Qu.:516.0   3rd Qu.:524.5  
# Max.   :16.00   Max.   :3.00   Max.   :555.0   Max.   :560.0   Max.   :565.0   Max.   :580.0   Max.   :590.0   Max.   :597.0   Max.   :595.0   Max.   :595.0   Max.   :612.0   Max.   :618.0  

# WD64      
# Min.   :245.0  
# 1st Qu.:278.0  
# Median :378.0  
# Mean   :404.1  
# 3rd Qu.:530.8  
# Max.   :628.0 

#structures

str(BPRS)
# 'data.frame':	40 obs. of  11 variables:
# $ treatment: int  1 1 1 1 1 1 1 1 1 1 ...
# $ subject  : int  1 2 3 4 5 6 7 8 9 10 ...
# $ week0    : int  42 58 54 55 72 48 71 30 41 57 ...
# $ week1    : int  36 68 55 77 75 43 61 36 43 51 ...
# $ week2    : int  36 61 41 49 72 41 47 38 39 51 ...
# $ week3    : int  43 55 38 54 65 38 30 38 35 55 ...
# $ week4    : int  41 43 43 56 50 36 27 31 28 53 ...
# $ week5    : int  40 34 28 50 39 29 40 26 22 43 ...
# $ week6    : int  38 28 29 47 32 33 30 26 20 43 ...
# $ week7    : int  47 28 25 42 38 27 31 25 23 39 ...
# $ week8    : int  51 28 24 46 32 25 31 24 21 32 ...

str(RATS)
# 'data.frame':	16 obs. of  13 variables:
# $ ID   : int  1 2 3 4 5 6 7 8 9 10 ...
# $ Group: int  1 1 1 1 1 1 1 1 2 2 ...
# $ WD1  : int  240 225 245 260 255 260 275 245 410 405 ...
# $ WD8  : int  250 230 250 255 260 265 275 255 415 420 ...
# $ WD15 : int  255 230 250 255 255 270 260 260 425 430 ...
# $ WD22 : int  260 232 255 265 270 275 270 268 428 440 ...
# $ WD29 : int  262 240 262 265 270 275 273 270 438 448 ...
# $ WD36 : int  258 240 265 268 273 277 274 265 443 460 ...
# $ WD43 : int  266 243 267 270 274 278 276 265 442 458 ...
# $ WD44 : int  266 244 267 272 273 278 271 267 446 464 ...
# $ WD50 : int  265 238 264 274 276 284 282 273 456 475 ...
# $ WD57 : int  272 247 268 273 278 279 281 274 468 484 ...
# $ WD64 : int  278 245 269 275 280 281 284 278 478 496 ...

head(BPRS)
#   treatment subject week0 week1 week2 week3 week4 week5 week6 week7 week8
# 1         1       1    42    36    36    43    41    40    38    47    51
# 2         1       2    58    68    61    55    43    34    28    28    28
# 3         1       3    54    55    41    38    43    28    29    25    24
# 4         1       4    55    77    49    54    56    50    47    42    46
# 5         1       5    72    75    72    65    50    39    32    38    32
# 6         1       6    48    43    41    38    36    29    33    27    25

head(RATS)
#   ID Group WD1 WD8 WD15 WD22 WD29 WD36 WD43 WD44 WD50 WD57 WD64
# 1  1     1 240 250  255  260  262  258  266  266  265  272  278
# 2  2     1 225 230  230  232  240  240  243  244  238  247  245
# 3  3     1 245 250  250  255  262  265  267  267  264  268  269
# 4  4     1 260 255  255  265  265  268  270  272  274  273  275
# 5  5     1 255 260  255  270  270  273  274  273  276  278  280
# 6  6     1 260 265  270  275  275  277  278  278  284  279  281


#converting categorical variables to factors:

BPRS$treatment <- factor(BPRS$treatment)
BPRS$subject <- factor(BPRS$subject)

RATS$ID <- factor(RATS$ID)
RATS$Group <- factor(RATS$Group)

#converting to long form and adding new variables:

BPRSL <- BPRS %>% gather(key = weeks, value = bprs, -treatment, -subject) %>% mutate(week = as.integer(substr(weeks,5,5)))

RATSL <- RATS %>% gather(key = WD, value = Weight, -ID, -Group) %>% mutate(Time = as.integer(substr(WD,3,5)))


##checking new datasets

#dimensions

dim(BPRSL) #360 x 5
dim(RATSL) #176 x 5

#variable names

names(BPRSL)
#[1] "treatment" "subject"   "weeks"     "bprs"      "week" 

names(RATSL)
#[1] "ID"     "Group"  "WD"     "Weight" "Time"

#structures

str(BPRSL)
# 'data.frame':	360 obs. of  5 variables:
# $ treatment: Factor w/ 2 levels "1","2": 1 1 1 1 1 1 1 1 1 1 ...
# $ subject  : Factor w/ 20 levels "1","2","3","4",..: 1 2 3 4 5 6 7 8 9 10 ...
# $ weeks    : chr  "week0" "week0" "week0" "week0" ...
# $ bprs     : int  42 58 54 55 72 48 71 30 41 57 ...
# $ week     : int  0 0 0 0 0 0 0 0 0 0 ...

str(RATSL)
# 'data.frame':	176 obs. of  5 variables:
# $ ID    : Factor w/ 16 levels "1","2","3","4",..: 1 2 3 4 5 6 7 8 9 10 ...
# $ Group : Factor w/ 3 levels "1","2","3": 1 1 1 1 1 1 1 1 2 2 ...
# $ WD    : chr  "WD1" "WD1" "WD1" "WD1" ...
# $ Weight: int  240 225 245 260 255 260 275 245 410 405 ...
# $ Time  : int  1 1 1 1 1 1 1 1 1 1 ...

head(BPRSL)
#   treatment subject weeks bprs week
# 1         1       1 week0   42    0
# 2         1       2 week0   58    0
# 3         1       3 week0   54    0
# 4         1       4 week0   55    0
# 5         1       5 week0   72    0
# 6         1       6 week0   48    0

head(RATSL)
#   ID Group  WD Weight Time
# 1  1     1 WD1    240    1
# 2  2     1 WD1    225    1
# 3  3     1 WD1    245    1
# 4  4     1 WD1    260    1
# 5  5     1 WD1    255    1
# 6  6     1 WD1    260    1

#summaries

summary(BPRSL)
# treatment    subject       weeks                bprs            week  
# 1:180     1      : 18   Length:360         Min.   :18.00   Min.   :0  
# 2:180     2      : 18   Class :character   1st Qu.:27.00   1st Qu.:2  
#           3      : 18   Mode  :character   Median :35.00   Median :4  
#           4      : 18                      Mean   :37.66   Mean   :4  
#           5      : 18                      3rd Qu.:43.00   3rd Qu.:6  
#           6      : 18                      Max.   :95.00   Max.   :8  
#           (Other):252                                                                                    

summary(RATSL)
# ID      Group       WD                Weight           Time      
# 1      : 11   1:88   Length:176         Min.   :225.0   Min.   : 1.00  
# 2      : 11   2:44   Class :character   1st Qu.:267.0   1st Qu.:15.00  
# 3      : 11   3:44   Mode  :character   Median :344.5   Median :36.00  
# 4      : 11                             Mean   :384.5   Mean   :33.55  
# 5      : 11                             3rd Qu.:511.2   3rd Qu.:50.00  
# 6      : 11                             Max.   :628.0   Max.   :64.00  
# (Other):110 

#Long form:
#Each measurement (bprs, Weight) is on its own row along with values of other variables

#Wide form:
#Measurements (bprs, Weight) are on the same row for each factor variable value (treatment+subject, ID+Group) combination


#writing wrangled datasets:

write.csv(BPRSL,file="/home/jkox/Git/proj/IODS-project/data/BPRSL.csv",row.names = FALSE)
write.csv(RATSL,file="/home/jkox/Git/proj/IODS-project/data/RATSL.csv",row.names = FALSE)
