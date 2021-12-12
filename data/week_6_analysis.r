

#Longitudinal data analysis

#Data analysis part:

#1. Implementing the analyses of Chapter 8 of MABS

library(ggplot2)

RATSL <- read.csv(file="/home/jkox/Git/proj/IODS-project/data/RATSL.csv") #reading RATS data is long format

#convertng as factors:
RATSL$ID <- factor(RATSL$ID)
RATSL$Group <- factor(RATSL$Group)

glimpse(RATSL) #glimpsing data

# Draw the plot
ggplot(RATSL, aes(x = Time, y = Weight, linetype = ID)) +
  geom_line() +
  scale_linetype_manual(values = rep(1:10, times=4)) +
  facet_grid(. ~ Group, labeller = label_both) +
  theme(legend.position = "none") +
  scale_y_continuous(limits = c(min(RATSL$Weight), max(RATSL$Weight)))


# Standardise the variable Weight
RATSL <- RATSL %>%
  group_by(Time) %>%
  mutate(stdweight = (Weight-mean(Weight))/sd(Weight)) %>%
  ungroup()

# Glimpse the data
glimpse(RATSL)

# Plot again with the standardised Weight
ggplot(RATSL, aes(x = Time, y = stdweight, linetype = ID)) +
  geom_line() +
  scale_linetype_manual(values = rep(1:10, times=4)) +
  facet_grid(. ~ Group, labeller = label_both) +
  theme(legend.position = "none") +
  scale_y_continuous(name = "standardized Weight")



# Summary data with mean and standard error of Weight by Group and Time 
RATSS <- RATSL %>%
  group_by(Group, Time) %>%
  summarise( mean = mean(Weight), se = sd(Weight)/sqrt(length(Weight)) ) %>%
  ungroup()

# Plot the mean profiles
ggplot(RATSS, aes(x = Time, y = mean, linetype = Group, shape = Group)) +
  geom_line() +
  scale_linetype_manual(values = c(1,2,3)) +
  geom_point(size=3) +
  scale_shape_manual(values = c(1,2,3)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se, linetype="1"), width=0.3) +
  theme(legend.position = c(0.9,0.5)) +
  scale_y_continuous(name = "mean(Weight) +/- se(Weight)")

# Boxplot by Time and Group
ggplot(RATSL, aes(x = factor(Time), y = Weight, fill=Group)) +
  geom_boxplot() +
  scale_x_discrete(name = "Time")

# Create a summary data by Group and ID with mean as the summary variable (ignoring baseline Time 1).
RATSL8S <- RATSL %>%
  filter(Time > 1) %>%
  group_by(Group, ID) %>%
  summarise( mean=mean(Weight) ) %>%
  ungroup()


# Draw a boxplot of the mean versus treatment
ggplot(RATSL8S, aes(x = Group, y = mean)) +
  geom_boxplot() +
  stat_summary(fun = "mean", geom = "point", shape=23, size=4, fill = "white") +
  scale_y_continuous(name = "mean(Weight), Time 1 excluded")

# Create a new data by filtering the outliers:

subset(RATSL8S,Group%in%1,'mean') #239
subset(RATSL8S,Group%in%2,'mean') #594
subset(RATSL8S,Group%in%3,'mean') #495

RATSL8S1 <- RATSL8S %>% filter(!((Group%in%1 & mean<240) | (Group%in%2 & mean>500) | (Group%in%3 & mean<500)))



ggplot(RATSL8S1, aes(x = Group, y = mean)) +
  geom_boxplot() +
  stat_summary(fun = "mean", geom = "point", shape=23, size=4, fill = "white") +
  scale_y_continuous(name = "mean(Weight), Time 1 excluded")


#t-tests

t.test(mean ~ Group, data = RATSL8S, subset = Group%in%c(1,2), var.equal = TRUE) #p<<<0.05
t.test(mean ~ Group, data = RATSL8S, subset = Group%in%c(1,3), var.equal = TRUE) #p<<<0.05
t.test(mean ~ Group, data = RATSL8S, subset = Group%in%c(2,3), var.equal = TRUE) #p=0.33

t.test(mean ~ Group, data = RATSL8S1, subset = Group%in%c(1,2), var.equal = TRUE) #p<<<0.05
t.test(mean ~ Group, data = RATSL8S1, subset = Group%in%c(1,3), var.equal = TRUE) #p<<<0.05
t.test(mean ~ Group, data = RATSL8S1, subset = Group%in%c(2,3), var.equal = TRUE) #p<<<0.05


# Add the baseline from the original data as a new variable to the summary data
RATSL8S2 <- RATSL8S %>%
  mutate(baseline = RATS$WD1)

# Fit the linear model with the mean as the response 
fit <- lm(mean ~ baseline + Group, data = RATSL8S2)

# Compute the analysis of variance table for the fitted model with anova()
anova(fit)

#part 2

BPRSL <- read.csv(file="/home/jkox/Git/proj/IODS-project/data/BPRSL.csv") #reading BPRS data is long format

# Factor variables subject and treatment
BPRSL$subject <- factor(BPRSL$subject)
BPRSL$treatment <- factor(BPRSL$treatment)

# Glimpse the data
glimpse(BPRSL)

# create a regression model RATS_reg
BPRS_reg <- lm(bprs ~ week + treatment, data=BPRSL)

# print out a summary of the model
summary(BPRS_reg)

# access library lme4
library(lme4)

# Create a random intercept model
BPRS_ref <- lmer(bprs ~ week + treatment + (1 | subject), data = BPRSL, REML = FALSE)

# Print the summary of the model
summary(BPRS_ref)

anova(BPRS_ref, BPRS_reg)

# create a random intercept and random slope model
BPRS_ref1 <- lmer(bprs ~ week + treatment + (week | subject), data = BPRSL, REML = FALSE)

# print a summary of the model
summary(BPRS_ref1)

# perform an ANOVA test on the two models
anova(BPRS_ref1, BPRS_ref)

# create a random intercept and random slope model with the interaction
BPRS_ref2 <- lmer(bprs ~ week + treatment + week * treatment + (week | subject), data = BPRSL, REML = FALSE)

# print a summary of the model
summary(BPRS_ref2)

# perform an ANOVA test on the two models
anova(BPRS_ref2, BPRS_ref1)

# Plot the BPRSL data
ggplot(BPRSL, aes(x = week, y = bprs, linetype = subject)) +
  geom_line() +
  scale_linetype_manual(values = rep(1:10, times=4)) +
  facet_grid(. ~ treatment, labeller = label_both) +
  theme(legend.position = "none") + 
  scale_y_continuous(limits = c(min(BPRSL$bprs), max(BPRSL$bprs)))

# Create a vector of the fitted values
Fitted <- fitted(BPRS_ref2)

# Create a new column fitted to RATSL
BPRSL <- BPRSL %>% mutate(fitted = Fitted)

# draw the plot with the fitted values
ggplot(BPRSL, aes(x = week, y = Fitted, linetype = subject)) +
  geom_line() +
  scale_linetype_manual(values = rep(1:10, times=4)) +
  facet_grid(. ~ treatment, labeller = label_both) +
  theme(legend.position = "none")
  #scale_y_continuous(limits = c(min(BPRSL$bprs), max(BPRSL$bprs)))


###################################################################

#from DataCamp:

# Convert to long form
BPRSL <-  BPRS %>% gather(key = weeks, value = bprs, -treatment, -subject)

# Extract the week number
BPRSL <-  BPRSL %>% mutate(week = as.integer(substr(weeks,5,5)))

# Take a glimpse at the BPRSL data
glimpse(BPRSL)

# Draw the plot
ggplot(BPRSL, aes(x = week, y = bprs, linetype = subject)) +
  geom_line() +
  scale_linetype_manual(values = rep(1:10, times=4)) +
  facet_grid(. ~ treatment, labeller = label_both) +
  theme(legend.position = "none") + 
  scale_y_continuous(limits = c(min(BPRSL$bprs), max(BPRSL$bprs)))

# Standardise the variable bprs
BPRSL <- BPRSL %>%
  group_by(week) %>%
  mutate(stdbprs = (bprs-mean(bprs))/sd(bprs)) %>%
  ungroup()

# Glimpse the data
glimpse(BPRSL)

# Plot again with the standardised bprs
ggplot(BPRSL, aes(x = week, y = stdbprs, linetype = subject)) +
  geom_line() +
  scale_linetype_manual(values = rep(1:10, times=4)) +
  facet_grid(. ~ treatment, labeller = label_both) +
  scale_y_continuous(name = "standardized bprs")

# Number of subjects (per group):
n <- 20

# Summary data with mean and standard error of bprs by treatment and week 
BPRSS <- BPRSL %>%
  group_by(treatment, week) %>%
  summarise( mean = mean(bprs), se = sd(bprs)/sqrt(n) ) %>%
  ungroup()

# Glimpse the data
glimpse(BPRSS)

# Plot the mean profiles
ggplot(BPRSS, aes(x = week, y = mean, linetype = treatment, shape = treatment)) +
  geom_line() +
  scale_linetype_manual(values = c(1,2)) +
  geom_point(size=3) +
  scale_shape_manual(values = c(1,2)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se, linetype="1"), width=0.3) +
  theme(legend.position = c(0.8,0.8)) +
  scale_y_continuous(name = "mean(bprs) +/- se(bprs)")

# Create a summary data by treatment and subject with mean as the summary variable (ignoring baseline week 0).
BPRSL8S <- BPRSL %>%
  filter(week > 0) %>%
  group_by(treatment, subject) %>%
  summarise( mean=mean(bprs) ) %>%
  ungroup()


# Glimpse the data
glimpse(BPRSL8S)

# Draw a boxplot of the mean versus treatment
ggplot(BPRSL8S1, aes(x = treatment, y = mean)) +
  geom_boxplot() +
  stat_summary(fun.y = "mean", geom = "point", shape=23, size=4, fill = "white") +
  scale_y_continuous(name = "mean(bprs), weeks 1-8")

# Create a new data by filtering the outlier and adjust the ggplot code the draw the plot again with the new data
BPRSL8S1 <- BPRSL8S %>% filter(mean<=70)

# Perform a two-sample t-test
t.test(mean ~ treatment, data = BPRSL8S1, var.equal = TRUE)

# Add the baseline from the original data as a new variable to the summary data
BPRSL8S2 <- BPRSL8S %>%
  mutate(baseline = BPRS$week0)

# Fit the linear model with the mean as the response 
fit <- lm(mean ~ baseline + treatment, data = BPRSL8S2)

# Compute the analysis of variance table for the fitted model with anova()
anova(fit)

# read the RATS data
RATS <- read.table("https://raw.githubusercontent.com/KimmoVehkalahti/MABS/master/Examples/data/rats.txt", header = TRUE, sep = '\t')

# Factor variables ID and Group
RATS$ID <- factor(RATS$ID)
RATS$Group <- factor(RATS$Group)

# Glimpse the data
glimpse(RATS)

# Convert data to long form
RATSL <- RATS %>%
  gather(key = WD, value = Weight, -ID, -Group) %>%
  mutate(Time = as.integer(substr(WD,3,5))) 

# Glimpse the data
glimpse(RATSL)

# Plot the RATSL data
ggplot(RATSL, aes(x = Time, y = Weight, group = ID)) +
  geom_line(aes(linetype = Group)) +
  scale_x_continuous(name = "Time (days)", breaks = seq(0, 60, 10)) +
  scale_y_continuous(name = "Weight (grams)") +
  theme(legend.position = "top")

# create a regression model RATS_reg
RATS_reg <- lm(Weight ~ Time + Group, data=RATSL)

# print out a summary of the model
summary(RATS_reg)

# access library lme4
library(lme4)

# Create a random intercept model
RATS_ref <- lmer(Weight ~ Time + Group + (1 | ID), data = RATSL, REML = FALSE)

# Print the summary of the model
summary(RATS_ref)

# create a random intercept and random slope model
RATS_ref1 <- lmer(Weight ~ Time + Group + (Time | ID), data = RATSL, REML = FALSE)

# print a summary of the model
summary(RATS_ref1)

# perform an ANOVA test on the two models
anova(RATS_ref1, RATS_ref)

# create a random intercept and random slope model with the interaction
RATS_ref2 <- lmer(Weight ~ Time + Group + Time * Group + (Time | ID), data = RATSL, REML = FALSE)

# print a summary of the model
summary(RATS_ref2)

# perform an ANOVA test on the two models
anova(RATS_ref2, RATS_ref1)

# draw the plot of RATSL with the observed Weight values
ggplot(RATSL, aes(x = Time, y = Weight, group = ID)) +
  geom_line(aes(linetype = Group)) +
  scale_x_continuous(name = "Time (days)", breaks = seq(0, 60, 20)) +
  scale_y_continuous(name = "Observed weight (grams)") +
  theme(legend.position = "top")

# Create a vector of the fitted values
Fitted <- fitted(RATS_ref2)

# Create a new column fitted to RATSL
RATSL <- RATSL %>% mutate(fitted = Fitted)

# draw the plot of RATSL with the Fitted values of weight
ggplot(RATSL, aes(x = Time, y = fitted, group = ID)) +
  geom_line(aes(linetype = Group)) +
  scale_x_continuous(name = "Time (days)", breaks = seq(0, 60, 20)) +
  scale_y_continuous(name = "Fitted weight (grams)") +
  theme(legend.position = "top")












