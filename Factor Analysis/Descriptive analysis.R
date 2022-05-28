
# Read data
df_aps <- read.csv("C:\\E DRIVE\\LANGARA COLLEGE\\DANA 4830-001\\Team project\\2019-aps-employee-census-dataset.csv", 
                   header = T, 
                   fileEncoding="UTF-8-BOM", 
                   na.strings=c(" "))
View(df_aps)
sum(is.na(df_aps))

windows()
# Check data
library(vtable)
st(df_aps)
vtable(df_aps)
str(df_aps)

# Demographic profile from q1 ~ 23 but in our dataset only have from AS to q7.
# Convert into factors
df_aps$AS <- as.factor(df_aps$AS)
df_aps$q1 <- as.factor(df_aps$q1)
df_aps$q2. <- as.factor(df_aps$q2.)
df_aps$q7. <- as.factor(df_aps$q7.)

str(df_aps[c(1:4)])
st(df_aps[c(1:4)])

# Freq barplots
library(ggplot2)
library(tidyverse)
library(scales)
## AS
AS_bar <- ggplot(df_aps, aes(x = AS)) + geom_bar() + geom_text(stat = 'count', aes(label=..count..), vjust=-1) + ggtitle("Size of Agency")
AS_bar

## q1
Gen_bar <- ggplot(df_aps, aes(x = q1), ) + geom_bar() + geom_text(stat = 'count', aes(label=..count..), vjust=-1) + ggtitle("Gender of Respondent")
Gen_bar

## q2.
Age_bar <- ggplot(df_aps, aes(x=reorder(q2., q2., function(x)-length(x)))) + geom_bar() + geom_text(stat = 'count', aes(label=..count..), vjust=-1) + ggtitle("Age of Respondent")
Age_bar

## q7.
Class_bar <- ggplot(df_aps, aes(x=reorder(q7., q7., function(x)-length(x)))) + geom_bar() + geom_text(stat = 'count', aes(label=..count..), vjust=-1) + ggtitle("Classification of Respondent")
Class_bar

# Missing Values?
missing <- sum(is.na(df_aps))
missing # 20631763 but including missing values in the multiple choice and tick q's

# Convert all to factors (Not sure if needed this step)
#df_aps[sapply(df_aps, is.character)] <- lapply(df_aps[sapply(df_aps, is.character)], as.factor)
#str(df_aps)
#st(df_aps)

# Check missing values by column
testcol <- map(df_aps, ~mean(is.na(.))) 
testcol
testcol[which (testcol > 0.15)]

# Check missing values continue
st(df_aps)
# Subset of data that are structurally missing
which(colnames(df_aps)=="q37.1")
which(colnames(df_aps)=="q37.15")

which(colnames(df_aps)=="q40.1")
which(colnames(df_aps)=="q40.11")

which(colnames(df_aps)=="q45.1")
which(colnames(df_aps)=="q45.4")

which(colnames(df_aps)=="q47")

which(colnames(df_aps)=="q52.1")
which(colnames(df_aps)=="q52.9")

which(colnames(df_aps)=="q61.1")
which(colnames(df_aps)=="q65.6")

which(colnames(df_aps)=="q67.1")
which(colnames(df_aps)=="q67.11")

which(colnames(df_aps)=="q69.1")
which(colnames(df_aps)=="q69.8")

which(colnames(df_aps)=="q78")
which(colnames(df_aps)=="q83.2")

which(colnames(df_aps)=="q85.1a")
which(colnames(df_aps)=="q85")

which(colnames(df_aps)=="q88")
which(colnames(df_aps)=="q93.7")

which(colnames(df_aps)=="q97")
which(colnames(df_aps)=="q98.9")

which(colnames(df_aps)=="q100.1")
which(colnames(df_aps)=="q102")

which(colnames(df_aps)=="q104.1")
which(colnames(df_aps)=="q108.11")

which(colnames(df_aps)=="q25")
which(colnames(df_aps)=="q36")
which(colnames(df_aps)=="q39")
which(colnames(df_aps)=="q46")
which(colnames(df_aps)=="q48")
which(colnames(df_aps)=="q50")
which(colnames(df_aps)=="q51")
which(colnames(df_aps)=="q55c")
which(colnames(df_aps)=="q57")
which(colnames(df_aps)=="q68")
which(colnames(df_aps)=="q73")
which(colnames(df_aps)=="q74")
which(colnames(df_aps)=="q84")
which(colnames(df_aps)=="q96")
which(colnames(df_aps)=="q99")
which(colnames(df_aps)=="q103")

sub_aps <- df_aps[-c(2, 16, 83, 84:98, 100, 101:111, 127:130, 131, 132, 133, 134, 135, 136:144, 148:150, 151, 152, 159:179, 181:191, 192, 193:200, 235:253, 254, 255:268, 277:295,300, 301:310,311, 312:332,333, 334:374)]
vtable(sub_aps)

# Drop missing values
library(DataCombine)
sub_aps2 <- sub_aps %>% drop_na()
sum(is.na(sub_aps2))
st(sub_aps2)
vtable(sub_aps2)

#df_aps2 <- df_new %>% drop_na(q38, q39, q43a:q44h, q46, q48:q51, q53a:q60d, q66, q68, q70:q77e)
#df_newcheck <- df_aps %>% drop_na(AS:q36, q38, q39, q43a:q44h, q46, q48:q51, q53a:q60d, q66, q68, q70:q77e)

# Relevel factors
# sub_aps2$q2. <- relevel(sub_aps2$q2., "Under 40 years")

# Convert to character and change the values to factor level orders
#sub_aps2[sapply(sub_aps2, is.factor)] <- lapply(sub_aps2[sapply(sub_aps2, is.factor)], as.character)

sub_aps2[sub_aps2 == "Large (1,001 or more employees)"] = 1
sub_aps2[sub_aps2 == "Medium (251 to 1,000 employees)"] = 2
sub_aps2[sub_aps2 == "Small (Less than 250 employees)"] = 3

sub_aps2[sub_aps2 == "Trainee/Graduate/APS"] = 1
sub_aps2[sub_aps2 == "EL"] = 2
sub_aps2[sub_aps2 == "SES"] = 3

sub_aps2[sub_aps2 == "Under 40 years"] = 1
sub_aps2[sub_aps2 == "40 to 54 years"] = 2
sub_aps2[sub_aps2 == "55 years or older"] = 3

sub_aps2[sub_aps2 == "Very satisfied"] = 1
sub_aps2[sub_aps2 == "Satisfied"] = 2
sub_aps2[sub_aps2 == "Neither satisfied nor dissatisfied"] = 3
sub_aps2[sub_aps2 == "Dissatisfied"] = 4
sub_aps2[sub_aps2 == "Very dissatisfied"] = 5

sub_aps2[sub_aps2 == "Always"] = 1
sub_aps2[sub_aps2 == "Often"] = 2
sub_aps2[sub_aps2 == "Sometimes"] = 3
sub_aps2[sub_aps2 == "Rarely"] = 4
sub_aps2[sub_aps2 == "Never"] = 5

sub_aps2[sub_aps2 == "Strongly agree"] = 1
sub_aps2[sub_aps2 == "Agree"] = 2
sub_aps2[sub_aps2 == "Neither agree nor disagree"] = 3
sub_aps2[sub_aps2 == "Disagree"] = 4
sub_aps2[sub_aps2 == "Strongly disagree"] = 5
sub_aps2[sub_aps2 == "Do not know"] = 6

sub_aps2[sub_aps2 == "Above my classification level"] = 1
sub_aps2[sub_aps2 == "Appropriate for my classification level"] = 2
sub_aps2[sub_aps2 == "Below my classification level"] = 3

sub_aps2[sub_aps2 == "Not at all"] = 1
sub_aps2[sub_aps2 == "Very little"] = 2
sub_aps2[sub_aps2 == "Somewhat"] = 3
sub_aps2[sub_aps2 == "To a great extent"] = 4
sub_aps2[sub_aps2 == "To a very great extent"] = 5


# Change factor to numeric
sub_aps2[sapply(sub_aps2, is.character)] <- lapply(sub_aps2[sapply(sub_aps2, is.character)], as.numeric)
vtable(sub_aps2)
st(sub_aps2)

## Mahalanobis
mahalanobis(sub_aps2, colMeans(sub_aps2), cov(sub_aps2))

library(psych)
library(ggplot2)
library(corrplot) #plotting correlation matrices
library(GPArotation) #methods for factor rotation
library(nFactors)

describe(sub_aps2)

## ANOVA test for satisfaction
sub_aps2$q24c #I am satisfied with the recognition I receive for doing a good job
sub_aps2$q24e #I am satisfied with my nonmonetary employment conditions e.g. leave, flexible work rrangements, other benefits)
sub_aps2$q24f #I am satisfied with the stability and security of my current job
sub_aps2$q24i #Considering everything, I am satisfied with my job
sub_aps2$q33f #I am satisfied with the opportunities for career progression in my agency
sub_aps2$q38 # Considering your work and life priorities, how satisfied are you with the work-life balance in your current job? 
sub_aps2$q44a #I am satisfied with the policies/practices in place to help me manage my health and wellbeing

# sample size comparison for group 1 ("Trainee/Graduate/APS") and group 2 ("EL")
nrow(sub_aps2[sub_aps2$q7. == "1",]) # number of subordinates
nrow(sub_aps2[sub_aps2$q7. == "2",]) # number of managers

sub_aps2$q7. <- ordered(sub_aps2$q7., levels = c("1", "2", "3"))
sub_aps2$q7. <- ordered(sub_aps2$q7., levels = c("1", "2"))
aps.aov <- aov(q24c ~ q7., data = sub_aps2)
summary(aps.aov)


# Compute t-test for each
q24c.tt <- t.test(q24c ~ q7., data = sub_aps2, var.equal = TRUE)
q24c.tt

t.test(q24c ~ q7., data = sub_aps2, var.equal = TRUE)
t.test(q24e ~ q7., data = sub_aps2, var.equal = TRUE)
t.test(q24f ~ q7., data = sub_aps2, var.equal = TRUE)
t.test(q24i ~ q7., data = sub_aps2, var.equal = TRUE)
t.test(q33f ~ q7., data = sub_aps2, var.equal = TRUE)
t.test(q38 ~ q7., data = sub_aps2, var.equal = TRUE)
t.test(q44a ~ q7., data = sub_aps2, var.equal = TRUE)

library(dplyr)
group_by(sub_aps2, q7.) %>%
  summarise(
    count = n(),
    mean = mean(q24c, na.rm = TRUE),
    sd = sd(q24c, na.rm = TRUE)
  )


library("ggpubr")
ggboxplot(sub_aps2, x = "q7.", y = "q24c", 
          color = "q7.", palette = c("#00AFBB", "#E7B800", "#FC4E07"),
          order = c("1", "2", "3"),
          ylab = "q24c", xlab = "Classification Level")

ggboxplot(sub_aps2, x = "q7.", y = "q24e", 
          color = "q7.", palette = c("#00AFBB", "#E7B800", "#FC4E07"),
          order = c("1", "2", "3"),
          ylab = "q24e", xlab = "Classification Level")

ggboxplot(sub_aps2, x = "q7.", y = "q24f", 
          color = "q7.", palette = c("#00AFBB", "#E7B800", "#FC4E07"),
          order = c("1", "2", "3"),
          ylab = "q24f", xlab = "Classification Level")

ggboxplot(sub_aps2, x = "q7.", y = "q24i", 
          color = "q7.", palette = c("#00AFBB", "#E7B800", "#FC4E07"),
          order = c("1", "2", "3"),
          ylab = "q24i", xlab = "Classification Level")

ggboxplot(sub_aps2, x = "q7.", y = "q33f", 
          color = "q7.", palette = c("#00AFBB", "#E7B800", "#FC4E07"),
          order = c("1", "2", "3"),
          ylab = "q33f", xlab = "Classification Level")

ggboxplot(sub_aps2, x = "q7.", y = "q38", 
          color = "q7.", palette = c("#00AFBB", "#E7B800", "#FC4E07"),
          order = c("1", "2", "3"),
          ylab = "q38", xlab = "Classification Level")

ggboxplot(sub_aps2, x = "q7.", y = "q44a", 
          color = "q7.", palette = c("#00AFBB", "#E7B800", "#FC4E07"),
          order = c("1", "2", "3"),
          ylab = "q44a", xlab = "Classification Level")
