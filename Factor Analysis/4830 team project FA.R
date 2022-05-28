# packages and libraries:
library(dplyr)
library(tidyverse)
library(caret)
library(psych)
library(corrplot)
library(ggplot2)
library(car)

## importing the selected and cleaned 2019 survey csv file from python:

# A  random sample of 4521 observation was prepared and imported from python.
data_survey <- read.csv("C:\\E DRIVE\\LANGARA COLLEGE\\DANA 4830-001\\Team project\\2019_for_EFA.csv")
view(data_survey)

# Looking at the data before performing the analysis:
# FACTORABILITY OF THE DATA

describe(data_survey)

# The acceptable range for skewness and kurtosis is -1 to +1. The variables having skewness and kurtosis within this range
# are considered to be univariate normal. From the describe procedure, we can see that all variables have the values under
# the normal range and mostly varies under 1.8 to 1.9. only one variable q27b.My.supervisor.treats.people.with.respect has
# a kurtosis value of 2.57

X <- data_survey[,-c(1)]
Y <- data_survey[,1]

# Not considering the 1st variable as it is kept as response variable

# The Kaiser-Meyer-Olkin (KMO) is used to measure sampling adequacy:

KMO(r = cor(X))


# Overall MSA: measure of smapling adequecy is found to be 0.98 >> than thumb rule of 0.6 
# (data is factorable when the KMO is above the minimum acceptable level of 0.60).
# based on this test, we can probably conduct a factor analysis as the sample is adequate 

# Bartlett's Test of Sphericity:
#To determine if the data multivariate normal:

# An identity matrix is a matrix in which all of the values along the diagonal are 1 and all of the other values are 0

# H0 = The variables correlation matrix is an identity matrix (or) the variables are orthogonal, 
# i.e. not correlated or 0 correlation among variables

# Ha = the variables are not orthogonal, i.e. they are correlated enough to where the 
# correlation matrix diverges significantly from the identity matrix 

cortest.bartlett(X)

# chi square value = 2521581, p value = 0 <0.05. Therefore the data were approximately multivariate normal.
# The result also confirmed that the correlation matrix could not be construed as an identity matrix
# and therefore, was sufficient to test the factor analysis

det(cor(as.matrix(X)))

fafitfree <- fa(data_survey,nfactors = ncol(X), rotate = "none")
n_factors <- length(fafitfree$e.values)
scree     <- data.frame(
  Factor_n =  as.factor(1:n_factors), 
  Eigenvalue = fafitfree$e.values)

windows()
ggplot(scree, aes(x = Factor_n, y = Eigenvalue, group = 1)) + 
  geom_point() + geom_line() +
  xlab("Number of factors") +
  ylab("Initial eigenvalue") +
  labs( title = "Scree Plot", 
        subtitle = "(Based on the unreduced correlation matrix)")

# Parallel Analysis
parallel <- fa.parallel(X)

fafitfree$e.values
# This scree plot shows that the first 3 factors account for most of the total variability in data (given by the eigenvalues).
# The eigenvalues for the first 3 factors are all greater than 1.The remaining factors account for a very small proportion
# of the variability and are likely unimportant

# Factor analysis using fa method:

fa.none <- fa(r=X, 
              nfactors = 3, 
              # covar = FALSE, SMC = TRUE,
              fm='pa', # type of factor analysis we want to use ("pa" is principal axis factoring)
              max.iter=100, # (50 is the default, but we have changed it to 100
              rotate='oblimin') # oblimin rotation
print(fa.none)

pchisq(fa.none$chi, df = fa.none$dof, lower.tail = FALSE)

# Factor analysis using the factanal method

factanal.none <- factanal(X, factors=3, scores = c("regression"), rotation = "promax")
print(factanal.none, digits = 2, cutoff = 0.5, sort=TRUE)

# Updating the column names of the loadings as per the variables loaded
colnames(factanal.none$loadings)<-c("Coaching","Transformation","Consideration")
print(loadings(factanal.none), digits = 2, cutoff = .5, sort = TRUE)

# Graph Factor Loading Matrices

colnames(fa.none$loadings)<-c("Coaching","Transformation","Consideration")
fa.diagram(fa.none)

factanal.none$scores

# creating a data of scores from factor analysis with response variable:

fadata <- cbind(data_survey["q24g"], factanal.none$scores)

#Labeling the data
names(fadata) <- c("q24g.I.suggest.ideas.to.improve.our.way.of.doing.things","Coaching","Transformation","Consideration")
head(fadata)

# exporting the above data to model in python
write.csv(fadata,"C:\\E DRIVE\\LANGARA COLLEGE\\DANA 4830-001\\Team project\\2019_with_factors.csv", row.names = FALSE)






# Extra below: Not included in report
# Splitting the data to train and test set

#Splitting the data 70:30

set.seed(100)
indices= sample(1:nrow(fadata), 0.7*nrow(fadata))
train=fadata[indices,]
test = fadata[-indices,]

# Regression Model using train data

model.fa.score = lm(q24g.I.suggest.ideas.to.improve.our.way.of.doing.things~., train)
summary(model.fa.score)

# Our model equation can be written as: Y = 2.057074 + 0.082465* F1 + 0.073912* F2 + 0.135969* F3

# Check vif
vif(model.fa.score)






