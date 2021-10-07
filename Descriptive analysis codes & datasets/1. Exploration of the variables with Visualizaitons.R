# Library
library(ggplot2)        # for generating visualizations
library(dplyr)
library(forcats)
library(grid)
library(gridExtra)
library(gmodels)
library(reshape)
library(plyr)
library(vtable)

# Read raw files
data2014 <- read.csv(file = 'C:/Users/yolee/Desktop/LANGARA/2021-2/DANA4810/Team/dataset2014.csv', header=TRUE, na.strings=c(" "), stringsAsFactors=T)
data2020 <- read.csv(file = 'C:/Users/yolee/Desktop/LANGARA/2021-2/DANA4810/Team/dataset2020.csv', header=TRUE, na.strings=c(" "), stringsAsFactors=T)

## Datasets with only the selected variables for the analysis ##

data2014clean <- read.csv(file = 'C:/Users/yolee/Desktop/LANGARA/2021-2/DANA4810/Team/dataset2014clean.csv', header=TRUE, na.strings=c(" "), stringsAsFactors=T)
data2020clean <- read.csv(file = 'C:/Users/yolee/Desktop/LANGARA/2021-2/DANA4810/Team/dataset2020clean.csv', header=TRUE, na.strings=c(" "), stringsAsFactors=T)

# summary 
summary(data2014)
str(data2014)
summary(data2014clean)
str(data2014clean)
summary(data2020clean)
str(data2020clean)

####################### Exploration of the selected variables (Single variable analysis) #####################

### 1. Agency size

# Comparison between 2014 and 2020
# 1. Explore variable for a test run
levels(data2014$AS)
nlevels(data2020$AS)

# 1. Check frequency distribution for the AS (Agency size)
#2014
tableAS_2014 <- table(data2014$AS)
freq_table_AS_2014 <- transform(table(data2014$AS))
freq_dist_table_AS_2014 <- transform(freq_table_AS, Rel_Freq=prop.table(Freq),Cum_Freq=cumsum(Freq))
#2020
tableAS_2020 <- table(data2020$AS)
freq_table_AS_2020 <- transform(tableAS_2020)
freq_dist_table_AS_2020 <- transform(freq_table_AS_2020, Rel_Freq=prop.table(Freq), Cum_Freq=cumsum(Freq))

# 2. Visualization

## Compare Agency size for both 2014 and 2020 dataset ##
# Proportion plot
AS2014 <- barplot(prop.table(tableAS_2014))
AS2020 <- barplot(prop.table(tableAS_2020))
# ggplot bar chart for AS
gg_ASbar2014 <- ggplot(data2014, aes(x = AS)) + geom_bar() + geom_text(stat = 'count', aes(label=..count..), vjust=-1) + ggtitle("Size of respondent's agency 2014")
gg_ASbar2020 <- ggplot(data2020, aes(x = AS)) + geom_bar() + geom_text(stat = 'count', aes(label=..count..), vjust=-1) + ggtitle("Size of respondent's agency 2020")
# Grid for both 2014 and 2020
grid.arrange(gg_ASbar2014, gg_ASbar2020, nrow = 1)

## 3. Analyzing AS
# a. Around 87% of the respondents' agency size are Large for both 2014 and 2020
# b. Less than 5% of the respondents' agency size are Small for both 2014 and 2020

### 2. Gender

gg_ASbar2014 <- ggplot(data2014, aes(x = q36a)) + geom_bar() + geom_text(stat = 'count', aes(label=..count..), vjust=-1) + ggtitle("Pressure 2014")
gg_ASbar2014

# Comparison between 2014 and 2020
# 1. Explore variable for a test run
levels(data2014$q1)
levels(data2020$q1)
nlevels(data2020$q1)

# 1. Check frequency distribution for the Gender
#2014
tableGND_2014 <- table(data2014$q1)
freq_table_GND_2014 <- transform(table(data2014$q1))
freq_dist_table_GND_2014 <- transform(freq_table_GND_2014, Rel_Freq=prop.table(Freq),Cum_Freq=cumsum(Freq))
#2020
tableGND_2020 <- table(data2020$q1)
freq_table_GND_2020 <- transform(table(data2020$q1))
freq_dist_table_GND_2020 <- transform(freq_table_GND_2020, Rel_Freq=prop.table(Freq),Cum_Freq=cumsum(Freq))

# 2. Visualization

## Compare Gender for both 2014 and 2020 dataset ##
# Proportion plot
GND2014 <- barplot(prop.table(tableGND_2014))
GND2020 <- barplot(prop.table(tableGND_2020))
# Count bar plot
GNDbar2014 <- barplot(summary(data2014$q1))
GNDbar2020 <- barplot(summary(data2020$q1))
# ggplot bar chart for AS
gg_GNDbar2014 <- ggplot(data2014, aes(x = q1)) + geom_bar() + geom_text(stat = 'count', aes(label=..count..), vjust=-1) + ggtitle("Respondent's gender 2014")
gg_GNDbar2020 <- ggplot(data2020, aes(x = q1)) + geom_bar() + geom_text(stat = 'count', aes(label=..count..), vjust=-1) + ggtitle("Respondent's gender 2020")
# Grid for both 2014 and 2020
grid_GND <- grid.arrange(gg_GNDbar2014, gg_GNDbar2020, nrow = 1)

## 3. Analyzing Gender
# a. More than 50% of the respondents' gender are female for both 2014 and 2020

#### Agency size and Gender #### ------------------ AS AND X1 -------------------

# 1. Compare frequencies and proportions for AS and gender

# aggregate AS and q1 values
count_AS_GND_2014 <- count(data2014, AS, q1)
count_AS_GND_2020 <- count(data2020, AS, q1)

# 1a. Check frequency distribution
#2014
table_AS_GND_2014 <- table(data2014$AS, data2014$q1)
freq_table_AS_GND_2014 <- transform(table(data2014$AS, data2014$q1))
freq_dist_table_AS_GND_2014 <- transform(freq_table_AS_GND_2014, Rel_Freq=prop.table(Freq),Cum_Freq=cumsum(Freq))
#2020
table_AS_GND_2020 <- table(data2020$AS, data2020$q1)
freq_table_AS_GND_2020 <- transform(table(data2020$AS, data2020$q1))
freq_dist_table_AS_GND_2020 <- transform(freq_table_AS_GND_2020, Rel_Freq=prop.table(Freq),Cum_Freq=cumsum(Freq))

# 2. Visualization

names(data2014clean)
ggplot(data = data2014clean, aes(x = X36a..I.have.unrealistic.time.pressures, y = 1, fill = Stage.of.Change)) + geom_bar() + coord_flip()


# Bar charts for AS and q1
bar_AS_GND_2014 <- ggplot() + geom_bar(position='dodge', data = data2014, aes(x = AS, fill = q1)) + ylim(0,60000)
bar_AS_GND_2020 <- ggplot() + geom_bar(position='dodge', data = data2020, aes(x = AS, fill = q1)) + ylim(0,60000)
grid_bar_AS_GND <- grid.arrange(bar_AS_GND_2014, bar_AS_GND_2020, nrow = 1)

# Stacked bar plot for AS and q1
sbar_AS_GND_2014 <- ggplot(count_AS_GND_2014) + geom_bar(aes(x= AS, y = n, fill = q1), stat = "identity") + scale_fill_discrete(name = "Gender") + ggtitle("Respondent's agency and gender 2014") + theme(axis.text.x=element_text(angle = 90, hjust = 0))
sbar_AS_GND_2020 <- ggplot(count_AS_GND_2020) + geom_bar(aes(x= AS, y = n, fill = q1), stat = "identity") + scale_fill_discrete(name = "Gender") + ggtitle("Respondent's agency and gender 2020") + theme(axis.text.x=element_text(angle = 90, hjust = 0))

# Stacked bar plot for AS and gender for 2014 and 2020
grid.arrange(sbar_AS_GND_2014, sbar_AS_GND_2020, nrow = 1)

# Change legend order for matching the chart below
count_AS_GND_2020$q1 <- factor(count_AS_GND_2020$q1, levels = c("Female", "Male", "X (Indeterminate/Intersex/Unspecified)", "Prefer not to say"))
count_AS_GND_2020 <- count(data2020, AS, q1)
sbar_AS_GND_2020 <- ggplot(count_AS_GND_2020) + geom_bar(aes(x= AS, y = n, fill = q1), stat = "identity") + scale_fill_discrete(name = "Gender") + ggtitle("Respondent's agency and gender 2020") + theme(axis.text.x=element_text(angle = 90, hjust = 0))
sbar_AS_GND_2020

# Stacked bar plot for AS and gender for 2014 and 2020
grid_sbar_AS_GND <- grid.arrange(sbar_AS_GND_2014, sbar_AS_q1_2020, nrow = 1)

# Pie chart
pie_AS_GND_2014 <- ggplot(count_AS_GND_2014) + geom_col(aes(x = 1, y =n, fill = AS), position = "fill") + coord_polar(theta = "y") + scale_fill_discrete(name = "Agency size") + ggtitle("Size of agency and gender 2014")
pie_AS_GND_2014 <- pie_AS_GND_2014 + facet_wrap(~ q1)
pie_AS_GND_2020 <- ggplot(count_AS_GND_2020) + geom_col(aes(x = 1, y =n, fill = AS), position = "fill") + coord_polar(theta = "y") + scale_fill_discrete(name = "Agency size") + ggtitle("Size of agency and gender 2020")
pie_AS_GND_2020 <- pie_AS_GND_2020 + facet_wrap(~ q1)

# Mosaicplot
# not suitable for this two variables..
Mos_AS_q1_2014 <- mosaicplot(~AS+q1, data = data2014, color = c("green", "blue", "red"), xlab = "Agency Size", ylab = "Gender", main ='Size of agency and gender 2014')
Mos_AS_q1_2020 <- mosaicplot(~AS+q1, data = data2020, color = c("green", "blue", "red"), xlab = "Agency Size", ylab = "Gender", main ='Size of agency and gender 2020')


#### Gender and Pressure ### --------------------------------- X1 AND X36a, X47a -----------------

Count_GND_PRS_2014 <- count(data2014clean, data2014clean$X1..What.is.your.gender., data2014clean$X36a..I.have.unrealistic.time.pressures)
Count_GND_PRS_2020 <- count(data2020clean, data2020clean$X1..What.is.your.gender., data2020clean$X47a..I.have.unrealistic.time.pressures)

# 1a. Check frequency distribution
#2014
table_GND_PRS_2014 <- table(data2014clean$X1..What.is.your.gender., data2014clean$X36a..I.have.unrealistic.time.pressures)
freq_table_GND_PRS_2014 <- transform(table_GND_PRS_2014)
freq_dist_table_GND_PRS_2014 <- transform(freq_table_GND_PRS_2014, Rel_Freq=prop.table(Freq),Cum_Freq=cumsum(Freq))
#2020
table_GND_PRS_2020 <- table(data2020clean$X1..What.is.your.gender., data2020clean$X47a..I.have.unrealistic.time.pressures)
freq_table_GND_PRS_2020 <- transform(table_GND_PRS_2020)
freq_dist_table_GND_PRS_2020 <- transform(freq_table_GND_PRS_2020, Rel_Freq=prop.table(Freq),Cum_Freq=cumsum(Freq))
#Cross table
Crosstab_GND_PRS_2014 <-CrossTable(data2014clean$X1..What.is.your.gender., data2014clean$X36a..I.have.unrealistic.time.pressures)
Crosstab_GND_PRS_2020 <-CrossTable(data2020clean$X1..What.is.your.gender., data2020clean$X47a..I.have.unrealistic.time.pressures)

# 2. Visualization

# bar charts and stacked bar plots of GND and PRS
bar_GND_PRS_2014 <- ggplot() + geom_bar(position='dodge', data = data2014clean, aes(x = X1..What.is.your.gender., fill = X36a..I.have.unrealistic.time.pressures))
bar_GND_PRS_2020 <- ggplot() + geom_bar(position='dodge', data = data2020clean, aes(x = X1..What.is.your.gender., fill = X47a..I.have.unrealistic.time.pressures))
grid_bar_GND_PRS <- grid.arrange(bar_GND_PRS_2014, bar_GND_PRS_2020, nrow = 1)

sbar_GND_PRS_2014 <- ggplot(data2014clean) + geom_bar(aes(x = X1..What.is.your.gender., fill = X36a..I.have.unrealistic.time.pressures)) + ggtitle("Gender and Pressure 2014") + theme(axis.text.x=element_text(angle = 90, hjust = 0))
sbar_GND_PRS_2020 <- ggplot(data2020clean) + geom_bar(aes(x = X1..What.is.your.gender., fill = X47a..I.have.unrealistic.time.pressures)) + ggtitle("Gender and Pressure 2020") + theme(axis.text.x=element_text(angle = 90, hjust = 0))
grid_sbar_GND_PRS <- grid.arrange(sbar_GND_PRS_2014, sbar_GND_PRS_2020, nrow = 1)

sbar_PRS_2014 <- ggplot(data2014clean) + geom_bar(aes(x = 1, fill = X36a..I.have.unrealistic.time.pressures)) + ggtitle("Gender and Pressure 2014") + theme(axis.text.x=element_text(angle = 90, hjust = 0)) + coord_flip()
sbar_PRS_2020 <- ggplot(data2020clean) + geom_bar(aes(x = 1, fill = X47a..I.have.unrealistic.time.pressures)) + ggtitle("Gender and Pressure 2014") + theme(axis.text.x=element_text(angle = 90, hjust = 0)) + coord_flip()
grid_sbar_PRS <- grid.arrange(sbar_PRS_2014, sbar_PRS_2020, nrow = 2)

sbar_REN_2014 <- ggplot(data2014clean) + geom_bar(aes(x = 1, fill = X18f..I.am.fairly.remunerated..e.g..salary..superannuation..for.the.work.that.I.do)) + ggtitle("Renumeration 2014") + theme(axis.text.x=element_text(angle = 90, hjust = 0)) + coord_flip()
sbar_REN_2020 <- ggplot(data2020clean) + geom_bar(aes(x = 1, fill = X17d..I.am.fairly.remunerated..e.g..salary..superannuation..for.the.work.that.I.do)) + ggtitle("Renumeration 2020") + theme(axis.text.x=element_text(angle = 90, hjust = 0)) + coord_flip()
grid_sbar_REN <- grid.arrange(sbar_REN_2014, sbar_REN_2020, nrow = 2)

# Mosaicplot
Mos_GND_PRS_2014 <- mosaicplot(~X1..What.is.your.gender.+X36a..I.have.unrealistic.time.pressures, color = c("green", "blue", "red", "yellow", "white"), data = data2014clean, xlab = "Gender", ylab = "Pressure", main ='Gender and Pressure 2014') 
Mos_GND_PRS_2020 <- mosaicplot(~X1..What.is.your.gender.+X47a..I.have.unrealistic.time.pressures, color = c("green", "blue", "red", "yellow", "white"), data = data2020clean, xlab = "Gender", ylab = "Pressure", main ='Gender and Pressure 2020') 

# Heatclean for GND, PRS and AS
heat_AS_GND_PRS_2014 <- ggplot(data2014clean, aes(x=X1..What.is.your.gender., y=AS, fill=X36a..I.have.unrealistic.time.pressures)) + geom_tile() + ggtitle("Heatclean of Agency size, gender and pressure 2014") + theme(axis.text.x=element_text(angle = 90, hjust = 0))
heat_AS_GND_PRS_2020 <- ggplot(data2020clean, aes(x=X1..What.is.your.gender., y=AS, fill=X47a..I.have.unrealistic.time.pressures)) + geom_tile() + ggtitle("Heatclean of Agency size, gender and pressure 2020") + theme(axis.text.x=element_text(angle = 90, hjust = 0))
grid_AS_GND_PRS <- grid.arrange(heat_AS_GND_PRS_2014, heat_AS_GND_PRS_2020, nrow = 2)

#### Agency size and Pressure ### ------------------ AS and X36a, X47a

Count_AS_PRS_2014 <- count(data2014clean, data2014clean$AS, data2014clean$X36a..I.have.unrealistic.time.pressures)
Count_AS_PRS_2020 <- count(data2020clean, data2020clean$AS, data2020clean$X47a..I.have.unrealistic.time.pressures)

# 1a. Check frequency distribution
#2014
table_AS_PRS_2014 <- table(data2014clean$AS, data2014clean$X36a..I.have.unrealistic.time.pressures)
freq_table_AS_PRS_2014 <- transform(table_AS_PRS_2014)
freq_dist_table_AS_PRS_2014 <- transform(freq_table_AS_PRS_2014, Rel_Freq=prop.table(Freq),Cum_Freq=cumsum(Freq))
#2020
table_AS_PRS_2020 <- table(data2020clean$AS, data2020clean$X47a..I.have.unrealistic.time.pressures)
freq_table_AS_PRS_2020 <- transform(table_AS_PRS_2020)
freq_dist_table_AS_PRS_2020 <- transform(freq_table_AS_PRS_2020, Rel_Freq=prop.table(Freq),Cum_Freq=cumsum(Freq))
#Cross table
Crosstab_AS_PRS_2014 <-CrossTable(data2014clean$AS, data2014clean$X36a..I.have.unrealistic.time.pressures)
Crosstab_AS_PRS_2020 <-CrossTable(data2020clean$AS, data2020clean$X47a..I.have.unrealistic.time.pressures)

# 2. Visualization

# bar charts and stacked bar plots of AS and PRS
bar_AS_PRS_2014 <- ggplot() + geom_bar(position='dodge', data = data2014clean, aes(x = AS, fill = X36a..I.have.unrealistic.time.pressures))
bar_AS_PRS_2020 <- ggplot() + geom_bar(position='dodge', data = data2020clean, aes(x = AS, fill = X47a..I.have.unrealistic.time.pressures))
grid_bar_AS_PRS <- grid.arrange(bar_AS_PRS_2014, bar_AS_PRS_2020, nrow = 1)

sbar_AS_PRS_2014 <- ggplot(data2014clean) + geom_bar(aes(x = AS, fill = X36a..I.have.unrealistic.time.pressures)) + ggtitle("Agency size and Pressure 2014") + theme(axis.text.x=element_text(angle = 90, hjust = 0))
sbar_AS_PRS_2020 <- ggplot(data2020clean) + geom_bar(aes(x = AS, fill = X47a..I.have.unrealistic.time.pressures)) + ggtitle("Agency size and Pressure 2020") + theme(axis.text.x=element_text(angle = 90, hjust = 0))
grid_sbar_AS_PRS <- grid.arrange(sbar_AS_PRS_2014, sbar_AS_PRS_2020, nrow = 1)

# Mosaicplot
Mos_AS_PRS_2014 <- mosaicplot(~AS+X36a..I.have.unrealistic.time.pressures, color = c("green", "blue", "red", "yellow", "white"), data = data2014clean, xlab = "Agency size", ylab = "Pressure", main ='Agency size and Pressure 2014') 
Mos_AS_PRS_2020 <- mosaicplot(~AS+X47a..I.have.unrealistic.time.pressures, color = c("green", "blue", "red", "yellow", "white"), data = data2020clean, xlab = "Agency size", ylab = "Pressure", main ='Agency size and Pressure 2020') 

# Heatclean for GND, PRS and AS
heat_AS_GND_PRS_2014 <- ggplot(data2014clean, aes(x=X1..What.is.your.gender., y=AS, fill=X36a..I.have.unrealistic.time.pressures)) + geom_tile() + ggtitle("Heatclean of Agency size, gender and pressure 2014") + theme(axis.text.x=element_text(angle = 90, hjust = 0))
heat_AS_GND_PRS_2020 <- ggplot(data2020clean, aes(x=X1..What.is.your.gender., y=AS, fill=X47a..I.have.unrealistic.time.pressures)) + geom_tile() + ggtitle("Heatclean of Agency size, gender and pressure 2020") + theme(axis.text.x=element_text(angle = 90, hjust = 0))
grid_AS_GND_PRS <- grid.arrange(heat_AS_GND_PRS_2014, heat_AS_GND_PRS_2020, nrow = 1)

#### Age group and Pressure ### ------------------ X2 and X36a, X47a

Count_AGE_PRS_2014 <- count(data2014clean, data2014clean$X2..How.old.were.you.at.your.last.birthday., data2014clean$X36a..I.have.unrealistic.time.pressures)
Count_AGE_PRS_2020 <- count(data2020clean, data2020clean$X2..How.old.were.you.at.your.last.birthday., data2020clean$X47a..I.have.unrealistic.time.pressures)

# 1a. Check frequency distribution
#2014
table_AGE_PRS_2014 <- table(data2014clean$X2..How.old.were.you.at.your.last.birthday., data2014clean$X36a..I.have.unrealistic.time.pressures)
freq_table_AGE_PRS_2014 <- transform(table_AGE_PRS_2014)
freq_dist_table_AGE_PRS_2014 <- transform(freq_table_AGE_PRS_2014, Rel_Freq=prop.table(Freq),Cum_Freq=cumsum(Freq))
#2020
table_AGE_PRS_2020 <- table(data2020clean$X2..How.old.were.you.at.your.last.birthday., data2020clean$X47a..I.have.unrealistic.time.pressures)
freq_table_AGE_PRS_2020 <- transform(table_AGE_PRS_2020)
freq_dist_table_AGE_PRS_2020 <- transform(freq_table_AGE_PRS_2020, Rel_Freq=prop.table(Freq),Cum_Freq=cumsum(Freq))
#Cross table
Crosstab_AGE_PRS_2014 <-CrossTable(data2014clean$X2..How.old.were.you.at.your.last.birthday., data2014clean$X36a..I.have.unrealistic.time.pressures)
Crosstab_AGE_PRS_2020 <-CrossTable(data2020clean$X2..How.old.were.you.at.your.last.birthday., data2020clean$X47a..I.have.unrealistic.time.pressures)

# 2. Visualization

# bar plots and stacked bar plots of AGE and PRS
bar_AGE_PRS_2014 <- ggplot() + geom_bar(position='dodge', data = data2014clean, aes(x = X2..How.old.were.you.at.your.last.birthday., fill = X36a..I.have.unrealistic.time.pressures))
bar_AGE_PRS_2020 <- ggplot() + geom_bar(position='dodge', data = data2020clean, aes(x = X2..How.old.were.you.at.your.last.birthday., fill = X47a..I.have.unrealistic.time.pressures))
grid_bar_AGE_PRS <- grid.arrange(bar_AGE_PRS_2014, bar_AGE_PRS_2020, nrow = 1)

sbar_AGE_PRS_2014 <- ggplot(data2014clean) + geom_bar(aes(x = X2..How.old.were.you.at.your.last.birthday., fill = X36a..I.have.unrealistic.time.pressures)) + ggtitle("Age group and Pressure 2014") + theme(axis.text.x=element_text(angle = 90, hjust = 0))
sbar_AGE_PRS_2020 <- ggplot(data2020clean) + geom_bar(aes(x = X2..How.old.were.you.at.your.last.birthday., fill = X47a..I.have.unrealistic.time.pressures)) + ggtitle("Age group and Pressure 2020") + theme(axis.text.x=element_text(angle = 90, hjust = 0))
grid_sbar_AGE_PRS <- grid.arrange(sbar_AGE_PRS_2014, sbar_AGE_PRS_2020, nrow = 1)

levels(data2014clean$X2..How.old.were.you.at.your.last.birthday.)

# Mosaicplot
Mos_AGE_PRS_2014 <- mosaicplot(~X2..How.old.were.you.at.your.last.birthday.+X36a..I.have.unrealistic.time.pressures, color = c("green", "blue", "red", "yellow", "white"), data = data2014clean, xlab = "Age group", ylab = "Pressure", main ='Age group and Pressure 2014') 
Mos_AGE_PRS_2020 <- mosaicplot(~X2..How.old.were.you.at.your.last.birthday.+X47a..I.have.unrealistic.time.pressures, color = c("green", "blue", "red", "yellow", "white"), data = data2020clean, xlab = "Age group", ylab = "Pressure", main ='Age group and Pressure 2020') 

# Heatclean for AGE, PRS and GND
heat_AGE_GND_PRS_2014 <- ggplot(data2014clean, aes(x=X1..What.is.your.gender., y=X2..How.old.were.you.at.your.last.birthday., fill=X36a..I.have.unrealistic.time.pressures)) + geom_tile() + ggtitle("Heatclean of Age group, gender and pressure 2014") + theme(axis.text.x=element_text(angle = 90, hjust = 0))
heat_AGE_GND_PRS_2020 <- ggplot(data2020clean, aes(x=X1..What.is.your.gender., y=X2..How.old.were.you.at.your.last.birthday., fill=X47a..I.have.unrealistic.time.pressures)) + geom_tile() + ggtitle("Heatclean of Age group, gender and pressure 2020") + theme(axis.text.x=element_text(angle = 90, hjust = 0))
grid_AGE_GND_PRS <- grid.arrange(heat_AGE_GND_PRS_2014, heat_AGE_GND_PRS_2020, nrow = 1)

#### Agency size and Communication ### ------------------ AS and 20e, 19a

Count_AS_COM_2014 <- count(data2014clean, data2014clean$AS, data2014clean$X20a..My.supervisor.provides.me.with.regular.and.constructive.feedback)
Count_AS_COM_2020 <- count(data2020clean, data2020clean$AS, data2020clean$X19a..My.supervisor.communicates.effectively)


# 1a. Check frequency distribution
#2014
table_AS_COM_2014 <- table(data2014clean$AS, data2014clean$X20a..My.supervisor.provides.me.with.regular.and.constructive.feedback)
freq_table_AS_COM_2014 <- transform(table_AS_COM_2014)
freq_dist_table_AS_COM_2014 <- transform(freq_table_AS_COM_2014, Rel_Freq=prop.table(Freq),Cum_Freq=cumsum(Freq))
#2020
table_AS_COM_2020 <- table(data2020clean$AS, data2020clean$X19a..My.supervisor.communicates.effectively)
freq_table_AS_COM_2020 <- transform(table_AS_COM_2020)
freq_dist_table_AS_COM_2020 <- transform(freq_table_AS_COM_2020, Rel_Freq=prop.table(Freq),Cum_Freq=cumsum(Freq))
#Cross table
Crosstab_AS_COM_2014 <-CrossTable(data2014clean$AS, data2014clean$X20a..My.supervisor.provides.me.with.regular.and.constructive.feedback)
Crosstab_AS_COM_2020 <-CrossTable(data2020clean$AS, data2020clean$X19a..My.supervisor.communicates.effectively)

# 2. Visualization

# bar plots and stacked bar plots of AS and PRS
bar_AS_COM_2014 <- ggplot() + geom_bar(position='dodge', data = data2014clean, aes(x = AS, fill = X20a..My.supervisor.provides.me.with.regular.and.constructive.feedback))
bar_AS_COM_2020 <- ggplot() + geom_bar(position='dodge', data = data2020clean, aes(x = AS, fill = X19a..My.supervisor.communicates.effectively))
grid_bar_AS_COM <- grid.arrange(bar_AS_COM_2014, bar_AS_COM_2020, nrow = 1)

sbar_AS_COM_2014 <- ggplot(data2014clean) + geom_bar(aes(x = AS, fill = X20a..My.supervisor.provides.me.with.regular.and.constructive.feedback)) + ggtitle("Agency size and Communication 2014") + theme(axis.text.x=element_text(angle = 90, hjust = 0))
sbar_AS_COM_2020 <- ggplot(data2020clean) + geom_bar(aes(x = AS, fill = X19a..My.supervisor.communicates.effectively)) + ggtitle("Agency size and Communication 2020") + theme(axis.text.x=element_text(angle = 90, hjust = 0))
grid_sbar_AS_COM <- grid.arrange(bar_AS_COM_2014, bar_AS_COM_2020, nrow = 1)

# Mosaicplot
Mos_AS_COM_2014 <- mosaicplot(~AS+X20a..My.supervisor.provides.me.with.regular.and.constructive.feedback, color = c("green", "blue", "red", "yellow", "white"), data = data2014clean, xlab = "Agency size", ylab = "Communication", main ='Agency size and Communication 2014') 
Mos_AS_COM_2020 <- mosaicplot(~AS+X19a..My.supervisor.communicates.effectively, color = c("green", "blue", "red", "yellow", "white"), data = data2020clean, xlab = "Agency size", ylab = "Communication", main ='Agency size and Communication 2020') 

# Heatclean for AS, COM and PRS
heat_AS_COM_PRS_2014 <- ggplot(data2014clean, aes(x=AS, y=X20a..My.supervisor.provides.me.with.regular.and.constructive.feedback, fill=X36a..I.have.unrealistic.time.pressures)) + geom_tile() + ggtitle("Heatclean of Agency size, communication and pressure 2014") + theme(axis.text.x=element_text(angle = 90, hjust = 0))
heat_AS_COM_PRS_2020 <- ggplot(data2020clean, aes(x=AS, y=X19a..My.supervisor.communicates.effectively, fill=X47a..I.have.unrealistic.time.pressures)) + geom_tile() + ggtitle("Heatclean of Agency size, communication and pressure 2020") + theme(axis.text.x=element_text(angle = 90, hjust = 0))
grid_AS_COM_PRS <- grid.arrange(heat_AS_COM_PRS_2014, heat_AS_COM_PRS_2020, nrow = 1)


#### Gender and Renumeration ### ------------------ X1 and 18f, 17d

Count_GND_REN_2014 <- count(data2014clean, data2014clean$X1..What.is.your.gender., data2014clean$X18f..I.am.fairly.remunerated..e.g..salary..superannuation..for.the.work.that.I.do)
Count_GND_REN_2020 <- count(data2020clean, data2020clean$X1..What.is.your.gender., data2020clean$X17d..I.am.fairly.remunerated..e.g..salary..superannuation..for.the.work.that.I.do)

# 1a. Check frequency distribution
#2014
table_GND_REN_2014 <- table(data2014clean$X1..What.is.your.gender., data2014clean$X18f..I.am.fairly.remunerated..e.g..salary..superannuation..for.the.work.that.I.do)
freq_table_GND_REN_2014 <- transform(table_GND_REN_2014)
freq_dist_table_GND_REN_2014 <- transform(freq_table_GND_REN_2014, Rel_Freq=prop.table(Freq),Cum_Freq=cumsum(Freq))
#2020
table_GND_REN_2020 <- table(data2020clean$X1..What.is.your.gender., data2020clean$X17d..I.am.fairly.remunerated..e.g..salary..superannuation..for.the.work.that.I.do)
freq_table_GND_REN_2020 <- transform(table_GND_REN_2020)
freq_dist_table_GND_REN_2020 <- transform(freq_table_GND_REN_2020, Rel_Freq=prop.table(Freq),Cum_Freq=cumsum(Freq))
#Cross table
Crosstab_GND_REN_2014 <-CrossTable(data2014clean$X1..What.is.your.gender., data2014clean$X18f..I.am.fairly.remunerated..e.g..salary..superannuation..for.the.work.that.I.do)
Crosstab_GND_REN_2020 <-CrossTable(data2020clean$X1..What.is.your.gender., data2020clean$X17d..I.am.fairly.remunerated..e.g..salary..superannuation..for.the.work.that.I.do)

# 2. Visualization

# bar plots and stacked bar plots of GND and REN
bar_GND_REN_2014 <- ggplot() + geom_bar(position='dodge', data = data2014clean, aes(x = X1..What.is.your.gender., fill = X18f..I.am.fairly.remunerated..e.g..salary..superannuation..for.the.work.that.I.do))
bar_GND_REN_2020 <- ggplot() + geom_bar(position='dodge', data = data2020clean, aes(x = X1..What.is.your.gender., fill = X17d..I.am.fairly.remunerated..e.g..salary..superannuation..for.the.work.that.I.do))
grid_bar_GND_REN <- grid.arrange(bar_GND_REN_2014, bar_GND_REN_2020, nrow = 1)

sbar_GND_REN_2014 <- ggplot(data2014clean) + geom_bar(aes(x = X1..What.is.your.gender., fill = X18f..I.am.fairly.remunerated..e.g..salary..superannuation..for.the.work.that.I.do)) + ggtitle("Gender and Renumeration 2014") + theme(axis.text.x=element_text(angle = 90, hjust = 0))
sbar_GND_REN_2020 <- ggplot(data2020clean) + geom_bar(aes(x = X1..What.is.your.gender., fill = X17d..I.am.fairly.remunerated..e.g..salary..superannuation..for.the.work.that.I.do)) + ggtitle("Gender and Renumeration 2020") + theme(axis.text.x=element_text(angle = 90, hjust = 0))
grid_sbar_GND_REN <- grid.arrange(sbar_GND_REN_2014, sbar_GND_REN_2020, nrow = 1)

# Mosaicplot
Mos_GND_REN_2014 <- mosaicplot(~X1..What.is.your.gender.+X18f..I.am.fairly.remunerated..e.g..salary..superannuation..for.the.work.that.I.do, color = c("green", "blue", "red", "yellow", "white"), data = data2014clean, xlab = "Gender", ylab = "Renumeration", main ='Gender and Renumeration 2014') 
Mos_GND_REN_2020 <- mosaicplot(~X1..What.is.your.gender.+X17d..I.am.fairly.remunerated..e.g..salary..superannuation..for.the.work.that.I.do, color = c("green", "blue", "red", "yellow", "white"), data = data2020clean, xlab = "Gender", ylab = "Renumeration", main ='Gender and Renumeration 2020') 

# Heatclean for GND, REN and AS
heat_AS_GND_REN_2014 <- ggplot(data2014clean, aes(x=X1..What.is.your.gender., y=AS, fill=X18f..I.am.fairly.remunerated..e.g..salary..superannuation..for.the.work.that.I.do)) + geom_tile() + ggtitle("Heatclean of Gender, Renumeration and Agency size 2014") + theme(axis.text.x=element_text(angle = 90, hjust = 0))
heat_AS_GND_REN_2020 <- ggplot(data2020clean, aes(x=X1..What.is.your.gender., y=AS, fill=X17d..I.am.fairly.remunerated..e.g..salary..superannuation..for.the.work.that.I.do)) + geom_tile() + ggtitle("Heatclean of Gender, Renumeration and Agency size 2020") + theme(axis.text.x=element_text(angle = 90, hjust = 0))
grid_AS_GND_REN <- grid.arrange(heat_AS_GND_REN_2014, heat_AS_GND_REN_2020, nrow = 2)

#### Gender and non-monetary condition ### ------------------ X1 and 18g, 17e

Count_GND_NMO_2014 <- count(data2014clean, data2014clean$X1..What.is.your.gender., data2014clean$X18g..I.am.satisfied.with.my.non.monetary.employment.conditions..e.g..leave..flexible.work.arrangements..other.benefits.)
Count_GND_NMO_2020 <- count(data2020clean, data2020clean$X1..What.is.your.gender., data2020clean$X17e..I.am.satisfied.with.my.non.monetary.employment.conditions..e.g..leave..flexible.work.arrangements..other.benefits.)

# 1a. Check frequency distribution
#2014
table_GND_NMO_2014 <- table(data2014clean$X1..What.is.your.gender., data2014clean$X18g..I.am.satisfied.with.my.non.monetary.employment.conditions..e.g..leave..flexible.work.arrangements..other.benefits.)
freq_table_GND_NMO_2014 <- transform(table_GND_NMO_2014)
freq_dist_table_GND_NMO_2014 <- transform(freq_table_GND_NMO_2014, Rel_Freq=prop.table(Freq),Cum_Freq=cumsum(Freq))
#2020
table_GND_NMO_2020 <- table(data2020clean$X1..What.is.your.gender., data2020clean$X17e..I.am.satisfied.with.my.non.monetary.employment.conditions..e.g..leave..flexible.work.arrangements..other.benefits.)
freq_table_GND_NMO_2020 <- transform(table_GND_NMO_2020)
freq_dist_table_GND_NMO_2020 <- transform(freq_table_GND_NMO_2020, Rel_Freq=prop.table(Freq),Cum_Freq=cumsum(Freq))
#Cross table
Crosstab_GND_NMO_2014 <-CrossTable(data2014clean$X1..What.is.your.gender., data2014clean$X18g..I.am.satisfied.with.my.non.monetary.employment.conditions..e.g..leave..flexible.work.arrangements..other.benefits.)
Crosstab_GND_NMO_2020 <-CrossTable(data2020clean$X1..What.is.your.gender., data2020clean$X17e..I.am.satisfied.with.my.non.monetary.employment.conditions..e.g..leave..flexible.work.arrangements..other.benefits.)

# 2. Visualization

# bar plots and stacked bar plots of GND and NMO
bar_GND_NMO_2014 <- ggplot() + geom_bar(position='dodge', data = data2014clean, aes(x = X1..What.is.your.gender., fill = X18g..I.am.satisfied.with.my.non.monetary.employment.conditions..e.g..leave..flexible.work.arrangements..other.benefits.))
bar_GND_NMO_2020 <- ggplot() + geom_bar(position='dodge', data = data2020clean, aes(x = X1..What.is.your.gender., fill = X17e..I.am.satisfied.with.my.non.monetary.employment.conditions..e.g..leave..flexible.work.arrangements..other.benefits.))
grid_bar_GND_NMO <- grid.arrange(bar_GND_NMO_2014, bar_GND_NMO_2020, nrow = 2)

sbar_GND_NMO_2014 <- ggplot(data2014clean) + geom_bar(aes(x = X1..What.is.your.gender., fill = X18g..I.am.satisfied.with.my.non.monetary.employment.conditions..e.g..leave..flexible.work.arrangements..other.benefits.)) + ggtitle("Gender and Non-monetary conditions 2014") + theme(axis.text.x=element_text(angle = 90, hjust = 0))
sbar_GND_NMO_2020 <- ggplot(data2020clean) + geom_bar(aes(x = X1..What.is.your.gender., fill = X17e..I.am.satisfied.with.my.non.monetary.employment.conditions..e.g..leave..flexible.work.arrangements..other.benefits.)) + ggtitle("Gender and Non-monetary conditions 2020") + theme(axis.text.x=element_text(angle = 90, hjust = 0))
grid_sbar_GND_NMO <- grid.arrange(sbar_GND_NMO_2014, sbar_GND_NMO_2020, nrow = 2)

# Mosaicplot
Mos_GND_NMO_2014 <- mosaicplot(~X1..What.is.your.gender.+X18g..I.am.satisfied.with.my.non.monetary.employment.conditions..e.g..leave..flexible.work.arrangements..other.benefits., color = c("green", "blue", "red", "yellow", "white"), data = data2014clean, xlab = "Gender", ylab = "Non-monetary conditions", main ='Gender and Non-monetary conditions 2014') 
Mos_GND_NMO_2020 <- mosaicplot(~X1..What.is.your.gender.+X17e..I.am.satisfied.with.my.non.monetary.employment.conditions..e.g..leave..flexible.work.arrangements..other.benefits., color = c("green", "blue", "red", "yellow", "white"), data = data2020clean, xlab = "Gender", ylab = "Non-monetary conditions", main ='Gender and Non-monetary conditions 2020') 

# Heatclean for GND, NMO and AS
heat_AS_GND_NMO_2014 <- ggplot(data2014clean, aes(x=X1..What.is.your.gender., y=AS, fill=X18g..I.am.satisfied.with.my.non.monetary.employment.conditions..e.g..leave..flexible.work.arrangements..other.benefits.)) + geom_tile() + ggtitle("Heatclean of Gender, Non-monetary conditions and Agency size 2014") + theme(axis.text.x=element_text(angle = 90, hjust = 0))
heat_AS_GND_NMO_2020 <- ggplot(data2020clean, aes(x=X1..What.is.your.gender., y=AS, fill=X17e..I.am.satisfied.with.my.non.monetary.employment.conditions..e.g..leave..flexible.work.arrangements..other.benefits.)) + geom_tile() + ggtitle("Heatclean of Gender, Non-monetary conditions and Agency size 2020") + theme(axis.text.x=element_text(angle = 90, hjust = 0))
grid_AS_GND_NMO <- grid.arrange(heat_AS_GND_NMO_2014, heat_AS_GND_NMO_2020, nrow = 2)