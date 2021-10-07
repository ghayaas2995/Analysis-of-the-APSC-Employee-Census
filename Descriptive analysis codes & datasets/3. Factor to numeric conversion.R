############ change factors to numerics ---------------- #########

data2014clean$AS <- as.numeric(data2014clean$AS) 
data2014clean$X1..What.is.your.gender. <- as.numeric(data2014clean$X1..What.is.your.gender.)
data2014clean$X2..How.old.were.you.at.your.last.birthday. <- as.numeric(factor(data2014clean$X2..How.old.were.you.at.your.last.birthday., c('Under 40 years', '40 to 54 years', '55 years or older')))
data2014clean$X36a..I.have.unrealistic.time.pressures <- as.numeric(factor(data2014clean$X36a..I.have.unrealistic.time.pressures, c("Always","Sometimes","Often","Rarely","Never")))
data2014clean$X20a..My.supervisor.provides.me.with.regular.and.constructive.feedback <- as.numeric(factor(data2014clean$X20a..My.supervisor.provides.me.with.regular.and.constructive.feedback, c('Strongly agree', 'Agree', 'Neither agree nor disagree', 'Disagree', 'Strongly disagree')))
data2014clean$X18f..I.am.fairly.remunerated..e.g..salary..superannuation..for.the.work.that.I.do <- as.numeric(factor(data2014clean$X18f..I.am.fairly.remunerated..e.g..salary..superannuation..for.the.work.that.I.do, c('Strongly agree', 'Agree', 'Neither agree nor disagree', 'Disagree', 'Strongly disagree')))
data2014clean$X18g..I.am.satisfied.with.my.non.monetary.employment.conditions..e.g..leave..flexible.work.arrangements..other.benefits. <- as.numeric(factor(data2014clean$X18g..I.am.satisfied.with.my.non.monetary.employment.conditions..e.g..leave..flexible.work.arrangements..other.benefits., c('Strongly agree', 'Agree', 'Neither agree nor disagree', 'Disagree', 'Strongly disagree')))
data2014clean$X73..During.the.last.12.months..have.you.been.subjected.to.harassment.or.bullying.in.your.workplace. <- as.numeric(factor(data2014clean$X73..During.the.last.12.months..have.you.been.subjected.to.harassment.or.bullying.in.your.workplace., c('Yes','Not sure', 'No')))
data2014clean$X70..In.the.last.12.months..did.you.witness.another.APS.employee.engaging.in.behaviour.that.you.consider.may.be.serious.enough.to.be.viewed.as.corruption. <- as.numeric(factor(data2014clean$X70..In.the.last.12.months..did.you.witness.another.APS.employee.engaging.in.behaviour.that.you.consider.may.be.serious.enough.to.be.viewed.as.corruption., c('Yes','Not sure', 'No', "Would prefer not to answer")))

data2020clean$AS <- as.numeric(data2020clean$AS)
data2020clean$X1..What.is.your.gender. <- as.numeric(factor(data2020clean$X1..What.is.your.gender., c("Female", "Male", "X (Indeterminate/Intersex/Unspecified)", "Prefer not to say")))
data2020clean$X2..How.old.were.you.at.your.last.birthday. <- as.numeric(factor(data2020clean$X2..How.old.were.you.at.your.last.birthday., c('Under 40 years', '40 to 54 years', '55 years or older')))
data2020clean$X47a..I.have.unrealistic.time.pressures <- as.numeric(factor(data2020clean$X47a..I.have.unrealistic.time.pressures, c("Always","Sometimes","Often","Rarely","Never")))
data2020clean$X19a..My.supervisor.communicates.effectively <- as.numeric(factor(data2020clean$X19a..My.supervisor.communicates.effectively, c('Strongly agree', 'Agree', 'Neither agree nor disagree', 'Disagree', 'Strongly disagree')))
data2020clean$X17d..I.am.fairly.remunerated..e.g..salary..superannuation..for.the.work.that.I.do <- as.numeric(factor(data2020clean$X17d..I.am.fairly.remunerated..e.g..salary..superannuation..for.the.work.that.I.do, c('Strongly agree', 'Agree', 'Neither agree nor disagree', 'Disagree', 'Strongly disagree')))
data2020clean$X17e..I.am.satisfied.with.my.non.monetary.employment.conditions..e.g..leave..flexible.work.arrangements..other.benefits. <- as.numeric(factor(data2020clean$X17e..I.am.satisfied.with.my.non.monetary.employment.conditions..e.g..leave..flexible.work.arrangements..other.benefits., c('Strongly agree', 'Agree', 'Neither agree nor disagree', 'Disagree', 'Strongly disagree')))
data2020clean$X58..During.the.last.12.months.and.in.the.course.of.your.employment..have.you.experienced.discrimination.on.the.basis.of.your.background.or.a.personal.characteristic. <- as.numeric(factor(data2020clean$X58..During.the.last.12.months.and.in.the.course.of.your.employment..have.you.experienced.discrimination.on.the.basis.of.your.background.or.a.personal.characteristic., c('Yes', 'No'))) 
data2020clean$X61..During.the.last.12.months..have.you.been.subjected.to.harassment.or.bullying.in.your.current.workplace. <- as.numeric(factor(data2020clean$X61..During.the.last.12.months..have.you.been.subjected.to.harassment.or.bullying.in.your.current.workplace., c('Yes','Not sure', 'No')))
data2020clean$X63..Excluding.behaviour.reported.to.you.as.part.of.your.duties..in.the.last.12.months.have.you.witnessed.another.APS.employee.in.your.agency.engaging.in.behaviour.that.you.consider.may.be.serious.enough.to.be.viewed.as.corruption. <- as.numeric(factor(data2020clean$X63..Excluding.behaviour.reported.to.you.as.part.of.your.duties..in.the.last.12.months.have.you.witnessed.another.APS.employee.in.your.agency.engaging.in.behaviour.that.you.consider.may.be.serious.enough.to.be.viewed.as.corruption., c('Yes','Not sure', 'No', "Would prefer not to answer")))

######### Two groups analysis ###########

# Median and Variances of the two groups (2014 and 2020 datasets)

vtable(data2014clean, data.title = "Variables in APS data 2014", lush = TRUE)
vtable(data2020clean, data.title = "Variables in APS data 2020", lush = TRUE)
vtable(data2014clean, data.title = "Summary of APS data 2014", out="browser", lush = TRUE)
vtable(data2020clean, data.title = "Summary of APS data 2020", out="browser", lush = TRUE)

st(data2014clean, summ = c('notNA(x)', 'median(x)', 'sd(x)^2'), summ.names = c('N', 'Median', 'Variance'))
st(data2020clean, summ = c('notNA(x)', 'median(x)', 'sd(x)^2'), summ.names = c('N', 'Median', 'Variance'))

st(data2014clean, summ = c('median(x)', 'mad(x, constant=1)'), summ.names = c('Median', 'Variance'))
st(data2020clean, summ = c('median(x)', 'mad(x, constant=1)'), summ.names = c('Median', 'Variance'))

# Comparing horizontal stacked bar charts for "Pressure"

sbar_PRS_2014 <- ggplot(data2014clean) + geom_bar(aes(x = 1, fill = X36a..I.have.unrealistic.time.pressures)) + ggtitle("Gender and Pressure 2014") + theme(axis.text.x=element_text(angle = 90, hjust = 0)) + coord_flip()
sbar_PRS_2020 <- ggplot(data2020clean) + geom_bar(aes(x = 1, fill = X47a..I.have.unrealistic.time.pressures)) + ggtitle("Gender and Pressure 2014") + theme(axis.text.x=element_text(angle = 90, hjust = 0)) + coord_flip()
grid_sbar_PRS <- grid.arrange(sbar_PRS_2014, sbar_PRS_2020, nrow = 2)

# Comparing horizontal stacked bar charts for "Remuneration"

sbar_REN_2014 <- ggplot(data2014clean) + geom_bar(aes(x = 1, fill = X18f..I.am.fairly.remunerated..e.g..salary..superannuation..for.the.work.that.I.do)) + ggtitle("remuneration 2014") + theme(axis.text.x=element_text(angle = 90, hjust = 0)) + coord_flip()
sbar_REN_2020 <- ggplot(data2020clean) + geom_bar(aes(x = 1, fill = X17d..I.am.fairly.remunerated..e.g..salary..superannuation..for.the.work.that.I.do)) + ggtitle("remuneration 2020") + theme(axis.text.x=element_text(angle = 90, hjust = 0)) + coord_flip()
grid_sbar_REN <- grid.arrange(sbar_REN_2014, sbar_REN_2020, nrow = 2)
