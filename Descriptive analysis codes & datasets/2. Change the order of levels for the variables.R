############## Order of levels for the variables #########################

# Data structures ----
vtable(data2014clean, data.title = "Summary of APS data 2014", out="browser", lush = TRUE)
vtable(data2020clean, data.title = "Summary of APS data 2020", out="browser", lush = TRUE)

########### change order of levels in factors ------------------------ #######

# Pressure
levels(data2014cleaned$X36a..I.have.unrealistic.time.pressures)
data2014cleaned$X36a..I.have.unrealistic.time.pressures.factor <- factor(data2014cleaned$X36a..I.have.unrealistic.time.pressures,ordered = TRUE,levels = c("Always","Sometimes","Often","Rarely","Never"))
data2014cleaned$X36a..I.have.unrealistic.time.pressures.factor

levels(data2020cleaned$X47a..I.have.unrealistic.time.pressures)
data2020cleaned$X47a..I.have.unrealistic.time.pressures.factor <- factor(data2020cleaned$X47a..I.have.unrealistic.time.pressures,ordered = TRUE, levels = c("Always","Sometimes","Often","Rarely","Never"))
data2020cleaned$X47a..I.have.unrealistic.time.pressures.factor
data2020cleaned$X47a..I.have.unrealistic.time.pressures


# Communication
levels(data2014cleaned$X20a..My.supervisor.provides.me.with.regular.and.constructive.feedback)
data2014cleaned$X20a..My.supervisor.provides.me.with.regular.and.constructive.feedback.factor <- factor(data2014cleaned$X20a..My.supervisor.provides.me.with.regular.and.constructive.feedback,ordered = TRUE,levels = c("Strongly agree","Agree","Neither agree nor disagree","Disagree","Strongly disagree")) 
data2014cleaned$X20a..My.supervisor.provides.me.with.regular.and.constructive.feedback.factor

levels(data2020cleaned$X19a..My.supervisor.communicates.effectively)
data2020cleaned$X19a..My.supervisor.communicates.effectively.factor <- factor(data2020cleaned$X19a..My.supervisor.communicates.effectively,ordered = TRUE,levels = c("Strongly agree","Agree","Neither agree nor disagree","Disagree","Strongly disagree")) 
data2020cleaned$X19a..My.supervisor.communicates.effectively.factor


# Remuneration
levels(data2014cleaned$X18f..I.am.fairly.remunerated..e.g..salary..superannuation..for.the.work.that.I.do)
data2014cleaned$X18f..I.am.fairly.remunerated..e.g..salary..superannuation..for.the.work.that.I.do <- factor(data2014cleaned$X18f..I.am.fairly.remunerated..e.g..salary..superannuation..for.the.work.that.I.do,ordered = TRUE,levels = c("Strongly agree","Agree","Neither agree nor disagree","Disagree","Strongly disagree")) 
data2014cleaned$X18f..I.am.fairly.remunerated..e.g..salary..superannuation..for.the.work.that.I.do

levels(data2020cleaned$X17d..I.am.fairly.remunerated..e.g..salary..superannuation..for.the.work.that.I.do)
data2020cleaned$X17d..I.am.fairly.remunerated..e.g..salary..superannuation..for.the.work.that.I.do.factor <- factor(data2020cleaned$X17d..I.am.fairly.remunerated..e.g..salary..superannuation..for.the.work.that.I.do,ordered = TRUE,levels = c("Strongly agree","Agree","Neither agree nor disagree","Disagree","Strongly disagree")) 
data2020cleaned$X17d..I.am.fairly.remunerated..e.g..salary..superannuation..for.the.work.that.I.do.factor


# Non-monetary
levels(data2014cleaned$X18g..I.am.satisfied.with.my.non.monetary.employment.conditions..e.g..leave..flexible.work.arrangements..other.benefits.)
data2014cleaned$X18g..I.am.satisfied.with.my.non.monetary.employment.conditions..e.g..leave..flexible.work.arrangements..other.benefits. <- factor(data2014cleaned$X18g..I.am.satisfied.with.my.non.monetary.employment.conditions..e.g..leave..flexible.work.arrangements..other.benefits.,ordered = TRUE,levels = c("Strongly agree","Agree","Neither agree nor disagree","Disagree","Strongly disagree")) 
data2014cleaned$X18g..I.am.satisfied.with.my.non.monetary.employment.conditions..e.g..leave..flexible.work.arrangements..other.benefits.

levels(data2020cleaned$X17e..I.am.satisfied.with.my.non.monetary.employment.conditions..e.g..leave..flexible.work.arrangements..other.benefits.)
data2020cleaned$X17e..I.am.satisfied.with.my.non.monetary.employment.conditions..e.g..leave..flexible.work.arrangements..other.benefits..factor <- factor(data2020cleaned$X17e..I.am.satisfied.with.my.non.monetary.employment.conditions..e.g..leave..flexible.work.arrangements..other.benefits.,ordered = TRUE,levels = c("Strongly agree","Agree","Neither agree nor disagree","Disagree","Strongly disagree")) 
data2020cleaned$X17e..I.am.satisfied.with.my.non.monetary.employment.conditions..e.g..leave..flexible.work.arrangements..other.benefits..factor