
# here, we use the sample where the gender is equally distributed

data_2020_sample<- read.csv('C:\\E DRIVE\\LANGARA COLLEGE\\DANA 4801 W01\\Team project\\data_2020_sample.csv')
data_2014_sample<- read.csv('C:\\E DRIVE\\LANGARA COLLEGE\\DANA 4801 W01\\Team project\\data_2014_sample.csv')

###### WORKING CONDITIONS ########

# Gender and remuneration(17c for 2020)

T4 <- wilcox.test(data_2020_sample$X17d..I.am.fairly.remunerated..e.g..salary..superannuation..for.the.work.that.I.do
                  ~ data_2020_sample$X1..What.is.your.gender.
                  , alternative = 'two.sided')
T4

# to determine if males are more remunerated than females: in 2020

tapply(data_2020_sample$X17d..I.am.fairly.remunerated..e.g..salary..superannuation..for.the.work.that.I.do,
       data_2020_sample$X1..What.is.your.gender., median, na.rm=T)


T4a <- wilcox.test(data_2020_sample$X17d..I.am.fairly.remunerated..e.g..salary..superannuation..for.the.work.that.I.do
                   ~ data_2020_sample$X1..What.is.your.gender.
                   , exact=F, alternative = 'greater')
T4a

# Gender and remuneration(18f for 2014)

T4_14 <- wilcox.test(data_2014_sample$X18f..I.am.fairly.remunerated..e.g..salary..superannuation..for.the.work.that.I.do
                     ~ data_2014_sample$X1..What.is.your.gender.
                     , alternative = 'two.sided')
T4_14


# to determine if males are more remunerated than females in 2014:

tapply(data_2014_sample$X18f..I.am.fairly.remunerated..e.g..salary..superannuation..for.the.work.that.I.do,
       data_2014_sample$X1..What.is.your.gender., median, na.rm=T)


T4_14a <- wilcox.test(data_2014_sample$X18f..I.am.fairly.remunerated..e.g..salary..superannuation..for.the.work.that.I.do
                      ~ data_2014_sample$X1..What.is.your.gender.
                      , exact=F, alternative = 'greater')
T4_14a




#################################################
# Gender and NON MONETORY CONDITIONS (17e for 2020)

T5 <- wilcox.test(data_2020_sample$X17e..I.am.satisfied.with.my.non.monetary.employment.conditions..e.g..leave..flexible.work.arrangements..other.benefits.
                  ~ data_2020_sample$X1..What.is.your.gender.
                  , alternative = 'two.sided')
T5

# to determine if males are more remunerated than females: in 2020

tapply(data_2020_sample$X17e..I.am.satisfied.with.my.non.monetary.employment.conditions..e.g..leave..flexible.work.arrangements..other.benefits.,
       data_2020_sample$X1..What.is.your.gender., median, na.rm=T)


T5a <- wilcox.test(data_2020_sample$X17e..I.am.satisfied.with.my.non.monetary.employment.conditions..e.g..leave..flexible.work.arrangements..other.benefits.
                   ~ data_2020_sample$X1..What.is.your.gender.
                   , exact=F, alternative = 'greater')
T5a

# Gender and NON MONETORY CONDITIONS (18g for 2014)

T5_14 <- wilcox.test(data_2014_sample$X18g..I.am.satisfied.with.my.non.monetary.employment.conditions..e.g..leave..flexible.work.arrangements..other.benefits.
                     ~ data_2014_sample$X1..What.is.your.gender.
                     , alternative = 'two.sided')
T5_14


# to determine if males are more remunerated than females in 2014:

tapply(data_2014_sample$X18g..I.am.satisfied.with.my.non.monetary.employment.conditions..e.g..leave..flexible.work.arrangements..other.benefits.,
       data_2014_sample$X1..What.is.your.gender., median, na.rm=T)


T5_14a <- wilcox.test(data_2014_sample$X18g..I.am.satisfied.with.my.non.monetary.employment.conditions..e.g..leave..flexible.work.arrangements..other.benefits.
                      ~ data_2014_sample$X1..What.is.your.gender.
                      , exact=F, alternative = 'greater')
T5_14a
