###### WORK PRESSURE ########

# comparing gender and pressure using mann whitney test

# here, we use the sample where the gender is equally distributed

data_2020_sample<- read.csv('C:\\E DRIVE\\LANGARA COLLEGE\\DANA 4801 W01\\Team project\\data_2020_sample.csv')

str(data_2020_sample)

T1 <- wilcox.test(data_2020_sample$X47a..I.have.unrealistic.time.pressures~ data_2020_sample$X1..What.is.your.gender.
                  , alternative = 'two.sided')
T1

# to determine if males feel more pressurized or females:

tapply(data_2020_sample$X47a..I.have.unrealistic.time.pressures,
       data_2020_sample$X1..What.is.your.gender., median, na.rm=T)


T1a <- wilcox.test(data_2020_sample$X47a..I.have.unrealistic.time.pressures~ data_2020_sample$X1..What.is.your.gender.
                   , exact=F, alternative = 'less')
T1a

data_2014_sample<- read.csv('C:\\E DRIVE\\LANGARA COLLEGE\\DANA 4801 W01\\Team project\\data_2014_sample.csv')

T1_14 <- wilcox.test(data_2014_sample$X36a..I.have.unrealistic.time.pressures~ data_2014_sample$X1..What.is.your.gender.
                     , alternative = 'two.sided')
T1_14


# Comparision of agency size and pressure: by using Kruskal wallis test: 2020 dataset:

# Here we import the sample size of 12000 which has 4000 observations each for agency size

data_2020_agencysample <-read.csv('C:\\E DRIVE\\LANGARA COLLEGE\\DANA 4801 W01\\Team project\\data_2020_agencysample.csv')

K1 <- kruskal.test(data_2020_agencysample$X47a..I.have.unrealistic.time.pressures~
                     data_2020_agencysample$X2)
K1

K1a <- pairwise.wilcox.test(data_2020_agencysample$X47a..I.have.unrealistic.time.pressures, 
                            data_2020_agencysample$X2..How.old.were.you.at.your.last.birthday.
                            , p.adjust.method = 'BH')

K1a


# Comparision of agency size and pressure: by using Kruskal wallis test: 2014 dataset:

data_2014_agencysample <-read.csv('C:\\E DRIVE\\LANGARA COLLEGE\\DANA 4801 W01\\Team project\\data_2014_agencysample.csv')

K1_14 <- kruskal.test(data_2014_agencysample$X36a..I.have.unrealistic.time.pressures~data_2014_agencysample$AS)
K1_14

K1_14a <- pairwise.wilcox.test(data_2014_agencysample$X36a..I.have.unrealistic.time.pressures, 
                               data_2014_agencysample$AS
                               , p.adjust.method = 'BH')

K1_14a



# Comparision of age groups and pressure: by using Kruskal wallis test: 2020 dataset:

data_2020_agesample <-read.csv('C:\\E DRIVE\\LANGARA COLLEGE\\DANA 4801 W01\\Team project\\data_2020_agesample.csv')

K2 <- kruskal.test(data_2020_agesample$X47a..I.have.unrealistic.time.pressures~data_2020_agesample$X2..How.old.were.you.at.your.last.birthday.)
K2

# To determine which age groups experiences more pressure, we run the pairwise wilcox test:

K2a <- pairwise.wilcox.test(data_2020_agesample$X47a..I.have.unrealistic.time.pressures, 
                            data_2020_agesample$X2..How.old.were.you.at.your.last.birthday.
                            , p.adjust.method = 'BH')

K2a


# Comparision of age groups and pressure: by using Kruskal wallis test: 2014 dataset:

data_2014_agesample <-read.csv('C:\\E DRIVE\\LANGARA COLLEGE\\DANA 4801 W01\\Team project\\data_2014_agesample.csv')

K2_14 <- kruskal.test(data_2014_agesample$X36a..I.have.unrealistic.time.pressures~data_2014_agesample$X2..How.old.were.you.at.your.last.birthday.)
K2_14

# To determine which age groups experiences more pressure, we run the pairwise wilcox test:

K2a_14 <- pairwise.wilcox.test(data_2014_agesample$X36a..I.have.unrealistic.time.pressures, 
                               data_2014_agesample$X2..How.old.were.you.at.your.last.birthday.
                               , p.adjust.method = 'BH')

K2a_14