###### IMMEDIATE SUPERVISOR ########

# Here we import the sample size of 12000 which has 4000 observations each for agency size

data_2020_agencysample <-read.csv('C:\\E DRIVE\\LANGARA COLLEGE\\DANA 4801 W01\\Team project\\data_2020_agencysample.csv')
data_2014_agencysample <-read.csv('C:\\E DRIVE\\LANGARA COLLEGE\\DANA 4801 W01\\Team project\\data_2014_agencysample.csv')

# 1.	Agency size and Communication 

K3 <- kruskal.test(data_2020_agencysample$X19a..My.supervisor.communicates.effectively~
                     data_2020_agencysample$X2)
K3


K3a <- pairwise.wilcox.test(data_2020_agencysample$X19a..My.supervisor.communicates.effectively, 
                            data_2020_agencysample$X2
                            , p.adjust.method = 'BH')

K3a

# Comparision of agency size and communication with supervisor: by using Kruskal wallis test: 2014 dataset:


K3_14 <- kruskal.test(data_2014_agencysample$X20a..My.supervisor.provides.me.with.regular.and.constructive.feedback~data_2014_agencysample$AS)
K3_14

K3a_14 <- pairwise.wilcox.test(data_2014_agencysample$X20a..My.supervisor.provides.me.with.regular.and.constructive.feedback, 
                               data_2014_agencysample$AS
                               , p.adjust.method = 'BH')

K3a_14