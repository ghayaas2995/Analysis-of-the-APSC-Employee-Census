
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
