test <- as.data.frame(dist) 
test <- test[which(!(test$historical == 0 & test$`2050` == 0 & test$`2090` == 0)), ]
# test <- test[which(!(test$historical == 0 & test$`2050` == 1 & test$`2090` == 0)), ]
ind <- which((test$historical == 0 & test$`2050` == 0 & test$`2090` == 1))
test[ind, 'historical'] <- 4
test[ind, '2050'] <- 3
test[ind, '2090'] <- 2
ind <- which((test$historical == 0 & test$`2050` == 1 & test$`2090` == 1))
test[ind, 'historical'] <- 3
test[ind, '2050'] <- 2
test[ind, '2090'] <- 2
#test[which((test$historical == 2 & test$`2050` == 2 & test$`2090` == 1)), '2090'] <- 2

test$historical <- paste('hist', test$historical)
test$`2050` <- paste('y2050', test$`2050`)
test$`2090` <- paste('y2090', test$`2090`)

dat <- data.frame(A = sample(LETTERS[1:4], 200, TRUE), 
                  b = sample(letters[1:3], 200, TRUE),
                  C = as.character(sample(1:2, 200, TRUE)),
                  d = as.character(sample(5:6, 200, TRUE)))

testplot <- make_sankey(test)
testplot
ggsave(test, filename = file.path(wd, 'test.pdf'))
