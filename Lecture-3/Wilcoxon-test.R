a <- c(214, 159, 169, 202, 103, 119, 200, 109, 132, 142, 194, 104, 219, 119, 234)
b <- c(159, 135, 141, 101, 102, 168, 62, 167, 174, 159, 66, 118, 181, 171, 112)

diff <- c(a - b) #calculating the vector containing the differences
diff <- diff[ diff!=0 ] #delete all differences equal to zero
diff.rank <- rank(abs(diff)) #check the ranks of the differences, taken in absolute
diff.rank.sign <- diff.rank * sign(diff) #check the sign to the ranks, recalling the signs of the values of the differences
ranks.pos <- sum(diff.rank.sign[diff.rank.sign > 0]) #calculating the sum of ranks assigned to the differences as a positive, ie greater than zero
ranks.neg <- -sum(diff.rank.sign[diff.rank.sign < 0]) #calculating the sum of ranks assigned to the differences as a negative, ie less than zero
ranks.pos #it is the value V of the wilcoxon signed rank test
ranks.neg

boxplot(data.frame(a, b))
