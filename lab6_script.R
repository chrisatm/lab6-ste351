
set_prelims <- function() {
  # prelims
  prelim_x_list <- c( 20, 31, 5, 11, 23, 45, 7 )
  print("Preliminary variables are set")
  return (prelim_x_list)
}

prelim_x_list = set_prelims()


question_1 <- function() {
  # 1. Use R to find the number of elements in x that are greater or equal to 6. Include your code in your answer.
  # set up the conditional statement: all elements greater than or equal to 6
  condition_1 <- prelim_x_list >= 6
  
  # get number of elements in prelim_x_list that meet the condition_1 statement
  elements_greater_eq_six <- length( prelim_x_list[condition_1] )
  
  # print elements_greater_eq_six
  msg <- paste("QUESTION 1:", "The number of elements in X prelim dataset that are greater than or equal to 6 is", elements_greater_eq_six)
  print(msg)
  
  return (elements_greater_eq_six)
}

elements_greater_eq_six = question_1()


question_2 <- function() {
  # 2. Use R to find the proportion of values greater or equal to 6 in the vector x. Include your code in your answer.
  # we already got the number of elements that are greater than or equal to 6, called elements_greater_eq_six, so calculate the proportion now
  
  # get the total number of preliminary elements
  total_number_prelim_elements <- length(prelim_x_list)
  
  # calculate proportion
  proportion_greater_eq_six <- elements_greater_eq_six / total_number_prelim_elements
  
  # print proportion_greater_eq_six
  msg <- paste("QUESTION 2:", "The proportion of values greater than or equal to six is",  proportion_greater_eq_six)
  print(msg)
  
  return (proportion_greater_eq_six)
}

proportion_greater_eq_six = question_2()


question_3 <- function() {
  # 3. Are there tips greater than $7? List all the tips greater than 7.
  # make sure to import the RestaurantTips.xls dataset
  
  # get the tips that are greater than $7
  tips_greater_seven <- RestaurantTips$Tip[ RestaurantTips$Tip > 7 ]
  
  # print tips_greater_seven
  msg <- "QUESTION 3: Resturant Tips that are greater than $7:"
  print(msg)
  print(tips_greater_seven)
  
  return (tips_greater_seven)
}

tips_greater_seven = question_3()


question_4 <- function() {
  # 4. How many tips are greater than 7?
  
  # get the length of the data that contain the tips greater than $7
  number_tips_greater_seven <- length(tips_greater_seven)
  
  # print number_tips_greater_seven
  msg <- paste("QUESTION 4:", "The number of tips that are greater than 7$ is",  number_tips_greater_seven)
  print(msg)
  
  return (number_tips_greater_seven)
}

number_tips_greater_seven = question_4()


question_5 <- function() {
  # 5. What proportion of tips are greater than 7 in this dataset?
  # we already got the number of tips that are greater than 7, called number_tips_greater_seven, so calculate the proportion now
  
  # get the total number of tips
  total_number_tips <- length(RestaurantTips$Tip)
  
  # calculate proportion
  proportion_tips_greater_seven <- number_tips_greater_seven / total_number_tips
  
  # print proportion_tips_greater_seven
  msg <- paste("QUESTION 5:", "The proportion of tips that are greater than $7 is",  proportion_tips_greater_seven)
  print(msg)
  
  return (proportion_tips_greater_seven)
}

proportion_tips_greater_seven = question_5()


question_6 <- function() {
  # 6. What is the average tip left in this restaurant?
  
  # get the mean of the tip data
  average_tip_amount <- mean(RestaurantTips$Tip)
  
  # print average_tip_amount
  msg <- paste("QUESTION 6:", "The average tip amount is",  average_tip_amount)
  print(msg)
  
  return (proportion_tips_greater_seven)
}

average_tip_amount = question_6()


question_7 <- function() {
  # 7. The manager of this restaurant believes that the average tip left is $4.
  # A waitress would like to show that the average tip left at this restaurant is less than $4. Setup the null and alternative hypotheses.
  
  print("QUESTION 7:")
  
  # print null hypothesis
  null_h <- "Ho: µ = $4.00"
  print(null_h)
 
  # print alternative hypothesis
  alternate_h <- "Ha: µ < $4.00"
  print(alternate_h)
}

question_7()


question_8 <- function() {
  # 8. Shift the dataset of tips such that the mean is the same as the null hypothesis (verify that the mean is 4).
  
  # before question #8, we are told that the difference is .1507006
  diff <- .1507006
  
  # shift the data by add the diff to all the dataset values
  null_tips <- RestaurantTips$Tip + diff
  
  # verify that the new average is the same as the null hypothesis
  mean_null_tips <- mean(null_tips)
  
  # print average_tip_amount
  msg <- paste("QUESTION 8:", "The null average tip amount is $4.00 and the shifted dataset average is", mean_null_tips)
  print(msg)
  
  return (null_tips)
}

null_tips = question_8()


question_9 <- function() {
  # 9. Generate a bootstrap distribution with 2000 average tips assuming that the null hypothesis is true, that is
  # bootstrap sample from the variable null tips. Then create a histogram of your bootstrap statistics.
  
  # initialize boot_stats variable
  boot_stats_tips <- 0
  
  # repeat 2000 times to create 2000 bootstrap statistics
  for (i in 1:2000) {
    boot_samples <- sample(null_tips, 157, replace = TRUE)
    boot_stats_tips[i] <- mean(boot_samples)
  }
  
  #create histogram of bootstrap
  hist_null_tips <- hist(boot_stats_tips)
  hist_null_tips
  
  print("QUESTION 9: display histogram")
  
  return (boot_stats_tips)
}

boot_stats_tips = question_9()


question_10 <- function() {
  # 10. The question now is, what proportion of samples
  # will have an average tip less than $3.85? This proportion is called the p-value of the test.
  # Find the p-value.
  
  # get total amount of dataset values from the bootstrap distribution of null_tips (should be 2000)
  total_number_bootstrap_stats <- length(boot_stats_tips)
  
  # get total number of tips that are less than or equal to $3.85
  # set conditional
  condition <- boot_stats_tips <= 3.85
  number_tips_less_eq <- length(boot_stats_tips[condition])
  
  #get the p-value
  p_value_tips <- number_tips_less_eq / total_number_bootstrap_stats
  
  # print average_tip_amount
  msg <- paste("QUESTION 10:", "The p-value is", p_value_tips)
  print(msg)
  
  return (p_value_tips)
}

p_value_tips = question_10()


question_11 <- function() {
  # 11. Do you think this p-value indicates that the average tip is less $4?

  msg1 <- "QUESTION 11: The p-value does not indicate that the average tip is less than $4.00"
  print(msg1)
  msg2 <- "because the p-value is not lower than 0.05 or not statistically significant."
  print(msg2)

}

question_11()


question_12 <- function() {
  # 12. Set up the null and alternative hypothesis
  
  print("QUESTION 12:")
  
  # print null hypothesis
  null_h <- "Ho: µ = 80 ppb"
  print(null_h)
  
  # print alternative hypothesis
  alternate_h <- "Ha: µ > 80 ppb"
  print(alternate_h)
}

question_12()


question_13 <- function() {
  # 13. Compute the sample mean.
  
  # initialize arsenic chicken data
  arsenic_chicken_data <- c( 75, 95, 81, 84, 90, 105 )
  
  #calculate mean
  arsenic_chicken_mean <- mean(arsenic_chicken_data)
  
  # print average_tip_amount
  msg <- paste("QUESTION 13:", "The sample mean is", arsenic_chicken_mean)
  print(msg)
  
  return (arsenic_chicken_data)
}

arsenic_chicken_data = question_13()


question_14 <- function() {
  # 14. Shift the dataset to obtain a mean equal to the mean in the null hypothesis (verify that the mean is 80).
  # Then generate a bootstrap distribution with 2000 averages assuming the null hypothesis is true. Show the
  # distribution of bootstrap stats in a histogram.
  
  # calculate current data mean
  arsenic_chicken_mean <- mean(arsenic_chicken_data)
  
  # calculate difference of sample mean data and mean of population data
  diff <- 80 - arsenic_chicken_mean 
  
  # shift the data by add the diff to all the dataset values
  null_arsenic_chicken <- arsenic_chicken_data + diff
  
  # calculate the adjusted mean
  mean_null_arsenic_chicken <- mean(null_arsenic_chicken)
  
  #verify if the means are the same
  msg <- paste("QUESTION 14:", "The average amount of arsenic in chicken is 80 ppb and the shifted dataset average is", mean_null_arsenic_chicken)
  print(msg)
  
  # generate the bootstrap distribution
  # initialize boot_stats variable
  boot_stats_arsenic_chicken <- 0
  
  # repeat 2000 times to create 2000 bootstrap statistics
  for (i in 1:2000) {
    boot_samples <- sample(null_arsenic_chicken, 6, replace = TRUE)
    boot_stats_arsenic_chicken[i] <- mean(boot_samples)
  }
  
  #create histogram of bootstrap
  hist_null_arsenic_chicken <- hist(boot_stats_arsenic_chicken)
  hist_null_arsenic_chicken
  
  print("QUESTION 14: display histogram")
  
  return (boot_stats_arsenic_chicken)
}

boot_stats_arsenic_chicken = question_14()


question_15 <- function() {
  # 15. Find the p-value.
  
  # get total amount of dataset values from the bootstrap distribution of boot_stats_arsenic_chicken (should be 2000)
  total_number_bootstrap_stats <- length(boot_stats_arsenic_chicken)
  
  # get total number of chickens that contain more than 80 ppb of arsenic
  # set conditional
  condition <- boot_stats_arsenic_chicken > 80
  number_arsenic_chicken_greater <- length(boot_stats_tips[condition])
  
  #get the p-value
  p_value_arsenic_chicken <- number_arsenic_chicken_greater / total_number_bootstrap_stats
  
  # print average_tip_amount
  msg <- paste("QUESTION 15:", "The p-value is", p_value_arsenic_chicken)
  print(msg)
  
  return (p_value_arsenic_chicken)
}

p_value_arsenic_chicken = question_15()


question_16 <- function() {
  # 16. Make a decision about the null hypothesis. Would you reject the null hypothesis?
  
  msg1 <- "QUESTION 16: The p-value does not indicate that the amount of arsenic in chicken is greater than 80 ppb"
  print(msg1)
  msg2 <- "because the p-value is not lower than 0.05 or not statistically significant."
  print(msg2)
  
}

question_16()
