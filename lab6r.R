
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
  # The manager of this restaurant believes that the average tip left is $4.
  # A waitress would like to show that the average tip left at this restaurant is less than $4. Setup the null and alternative hypotheses.
  
  # get the mean of the tip data
  average_tip_amount <- mean(RestaurantTips$Tip)
  
  # print average_tip_amount
  msg <- paste("QUESTION 7:", "The average tip amount is",  average_tip_amount)
  print(msg)
  
  return (proportion_tips_greater_seven)
}

average_tip_amount = question_7()
