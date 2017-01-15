# Function to check for Leap year
Leap <- function(year){
  #check if input is a numeric value, otherwise print error-message
  if (!is.numeric(year)){
    print("Enter a valid year")
    result <- 'error'
  }
  #check if input is an integer and not a float-value, otherwise print error-message
  else if (year %% 1 != 0){
    print("Enter a valid year")
    result <- 'error'
  } 
  #check if input-year is after 1581, otherwise print error-message
  else if (year < 1582){
    print(paste0(year," is not a valid year. The calendar starts at 1582"))
    result <- 'error'
    }
  # check if input year can be devided by 4 and not by 100 --> leap year
  else if (((year %% 4) == 0) && ((year %% 100) != 0)) {
    result <- TRUE
  }
  # check if input year can be devided by 400 --> leap year
  else if ((year %% 400) == 0) {
      result <- TRUE
    }
  # if upper statements are wrong --> common year 
  else {
    result <- FALSE
  }
  # Leap years return a result 'TRUE'
  if (result == TRUE) {
    print(paste0(result, ', ', year, ' is a leap year'))
    }
  # Common years return a result 'FALSE'
  if (result == FALSE) {
    print(paste0(result, ', ', year, ' is a common year'))
  }
  # Unvalid input will results in a print according to the conditionstatements'
  if (result == 'error') {
    
  }
   
}
