

# We worked together for a lot of the project, discussing how we wanted to go 
# about each task. We worked together to write the strategy functions to be used
# within the Pone and Pall functions. Then Corinne and Pablo wrote Pone and Pall 
# whilst Caitlin wrote dloop. We feel we all had equal contributions. 

# In this file we have written code to simulate and analyse the prisoner problem.
# Functions addresing three different strategies have been defined, and they 
# have been used to construct more powerful functions that could simulate the
# probability of a single prisoner finding their own number, and the probability
# of all prisoners finding their own number and being able to escape.
  

strategy1 <- function(n, k, boxes_content){
  
# The below function simulates one prisoner's journey through the boxes,
# using strategy 1: it returns a 1 if the prisoner escapes, and a 0 if not. 
# It takes the maximum number of boxes that can be opened (n), the number that
# has to be found (k), and the numbers that are hidden in each of the 2*n boxes
# as the inputs. strategy1 takes the prisoner's number as the first box to check.
# If the prisoner's number is found then success; if not, the new box to check  
# is the box corresponding to the number found within the last box. 
  
  box_to_check <- k # Per strategy 1, the first box to check is the prisoner's
  # number
  success <- 0 # This 'success' will indicate whether the prisoner escapes
  boxes_opened <- 0 # This will count the number of boxes that the prisoner
  # opens, to ensure it is no more than n
  while (success == 0 && boxes_opened < n){ # Ensures that the prisoner keeps
    # opening boxes while they have not found their number, and have opened
    # less than n boxes
    
    if (boxes_content[box_to_check] == k){ # If the prisoner's number is found..
      success  <- 1 # .. then the prisoner escapes, indicated by 'success'
    }
    else{ # If the prisoner's number is not found in the box ..
      box_to_check <- boxes_content[box_to_check] # .. the box to check becomes
      # the number found inside the last box, per Strategy 1
      boxes_opened = boxes_opened + 1 # Update the count for number of boxes 
      # opened by the prisoner
    }
  }
  return (success)} # Return 1 if the prisoner escapes, 0 if not






strategy2 <- function(n, k, boxes_content){
  
# This function simulates one prisoner's journey through the boxes, using 
# strategy 2. It takes the maximum number of boxes that can be opened (n), the  
# number that has to be found (k), and the numbers that are hidden in each of  
# the 2*n boxes as the inputs. The function picks a random box as the first box 
# to be checked, and subsequently follows the same pattern as strategy 1. The 
# function returns a 1 if the prisoner escapes, and a 0 if not. 
  
  random_start <- sample(2*n, 1) # picks a random number 
  # between 1 and 2n (the number of prisoners)
  box_to_check <- random_start # first box to check is this random number
  success <- 0 #This will indicate whether the prisoner escapes
  boxes_opened <- 0 # This will count the number of boxes that the prisoner
  # opens, used to ensure it is no more than n
  while (success == 0 && boxes_opened < n){# Ensures that the prisoner keeps
    # opening boxes while they have not found their number, and have opened
    # less than n boxes
    
    if (boxes_content[box_to_check] == k){ #If the prisoners number is found..
      success  <- 1   #... Then the prisoner escapes, indicated by success = 1
    }
    else{ # If the prisoner's number is not found in the box ..
      box_to_check <- boxes_content[box_to_check] # .. the box to check becomes
      # the number found inside the last box, per Strategy 2
      boxes_opened = boxes_opened+1  # Update the count for number of boxes 
      # opened by the prisoner
      
    }
  } 
  return(success)} # Return 1 if the prisoner escapes, 0 if not




strategy3 <- function(n, k, boxes_content){
  
# This function simulates one prisoner's journey through the boxes, using 
# strategy 3. It takes as inputs the maximum number of boxes that can be opened
# (n), the number of the prisoner that needs to be found (k) and a certain random 
# ordering of the boxes (boxes_content). strategy3 simulates the random choice
# of n boxes, and checks whether the required number is hidden in any of them. 
# The function returns a 1 if the prisoner escapes, and a 0 if not. 
  
  success <- 0 #This variable will indicate whether the prisoner escapes.
  order_of_checking <- sample(2*n, n, replace=FALSE) #choose n numbers at random, 
  #from 1 to 2*n. These will be the indices of the boxes to be checked.
  for (o in order_of_checking){ #For each of the boxes that are to be opened..
    if (boxes_content[o] == k){ #..if the prisoner number is found..
      success <- 1        #..then the prisoner escapes, indicated by success = 1
      break
    }
  }  
  return(success)} #Return 1 if the prisoner escapes, 0 otherwise.


Pone <- function(n, k, strategy, nreps = 10000){
  
  #This function runs each strategy function "nreps" times using the given  
  #value of k, opening "n" boxes and returns the proportion of times that the 
  #prisoner escapes.  By default, nreps is set to 10000.
  
  prisoners <- c(1:(2*n)) # Creates a vector containing the number of each 
  # prisoner
  nsuccess <- 0 # This will count whether the prisoner escapes in each rep
  if (strategy == 1){ #If strategy 1 is used...
    for (i in 1:nreps){ # ... for each iteration from 1:nreps...
      boxes_content <- sample(prisoners) # order the number in the boxes randomly
      nsuccess <- nsuccess + strategy1(n,k, boxes_content) # Add 1 if the 
      # prisoner escapes, using the return of the strategy 1 function
    }
  }
  else if (strategy == 2){ #If strategy 2 is used...
    for (i in 1:nreps){ # ... for each iteration from 1:nreps...
      boxes_content <- sample(prisoners) # order the number in the boxes randomly
      nsuccess <- nsuccess + strategy2(n,k, boxes_content) # Add 1 if the 
      # prisoner escapes, using the return of the strategy 2 function
    }
  }
  else { #Otherwise, if strategy 3 is used ... 
    for (i in 1:nreps){ # ... for each iteration from 1:nreps...
      boxes_content <- sample(prisoners) # order the number in the boxes randomly
      nsuccess <- nsuccess + strategy3(n,k, boxes_content) # Add 1 if the 
      # prisoner escapes, using the return of the strategy 3 function
    }
  }
  return(nsuccess/nreps)} #Returns the probability that a prisoner is successful


Pall <- function(n, strategy, nreps = 10000){
  
# This function returns the probability of all prisoners finding their number
# by using a certain strategy, therefore escaping. Pall takes the number of 
# boxes that can be opened at maximum (n), the strategy to be used by all the 
# prisoners (1, 2 or 3) and the number of simulations that want to be carried 
# out (nreps, set to 10000 by default).
  
  prisoners <- c(1:(2*n)) # Creates a vector containing the number of each 
  # prisoner
  total_successes <- 0   # Variable to count how many times
  # all prisoners escape from jail
  for (i in 1:nreps){ # For each simulation..
    boxes_content <- sample(prisoners) #.. generate random ordering of the boxes
    count_of_prisoners_succesful <- 0 #Variable to count how many prisoners 
    #find their number
    if (strategy == 1){  #If strategy 1 is used..
      for (k in prisoners){ #.. for each prisoner..
        prisoner_k_outcome <- strategy1(n, k, boxes_content) # Simulate whether
        #prisoner k is succesful finding their number
        if (prisoner_k_outcome == 1){ #If prisoner finds their number
          count_of_prisoners_succesful <- count_of_prisoners_succesful + 1
          # Add 1 to the counter of prisoners that are succesful
        }
        else{ #If prisoner k is not succesful, then stop iteration i since not
          # all prisoners are able to find their number. 
          break
        }
      }
    }
    else if (strategy == 2){   #If strategy 2 is used..
      for (k in prisoners){ #.. for each prisoner..
        prisoner_k_outcome <- strategy2(n, k, boxes_content) # Simulate whether
        #prisoner k is succesful finding their number
        if (prisoner_k_outcome == 1){ #If prisoner finds their number
          count_of_prisoners_succesful <- count_of_prisoners_succesful + 1
          # Add 1 to the counter of prisoners that are succesful
        }
        else{ #If prisoner k is not succesful, then stop iteration i since not
          # all prisoners are able to find their number. 
          break
        }
      }
    }
    else{   #If strategy 3 is used..
      for (k in prisoners){ #.. for each prisoner..
        prisoner_k_outcome <- strategy3(n, k, boxes_content) # Simulate whether
        #prisoner k is succesful finding their number
        if (prisoner_k_outcome == 1){ #If prisoner finds their number
          count_of_prisoners_succesful <- count_of_prisoners_succesful + 1
          # Add 1 to the counter of prisoners that are succesful
        }
        else{ #If prisoner k is not succesful, then stop iteration i since not
          # all prisoners are able to find their number.
          break
        }
      }
    }
    if (count_of_prisoners_succesful == (2*n)){ #If all prisoners have been
      #succesful...
      total_successes <- total_successes + 1 #Add 1 to the counter of times 
      #when all prisoners escape.
      
    }
  }
  probability <- total_successes/nreps #Calculate probability of all prisoners
  #escaping according to the simulation.
  return(probability) #return probability of all prisoners escaping
}



for(n in c(5, 50)){
  
  for(strategy in c(1,2,3)){
    
    print(paste("Pone with Strategy ",strategy," and n = ", n,": ",
                Pone(n,k=1,strategy,10000)))
    print(paste("Pall with Strategy ",strategy," and n = ", n,": ", 
                Pall(n,strategy,10000))) 
  }
}


# Using the function Pone, we can see that regardless of number of prisoners 
# involved, the probability of one prisoner escaping is around 50% using 
# strategies 1 and 3, and around 40% using strategy 2. It is therefore  
# surprising that when using Pall, strategies 2 and 3 give a probability that 
# all prisoners escape of almost 0 (again, regardless of prisoners involved), 
# whilst strategy 1 leads to a probability of around 31%. This is surprising 
# because, whilst using strategy 1 does not increase the probability of escape  
# for each individual prisoner, if all prisoners were to use it, it would  
# greatly increase their chance of collectively escaping, compared to the other 
# strategies. 



dloop <- function(n, nreps){

# The function dloop estimates, by simulation, the probability of each loop 
# length from 1 to 2n occurring at least once in a random shuffling of cards to 
# boxes. It requires n, which represents the number of boxes that can be opened, 
# and nreps, the number of simulations to be run in order to estimate
# probabilities. It returns a 2n-length vector of probabilities, where the 
# value in element i represents the probability that a loop of length i appears
# at least once in a random shuffling of cards to boxes.
  
  prisoners <- c(1:(2*n)) # This vector represents all of the prisoners by their 
  # respective numbers.
  
  probabilities <- rep(0,(2*n)) # Establishing the probability vector which will
  # be returned.
  
  for(i in 1:nreps){ # Repeats the simulation nreps times
    
    boxes_content <- sample(prisoners) # Randomly construct boxes differently in
    # each simulation
    
    loop_count <- rep(0, (2*n)) # Will count how many times loops of each length
    # occur in this simulation's boxes
    
    for(p in 1:(2*n)){ # Running through all of the prisoners' numbers, as each 
      # represents a card in a box
      
      target <- p # Simulates a prisoner of number p stepping into the room 
      
      box_to_check <- p # First look in the box with the prisoner's number, per
      # strategy 1 (as a way to find all loops)
      
      for(l in 1:(2*n)){ # Runs through all possible lengths of loop
        
        x <- boxes_content[[box_to_check]] # Storing the content of the box 
        # opened as x
        
        if(x == target){ # If our target (p) was in this box ...
          loop_count[[l]] <- loop_count[[l]] + 1 # .. add a 1 into the loop_count
          # in the position corresponding to the length of the loop (p) was in 
          break # Once the target has been found, stop looking, as each only
          # appears in one loop
        }
        else{ # If the target was not in the box opened ...
          loop_count[[l]] <- loop_count[[l]] # ..don't change loop_count yet
        }
        box_to_check <- x  # The next box to check is the number which was found
        # inside the previous box
        
        
      } # Repeating the above will search through all boxes until the target is 
      # found, determining the length of loop which the target is contained in
      
    }
    for(i in 1:length(loop_count)){ # For each simulation, we then have a 
      # loop_count vector, indicating the number of loops of each length which 
      # were present in the random generation
      if(loop_count[[i]] > 0){ # If the number of loops is positive ...
        probabilities[[i]] <- probabilities[[i]] + 1 # ... then it has appeared
        # at least once, so add one to the count
      }
      else{
        probabilities[[i]] <- probabilities[[i]] # If no loops of length i were
        # present in the generation, it shouldn't be counted
      }
    }
  }
  
  probabilities <- probabilities / nreps # Dividing these counts by the number
  # of simulations runs provides the average number of times that a loop 
  # of each length appeared at least once
  
  
  return(probabilities)} # Returns the desired vector of probabilities




# Running the function for n = 50
n_50 <- dloop(50,10000)
print(n_50)

# Displaying the probabilities of loops of each length occurring at least once
barplot(n_50, names.arg = c(1:100), 
        main = "Probability of loops of each length occurring \n in a random shuffling",
        xlab = "Loop length", ylab = "Probability of occurence", ylim = c(0,0.7))


# Calculating the probability that there is no loop longer than 50 in a random 
# shuffling:
print(paste("Probability that there is no loop longer than 50: ",
            1-sum(dloop(50,10000)[51:100])))
