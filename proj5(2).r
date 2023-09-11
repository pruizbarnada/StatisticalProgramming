# Pablo Ruiz Barnada - s1854579

# This file contains code to implement a demographic model to predict the
# expected number of deaths per week over a certain period of time. In
# particular, the code will work with data from 2017-2019 of England and
# Wales to set the model, and will aim to predict the expected deaths from
# 2020 onwards, provided that the death rates had stayed at the levels of the
# previous years. The predictions will be compared to the actual deaths observed
# over a period of 149 weeks (starting in 2020). The model has and adjustment 
# that allows for seasonal variation (each week of the year has an individual
# "mortality rate"). Moreover, the model also takes into account the different
# mortality rates associated with age, which also differs from men to women.
# Thus, the predictions are generated for men and women individually, and 
# then are summed together. Different plots are generated to analyse the
# parameters obtained from the model.

# setwd("D:\\Pablo\\Documents\\Practical-5")

library(rjags) #load rjags package

death1772 <- scan("death1722uk.dat",what=list("a",1,1,1),skip=1) #load file that
  #contains information of defunctions for 305 weeks, and a mortality rate 
  #modifier associated to each week

deaths <- death1772[[2]] # number of deaths in particular weeks
d <- death1772[[3]]  # mortality rate parameter of the week 
week <- death1772[[4]] # number of week


lt1720 <- scan("lt1720uk.dat",what=list("a",1,1,1,1,1,1,1),skip=1) #load file
  # with demographic information

age <- lt1720[[2]] # Age intervals for the population (0-1 years, ..., 100-101)
fpop17  <- lt1720[[3]]  #female population starting 2017
mpop17 <- lt1720[[4]] #male population starting 2017
mf <- lt1720[[5]] #female annual death rate for each 1-year age band
mm <- lt1720[[6]] #male annual death rate for each 1-year age band
fpop20 <- lt1720[[7]] #female population starting 2020
mpop20 <- lt1720[[8]] #male population starting 2020



ndeaths_prediction <- function(malepop, femalepop, male_m,
                                 female_m, d){
# This function is used to predict the number of deaths over a defined period
# of time. As inputs, it takes an initial male population (malepop), an 
# initial female population (femalepop), vectors for male and female annual 
# death rates for every 1-year age band (male_m and female_m), and the mortality
# rate modifier for each week (d). The function calculates the expected deaths
# for every age band, for both male and female, in a week, and stores the total
# deaths of the week in a vector. Then, it updates the population for each age
# band for the next week (considering birthdays and expected deaths). The
# expected deaths are calculated for a total of length(d) weeks. Throughout all  
# the process, we assume constant birth rate. Finally, the function returns
# the vector with the predicted total deaths each week.

  
  predicted_deaths <- c() #Initialise vector of predicted deaths per week
  new_malepop <- malepop #Initialise male population variable for next week
  new_femalepop <- femalepop #Initialise female population variable for next week
  Nm_star <- rep(malepop[1], length(malepop)) #Initialise vector containing the
    #male population alive per age band
  Nf_star <- rep(femalepop[1], length(femalepop)) #Initialise vector containing 
  #the female population alive per age band
  
    
  for (j in (1:length(d))){ #for each week in which we predict deaths...
    total_deaths <- 0 #...set deaths of the week to 0
    
    for (i in (1:length(age))){ #for each age band...
      dead_male <- 0.9885 * d[j] * (1 - exp(- male_m[i] / 52)) * malepop[i]
        #predict how many men will die
      dead_female <- 0.9885 * d[j] * (1 - exp(- female_m[i] / 52)) * femalepop[i]
        #predict how many women will die
      total_deaths <- total_deaths + dead_male + dead_female #add male and
        #female deaths of this age band to the total deaths in the week
      
      Nm_star[i] <- malepop[i] - dead_male #update the number of men alive
        #of this age band after the predictions of deaths
      Nf_star[i] <- femalepop[i] - dead_female #update the number of women alive
        #of this age band after the predictions of deaths
      
      if (i > 1){ #If the analysed age band is not the age = 0 population... 
        Nm <- (Nm_star[i]) * (51/52) + ((Nm_star[i-1]) / 52) #calculate the
          #male population of this age band in the next week
        Nf <- (Nf_star[i]) * (51/52) + ((Nf_star[i-1]) / 52) #calculate the
          #female population of this age band in the next week
        new_malepop[i] <- Nm #update the vector of male population for the 
          #next week
        new_femalepop[i] <- Nf #update the vector of female population for the 
          #next week
      }
    }
    predicted_deaths <- append(predicted_deaths, total_deaths) #add the total
     #predicted deaths of the week to the vector containing all weeks' deaths
    malepop <- new_malepop #set the current male population to be what was 
      #calculated for the coming week 
    femalepop <- new_femalepop #set the current female population to be what was 
     #calculated for the coming week
  }
  return(predicted_deaths) #return vector containing total predicted deaths 
    #each week
}




d_total <- ndeaths_prediction(mpop20, fpop20, mm, mf, 
                              d[1: (length(deaths) - 156)]) #predict deaths per
  #week from 2020 to the end of data
excess_deaths <- sum(deaths[157:length(deaths)] - d_total) #calculate 
  #excess deaths from 2020 to the end of data
excess_deaths_vector <- deaths[157:length(deaths)] - d_total #calculate
  #excess deaths per week from 2020 to the end of data
d2020 <- ndeaths_prediction(mpop20, fpop20, mm, mf, d[1:52]) #predict deaths per
  #week for 2020
excess_deaths_2020 <- sum(deaths[157:208] - d2020 ) #calculate the
  #excess deaths for 2020
  



plot(week[1:(length(deaths)-156)], deaths[157:length(deaths)], 
     xlab = "Week number", ylab = "Observed deaths",
     ylim = c(0, max(deaths[157:208])), 
     pch = 19) #plot observed deaths against week from 2020 to end of data

lines(d_total, col = "red", type = "l") #plot curve showing predicted deaths

title(paste("Observed and predicted weekly deaths, from 2020 onwards",
      "\nOverall excess deaths:",(excess_deaths), "\nExcess deaths in 2020:",
            (excess_deaths_2020))) #Set title

legend("topright", legend = c("Observed deaths","Predicted deaths"), 
       lty = c(NA, 1), pch = c(19, NA), col = c("black", "red")) #Set legend




cum_excess_deaths <- cumsum(deaths[157:length(deaths)] - d_total) #generate
  #vector of cumulative excess deaths by week, from 2020 to the end of data

names(cum_excess_deaths) <- c(1:(length(deaths)-156)) #create variable 
  #containing the weeks for which we predicted deaths, starting from 2020

barplot(cum_excess_deaths, las = 0, 
        xlab = "Week", ylab = "Number of excess deaths",
        main = "Cumulative excess deaths by week, from 2020 onwards",
        ylim = c(min(cum_excess_deaths), max(cum_excess_deaths)), col = "pink")
  #plot the cumulative excess deaths by week, from 2020 to the end of data




xs <- excess_deaths_vector #define new variable to be the vector of excess 
  #deaths per week
for (i in c(51, 52, 53, 105, 106)){ #for certain weeks...
  xs[i] <- NA #...set the predicted deaths to NA since the data was not reliable
}

mod <- jags.model("model.jags", 
                  data = list(x = xs, N = (length(excess_deaths_vector))))
                  #load jags model
sam <- jags.samples(mod, c("mu", "rho", "k"), n.iter = 10000) #extract random
  #samples from the jags model

sam.coda <- coda.samples(mod, c("mu", "rho", "k"), n.iter = 10000) #obtain MCMC
  #output, with automatic burn in of the model samplers.
sam.coda_matrix <- as.matrix(sam.coda, iters = FALSE) #transform sam.coda to
  #be a matrix





hist(sam.coda_matrix[, 152], main = expression(paste("Histogram of ", rho)), 
     xlab = expression(paste(rho, " values")), ylab = "Frequency") #produce 
      #histogram for the rho parameter from the model
plot(sam.coda_matrix[, 152], type = "l", 
     xlab = "Iterations", ylab = expression(rho)) #Produce trace plot of rho
title(expression(paste("Trace plot for ", rho))) #Set title




mat_mu <- sam.coda_matrix[, 2:150] #select the columns from the matrix
  #corresponding to the mu vector.
expected_mu <- colMeans(mat_mu) #calculate the posterior expected value vector 
  #for mu





reduced_sample <- c() #Initialise empty vector
for (i in (1:10000)){ #for the sample from the model...
  reduced_sample <- append(reduced_sample, (i*50)) #save the index of every 
    #50th sample 
}

reduced_sample_matrix <- matrix(, nrow = (10000/50), ncol = 149) #initialise
  #matrix that will contain only every 50th sampled mu of the model
  

for (i in (1:(10000/50))){ #for each row in reduced_sample_matrix... 
  reduced_sample_matrix[i,] <- mat_mu[(50*i),] #... update the rows to contain
    #every 50th mu sample from our model
}

plot(NULL, xlim=c(0,length(xs)), ylim=c(-5000, 15000), 
     ylab="Deaths", xlab="Weeks") #set axis labels and limits of a new plot

for (i in 1:dim(reduced_sample_matrix)[2]){ #for each mu sample in 
      #reduced_sample_matrix
  lines(reduced_sample_matrix[i,], lty = 1, col = "grey" ) #plot the mu vector
}

lines(expected_mu, lty = 1, col = "blue") #plot the estimated expectation of mu
points(c(1:length(excess_deaths_vector)), excess_deaths_vector, 
       col = ifelse(is.na(xs) == TRUE, "red", "black"), pch = 19) #plot observed
    #excess deaths

title("Observed and expected excess deaths, from 2020 onwards") #Set title

legend("topright", 
       legend = c("Expected excess deaths", "Average expected excess deaths", 
                  "Observed excess deaths", "Unreliable data"), 
       col = c("grey", "blue", "black", "red"), 
       lty = c(1,1, NA, NA), pch = c(NA, NA, 19, 19)) #Set legend




residuals_ <- xs - expected_mu #calculate residuals of the model
plot(c(1:(length(deaths) - 156)), residuals_, 
     xlab = "Weeks", ylab = "Residuals", pch = 19) #plot residuals
abline(h = 0) #plot line through y = 0
title("Residuals plot")