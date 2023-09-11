# This practical explores the creation of the function 'newt', which implements
# Newton's method for minimization of functions. The main function 'newt' returns 
# the value of the objective function and parameters at the minimum, the number
# of iterations taken to reach the minimum and the inverse of the hessian matrix 
# at the minimum for the given inputs. 
# If the function newt is not given a hessian matrix as an input, it calls on 
# the function 'makehessian' to obtain an approximation of the Hessian using 
# finite differencing of the gradient vector. 

makehessian <- function(theta, grad, eps, ...){
  
  # This function is used to obtain an approximation of the Hessian by finite
  # differencing the gradient vector. It is applied in the function 'newt' when the   
  # Hessian matrix function is not supplied. The function arguments are "theta", 
  # a vector of initial values for the optimization parameters in newt, "grad", 
  # the gradient function, and "eps", the finite difference intervals to be used
  # when approximating the Hessian. The "..." argument represents extra arguments
  # that the "grad" function might require.
  
  gradient <- grad(theta, ...) #calculate the gradient value from inital theta
  hessian <- matrix(0, length(theta), length(theta)) #initialise the Hessian
  #matrix, full of zeros, and having "length(theta)" rows and columns
  
  for (i in (1:length(theta))){ #for each number in the range 1 to length(theta) 
    theta1 <- theta #define a new vector theta1, equal to the original theta
    theta1[i] <- theta1[i] + eps #update the element 'i' in the vector theta1
    #by adding eps
    gradient1 <- grad(theta1, ...) #calculate the gradient function with the 
    #updated theta1
    hessian[i,] <- (gradient1 - gradient) / eps #set the ith row of the 
    #Hessian matrix to be the difference between the gradient function
    #evaluated at the updated theta1 and the original theta, divided by eps
  }
  
  return (hessian) #return the approximated Hessian matrix
}

newt <- function(theta, func, grad, hess = NULL, ..., tol=1e-8, fscale=1, 
                 maxit=100,  max.half=20, eps=1e-6){
  
  # This function can be used to minimize a given function. It 
  # requires as inputs: a vector of initial values to plug in (theta), the objective
  # function to be minimized (func), a function giving the gradient of the 
  # objective function at a given vector. It also has optional inputs, with 
  # their defaults shown above: a function used to calculate the Hessian of the 
  # objective function at a given vector (hess), the tolerance of the
  # convergence (tol), a rough estimate of the magnitude of the objective 
  # function at the minimum (fscale), the maximum numbers of both iterations 
  # and of step halvings within each iteration (maxit, max.half respectively)
  # and the finite difference intervals to use when hess=NULL. It returns as
  # output, in a list: the value of the objective at the minimum,
  # the gradient at this point, the final value of theta which minimizes the 
  # objective function, the number of iterations taken to achieve this, and the
  # inverse of the Hessian at the minimum.
  
  # Generating the Hessian matrix
  if (is.null(hess) == TRUE){ # If no hess function is inputted...
    # ...calls our premade function, inputting theta, grad and eps
    hessian <- makehessian(theta, grad, ..., eps)
  }
  else{ # If hess is inputted...
    hessian <- hess(theta, ...) #...uses this function to create the Hessian
  }
  
  # If the generated Hessian is not positive definite (has eigenvalues which are
  # not strictly positive)...
  if(min(eigen(hessian)$values) <= 0){
    #...prints a warning
    warning("The initial Hessian is not positive definite.")
  }
  
  count <- 0 # Counter for number of iterations
  D <- func(theta, ...) # D is the value of the function to be minimized at the
  # given theta
  gradient <- grad(theta, ...) # Gradient of the function at theta
  
  # If any one out of the function value at theta, the gradient at theta or any 
  # element of the generated Hessian are infinite ...
  if (all(is.finite(D) == TRUE) != TRUE | all(is.finite(gradient) == TRUE) != 
      TRUE | all(is.finite(hessian) == TRUE) != TRUE){
    
    # ... stops the function and prints the given error
    stop("The objective or derivatives are not finite at the initial theta.")
  }
  
  # Repeats iterations until either the gradient is sufficiently close to 0 or
  # the maximum number of iterations is reached
  while (max(abs(gradient)) >= (tol * (abs(D) + fscale)) &&  count < maxit ){
    
    # Forcing the Hessian to be positive definite ...
    if(min(eigen(hessian)$values) <= 0){ #Only used if Hessian isn't already
      k <- 1e-6 # Multiplying factor
      while (min(eigen(hessian)$values) <= 0){ # Stops at positive definiteness
        norm <- norm(hessian) # Find norm of Hessian
        hessian <- hessian + diag(ncol(hessian)) * k * norm # Update Hessian
        k <- k * 10 # Increase multiplying factor for next loop
      }
      
      # If this loop is entered, print the given warning to alert the user.
      warning("The Hessian is being forced to positive definiteness.")
    }
    
    half_count <- 0 # Counter for number of times a step is halved
    count <- count + 1 # Update number of iterations by 1
    inv_hessian <- chol2inv(chol(hessian)) # Invert Hessian using chol2inv 
    step <- - inv_hessian %*% gradient # Define the step we are going to take 
    # away from theta
    
    # Repeats halving the step (and calculating the Taylor approximation
    # at this point) until we have reached the max number of halvings
    while (half_count < max.half){
      # Defines the Taylor approximation of the function at theta + step
      T_func <- D + t(step) %*% gradient + 0.5*t(step) %*% hessian %*% step
      
      if (is.finite(T_func) == TRUE){ # If the approximation is finite ...
        
        if (T_func < D){ # ...and if the approximation is less than the value...
          theta = theta + step # ...update theta, as we have moved in the right
          # direction
          break # Leave the loop as this new theta is suitable for the iteration
        }
        
        else { # If the approximation isn't less than the value...
          step <- step/2 #... half the step, and repeat the process above
          half_count <- half_count + 1 # Update the number of halvings
        }
      }
      else{ # If the approximation is not finite...
        # ...stop the function and print the given warning
        stop("The objective or derivatives are not finite at theta.")
      }
    }
    
    # If we leave the loop of halvings because we have reached the maximum..
    if (half_count == max.half){
      # ... stop the function and print the given error
      stop("The step failed to reduce the objective despite trying halving it 
           the maximum established number of times.")   
    }
    
    # Updates these values to be used in the next iteration, at the new theta
    D <- func(theta, ...) # the value of the function 
    gradient <- grad(theta, ...) # the gradient
    
    # Hessian matrix
    if (is.null(hess) == TRUE){ # Again, if no hess provided, use our function
      hessian <- makehessian(theta, grad, eps, ...)
    }
    else{ # If hess function is provided, use this to generate Hessian
      hessian <- hess(theta, ...)
    }
  }
  
  # If we stop iterating because we have reached the max number of iterations..
  if (count == maxit){
    # ..stop the function and print the given error
    stop("The maximum number of iterations has been reached without
         convergence.")
  }
  
  # We have stopped iterating: generate the final Hessian matrix, either with
  # our own premade function or with the given hess function
  if (is.null(hess) == TRUE){
    hessian <- makehessian(theta, grad, eps, ...)
  }
  else{
    hessian <- hess(theta, ...)
  }
  
  if (all(eigen(hessian)$values > 0) == TRUE){#If the Hessian is positive 
    #definite...
    
    # Call chol2inv function to generate the inverse of the final Hessian
    inv_hessian <- chol2inv(chol(hessian))
    
    # Returns a list containing the value of the objective at the minimum,
    # the gradient at this point, the final value of theta which minimizes the 
    # objective function, the number of iterations taken to achieve this, and 
    # the inverse of the Hessian at the minimum
    output <- list(f = D, g = gradient, theta = theta, iter = count, 
                   Hi = inv_hessian)
    
    return(output)
  }
  
  else{ #if the Hessian is not positive definite...
    warning("The Hessian is not positive definite, so the inverse is not
            returned.") #Raise a warning
    
    # Returns a list containing the value of the objective at the minimum,
    # the gradient at this point, the final value of theta which minimizes the 
    # objective function and the number of iterations taken to achieve this.
    output <- list(f = D, g = gradient, theta = theta, iter = count)
    
    return(output)
  }
  
} 
