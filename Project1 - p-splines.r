# Pablo Ruiz Barnada - s1854579

# This file contains four functions, the first one is used for fitting P-splines 
# to x, y data, choosing the smoothing parameter by GCV and returning an object  
# of class pspline, and the next three are method functions that can print, 
# predict and plot objects of this class. The idea behind smoothing data is the
# following: there is a model as y = f(x) + e, where multiple x and y are
# observed, and e is a zero mean error with variance sigma^2, and the function
# f is unknown and smooth. Then, f can be approximated by basis expansion, 
# where you need to estimate the coefficients (beta_hats). To avoid choosing
# excesively many basis functions and produce over-fitting in the model, we 
# impose a smoothing penalty so that the beta_hats vary smoothly from one to the
# next. 
# In our case, the smoothing penalty will be the matrix product
# beta_hats^{T} * D^{T} * D * beta_hats, where D is a (k − 2) × k matrix such 
# that D_{i,i} = D_{i,i+2} = 1, D{i,i+1} = −2, and the remaining elements are 
# zeroes. The beta_hats are estimated by penalized least squares, where the 
# smoothing parameter multiplies the smoothing penalty, and this has to be 
# minimized together with the squares of the residuals. 
# The smoothing parameter (lambda) is chosen by minimizing the GCV score,
# GCV = sig2 / (n-edf), where sig2 is the variance of the residuals, n is
# the number of observations of the model, and edf represents the effective 
# degrees of freedom of the model. 
# In order to calculate these in an efficient manner, some  previous 
# matrix decomposition is preferred. Particularly, the QR decomposition of
# the x-data matrix and definition of the eigen-decomposition as
# U^{T} * CLambda * U = R^{-T} * D^{T} * D * R{-1}
# leads to 
# beta_hats = R^{-1} * U * (I + lambda * Clambda)^{-1} * U^{T} * Q^{T} * y,
# where the matrices R and Q come from the QR decomposition, U is a matrix whose 
# columns are the normalized eigenvectors of R^{-T} * D^{T} * D * R{-1}, I is  
# the identity matrix, lambda is the smoothing parameter, and CLambda is a  
# diagonal matrix of the eigenvalues of R^{-T} * D^{T} * D * R{-1}.
# This method is computationally efficient since the matrix
# (I + lambda * Clambda) is diagonal, and therefore easily invertible. This 
# is also helpful to calculate the effective degrees of freedom, which will just 
# be the trace of this inverted matrix.



pspline <- function (x, y, k = 20, logsp = c(-5, 5), bord = 3,
                     pord = 2, ngrid = 100){
 
# This function takes as inputs the data vectors x, the independent variables,
# and y, the dependent variable, and will return the best fit spline
# smoother. It also takes the inputs k, logsp, bord, pord and ngrid. k is the 
# number of basis functions used to approximate an unknown function f, which 
# is set to be 20 by default. logsp, which is set by default to be the vector
# containing -5 and 5, represents the interval limits in which to look for
# optimal smoothing parameter (lambda), and it is given in the log lambda scale.
# bord is the b-spline order to be used, and is set to 3 by default. pord is
# the order of difference to use in the penalty, and by default is 2.
# Finally, ngrid is the number of smoothing parameters that the function
# evaluates, and these will be evenly spaced on the log scale as established
# by logsp. ngrid is set to 100 by default.
# The function returns a list of elements defining the best fit spline smoother,
# and the elements that are required for the following funtions. The list of
# elements returned contains the optimal coefficients estimated (coef),
# the fitted values of the model (fitted), the residual variance (sig2), 
# the effective degrees of freedom (effective_df), the optimal value of the 
# smoothing parameter (lambda) and its GCV (gcv), and other
# terms associated with the input data and the matrix design for the 
# function to work (knots, bord, the values of some recurrent matrices).
  
  dk <- diff(range(x))/(k-bord) # knot spacing
  knots <- seq(min(x)-dk*bord,by=dk,length=k+bord+1) #define all knots
  X <- splines::splineDesign(knots,x,ord=bord+1,outer.ok=TRUE) #set up basis X.
  D <- diff(diag(k),differences=pord) #set up matrix of zeros except the 
    #elements D_{i,i} = D_[i,i+2} = 1, D_{i,i+1} = -2 by default, can be 
    #altered by changing the value of pord.
  
  qr_decomposition <- qr(X) #QR decomposition of matrix X
  r_inverse <- solve(qr.R(qr_decomposition)) #Define the inverse of matrix R,
                                            #from the QR decomposition

  eigen_decomposition <- eigen(t(r_inverse) %*% crossprod(D)  %*%
            r_inverse, symmetric = TRUE) #Define eigen-decomposition 
  eigenvalue_matrix <- diag(eigen_decomposition$values) #Define matrix Capital
  #Lambda, diagonal matrix with the eigenvalues of the eigen-decomposition.
  eigen_vector <- eigen_decomposition$vectors #Define matrix U, made up of
  #the eigenvectors of the eigen-decomposition.
  
  all_sp <- seq(from = logsp[1], logsp[2], length.out = ngrid) #set all the
  #smoothing values that have to be evaluated, in the log scale.
  beta_hats <- c() #Create vector to store values of estimated coefficients
  #for each smoothing parameter
  edfs <- c() #Create vector to store values of edf for each smoothing parameter
  mu_hats <- c() #Create vector to store values of fitted values for each
  #smoothing parameter
  sigma_squares <- c() #Create vector to store values of estimated variance of
  #residuals for each smoothing parameter
  gcvs <- c() #Create vector to store GCV scores for each smoothing parameter
  
  for (i in all_sp){ #for each smoothing parameter in log scale...
    lambda <- exp(i) #... define the smoothing parameter in natural scale
    
    
    ilL_inverse <- backsolve(diag(ncol(eigenvalue_matrix)) + 
        lambda * eigenvalue_matrix, diag(ncol(eigenvalue_matrix))) #Store value
    #of the inverse of the matrix (I + smoothing_parameter*Capital_Lambda), 
    #where I is the identity matrix of required dimensions
    
    beta_hat <- c(r_inverse %*% eigen_vector %*% ilL_inverse %*%
       t(eigen_vector) %*% t(qr.Q(qr_decomposition)) %*% y) #Store estimated
    #coefficients vector
    
    edf <- sum(diag(ilL_inverse)) #calculate edf
    
    mu_hat <- c(X %*% beta_hat) #calculate fitted values
    
    sigma_squared <- (norm(y - mu_hat, type = "2")^2) / (length(y) - edf)
      #Estiamte residuals variance
    gcv <- sigma_squared/(length(y)-edf) #Calculate GCV score
    
    beta_hats <- append(beta_hats, list(beta_hat)) #add estimated coefficients
    #to the vector that keeps the estimated coefficients for every smoothing
    #parameter
    edfs <- append(edfs, edf) #add edf to the vector that keeps the edfs for 
    #every smoothing parameter
    mu_hats <- append(mu_hats,list( mu_hat)) #add fitted values to the
    #vector that keeps the fitted values for every smoothing parameter
    sigma_squares <- append(sigma_squares, sigma_squared) #add estimated
    #residuals variance to the vector that keeps the residuals variance
    #for every smoothing parameter
    gcvs <- append(gcvs, gcv) #add GCV to the vector that keeps the
    #GCVs for every smoothing parameter
  }  
  
  min_gcv <- min(gcvs) #Find the smallest GCV in the list of GCVs for 
  #all smoothing parameters
  index <- match(min_gcv, gcvs) #Find the position of that minimum GCV
  optimal_lambda <- exp(all_sp[index]) #Store the optimal smoothing paramter
  #which yields the lowest GCV.
  
  return(list(coef = beta_hats[[index]], fitted = mu_hats[[index]], 
       sig2 = sigma_squares[index], effective_df = edfs[index], knots = knots, 
       gcv = gcvs[index], lambda = optimal_lambda, x = x, y = y, 
       r_inverse = r_inverse, U = eigen_vector, bord = bord, pord = pord, k = k,
       ilL_inverse = backsolve(diag(ncol(eigenvalue_matrix)) + optimal_lambda * 
       eigenvalue_matrix, diag(ncol(eigenvalue_matrix))))) #return list
  #of class pspline that define the best fit spline smoother, and other 
  #values needed by the following functions.
}



print.pspline <- function (m){
  
  # This method function only takes one input, m, an object of class pspline,
  # and returns some details of the model fit m. In particular, it returns
  # the order of the p-spline and the order of the penalty,
  # the edf, the number of coefficients, the residual standard deviation,
  # the R^2 of the model and the GCV. It also returns silently a list containing
  # R^2, GCV and edf.
  
  mean = mean(m$y) #calculate mean value of y 
  number_of_observations = length(m$y) #Claculate umber of observations of y
  
  sum_of_squares <- sum((m$y - mean)^2) #Calculate the sum of squares
  r_squared <- 1 - ((number_of_observations - 1) * m$sig2) / sum_of_squares
       #calculates R^2 value

  cat("Order",m$bord, "p-spline with order", m$pord , 
      "penalty", "\nEffective degrees of freedom:", signif(m$effective_df, 7), 
      "Coefficients:", m$k, "\nresidual std dev:", signif(sqrt(m$sig2),7), 
      "r-squared:", signif(r_squared,7), "GCV:", signif(m$gcv,7))

  
  return(invisible(list(gcv = m$gcv, edf = m$effective_df,  r2 = r_squared)))
}



predict.pspline <- function (m, x, se = TRUE){
  
  # This function makes predictions from the smooth fit for some given data.
  # Therefore, it takes x, a data matrix with values that - preferably - have
  # to be within the range of the data passed to pspline. It also takes the
  # model fit from pspline, from which it takes the estimated coefficients,
  # certain matrices and the values for bord and k. The function also
  # requires a boolean input, se, set to TRUE by default, which alters what the
  # function returns. In particular, if se is TRUE, then the function returns
  # a 2-item named list, with the predicted values in the "fit" item, 
  # and their respective standard errros in the "se" item. If the boolean is
  # forced to FALSE, only a vector with the predictions is returned. The 
  # predictions are made by setting up a new matrix for the new data.


  dk <- diff(range(x))/(m$k-m$bord) # knot spacing
  knots <- seq(min(x)-dk*m$bord,by=dk,length=m$k+m$bord+1) #define all knots
  Xp <- splines::splineDesign(knots,x,ord=m$bord+1,outer.ok=TRUE) #Set up new
      #basis matrix for the new data - x.
  
  predic <- Xp %*% m$coef #Predict the value of the dependent variable according
                #to the new data
  V <- (m$r_inverse %*% m$U %*% (m$ilL_inverse) %*%
          t(m$U) %*% t(m$r_inverse)) * m$sig2 #Find the covariance matrix
                #for the coefficients
  st_error <- rowSums(Xp * (Xp %*% V))^0.5 #Calculate the standard errors
  
  if (se == TRUE){ #If the boolean is set to TRUE...
    return(list(fit = predic, se = st_error)) #... Return a 2-item named list 
                #with the predictions and their standard errors
  }
  
  else{ #Othersise...
    return (predic)} #... return a vector with the predicted values.
}



plot.pspline <- function (m) {
  
  # This function produces three different plots from a unique input,
  # m, an object of class pspline. The plots appear in the following
  # order: Firstly, a compound graph that plots the original x,y data,
  # alongside the estimated smooth function and its 95% credible intervals.
  # Next, a plot of the model residuals against the fitted values will
  # be produced. Lastly, a QQ-plot of the residuals is generated. 
  # This function also returns silently a list with the vectors defining
  # lower and upper confidence intervals and the correspoding x values of 
  # the smooth function.
   
  
  x <- seq(min(m$x), max(m$x), length = length(m$x)) #From the original X data
  #obtain the range and generate N evenly spaced datapoints in that range,
  #where N is the number of points in the original X dataset.
  p <- predict.pspline(m, x, se = TRUE) #Use the above generated data and the
  #previously defined prediction.pspline function to make predictions from the
  #smooth fit
  
  ul <- p$fit + 1.96*(p$se) #Calculate upper confidence limit
  ll <- p$fit - 1.96*(p$se) #Calculate lower confidence limit
  
  plot(m$x, m$y, xlab = expression(x), ylab = expression(y)) #Plot original data
  lines(x, p$fit) #Plot estimated smooth function 
  polygon(c(x,rev(x)),c(ul,rev(ll)), col=rgb(1, 0, 0,0.1), border = FALSE)
    #Define 95% credible region
  lines(x, ul, col="red",lty=2) #Highlight upper confidence limit
  lines(x, ll, col="red",lty=2) #Highlight lower confidence limit
  abline(a = 0, b = 0, lty = 3) #Add a line through y = 0
  
  title("Original x,y data, smooth function and 95% credible interval")
    #Add title to the graph
  
  
  plot(m$fitted, m$y - m$fitted, xlab = "Fitted values", ylab = "Residuals")
    #Plot residuals against fitted values
  abline(a = 0, b = 0, lty = 2) #Add a line through residual = 0
  title("Residuals plotted against the fitted values") #Add title

  
  qqnorm(m$y-m$fitted) #QQ-plot of residuals
  qqline(m$y-m$fitted) #Add a theoretical line to the QQ-plot   
  
  return (invisible(list(ll =  ll, ul = ul, x = x))) #return invisible list
   #with confidence limits and the x values of the smooth function
} 
  
