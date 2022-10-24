
# Generate the data
n = 200
xs = seq(-2,2, length.out=n)
ys = exp(1.5*xs - 1) + rnorm(n, 0, 0.64)

# # Choose default hyperparameters (max degree of feature transform, deg, and regularization coefficient, lambda)
# deg    = 7
# m      = deg+1
# lambda = 4


#' Perform feature transformation of input data x
#'
#' @param x input data
#' @param d maximum degree of feature transform
#'
#' @return transformed data x
phi <- function(x, d=deg){
  phiX = matrix(NA, nrow=length(x), ncol=d+1)
  for(i in 1:length(x)){
    row = rep(x[i], d+1)
    for (j in 0:d){
      row[j+1] = row[j+1]**j
    }
    phiX[i, ] = row
  }
  return(phiX)
}

#' Find regularized least-squares weights
#'
#' @param X input values
#' @param y target values
#' @param regcoeff regularization coefficient
#'
#' @return vector of optimal weights
fit <- function(X, y, regcoeff=lambda){
  w <- solve(t(X) %*% X + diag(regcoeff, dim(X)[2], dim(X)[2])) %*% t(X) %*% y
  return(w)
}

#' Calculate the predicted output value for a single input x using weights w
#'
#' @param x input value
#' @param w weights
#'
#' @return predicted output
predict <- function(x, w){
  y = 0
  for (i in 0:length(w)-1){
    y = y + w[i+1] * (x**i)
  }
  return(y)
}

#' Compute mean CV error (leave-one-out CV)
#'
#' @param xs input values
#' @param ys target values
#' @param deg maximum degree of feature transform
#' @param lambda regularization coefficient
#'
#' @return mean CV error
crossValidation <- function(xs=xs, ys=ys, deg=deg, lambda=lambda){
  err = 0
  for (i in 1:length(xs)){
    phiX = phi(xs[-i], deg)
    w = fit(phiX, ys[-i], lambda)
    predYs = phiX %*% w
    err = err + norm(ys[-i]-predYs)
  }
  return(err/length(xs))
}

findOptimumDegAndLambda <- function(xs, ys, dValues=1:8, lambdaVals=c(seq(0,5,length.out=26), 6:25, seq(30, 100, length.out=15)), verbose=TRUE){
  optErr = Inf
  optLambda = NA
  optD = NA

  if(verbose){
    print("deg      lambda  err")
  }

  for(d in dValues){
    errs = c()

    optErrTemp = Inf
    optLambdaTemp = NA

    for(lambda in lambdaVals){
      err = crossValidation(xs, ys, d, lambda)
      errs = c(errs, err)
      if(verbose){
        # sprintf("lambda=%.4f, err=%.4f", lambda, err)
        print(c(d, lambda, err))
      }
      if (err < optErr){
        optErr = err
        optLambda = lambda
        optD = d
      }
      if (err < optErrTemp){
        optErrTemp = err
        optLambdaTemp= lambda
      }
    }
    par(mfrow=c(2,2))
    plot(lambdaVals, errs, main=sprintf("d=%s", d))

    for (l in c(optLambdaTemp, lambdaVals[1], lambdaVals[length(lambdaVals)])){
      wForNow = fit(phi(xs, d), ys, l)


      predictGivenW <- function(x){
        return(phi(x, d) %*% wForNow)
      }

      plot(xs, ys, xlab="x", ylab="y", main=sprintf("d=%s, lambda=%s", d, l))#
      curve(predictGivenW, add=TRUE, col="red", lty=2, lwd=3)
      curve(exp(1.5*x -1), add=TRUE, lty=2, lwd=2)

    }

    # plotVsTrue(xs, ys, fit(phi(xs, d), ys, 68))
    my.name <- readline(prompt="next?")
  }
  return(c(d, optLambda))
}
# phiX = phi(xs, deg)
# w = fit(phiX, ys, lambda)


plotVsTrue <- function(xs, ys, w, sd=NA, title=""){
  par(mfrow=c(1,1))
  d = length(w)-1
  predictGivenW <- function(x){
    return(phi(x, d) %*% w)
  }

  predictGivenWPlusSd <- function(x){
    return((phi(x, d) %*% w) + sd)
  }
  predictGivenWMinusSd <- function(x){
    return((phi(x, d) %*% w) - sd)
  }

  plot(xs, ys, xlab="x", ylab="y", main=title)
  curve(exp(1.5*x -1), add=TRUE, lty=2, lwd=4)
  curve(predictGivenW, add=TRUE, col="red", lwd=2)
  # curve(phi(x)%*%w, add=TRUE, col="red", lwd=2)

  # Add standard deviation 'tube'
  if (!is.na(sd)){
    curve(predictGivenWPlusSd, add=TRUE, col="red", lty=2, lwd=2)
    curve(predictGivenWMinusSd, add=TRUE, col="red", lty=2, lwd=2)
  }

  legend("topleft", legend=c("True", "Prediction"),
         col=c("black", "red"), lty=c(2,1), cex=0.8)
  # lines(xs, phiX%*%w, col="red", lwd=2)
}

# Find optimal parameters
params = findOptimumDegAndLambda(xs, ys, verbose=FALSE)
deg    = params[1]
lambda = params[2]
# print(sprintf("optimum d=%s, lambda=%s", deg, lambda))

phiX = phi(xs, deg)
w = fit(phiX, ys, lambda)
predYs = phiX %*% w
variance = norm(ys - predYs)/(n-1)
print(variance)

# Find percentage of data points within 1 SD of prediction
diffs = abs(predYs - ys) - sqrt(variance)
count = length(diffs[diffs < 0])
print(sprintf("Total percentage of data points within the 1SD 'tube': %s", 100*count/length(xs)))

plotVsTrue(xs, ys, w, sqrt(variance), title=sprintf("optimum d=%s, lambda=%s", deg, lambda))
