### Function to reduce variables by colinearity using VIF ###

viffer <- function(x, z = 5){
  
  ## function will reduce a set of numeric variables to those which are not collinear as determined
  ## by a VIF threshold. Will delete variables with max VIF, one by one, until all VIF are below threshold
  ## or just two variables remain.
  
  # x is a data frame with numeric columns only. If a matrix it is coerced to data frame
  # z is a threshold for VIF, typically, 2, 5, or 10
  
  #
  # if(!existsFunction("vif")) source("../Fn2Sauce/VIF.r") ## copy in the function here.... 
  
  ## Functions for VIF from Zuur 2009  AED package #####
  ## from Mixed Effects models and extensions in ecology with R
  ## Adapted by combining into one function (corvif and myvif)
  
  ## The function is currently defined as
  vif <- function(dataz) {
    
    dataz <- as.data.frame(dataz)
    
    ## include the myvif function as well
    
    tmp_cor <- cor(dataz,use="complete.obs")
    
    form <- formula(paste("fooy ~ ",paste(strsplit(names(dataz)," "),collapse=" + ")))
    dataz <- data.frame(fooy=1,dataz)
    #head(dataz)
    mod <- lm(form,dataz)
    
    v <- vcov(mod)
    assign <- attributes(model.matrix(mod))$assign
    if (names(coefficients(mod)[1]) == "(Intercept)") {
      v <- v[-1, -1]
      assign <- assign[-1]
    } else warning("No intercept: vifs may not be sensible.")
    terms <- labels(terms(mod))
    n.terms <- length(terms)
    if (n.terms < 2) stop("The model contains fewer than 2 terms")
    if (length(assign) > dim(v)[1] ) {
      diag(tmp_cor)<-0
      if (any(tmp_cor==1.0)){
        return("Sample size is too small, 100% collinearity is present")
      } else {
        return("Sample size is too small or one variable has been dropped due to singularities. Use alias() on model")
      }
    }
    R <- cov2cor(v)
    detR <- det(R)
    result <- matrix(0, n.terms, 3)
    rownames(result) <- terms
    colnames(result) <- c("GVIF", "Df", "GVIF^(1/2Df)")
    for (term in 1:n.terms) {
      subs <- which(assign == term)
      result[term, 1] <- det(as.matrix(R[subs, subs])) * det(as.matrix(R[-subs, -subs])) / detR
      result[term, 2] <- length(subs)
    }
    if (all(result[, 2] == 1)) {
      result <- data.frame(GVIF=result[, 1])
    } else {
      result[, 3] <- result[, 1]^(1/(2 * result[, 2]))
    }
    
    return(result)
    
  }
  
  
  
  if(any(sapply(x, class) != "numeric")) stop("All columns should be numeric")
  
  if(class(x) == "matrix") x <- data.frame(x)
  
  z1 <- vif(x)
  counter <- 0
  
  while(max(z1$GVIF) >= z & nrow(z1) > 2){
    counter <- counter + 1
    del <- which(z1$GVIF == max(z1$GVIF))
    if(length(del) > 1){
      del <- sample(del,1)
      warning("Identical VIF scores, choosing one at random")
    }
    
    cat("step ", counter, ": ", colnames(x)[del], " deleted\n", sep = "")
    x[,del] <- NULL
    # head(x)
    z1 <- vif(x)
    
  }
  cat("\n")
  return(z1)
}

