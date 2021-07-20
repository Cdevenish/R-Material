### Function to reduce variables by colinearity using VIF ###

viffer <- function(x, z = 5, keep = NULL, df = FALSE){
  
  ## function will suggest a set of numeric variables which are not collinear as determined
  ## by a VIF threshold. Will remove variables with max VIF above z, one by one, until all VIF are below
  ## threshold or just max of 2 or length(keep) variables remain.
  
  # x is a data frame with numeric columns only. If a matrix it is coerced to data frame
  # z is a threshold for VIF, typically, 2, 5, or 10
  # keep - character vector of variable names (matching colnames in x) of variables to keep always
  # df - return a data frame of only VIF values for remaining variables (default) with variable names in
  # rownames, or (TRUE) return a data frame with final set of variables, with collinear variables removed
  
  ## Functions for VIF adapted from Zuur 2009 AED package #####
  ## from Mixed Effects models and extensions in ecology with R
  ## Adapted by combining into one function (corvif and myvif)
  
  ## The function is currently defined as
  vif <- function(dataz) {
    
    dataz <- as.data.frame(dataz)
    tmp_cor <- cor(dataz,use="complete.obs")
    form <- formula(paste("fooy ~ ", paste(strsplit(names(dataz)," "), collapse=" + ")))
    dataz <- data.frame(fooy=1, dataz)
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
  
  
  # start viffer function
  if("matrix" %in% class(x)) x <- data.frame(x)
  
  if(!all(sapply(x, is.numeric))) stop("All columns should be numeric")
  
  if(!is.null(keep)) {
    if(any(!keep %in% colnames(x))) stop("Variables in keep must match colnames of x")
    varN <- max(2, length(keep)) # adjust minimum number of vars to remain at end
  } else varN <- 2
  
  z1 <- vif(x)
  counter <- 0
  
  while(max(z1$GVIF[!rownames(z1) %in% keep]) >= z & nrow(z1) > varN){
    
    counter <- counter + 1
    ind <- which(z1$GVIF[!rownames(z1) %in% keep] == max(z1$GVIF[!rownames(z1) %in% keep]))
    del <- rownames(z1)[!rownames(z1) %in% keep][ind]
    if(length(del) > 1){
      del <- sample(del,1)
      warning("Identical VIF scores, choosing one at random")
    }
    
    cat("step ", counter, ": ", del, " deleted\n", sep = "")
    x[, del] <- NULL
    # head(x)
    
    z1 <- vif(x)
    
  }
  cat("\n")
  
  if(!df) return(z1) else return(x)
}

# # testing
# set.seed(99)
# x <- as.data.frame(lapply(1:5, function(x) rnorm(100, x)))
# x <- cbind(x, t(t(x)*(c(1,2,5,10,11))))
# colnames(x) <- letters[1:10]
# x[6:10] <- x[6:10] + as.data.frame(lapply(1:5, function(x) rnorm(100, 0, 2)))
# pairs(x)
# 
# viffer(x)
# 
# viffer(x, keep = c("h", "c"))


