#Function for T_test:

T_test <- function(data, newvar = 0, t)
{
  if(!is.data.frame(data))
  {
    stop("The given object is not a data frame") 
  }
  
  {if (newvar == 0)
  {
    d=data
  }
    else
    {
      d=data[,newvar]
    }}
  
  #initializing the count of numerical variables
  count = 0
  
  #initializing vectors
  name_p = c()      #to store name of numerical variable
  test = c()        #to store p-values
  name_t = c()      #to store name of target variable
  result = c()      #to store result
  
  for(i in 1:ncol(d))
  {
    
    if(is.numeric(d[,i]))
    {
      
      count = count + 1
      name_p[count] <- names(d[i])
      name_t[count] <- names(d[t])
      test[count]   <- t.test(d[,i], y = d[t],
                              alternative = c("two.sided", "less", "greater"),
                              mu = 0, paired = FALSE, var.equal = FALSE, conf.level = 0.95)$p.value
      result[count] <- ifelse(test[count]<0.05,'Reject Null','Fail to Reject Null')
    }
    
  }
  
  k = data.frame(Variable = c(name_p), Target = c(name_t), P_value = c(test), Result = c(result))
  write.table(k, file="T_test.csv",append=TRUE, row.names=FALSE, sep = ",")
}

setwd("C:\\Users\\lenovo\\Documents")
getwd()

T_test(Boston,,2)




#Function for Chi-Square Test:



Chisqr_test<- function(data, newvar = 0, t)
{
  response=data[,t] #to store the target variable
  
  if(!is.data.frame(data))
  {
    stop("The given object is not a data frame") 
  }
  
  {if (newvar == 0)
  {
    d=data
  }
    else
    {
      d=data[,newvar]
    }}
  
  #initializing the count of categorical variables
  count = 0
  
  #initializing vectors
  name_t = c()     #to store name of target variable for factor
  name_c = c()     #to store name of categorical variable
  test   = c()     #to store p-values
  result = c()     #to store result
  
  for(i in 1:ncol(d))
  {
    
    if(is.factor(d[,i]))
    {
      count = count + 1
      
      tbl <- table(d[,i], response)
      
      name_t[count] <- names(d[t])
      name_c[count] <- names(d[i])
      test[count]   <- chisq.test(tbl)$p.value
      result[count] <- ifelse(test[count]<0.05,'Reject Null','Fail to Reject Null')
    }
  }
  
  l = data.frame(Predictor = c(name_c), Target = c(name_t), P_value = c(test), Result = c(result))
  write.table(l, file="Chisqrtest.csv",append=TRUE, row.names=FALSE, sep = ",")
}


Chisqr_test(Boston1,,2)



# T_test and Chi-Square test Function:

Impvariables <- function(data, newvar = 0, t)
{
  response = data[,t] #to store the target variable
  
  options(warn = -1)
  
  if(!is.data.frame(data))
    stop("The given object is not a data frame")
  {if (newvar == 0)
  {
    d = data
  }
    else
    {
      d = data[,newvar]
    }}
  
  
  #initializing the count of numerical and categorical variables
  
  count_n = 0
  count_c = 0
  
  
  #initializing vectors
  
  name_t = c()      #to store name of numerical variable
  test_n = c()      #to store p-values of T-test
  name_k = c()      #to store name of target variable for numeric
  name_r = c()      #to store name of target variable for factor
  name_c = c()      #to store name of categorical variable
  test_c = c()      #to store p-values of Chi-square test
  result_n = c()    #to store result for t-test
  result_c = c()    #to store result for chi-square test
  
  for(i in 1:ncol(d))
  {
    
    if(is.numeric(d[,i])) #check if the variable is numeric
    {
      
      count_n = count_n+1
      name_t[count_n] <- names(d[i])
      name_k[count_n] <- names(d[t])
      test_n[count_n] <- t.test(d[,i], y = d[t],
                              alternative = c("two.sided", "less", "greater"),
                              mu = 0, paired = FALSE, var.equal = FALSE,
                              conf.level = 0.95)$p.value
      result_n[count_n] <- ifelse(test_n[count_n]<0.05,'Reject Null','Fail to Reject Null')
    }
    
    
    if(is.factor(d[,i])) #check if the variable is a factor
    {
      count_c = count_c+1
      tbl <- table(d[,i], response  )
      name_r[count_c] <- names(d[t])
      name_c[count_c] <- names(d[i])
      test_c[count_c] <- chisq.test(tbl)$p.value
      result_c[count_c] <- ifelse(test_c[count_c]<0.05,'Reject Null','Fail to Reject Null')
    }
  }
  k = data.frame(Variable = c(name_t), Target = c(name_k), P_value = c(test_n), Result = c(result_n))
  write.table(k, file="T_test.csv", append=TRUE, row.names=FALSE, sep = ",")
  l = data.frame(Predictor = c(name_c), Target = c(name_r), P_value=c(test_c), Result = c(result_c))
  write.table(l, file="Chisqrtest.csv", append=TRUE, row.names=FALSE, sep = ",")
}

setwd("C:\\Users\\lenovo\\Documents")
getwd()

Impvariables(Boston,,2)


Boston1 <- Boston
str(Boston1)
Boston1$chas <- as.factor(Boston$chas)
Boston1$rad <- as.factor(Boston$rad)


Impvariables(Boston1,,2)
