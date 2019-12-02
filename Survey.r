library(ggplot2)
library(tidyverse)
library(stringr)
library(readr)
library(gridExtra)
library(binom)
library(gmodels)
library(MASS)
library(goftest)
library(curl)
library(gsheet)

if(curl::has_internet()) {
  print("Internet: TRUE")
  temp <-  gsheet2tbl('https://docs.google.com/spreadsheets/d/1CLEozMTkZEtuf8743O1_R8gzvqAXUdb29YBbtxPc9Lc/edit#gid=687952067')  
} else {
  print("Internet: FALSE")
  Canteen_Campus_Chill_Survey <- read_csv("Canteen & Campus Chill Survey.csv")
  temp <- Canteen_Campus_Chill_Survey
} 

#for(i in which(temp[[3]] == "Prefer not to say")){
#  temp[[i, 3]] <- "Male"
#}

temp[[11]] <- as.numeric(temp[[11]])
temp[[12]] <- as.numeric(temp[[12]])

foodChill <- temp[[10]]
foodCanteen <- temp[[9]]

uniqueLst <- factor(foodChill)

newtemp <- str_split(foodChill, ",| and |&| or| ")
newtemp2 <- str_split(foodCanteen, ",| and |&| or| ")

correctionList <- c(
  "samosa*" = "Samosa", 
  "^ice" = "Ice-Cream", 
  "^puf*" = "Puff" ,
  "^tea" = "Tea",
  "^cof*" = "Coffee",
  "^vada*" = "Vada-Pav", 
  "^chi*" = "Chicken"
)
removeList <- c("Roll", "Juice", "Cutlet", "Sandwich", "Toast") 

correctionList2 <- c(
  "^chi*"= "Chicken",
  "^biry*" = "Biryani",
  "^ko*" = "Kori",
  "^kebab*|^kabab" = "Kebab",
  "^rot*i" = "Roti",
  "^fried" = "Fried-Rice",
  "^no*dles" = "Noodles",
  "^sh*w*" = "Shawarma"
)

removeList2 <- c("Meal")

hypotheses <- seq(0, 1, 0.01)

transformTemp <- function(tempVar){
  newtemp <- tempVar
  newtemp[[3]] <- ifelse(newtemp[[3]] == "Male", 0, 1)
  newtemp[[11]] <- as.numeric(newtemp[[11]])
  newtemp[[17]] <- ifelse(newtemp[[17]] == "Yes", 1, 0)
  newtemp[[21]] <- ifelse(newtemp[[21]] == "Yes", 1, 0)
  newtemp[[which(newtemp[[11]] == 800), 11]] <- 500
  return(newtemp)
}

RecTemp <- transformTemp(temp)

R <- function(sample){
  return(max(sample) - min(sample))
}

fList <- c(var, max, min, R)

countRegex <- function(word){
  lst <- str_detect(foodChill, regex(word, ignore_case = TRUE))
  wordLst <- foodChill[lst]
  print(lst)
  print(wordLst)
  return(sum(lst))
}

replaceRegex <- function(expr,newExpr){
  temp <- foodChill
  expr <- regex(expr, ignore_case = TRUE)
  temp <- str_replace_all(temp, expr, newExpr)
  return(temp)
}

changeLevels <- function(exp, newexp, fact){
  expr <- regex(exp, ignore_case = TRUE)
  newExpr <- newexp
  levels(fact) <- str_replace_all(levels(fact), expr, newExpr) 
  return(fact)
}



splitMixStringList <- function(list){ 
	stringList <- NULL
	for(i in list){
	  for(j in i){
	    #print(j)
	    if(j != "" && !is.na(j)){
	      #print(j)
	      stringList <- c(stringList, j)
	    }  
	  }
	}
	stringList <- tolower(stringList)
	return(stringList)
}

changeRegexV2 <- function(expr,newExpr, list){
   expr <- regex(expr, ignore_case = TRUE)
   newList <- NULL
   for(i in list){
     if(str_detect(i, expr)){
       newList <- c(newList,newExpr)
     } else {
       newList <- c(newList, i)
     }
   }
   return(newList)
 }

 removeExtra <- function(word, list){
   return(changeRegexV2(paste("^",word,"*",sep= ""), word, list))   
 }
 
correctAndRemove <- function(corList, remList, lookList){
  tempList <- lookList
  for(i in names(corList)){
    tempList <- changeRegexV2(i, corList[[i]], tempList)
  }
  for(i in remList){
    tempList <- removeExtra(i, tempList)
  }
  return(tempList)
}

CLT <- function(pop, sampleSize = 10, iter = 100){
  means <- NULL
  for(i in seq(1, iter, 1)){
    samples <- sample(pop, size = sampleSize, replace = FALSE)
    means <- c(means, mean(samples))
  }
  return(means)
}

someExtraPlotting <- function(col1 = temp[[11]], col2 = temp[[12]], sampleSize = 40, iter = 1000){
  magicScale <- 3
  canteenSpending <- as.numeric(col1)
  #print(canteenSpending)
  chillSpending <- as.numeric(col2)
  #print(chillSpending)
  
  binwidth1 <- max(canteenSpending)
  binwidth2 <- max(chillSpending)
  #aes(y = (..density..)*magicScale),
  canteenSpendingPlot <- ggplot() +
    aes(canteenSpending) + 
    geom_histogram(color = "black", fill = "White") #+
    #stat_function(fun = dpois, args = list(lambda = mean(canteenSpending)), color = "red")

  chillSpendingPlot <- ggplot() + 
    aes(chillSpending) + 
    geom_histogram(color = "black", fill = "White") #+
    #stat_function(fun = dpois, args = list(lambda = mean(chillSpending)), color = "red")
    
  samplingMeansCanteenSpend <- CLT(canteenSpending, sampleSize = sampleSize, iter = iter)
  #print(samplingMeansCanteenSpend)
  samplingMeansChillSpend <- CLT(chillSpending, sampleSize = sampleSize, iter = iter)
  #print(samplingMeansChillSpend)
  
  means <- c(mean(samplingMeansCanteenSpend), mean(samplingMeansChillSpend))
  sds <- c(sd(samplingMeansCanteenSpend), sd(samplingMeansChillSpend))
  means <- round(means,4)
  sds <- round(sds,4)
  
  #binwidth1 <- (max(canteenSpending) - min(canteenSpending))/length(canteenSpending)
  #print(binwidth1)
  #binwidth2 <- (max(chillSpending) - min(chillSpending))/length(chillSpending)
  #print(binwidth2)
  
  bins1 <- sort(canteenSpending)[length(canteenSpending) - 3]
  bins2 <- sort(chillSpending)[length(chillSpending) - 3]
  
  sampleCanteenPlot <- ggplot() + 
    aes(samplingMeansCanteenSpend) + 
    geom_histogram( aes(y = ..density..), color = "black", fill = "white") +
    xlab(paste(means[1], sds[1], sep=", ")) + 
    stat_function(fun = dnorm, args = list(mean = means[1], sd = sds[1]), color = "red") #+
    #stat_function(fun = dt, args = list(df = 222), color = "blue")
  
  sampleChillPlot <- ggplot() + 
    aes(samplingMeansChillSpend) +
    geom_histogram(aes(y = ..density..), color = "black", fill = "white") +
    xlab(paste(means[2], sds[2], sep=", ")) +
    stat_function(fun = dnorm, args = list(mean = means[2], sd = sds[2]), color = "red") #+
    #stat_function(fun = dt, args = list(df = 222), color = "blue")
  
  box1 <- ggplot(data = RecTemp) + geom_boxplot(mapping = aes(x = "Avg Canteen\nSpending", y = RecTemp[[11]])) + geom_boxplot(mapping = aes(x = "Avg Chill\nSpending", y = RecTemp[[12]]))
  box2 <- ggplot(data = RecTemp) + geom_boxplot(mapping = aes(x = "Avg Canteen\nRating", y = RecTemp[[13]])) + geom_boxplot(mapping = aes(x = "Avg Chill\nRating", y = RecTemp[[14]]))
  grid.arrange(box1, box2, canteenSpendingPlot,  chillSpendingPlot, sampleCanteenPlot,  sampleChillPlot, ncol=3)
}

cochranFormula <- function(p = 0.5, step = 0.001){
  q <- 1-p
  errors <- seq(step, 0.10, step)
  sampleSizes <- NULL
  for(i in errors){
    alpha <- i
    sampleSizes <- c(sampleSizes ,round(qnorm(1-alpha/2, mean = 0, sd = 1)**2*p*q/alpha**2,2))
  }
  df <- data.frame("alpha" = errors, "sampleSize" = sampleSizes)
  return(df)
}


confidenceInterval <- function(tableCategory, alpha = 0.05){
  #print(tableCategory)
  df <- data.frame("Categories" = names(tableCategory),binom.confint(x = tableCategory, n = sum(tableCategory), conf.level = 1 - alpha, methods = "all"))
  return(df)
}

# gender & spend
chiSquareTestIndependence <- function(col1 = temp[[17]], col2 = temp[[8]], table = NULL){
  if(!is.null(table)){
    table <- table[1:2, ]
  } else {
    table <- table(col1, col2)  
  }
  
  test <- chisq.test(table)
  print(table)
  print(test)
}

# mostly spending, ratingrelation with gender, academic year, academic section
regressionIndependenceTest <- function(formula, df){
  GLM <- glm(formula = formula,data = df, family = poisson("log"))
  LM <- lm(formula = formula, data = df)
  #print(GLM)
  #print(anova(GLM))
  print(LM["coefficients"])
  print(anova(LM))
  #ANVGLM <- anova(GLM)
  #ANVLM <- anova(LM)
}

bayesFactor <- function(hypothesesTop, hypothesesBottom, table, successName){
  success <- table[[successName]]
  total <- sum(table)
  (hypothesesTop^success * (1- hypothesesTop)^(total - success))/ (hypothesesBottom^success * (1- hypothesesBottom)^(total - success)) 
}

beta_bfsComparison <- function(table, successName, hypotheses){
  print("TABLE NAME: ")
  print(table)
  print("COLUMN NAME:")
  print(successName)
  success <- as.numeric(table[[successName]])
  total <- sum(table)
  dx <- hypotheses[2] - hypotheses[1]
  bfs <- bayesFactor(hypotheses, 0.5, table, successName)
  scaledBFS <-  dbeta(hypotheses , success + 1, total - success + 1)*dx
  normalizedBFS <- bfs/sum(bfs)
  df <- data.frame("hypotheses" = hypotheses, "SBFS" = scaledBFS, "NBFS" = normalizedBFS)
  
  ggplot(data = df) + 
    geom_line(mapping = aes(x = df[["hypotheses"]], y = df[["SBFS"]])) +
    geom_point(mapping = aes(x = df[["hypotheses"]], y = df[["NBFS"]]), color = "red", size = 1)
}

SpendingVsRating <-  function(){
  df <- data.frame(temp[[11]], temp[[12]], temp[[13]], temp[[14]])
  
  canteenLM <- lm(formula = temp[[11]] ~ temp[[13]]) 
  chillLM <- lm(formula = temp[[12]] ~ temp[[14]])
  
  canteenPlot <- ggplot(data = df, aes(x = temp[[13]], y = temp[[11]])) + geom_point() +
    xlab("Canteen Rating") + ylab("Canteen Spending") + 
    geom_smooth(method = 'lm')
  
  chillPlot <- ggplot(data = df, aes(x = temp[[14]], y = temp[[12]])) + geom_point() +
    xlab("Chill Rating") + ylab("Chill Spending") + 
  geom_smooth(method = 'lm')
  
  grid.arrange(canteenPlot, chillPlot, ncol = 2)
  
}

distribution <- function(pop = temp[[11]], sampleSize = 10, iter = 100, f){
  variates <- NULL
  for(i in seq(1, iter, 1)){
    samples <- sample(pop, size = sampleSize, replace = FALSE)
    variates <- c(variates, f(samples))
  }
  hist(variates, breaks = 30)
}


main <- function(){
  look <- splitMixStringList(newtemp)
  look2  <- splitMixStringList(newtemp2)
  look <- correctAndRemove(correctionList, removeList, look)
  look2 <- correctAndRemove(correctionList2, removeList2, look2)  
  
  countList <- count(as.data.frame(look),look)
  countList2 <- count(as.data.frame(look2),look2)
  
  orderedList <- countList[order(countList$n, decreasing = TRUE), ]
  orderedList2 <- countList2[order(countList2$n, decreasing = TRUE), ]
  
  #chiSquareTestIndependence(temp[[4]], temp[[7]])
  
  #someExtraPlotting(temp[[11]], temp[[12]])
  #someExtraPlotting(temp[[13]], temp[[14]])
  
  #chiSquareTestIndependence(temp[["Gender"]], temp[[17]])
  #chiSquareTestIndependence(temp[["Gender"]], temp[[21]])

  #regressionIndependenceTest(formula = temp[[11]] ~ temp[["Gender"]]*temp[[7]]*temp[[8]]*temp[[4]], data= temp)
  #regressionIndependenceTest(formula = temp[[12]] ~ temp[["Gender"]]*temp[[7]]*temp[[8]]*temp[[4]], data = temp)
  
  #hypotheses <- seq(0, 1, 0.01)
  #bayesFactor(hypothesesTop = hypotheses, hypothesesBottom = 0.5, table(temp[[7]]), "Occasionally")
  #bayesFactor(hypothesesTop = hypotheses, hypothesesBottom = 0.5, table(temp[[8]]), "Occasionally")
  
  beta_bfsComparison(table = table(temp[[3]]), successName = "Male", hypotheses = hypotheses)
  #graph2 <-beta_bfsComparison(table = table(temp[[7]]), successName = "Sometimes", hypotheses = hypotheses)
  
  #binConfidenceIntervals(temp[[3]])
  #binConfidenceIntervals(temp[[17]])
  #View(binConfidenceIntervals(temp[[3]]))
  #View(binConfidenceIntervals(temp[[17]]))
  #View(cochranFormula())
  
  #View(countList[order(countList$n, decreasing = TRUE), ])
  #View(countList2[order(countList2$n, decreasing = TRUE), ])

  #View(as.data.frame(look) %>% count(look))
  #View(count(as.data.frame(look2),look2))
  
  #SpendingVsRating()
}

if(interactive()) {
  #main()
}
