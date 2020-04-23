
#
#
#
# Load Packages
#
#
#

library(GA) 
library(e1071)

library(foreach)
#install.packages("doParallel")
library(doParallel)
#
#
#
# Load data 
#
#
#

setwd("C:/Users/Sean/Desktop/S1 BCUBDA Textbooks/Optimization/")
consume.raw <- read.csv("drug_consumption_cl.csv")

#
#
#
# Clean data 
#
#
#

consume.cl <- consume.raw

library(dplyr)
library(tidyr)

# Use decade based classification as in the original paper, this helps with the sparse dataset problem 
consume.cl <- consume.cl %>% 
  gather(Drug, Use_Cat, Alcohol:Heroin) %>% 
  mutate(Use_Bi = factor(ifelse(Use_Cat == "CL0" | Use_Cat == "CL1", 'Non-Users', 'Users'))) %>% 
  select(-Use_Cat) %>% 
  spread(Drug, Use_Bi)

#
#
#
# Test Data and Training Data: Holdout Method
# 
#
#

#index <- sample(1:dim(consume.cl)[1]) 
#htrain <- consume.cl[index[1:floor(dim(consume.cl)[1]/3)], ] 
#htest <- consume.cl[index[((ceiling(dim(consume.cl)[1]/3)) + 1):dim(consume.cl)[1]], ]

#htrain <- htrain[-1]
#htest <- htest[-1]

#htrain.nl <- select(htrain, -Heroin)
#htest.nl <- select(htest, -Heroin)

#htrain.l <- htrain[,c("Heroin")]
#htest.l <- htest[,c("Heroin")]

#
#
#
# 3 Fold Cross validation
#
#
#

set.seed(001)
folds <- caret::createFolds(consume.cl$Heroin, k = 3)

htrain1 <- consume.cl[-folds[[1]], ]
htest1 <- consume.cl[folds[[1]], ]

htrain2 <- consume.cl[-folds[[2]], ]
htest2 <- consume.cl[folds[[2]], ]

htrain3 <- consume.cl[-folds[[3]], ]
htest3 <- consume.cl[folds[[3]], ]

# Validate Even Groups - Between 12.6 and 12.7 % of pop in all samples
#table(htrain1$Heroin)
#table(htest1$Heroin)

#table(htrain2$Heroin)
#table(htest2$Heroin)

#table(htrain3$Heroin)
#table(htest3$Heroin)

#
#
#
# Baseline SVM 
#
#
#

# K1
model.base <- svm(Heroin ~ ., data=htrain1[-1])
model.svm.base <- predict(model.base, select(htrain1, -Heroin, -Id)) 

t.b <- table(model.svm.base, htrain1$Heroin)
sens.base <- t.b[2,2]/(t.b[2,2]+t.b[1,2])
spec.base <- t.b[1,1]/(t.b[1,1]+t.b[2,1])
t.b

sens.base1 <- sens.base
spec.base1 <- spec.base

# K2
model.base <- svm(Heroin ~ ., data=htrain2[-1])
model.svm.base <- predict(model.base, select(htrain2, -Heroin, -Id)) 

t.b <- table(model.svm.base, htrain2$Heroin)
sens.base <- t.b[2,2]/(t.b[2,2]+t.b[1,2])
spec.base <- t.b[1,1]/(t.b[1,1]+t.b[2,1])
t.b

sens.base2 <- sens.base
spec.base2 <- spec.base

# K3
model.base <- svm(Heroin ~ ., data=htrain3[-1])
model.svm.base <- predict(model.base, select(htrain3, -Heroin, -Id)) 

t.b <- table(model.svm.base, htrain3$Heroin)
sens.base <- t.b[2,2]/(t.b[2,2]+t.b[1,2])
spec.base <- t.b[1,1]/(t.b[1,1]+t.b[2,1])
t.b

sens.base3 <- sens.base
spec.base3 <- spec.base

sense.base.avg <- (sens.base1 + sens.base2 + sens.base3)/3
spec.base.avg <- (spec.base1 + spec.base2 + spec.base3)/3



#
#
#
# GA Fitness Function 
#
#
#

f <- function(x) 
{ 
  # SVM Params
  x1 <- x[(length(x)-1)] 
  x2 <- x[(length(x))] 
  
  # Feature Set 
  f.set <- x[1:((length(x)-2))]
  inc <- which(f.set >= 0.5)  # if we throw an error where none are selected then it doesnt go
  
  # Train
  f.cut <- htrain.nl
  f.sel <- f.cut[,inc]  
  
  # Test 
  f.cut.t <- htest.nl
  f.sel.t <- f.cut.t[,inc]
  
  # IF inc is none then give worst score, else run SVM
  f.lab <- htrain.l
  f.comp <- cbind(f.sel, f.lab)
  
  f.lab.t <- htest.l
  f.comp.t <- cbind(f.sel.t, f.lab.t)
  
  # Run SVM
  model <- svm(f.lab ~ .,data=f.comp, cost = x1, gamma = x2) 
  
  if (model$type == 0) {
    
    model.svm <- predict(model, f.sel.t) 
    t <- table(model.svm, f.lab.t) 
    
    # Fitness Function
    # Sensitivity
    sens <- t[2,2]/(t[2,2]+t[1,2])
    # Specificity 
    spec <- t[1,1]/(t[1,1]+t[2,1])
    # SVM Acc
    svm_acc <- sens * spec
    # Features 
    features.n <- (length(names(f.sel)))^-1
    
    return((0.35 * svm_acc) + (0.65 * features.n))    ############# CHANGE PROPORTIONS HERE ###########
    
  } else {
    
    return(0)
    
  } 
  
}

#
#
#
# Monitor Function 
#
#
#

monitor <- function(obj){
  # gaMonitor(obj)                      #call the default gaMonitor to print the usual messages during evolution
  iter <- obj@iter                      #get the current iternation/generation number 
  if (iter <= maxGenerations){          #some error checking
    fitness <- obj@fitness              #get the array of all the fitness values in the present population
    #<<- assigns a value to the global variable declared outside the scope of this function.    
    thisRunResults[iter,1] <<- max(fitness)
    thisRunResults[iter,2] <<- mean(fitness)    
    thisRunResults[iter,3] <<- median(fitness)
    cat(paste("\rGA | iter =", obj@iter, "Mean =", thisRunResults[iter,2], "| Best =", thisRunResults[iter,1], "\n"))
    flush.console()
  }  
  else{                               #print error messages
    cat("ERROR: iter = ", iter, "exceeds maxGenerations = ", maxGenerations, ".\n")
    cat("Ensure maxGenerations == nrow(thisRunResults)")
  }
}

#
#
# Run GA Function
#
#
#

runGA <- function(noRuns = 30) {

maxGenerations <<- 100

statnames = c("best", "mean", "median")
thisRunResults <<- matrix(nrow=maxGenerations, ncol = length(statnames)) #stats of a single run
resultsMatrix = matrix(1:maxGenerations, ncol = 1)  #stats of all the runs

resultNames = character(length(statnames)*noRuns)
resultNames[1] = "Generation"

svmResult = matrix(nrow=1, ncol = 5)
solutionVec = as.vector(rep(0,33))


for (i in 1:noRuns){
  
lower <- c(rep(0, 30), 1, 0.01)
upper <- c(rep(1, 30), 100, 10)

GA <- ga(type = "real-valued", fitness = f, lower = lower, upper = upper, popSize = 100, # Maybe 0-1 for feature selection vars- or just use all binary
         maxiter = maxGenerations, monitor = monitor, run = 20, elitism = 10, pmutation = .1, pcrossover =.9, parallel = TRUE )

resultsMatrix = cbind(resultsMatrix, thisRunResults)

#
# Add SVM Stats for Best of Each Run
#

solution.vec <- GA@solution[1,]

# SVM Params
x1 <- solution.vec[(length(solution.vec)-1)] 
x2 <- solution.vec[(length(solution.vec))] 

# Feature Set 
f.set <- solution.vec[1:((length(solution.vec)-2))]

inc <- which(f.set >= 0.5)  

# Train
f.cut <- htrain.nl
f.sel <- f.cut[,inc]  

# Test 
f.cut.t <- htest.nl
f.sel.t <- f.cut.t[,inc]

# Labels
f.lab <- htrain.l
f.comp <- cbind(f.sel, f.lab)

f.lab.t <- htest.l
f.comp.t <- cbind(f.sel.t, f.lab.t)

# Run SVM
model <- svm(f.lab ~ .,data=f.comp, cost = x1, gamma = x2)
model.svm <- predict(model, f.sel.t) 
t <- table(model.svm, f.lab.t)

# Report 
# Fitness Value 
fit <- round(GA@fitnessValue, 3)
# Sensitivity
sens <- t[2,2]/(t[2,2]+t[1,2])
# Specificity 
spec <- t[1,1]/(t[1,1]+t[2,1])
# Features 
features.n <- length(names(f.sel))
# Generation
generation <- i

# SVM Stats
runstats <- cbind(generation, sens)
runstats <- cbind(runstats, spec)
runstats <- cbind(runstats, features.n)
runstats <- cbind(runstats, fit)

svmResult <- rbind(svmResult, runstats)

# GA Best Solution Features
  
solution.vec.g <- append(generation, solution.vec)
solutionVec <- rbind(solutionVec, solution.vec.g)

#Create column names for the resultsMatrix
for (j in 1:length(statnames)) resultNames[1+(i-1)*length(statnames)+j] = paste(statnames[j],i)
}

colnames(resultsMatrix) = resultNames

resultsMatrix <<- resultsMatrix
svmResult <<-svmResult[-1,]
solutionVec <<- solutionVec[-1,]

return (svmResult)

}


#
#
#
# Run GA with 3-Fold Cross Validation
# 
#
#

#
#
#
# Experiment 1 - Accuracy = .8 Features = .2
#
#
#

## Run first fold   

htrain <- htrain1[-1]
htest <- htest1[-1]

htrain.nl <- select(htrain, -Heroin)
htest.nl <- select(htest, -Heroin)

htrain.l <- htrain[,c("Heroin")]
htest.l <- htest[,c("Heroin")]

runGA()

resultsMatrix1 <- resultsMatrix
solutionVec1 <- solutionVec
svmResult1 <- svmResult

## Run Second Fold

htrain <- htrain2[-1]
htest <- htest2[-1]

htrain.nl <- select(htrain, -Heroin)
htest.nl <- select(htest, -Heroin)

htrain.l <- htrain[,c("Heroin")]
htest.l <- htest[,c("Heroin")]

runGA()

resultsMatrix2 <- resultsMatrix
solutionVec2 <- solutionVec
svmResult2 <- svmResult
  
  
# Run Third Fold
  
htrain <- htrain3[-1]
htest <- htest3[-1]

htrain.nl <- select(htrain, -Heroin)
htest.nl <- select(htest, -Heroin)

htrain.l <- htrain[,c("Heroin")]
htest.l <- htest[,c("Heroin")]

runGA()

resultsMatrix3 <- resultsMatrix
solutionVec3 <- solutionVec
svmResult3 <- svmResult

# Combine Results

resultsMatrix_3f <- bind_rows(list(data.frame(resultsMatrix1), data.frame(resultsMatrix2), data.frame(resultsMatrix3)), .id = 'nfold')
solutionVec_3f <- bind_rows(list(data.frame(solutionVec1), data.frame(solutionVec2), data.frame(solutionVec3)), .id = 'nfold')
svmResult_3f <- bind_rows(list(data.frame(svmResult1), data.frame(svmResult2), data.frame(svmResult3)), .id = 'nfold')

#write.csv(resultsMatrix_3f, file = "resultsMatrix_3f_full_08_02.csv")
#write.csv(solutionVec_3f, file = "solutionVec_3f_full_08_02.csv")
#write.csv(svmResult_3f, file = "svmResult_3f_full_08_02.csv")

#
#
#
# Experiment 2 - Accuracy = .5 Features = .5
#
#
#

## Run first fold

htrain <- htrain1[-1]
htest <- htest1[-1]

htrain.nl <- select(htrain, -Heroin)
htest.nl <- select(htest, -Heroin)

htrain.l <- htrain[,c("Heroin")]
htest.l <- htest[,c("Heroin")]

runGA()

resultsMatrix1 <- resultsMatrix
solutionVec1 <- solutionVec
svmResult1 <- svmResult

## Run Second Fold

htrain <- htrain2[-1]
htest <- htest2[-1]

htrain.nl <- select(htrain, -Heroin)
htest.nl <- select(htest, -Heroin)

htrain.l <- htrain[,c("Heroin")]
htest.l <- htest[,c("Heroin")]

runGA()

resultsMatrix2 <- resultsMatrix
solutionVec2 <- solutionVec
svmResult2 <- svmResult


# Run Third Fold

htrain <- htrain3[-1]
htest <- htest3[-1]

htrain.nl <- select(htrain, -Heroin)
htest.nl <- select(htest, -Heroin)

htrain.l <- htrain[,c("Heroin")]
htest.l <- htest[,c("Heroin")]

runGA()

resultsMatrix3 <- resultsMatrix
solutionVec3 <- solutionVec
svmResult3 <- svmResult

# Combine Results

resultsMatrix_3f <- bind_rows(list(data.frame(resultsMatrix1), data.frame(resultsMatrix2), data.frame(resultsMatrix3)), .id = 'nfold')
solutionVec_3f <- bind_rows(list(data.frame(solutionVec1), data.frame(solutionVec2), data.frame(solutionVec3)), .id = 'nfold')
svmResult_3f <- bind_rows(list(data.frame(svmResult1), data.frame(svmResult2), data.frame(svmResult3)), .id = 'nfold')

write.csv(resultsMatrix_3f, file = "resultsMatrix_3f_full_05_05.csv")
write.csv(solutionVec_3f, file = "solutionVec_3f_full_05_05.csv")
write.csv(svmResult_3f, file = "svmResult_3f_full_05_05.csv")

#
#
#
# Experiment 3 - Accuracy = .2 Features = .8
#
#
#

## Run first fold

htrain <- htrain1[-1]
htest <- htest1[-1]

htrain.nl <- select(htrain, -Heroin)
htest.nl <- select(htest, -Heroin)

htrain.l <- htrain[,c("Heroin")]
htest.l <- htest[,c("Heroin")]

runGA()

resultsMatrix1 <- resultsMatrix
solutionVec1 <- solutionVec
svmResult1 <- svmResult

## Run Second Fold

htrain <- htrain2[-1]
htest <- htest2[-1]

htrain.nl <- select(htrain, -Heroin)
htest.nl <- select(htest, -Heroin)

htrain.l <- htrain[,c("Heroin")]
htest.l <- htest[,c("Heroin")]

runGA()

resultsMatrix2 <- resultsMatrix
solutionVec2 <- solutionVec
svmResult2 <- svmResult


# Run Third Fold

htrain <- htrain3[-1]
htest <- htest3[-1]

htrain.nl <- select(htrain, -Heroin)
htest.nl <- select(htest, -Heroin)

htrain.l <- htrain[,c("Heroin")]
htest.l <- htest[,c("Heroin")]

runGA()

resultsMatrix3 <- resultsMatrix
solutionVec3 <- solutionVec
svmResult3 <- svmResult

# Combine Results

resultsMatrix_3f <- bind_rows(list(data.frame(resultsMatrix1), data.frame(resultsMatrix2), data.frame(resultsMatrix3)), .id = 'nfold')
solutionVec_3f <- bind_rows(list(data.frame(solutionVec1), data.frame(solutionVec2), data.frame(solutionVec3)), .id = 'nfold')
svmResult_3f <- bind_rows(list(data.frame(svmResult1), data.frame(svmResult2), data.frame(svmResult3)), .id = 'nfold')

write.csv(resultsMatrix_3f, file = "resultsMatrix_3f_full_02_08.csv")
write.csv(solutionVec_3f, file = "solutionVec_3f_full_02_08.csv")
write.csv(svmResult_3f, file = "svmResult_3f_full_02_08.csv")


#
#
#
# Experiment 4 - Accuracy = .65 Features = .35
#
#
#

## Run first fold

htrain <- htrain1[-1]
htest <- htest1[-1]

htrain.nl <- select(htrain, -Heroin)
htest.nl <- select(htest, -Heroin)

htrain.l <- htrain[,c("Heroin")]
htest.l <- htest[,c("Heroin")]

runGA()

resultsMatrix1 <- resultsMatrix
solutionVec1 <- solutionVec
svmResult1 <- svmResult

## Run Second Fold

htrain <- htrain2[-1]
htest <- htest2[-1]

htrain.nl <- select(htrain, -Heroin)
htest.nl <- select(htest, -Heroin)

htrain.l <- htrain[,c("Heroin")]
htest.l <- htest[,c("Heroin")]

runGA()

resultsMatrix2 <- resultsMatrix
solutionVec2 <- solutionVec
svmResult2 <- svmResult


# Run Third Fold

htrain <- htrain3[-1]
htest <- htest3[-1]

htrain.nl <- select(htrain, -Heroin)
htest.nl <- select(htest, -Heroin)

htrain.l <- htrain[,c("Heroin")]
htest.l <- htest[,c("Heroin")]

runGA()

resultsMatrix3 <- resultsMatrix
solutionVec3 <- solutionVec
svmResult3 <- svmResult

# Combine Results

resultsMatrix_3f <- bind_rows(list(data.frame(resultsMatrix1), data.frame(resultsMatrix2), data.frame(resultsMatrix3)), .id = 'nfold')
solutionVec_3f <- bind_rows(list(data.frame(solutionVec1), data.frame(solutionVec2), data.frame(solutionVec3)), .id = 'nfold')
svmResult_3f <- bind_rows(list(data.frame(svmResult1), data.frame(svmResult2), data.frame(svmResult3)), .id = 'nfold')

#write.csv(resultsMatrix_3f, file = "resultsMatrix_3f_full_065_035.csv")
#write.csv(solutionVec_3f, file = "solutionVec_3f_full_065_035.csv")
#write.csv(svmResult_3f, file = "svmResult_3f_full_065_035.csv")


#
#
#
# Experiment 5 - Accuracy = .35 Features = .65
#
#
#

## Run first fold

htrain <- htrain1[-1]
htest <- htest1[-1]

htrain.nl <- select(htrain, -Heroin)
htest.nl <- select(htest, -Heroin)

htrain.l <- htrain[,c("Heroin")]
htest.l <- htest[,c("Heroin")]

runGA()

resultsMatrix1 <- resultsMatrix
solutionVec1 <- solutionVec
svmResult1 <- svmResult

## Run Second Fold

htrain <- htrain2[-1]
htest <- htest2[-1]

htrain.nl <- select(htrain, -Heroin)
htest.nl <- select(htest, -Heroin)

htrain.l <- htrain[,c("Heroin")]
htest.l <- htest[,c("Heroin")]

runGA()

resultsMatrix2 <- resultsMatrix
solutionVec2 <- solutionVec
svmResult2 <- svmResult


# Run Third Fold

htrain <- htrain3[-1]
htest <- htest3[-1]

htrain.nl <- select(htrain, -Heroin)
htest.nl <- select(htest, -Heroin)

htrain.l <- htrain[,c("Heroin")]
htest.l <- htest[,c("Heroin")]

runGA()

resultsMatrix3 <- resultsMatrix
solutionVec3 <- solutionVec
svmResult3 <- svmResult

# Combine Results

resultsMatrix_3f <- bind_rows(list(data.frame(resultsMatrix1), data.frame(resultsMatrix2), data.frame(resultsMatrix3)), .id = 'nfold')
solutionVec_3f <- bind_rows(list(data.frame(solutionVec1), data.frame(solutionVec2), data.frame(solutionVec3)), .id = 'nfold')
svmResult_3f <- bind_rows(list(data.frame(svmResult1), data.frame(svmResult2), data.frame(svmResult3)), .id = 'nfold')

write.csv(resultsMatrix_3f, file = "resultsMatrix_3f_full_035_065.csv")
write.csv(solutionVec_3f, file = "solutionVec_3f_full_035_065.csv")
write.csv(svmResult_3f, file = "svmResult_3f_full_035_065.csv")











#
#
#
# Save sample output
#
#
#

#write.csv(resultsMatrix, file = "results_matrix_x.5_c.8_r10.csv")
#write.csv(svmResult, file = "svm_result_x.5_c.8_r10.csv")

#write.csv(resultsMatrix, file = "results_matrix_x.1_c.9_r10.csv")
#write.csv(svmResult, file = "svm_result_x.1_c.9_r10.csv")

#write.csv(resultsMatrix, file = "results_matrix_x.1_c.9_+_r10.csv")
#write.csv(svmResult, file = "svm_result_x.1_c.9_+_r10.csv")

#write.csv(resultsMatrix, file = "results_matrix_x.1_c.9_+_r20_e20.csv")
#write.csv(svmResult, file = "svm_result_x.1_c.9_+_r20_e20.csv")

#write.csv(resultsMatrix, file = "results_matrix_x.1_c.9_+_r20_e10.csv") **** Best Fitness
#write.csv(svmResult, file = "svm_result_x.1_c.9_+_r20_e10.csv")

#write.csv(resultsMatrix, file = "results_matrix_x.02_c.8_r10.csv")
#write.csv(svmResult, file = "svm_result_x.02_c.8_r10.csv")

#### End Report Solution

# Save results of Actual Full 30 Sample Runs 

#write.csv(resultsMatrix, file = "results_matrix_a8_f2_v1=.csv")
#write.csv(svmResult, file = "svm_result__a8_f2_v1.csv")
#write.csv(solutionVec, file = "solution_vec__a8_f2_v1.csv")




#
#
#
# Compare Results 
#
#
#



