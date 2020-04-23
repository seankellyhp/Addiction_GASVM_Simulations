
setwd("C:/Users/Sean/Desktop/S1 BCUBDA Textbooks/Optimization/GA_Experiment/")

temp = list.files(pattern="*.csv")
myfiles = lapply(temp, read.csv)

for(i in 1:length(temp)) {
  df <- myfiles[[i]]
  assign(temp[[i]], df)
  
}

library(dplyr)
library(tidyr)


# Combine Results for Comparison 

# Best, Mean, Median Matrix
resultsMatrix0802 <- resultsMatrix_3f_full_08_02.csv
resultsMatrix0505 <- resultsMatrix_3f_full_05_05.csv
resultsMatrix0208 <- resultsMatrix_3f_full_02_08.csv
resultsMatrix6535 <- resultsMatrix_3f_full_065_035.csv
resultsMatrix3565 <- resultsMatrix_3f_full_035_065.csv


resultsMatrix0802$Source <- "0802"
resultsMatrix0505$Source <- "0505"
resultsMatrix0208$Source <- "0208"
resultsMatrix6535$Source <- "6535"
resultsMatrix3565$Source <- "3565"

a.resultsMatrix <- bind_rows(list(resultsMatrix0802, resultsMatrix0505, 
                                  resultsMatrix0208, resultsMatrix6535, 
                                  resultsMatrix3565))

# SVM Results 
svmResults0802 <- svmResult_3f_full_08_02.csv
svmResults0505 <- svmResult_3f_full_05_05.csv
svmResults0208 <- svmResult_3f_full_02_08.csv
svmResults6535 <- svmResult_3f_full_065_035.csv
svmResults3565 <- svmResult_3f_full_035_065.csv

svmResults0802$Source <- "0802"
svmResults0505$Source <- "0505"
svmResults0208$Source <- "0208"
svmResults6535$Source <- "6535"
svmResults3565$Source <- "3565"

a.svmResults <- bind_rows(list(svmResults0802, svmResults0505, 
                               svmResults0208, svmResults6535, 
                               svmResults3565))

# Best Solution 
solutionVec0802 <- solutionVec_3f_full_08_02.csv
solutionVec0505 <- solutionVec_3f_full_05_05.csv
solutionVec0208 <- solutionVec_3f_full_02_08.csv
solutionVec6535 <- solutionVec_3f_full_065_035.csv
solutionVec3565 <- solutionVec_3f_full_035_065.csv


solutionVec0802$Source <- "0802"
solutionVec0505$Source <- "0505"
solutionVec0208$Source <- "0208"
solutionVec6535$Source <- "6535"
solutionVec3565$Source <- "3565"


a.solutionVec <- bind_rows(list(solutionVec0802, solutionVec0505, 
                               solutionVec0208, solutionVec6535, 
                               solutionVec3565))


# Clean and Create Unique Ids 

a.resultsMatrix <- a.resultsMatrix %>% 
  unite(Id, c("Source", "nfold", "Generation"), remove = F)
a.resultsMatrix <- a.resultsMatrix[-1]

a.solutionVec <- a.solutionVec %>% 
  rename("Generation"=V1) %>% 
  unite(Id, c("Source", "nfold", "Generation"), remove = F)
a.solutionVec <- a.solutionVec[-1]

#names(a.solutionVec) <- c(realcols)

a.svmResults <- a.svmResults %>% 
  rename("Generation"=generation) %>% 
  unite(Id, c("Source", "nfold", "Generation"), remove = F)
a.svmResults <- a.svmResults[-1]


# Convert SolutionVec

feature.names <- c("Id", "nfold", "Generation", "Age","Gender","Education","Country","Ethnicity", "Nscore","Escore", "Oscore", "Ascore", "Cscore", "Impulsive", "SS",  "Alcohol", "Amphet",    "Amyl",      "Benzos",    "Caff",     
                   "Cannabis", "Choc", "Coke", "Crack", "Ecstacy", "Ketamine",  "Legalh",  "LSD",  "Meth", "Mushrooms", "Nicotine",  "Semer",  "VSA", "Cost", "Gamma", "Source")

names(a.solutionVec) <- feature.names

a.solutionVec.bin <- a.solutionVec 

a.solutionVec.bin[,4:33][a.solutionVec.bin[,4:33] >= .5] <- 1
a.solutionVec.bin[,4:33][a.solutionVec.bin[,4:33] < .5] <- 0

solutionVec.ag <- a.solutionVec.bin %>% 
  select(-Generation, -Id, -Cost, -Gamma) %>% 
  group_by(Source, nfold) %>% 
  summarise_all(list(sum=sum))

solutionVec.ag.fold <- solutionVec.ag %>% 
  group_by(Source) %>% 
  summarise_all(list(avg=median))


# Create Fold Averages

a.svmResults.cl <- a.svmResults
is.na(a.svmResults.cl)<-sapply(a.svmResults.cl, is.infinite)


svmResults.ag <- a.svmResults.cl %>% 
  select(-Id, -Generation, -nfold) %>% 
  group_by(Source) %>% 
  summarise_all(list(min=min, max=max, sd=sd, mean=mean, median=median), na.rm=T)

library(ggplot2)
library(ggthemes)

ggplot(a.svmResults.cl, aes(Source, sens)) + geom_boxplot()

sens.aov <- aov(sens ~ Source, data = a.svmResults.cl)
summary(sens.aov)

TukeyHSD(sens.aov)

plot(sens.aov)

install.packages("car")
library(car)
leveneTest(sens ~ Source, data = a.svmResults.cl)

sens.oneway <- oneway.test(sens ~ Source, data = a.svmResults.cl)
summary(sens.oneway)


a.svmResults.cl$Source <- factor(a.svmResults.cl$Source, levels = c("0802", "6535", "0505","3565","0208"))
sens.pairwise <- pairwise.t.test(a.svmResults.cl$sens, a.svmResults.cl$Source, 
                               p.adjust.method = "BH", pool.sd = FALSE)
summary(sens.pairwise)

#
#
# Visualize
#
#

# Growth in Best Result by Generation

a.resultsMatrix.cl <- a.resultsMatrix
is.na(a.resultsMatrix.cl)<-sapply(a.resultsMatrix.cl, is.infinite)

resultsMatrix.cl <- a.resultsMatrix.cl %>% 
  select(Id, Source, contains('best')) %>%
  gather(Iteration, Score, 3:32) %>% 
  group_by(Source, Id) %>% 
  summarise(Avg = mean(Score, na.rm=T), SD = sd(Score, na.rm=T)) %>% 
  filter(!is.na(SD)) %>% 
  ungroup()

resultsMatrix.cl.fold <- resultsMatrix.cl %>% 
  separate(Id, into=c("Source.b", "Fold", "Generation"), sep = "_") %>% 
  select(-Source.b) %>% 
  group_by(Source, Generation) %>% 
  summarise(Avg.f = mean(Avg, na.rm=T), SD.f = mean(SD, na.rm=T)) %>% 
  ungroup() 

resultsMatrix.cl.fold$Fitness_Weights <- plyr::revalue(
  factor(resultsMatrix.cl.fold$Source, levels = c("0802", "6535", "0505","3565","0208")), 
              c("0208" = "A:20% F:80%",
                "0505" = "A:50% F:50%",
                "0802" = "A:80% F:20%",
                "3565" = "A:35% F:65%",
                "6535" = "A:65% F:35%"))

ggplot(resultsMatrix.cl.fold, aes(as.numeric(Generation), Avg.f, colour = Fitness_Weights)) + geom_line(size = 2) + 
  geom_errorbar(ymin = resultsMatrix.cl.fold$Avg.f-resultsMatrix.cl.fold$SD.f, 
                ymax = resultsMatrix.cl.fold$Avg.f + resultsMatrix.cl.fold$SD.f) + 
  scale_y_continuous(limits = c(0, 0.7)) +
  xlab("GA Generation") + 
  ylab("GA Fitness") +
  scale_x_continuous(breaks = seq(from=0, to=100, 5)) +
  theme_tufte(base_size = 18, base_family = "georgia") + 
  theme(legend.title = element_text(color = "black", size = 18),
        legend.text = element_text(color = "black", size = 18), 
        axis.text.x = element_text(color = "black", size = 18), 
        axis.text.y = element_text(color = "black", size = 18)) + 
  scale_colour_discrete(name="Accuracy and \nFeature Weight")


# Experiment for Mutation and Crossover 



# Most Used Features 

solutionVec.ag.fold.cl <- solutionVec.ag.fold %>%
  select(-nfold_avg) %>% 
  gather(Measure, Count, Age_sum_avg:VSA_sum_avg) %>% 
  separate(Measure, into=c("Feature", "Agg1", "Agg2"), sep="_") 

ggplot(solutionVec.ag.fold.cl, aes(x=Feature, y = Count)) + 
  geom_bar(stat="identity") + 
  coord_flip() + 
  facet_grid(.~Source) + 
  theme_tufte()


# Table of Fitness Comparison 


# Table of Most Accurate Models by Group - Measures, Accuracy, SVM Params 


# Merge by Id and keep highest Acc

best.solution <- left_join(a.solutionVec.bin, a.svmResults.cl, by = c("Id"))

best.solution.f <- best.solution %>% 
  mutate(acc.sum = sens + spec, feature.red = (100*(30-features.n)/30)) %>% 
  group_by(Source.x) %>% 
  slice(which.max(acc.sum))
  


write.csv(solutionVec.ag.fold.cl, file = "C:/Users/Sean/Desktop/S1 BCUBDA Textbooks/Optimization/solutionVec.ag.fold.cl.csv")


#### Charts for Param Experiment

setwd("C:/Users/Sean/Desktop/S1 BCUBDA Textbooks/Optimization/GA_Test/")

temp = list.files(pattern="*.csv")
myfiles = lapply(temp, read.csv)

for(i in 1:length(temp)) {
  df <- myfiles[[i]]
  assign(temp[[i]], df)
  
}

# Param test for m:.5 c:.8, m:.02 c:.8, m:.1 c:.9

resultsMatrix19 <- results_matrix_x.1_c.9_r10.csv
resultsMatrix028 <- results_matrix_x.02_c.8_r10.csv
resultsMatrix58 <- results_matrix_x.5_c.8_r10.csv

resultsMatrix19$Source <- "m1c9"
resultsMatrix028$Source <- "m02c8"
resultsMatrix58$Source <- "m5c8"

resultsMatrix.param <- bind_rows(list(resultsMatrix19, resultsMatrix028, 
                                  resultsMatrix58))


resultsMatrix.param.cl <- resultsMatrix.param
is.na(resultsMatrix.param.cl)<-sapply(resultsMatrix.param.cl, is.infinite)

resultsMatrix.param.cl.b <- resultsMatrix.param.cl %>% 
  select(Source, Generation, contains('best')) %>%
  gather(Iteration, Score, 3:12) %>% 
  group_by(Source, Generation) %>% 
  summarise(Avg = mean(Score, na.rm=T), SD = sd(Score, na.rm=T)) %>% 
  filter(!is.na(SD)) %>% 
  ungroup()

ggplot(resultsMatrix.param.cl.b, aes(as.numeric(Generation), Avg, colour = Source)) + geom_line(size = 2) + 
  geom_errorbar(ymin = resultsMatrix.param.cl.b$Avg-resultsMatrix.param.cl.b$SD, 
                ymax = resultsMatrix.param.cl.b$Avg + resultsMatrix.param.cl.b$SD) + 
  #scale_y_continuous(limits = c(0, 0.7)) + 
  #ggtitle("GA-SVM Fitness Change by Mutation and Crossover Settings") + 
  xlab("GA Generation") + 
  ylab("GA Fitness") +
  #scale_x_continuous(breaks = seq(from=0, to=100, 5)) + 
  theme_tufte(base_size = 18, base_family = "georgia") + 
  theme(legend.title = element_text(color = "black", size = 18),
        legend.text = element_text(color = "black", size = 18), 
        axis.text.x = element_text(color = "black", size = 18), 
        axis.text.y = element_text(color = "black", size = 18)) + 
  scale_colour_discrete(name="Mutation and \nCrossover Rate", 
                        labels=c("M:0.02 C:0.8", "M:0.1 C:0.9", "M:0.5 C:0.8"))
  



# Param test for e10, e20, e2

svmResulte10 <- svm_result_x.1_c.9_p_r20_e10.csv
svmResulte20 <- svm_result_x.1_c.9_p_r20_e20.csv
svmResulte2 <- svm_result_x.1_c.9_p_r20_e2.csv

svmResulte10$Source <- "e10"
svmResulte20$Source <- "e20"
svmResulte2$Source <- "e2"

svmResult.elite <- bind_rows(list(svmResulte10, svmResulte20, 
                                  svmResulte2))

svmResult.elite.comp <- svmResult.elite %>% 
  group_by(Source) %>% 
  summarise(Avg_Sens = mean(sens, na.rm=T), Avg_Spec = mean(spec, na.rm=T))


# ROC curve 

install.packages("ROCR")
library(ROCR)
devtools::install_github("sachsmc/plotROC")

ggplot(a.svmResults, aes(x = sens, y = 1-spec)) + geom_roc()

