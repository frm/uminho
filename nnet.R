# Todo: 
# - [x] Identificar os 7 niveis de fadiga
# - [x] Identificar existencia (ou nao) de fadiga
# - [x] Identificar escala ideal de fadiga

# Extras:
# - [ ] plots
# - [ ] comparar algoritmos
# - [x] obter tarefa em funcao do resto
# - [x] obter tarefa em funcao da existencia (ou nao) de fadiga
# - [ ] obter tarefa em funcao da escala ideal de fadiga
# - [ ] ir removendo parametros e ver o peso que cada um tem
# - [ ] ir removendo parametros e ver o peso que cada um tem na escala ideal de fadiga
# - [ ] ir removendo parametros e ver o peso que cada um tem em funcao da existencia (ou nao) de fadiga
# - [ ] ir removendo parametros e ver o peso que cada um tem (no caso em que obtemos a tarefa como output)
# - [ ] descobrir parametro com mais peso na escala de fadiga
# - [ ] descobrir parametro com mais peso na escala ideal de fadiga
# - [ ] descobrir parametro com mais peso na existencia (ou nao) de fadiga
# - [ ] descobrir parametro com mais peso na tarefa final

# Plots:
# Para cada rede neuronal que formos verificar o peso, expressar o output em funcao dos parametros


# Loading necessary libs
library("neuralnet")
library("hydroGOF")

# Reading the CSV
csv <- read.csv("data\\sample.csv", header=TRUE, sep=",", dec=".")
dataset <- csv[1:600, ]
trainset <- csv[601:844, ]

# Analysing the CSV contents
# str(dataset)
# summary(dataset)

# Setting Performance.Task to a numeric value
# since neuralnet only works with those
dataset$Performance.Task = as.numeric(dataset$Performance.Task)

# Training the neural network
nnet <- neuralnet(FatigueLevel ~ Performance.KDTMean + Performance.MAMean +
                    Performance.MVMean + Performance.TBCMean + Performance.DDCMean +
                    Performance.DMSMean + Performance.AEDMean + Performance.ADMSLMean +
                    Performance.Task,
                    dataset, threshold = 0.01, hidden=c(40, 20))


################################################
# Identifying the (non)-existance of fatigue

# Making a copy of the dataset
bin_dataset <- dataset

# Setting any fatigue level below 3 to "no-fatigue"
bin_dataset$FatigueLevel[bin_dataset$FatigueLevel <= 3] = 0

# Setting fatigye level over 3 to "fatigue"
bin_dataset$FatigueLevel[bin_dataset$FatigueLevel > 3] = 1

bin_nnet <- neuralnet(FatigueLevel ~ Performance.KDTMean + Performance.MAMean +
                    Performance.MVMean + Performance.TBCMean + Performance.DDCMean +
                    Performance.DMSMean + Performance.AEDMean + Performance.ADMSLMean +
                    Performance.Task,
                    bin_dataset, threshold = 0.01, hidden=c(40, 20))


################################################
# Calculating the ideal scale
# Use of clusters

# We are clustering everything in order to FatigueLevel
# So we need to select a subset without FatigueLevel
clusterset <- subset(dataset, select=c(Performance.KDTMean, Performance.MAMean,
                                Performance.MVMean, Performance.TBCMean, Performance.DDCMean,
                                Performance.DMSMean, Performance.AEDMean, Performance.ADMSLMean,
                                Performance.Task))

# Determine number of clusters
# Using partitioning method
wss <- ( nrow(clusterset) - 1) * sum( apply(clusterset, 2, var) )

# Iterating over a range of possible clusters
for (i in 2:7) {
                  wss[i]<- sum(kmeans(clusterset,
                                 centers=i)$withinss)
                }

plot(1:7, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares") 


# Showing the clusters

# K-Means Clustering with 3 clusters
fit <- kmeans(clusterset, 3)

# Cluster Plot against 1st 2 principal components

# vary parameters for most readable graph
library(cluster)
clusplot(clusterset, fit$cluster, color=TRUE, shade=TRUE,
         labels=2, lines=0)

# Centroid Plot against 1st 2 discriminant functions
library(fpc)
plotcluster(clusterset, fit$cluster) 


################################################
# Obtaining the current task regarding the remaining params


# Using the default fatigue scale

task_dataset <- dataset

# Training the neural network
task_nnet <- neuralnet(Performance.Task ~ Performance.KDTMean + Performance.MAMean +
                    Performance.MVMean + Performance.TBCMean + Performance.DDCMean +
                    Performance.DMSMean + Performance.AEDMean + Performance.ADMSLMean +
                    FatigueLevel,
                    dataset, threshold = 0.01, hidden=c(40, 20))


# ------------------------------------------
# Using the binary fatigue scale

task_dataset <- bin_dataset

# Training the neural network
task_nnet_bin <- neuralnet(Performance.Task ~ Performance.KDTMean + Performance.MAMean +
                         Performance.MVMean + Performance.TBCMean + Performance.DDCMean +
                         Performance.DMSMean + Performance.AEDMean + Performance.ADMSLMean +
                         FatigueLevel,
                         bin_dataset, threshold = 0.01, hidden=c(40, 20))
