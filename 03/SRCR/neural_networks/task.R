################################################
#### Data Init

# Loading necessary libs
library("neuralnet")
library("hydroGOF")

# Reading the CSV
csv <- read.csv("data\\sample.csv", header=TRUE, sep=",", dec=".")

# Setting Performance.Task to a numeric value
# since neuralnet only works with those
csv$Performance.Task = as.numeric(csv$Performance.Task)


# Analysing the CSV contents
# str(csv)
# summary(csv)

# Function that returns a neural network for the fatigue
task_nnet <- function(dataset, hidden, testset, algorithm="rprop+", threshold = 0.01,
                         formula = Performance.Task ~ Performance.KDTMean + Performance.MAMean +
                           Performance.MVMean + Performance.TBCMean + Performance.DDCMean +
                           Performance.DMSMean + Performance.AEDMean + Performance.ADMSLMean +
                           FatigueLevel) {
  
  # Training the neural network
  nnet <- neuralnet(formula, dataset, threshold=threshold, hidden=hidden)
  
  # Duplicating the testset and removing the output variable
  inputset <- testset
  inputset$Performance.Task <- NULL
  
  # Testing the network
  nnet$results <- compute(nnet, inputset)
  
  # Saving the results
  results <- data.frame(actual = testset$Performance.Task, prediction = nnet$results$net.result)
  
  # Saving rounded up results
  nnet$prediction <- round(results$prediction)
  
  # Saving RSME
  nnet$rmse <- rmse( c(testset$Performance.Task), c(nnet$prediction) )
  
  return (nnet)
}

to_binary_fatigue <- function(dataset) {
  # Making a copy of the dataset
  bin_dataset <- dataset
  
  # Setting any fatigue level below 3 to "no-fatigue"
  bin_dataset$FatigueLevel[bin_dataset$FatigueLevel <= 3] = 0
  
  # Setting fatigye level over 3 to "fatigue"
  bin_dataset$FatigueLevel[bin_dataset$FatigueLevel > 3] = 1
  
  return (bin_dataset)
}

to_scale <- function(dataset) {
  # Making a copy
  d <- dataset
  # Setting new dataset to the cluster groups
  d$FatigueLevel[d$FatigueLevel < 3] = 1
  d$FatigueLevel[d$FatigueLevel > 2 && d$FatigueLevel < 5] = 2
  d$FatigueLevel[d$FatigueLevel > 4] = 3
  
  return(d);
}

# Various datasets
# dsn = DataSet #n
ds1 <- csv[1:50, ]
ds2 <- csv[1:100, ]
ds3 <- csv[1:200, ]
ds4 <- csv[1:400, ]
ds5 <- csv[1:600, ]

# Various binary fatigue level datasets
# bdsn = Binary DataSet #n
bds1 <- to_binary_fatigue(ds1)
bds2 <- to_binary_fatigue(ds2)
bds3 <- to_binary_fatigue(ds3)
bds4 <- to_binary_fatigue(ds4)
bds5 <- to_binary_fatigue(ds5)


# Various scaled fatigue level datasets
# bdsn = Scaled DataSet #n
sds1 <- to_scale(ds1)
sds2 <- to_scale(ds2)
sds3 <- to_scale(ds3)
sds4 <- to_scale(ds4)
sds5 <- to_scale(ds5)

# Various testsets
# tsn = TestSet #n
ts1 <- csv[601:844, ] # > 200 regs
ts2 <- csv[795:844, ] # 150 regs
ts3 <- csv[745:844, ] # 100 regs

# Various binary testsets
# btsn = Binary TestSet #n
bts1 <- to_binary_fatigue(ts1) # > 200 regs
bts2 <- to_binary_fatigue(ts2) # 150 regs
bts3 <- to_binary_fatigue(ts3) # 100 regs

# Various scaled testsets
# stsn = Scaled TestSet #n
sts1 <- to_scale(ts1) # > 200 regs
sts2 <- to_scale(ts2) # 150 regs
sts3 <- to_scale(ts3) # 100 regs

# Various neural network configurations
# nncn = Neural Network Configuration #n
nnc1 <- c(2)
nnc1 <- c(4,2)
nnc3 <- c(10,5)
nnc4 <- c(20,10)
nnc5 <- c(40,20)

################################################
#### Identifying the level of fatigue
# Testing diferent samples of datasets, testsets, configuration and algorithms

fnnet1 <- task_nnet(ds3, nnc5, ts1)

################################################
#### Identifying the (non)-existance of fatigue

bfnnet1 <- task_nnet(bds1, nnc5, ts1)
