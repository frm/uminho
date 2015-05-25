# Todo: 
# - [x] Identificar os 7 niveis de fadiga
# - [x] Identificar existencia (ou nao) de fadiga
# - [ ] Identificar escala ideal de fadiga

# Extras:
# - [ ] plots
# - [ ] comparar algoritmos
# - [x] obter tarefa ideal em funcao do resto
# - [x] obter tarefa ideal em funcao da existencia (ou nao) de fadiga
# - [ ] obter tarefa ideal em funcao da escala ideal de fadiga
# - [ ] ir removendo parametros e ver o peso que cada um tem
# - [ ] ir removendo parametros e ver o peso que cada um tem na escala ideal de fadiga
# - [ ] ir removendo parametros e ver o peso que cada um tem em funcao da existencia (ou nao) de fadiga
# - [ ] ir removendo parametros e ver o peso que cada um tem (no caso em que obtemos a tarefa como output)
# - [ ] descobrir parametro com mais peso na escala de fadiga
# - [ ] descobrir parametro com mais peso na escala ideal de fadiga
# - [ ] descobrir parametro com mais peso na existencia (ou nao) de fadiga
# - [ ] descobrir parametro com mais peso na tarefa final


# Loading necessary libs
library("neuralnet")
library("hydroGOF")

# Reading the CSV
dataset <- read.csv("data\\sample.csv", header=TRUE, sep=",", dec=".")[1:600, ]

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

plot(nnet)


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


# Obtaining the ideal task regarding the remaining params
# NOTE: Using the default fatigue scale
task_dataset <- dataset

# Training the neural network
task_nnet <- neuralnet(Performance.Task ~ Performance.KDTMean + Performance.MAMean +
                    Performance.MVMean + Performance.TBCMean + Performance.DDCMean +
                    Performance.DMSMean + Performance.AEDMean + Performance.ADMSLMean +
                    FatigueLevel,
                    dataset, threshold = 0.01, hidden=c(40, 20))


# Obtaining the ideal task regarding the remaining params
# NOTE: Using the binary fatigue scale
task_dataset <- bin_dataset

# Training the neural network
task_nnet_bin <- neuralnet(Performance.Task ~ Performance.KDTMean + Performance.MAMean +
                         Performance.MVMean + Performance.TBCMean + Performance.DDCMean +
                         Performance.DMSMean + Performance.AEDMean + Performance.ADMSLMean +
                         FatigueLevel,
                         bin_dataset, threshold = 0.01, hidden=c(40, 20))
