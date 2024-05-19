install.packages("neuralnet")
library(neuralnet)

set.seed(42)
input <- as.data.frame(runif(100, min=1, max=100))
output <- input^3 + 2*input

normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

input <- as.data.frame(normalize(input))
output <- normalize(output)


trainingdata <- cbind(input, output)
colnames(trainingdata) <- c("Wejscie", "Wyjscie")
net.sqrt <- neuralnet(Wyjscie ~ Wejscie, trainingdata, hidden=3, threshold=0.01, stepmax=500000)
print(net.sqrt)
plot(net.sqrt, rep = "best")

testdata <- as.data.frame(runif(300, min=1, max=10))
testdata <- normalize(testdata)
net.results <- compute(net.sqrt, testdata)
print(net.results$net.result)

cleanoutput <- cbind(testdata, normalize(testdata^3 + 2*testdata), as.data.frame(net.results$net.result))
colnames(cleanoutput) <- c("Wejscie", "Oczekiwane Wyjscie", "Wyjscie sieci neuronowej")
print(cleanoutput)


