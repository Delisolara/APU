df <- read.csv("D:/Studia/APU/smartfony.csv")
ram <- df[["pamiec_RAM"]]  
cena <- df[["cena"]]  

compare.trainingdata <- cbind(ram, cena)
scaled.ram <- as.data.frame(scale(ram))
trainingdata <- cbind(ram, cena)

colnames(trainingdata) <- c("Pamięć_Ram", "Cena")
#(error ≤ 100 z l)
net.price <- neuralnet(Cena~Pamięć_Ram,trainingdata, hidden<-c(7,1), threshold<-100, lifesign <- "full")
plot(net.price)

testdata <- data.frame(c(20,130))
scaled.testdata <- as.data.frame(scale(testdata))

net.results <- compute(net.price, scaled.testdata)
fixed_cena <- cbind(testdata, as.data.frame(net.results$net.result))
colnames(fixed_cena) <- c("Pamięć_Ram", "Cena")

print(fixed_cena)

