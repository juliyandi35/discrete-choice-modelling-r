# Import Data
library(readxl)
Data <- read_excel("Dataset.xlsx")
names(Data)

# Filtering Data
Data <- Data[,-c(1,2,6,10,14,15,20,22,23,25,27,28,31,33,36,39,40,45,46,47)]
names(Data)
Sosiometri <- Data[,c(1,2,3,4)]
Penjual_Keliling <- Data[,c(5:19)]
names(Penjual_Keliling)
Penjual_Keliling <- Penjual_Keliling[,c(1,2,3,5,6,7,8,9)]
names(Penjual_Keliling)
Pedagang <- Data[,c(20:27)]
names(Pedagang)

n <- nrow(Data)
Dataset <- matrix(NA,nrow = 2*n,ncol =(ncol(Pedagang)+3))

for (i in 1:n) {
  Dataset[(2*i),1] <- i 
  Dataset[(2*i-1),1] <- i 
}

for (i in 1:n) {
  Dataset[(2*i),2] <- "Pedagang"
  Dataset[(2*i-1),2] <- "Penjual Sayur Keliling"
}

for (i in 1:n) {
  Dataset[(2*i),4:11] <- as.matrix(Pedagang)[i,1:8]
  Dataset[(2*i-1),4:11] <- as.matrix(Penjual_Keliling)[i,1:8]
}
colnames(Dataset) <- c("individual","sales","choice","shopping frequency",
                       "products bought","spending","shopping variations attention",
                       "amount purchased on weekdays","amount purchased on weekend",
                       "amount purchased on holidays","shopping amount attention")
Dataset <- data.frame(Dataset)
str(Dataset)
names(Dataset)
head(Dataset)

Dataset$individual <- as.factor(Dataset$individual)
Dataset$sales <- as.factor(Dataset$sales)
Dataset$shopping.frequency<- factor(Dataset$shopping.frequency,levels = c("Jarang","1 kali per minggu","2 kali per minggu","3-5 kali per minggu","Setiap hari"))
Dataset$products.bought <- as.factor(Dataset$products.bought)
Dataset$spending <- factor(Dataset$spending,levels = c("Kurang dari Rp 50.000","Rp 50.001-Rp 100.000","Rp 100.001-Rp 200.000","Lebih dari Rp 200.000"))
Dataset$shopping.variations.attention <- factor(Dataset$shopping.variations.attention,levels = c("Ya","Tidak"))
Dataset$amount.purchased.on.weekdays <- factor(Dataset$amount.purchased.on.weekdays,levels = c("Kurang dari 1 kg","1-2 kg","3-5 kg","Lebih dari 5 kg"))
Dataset$amount.purchased.on.weekend <- factor(Dataset$amount.purchased.on.weekend,levels = c("Kurang dari 1 kg","1-2 kg","3-5 kg","Lebih dari 5 kg"))
Dataset$amount.purchased.on.holidays <- factor(Dataset$amount.purchased.on.holidays,levels = c("Kurang dari 2 kg","3-5 kg","5-10 kg","Lebih dari 10 kg"))
Dataset$shopping.amount.attention <- factor(Dataset$shopping.amount.attention,levels = c("Ya","Tidak"))
str(Dataset)

for (i in 1:n) {
  if(as.numeric(Dataset$shopping.frequency[(2*i)]) > as.numeric(Dataset$shopping.frequency[(2*i-1)])){
    Dataset$choice[(2*i)] <- "yes"
    Dataset$choice[(2*i-1)] <- "no"
  }
  else{
    Dataset$choice[(2*i)] <- "no"
    Dataset$choice[(2*i-1)] <- "yes"
  }
}
Dataset$choice <- factor(Dataset$choice,levels = c("no","yes"))
head(Dataset)
names(Dataset)
str(Dataset)

Dataset$shopping.frequency<- as.numeric(Dataset$shopping.frequency)
Dataset$products.bought <- as.numeric(Dataset$products.bought)
Dataset$spending <- as.numeric(Dataset$spending)
Dataset$shopping.variations.attention <- as.numeric(Dataset$shopping.variations.attention)
Dataset$shopping.variations.attention <- ifelse(Dataset$shopping.variations.attention == 2, 0, Dataset$shopping.variations.attention)
Dataset$amount.purchased.on.weekdays <- as.numeric(Dataset$amount.purchased.on.weekdays)
Dataset$amount.purchased.on.weekend <- as.numeric(Dataset$amount.purchased.on.weekend)
Dataset$amount.purchased.on.holidays <- as.numeric(Dataset$amount.purchased.on.holidays)
Dataset$shopping.amount.attention <- as.numeric(Dataset$shopping.amount.attention)
Dataset$shopping.amount.attention <- ifelse(Dataset$shopping.amount.attention == 2, 0, Dataset$shopping.amount.attention)

head(Dataset)

# Analisis Data
# Analisis deskriptif sosiometri
Data$`2. Usia` <- as.factor(Data$`2. Usia`)
Data$`3. Jenis kelamin` <- as.factor(Data$`3. Jenis kelamin`)
Data$`4. Pekerjaan` <- as.factor(Data$`4. Pekerjaan`)
Data$`6. Silakan pilih lokasi rumah anda (Kelurahan)` <- as.factor(Data$`6. Silakan pilih lokasi rumah anda (Kelurahan)`)
summary(Data[,1:4])

barplot(table(Data$`2. Usia`), main = "Frekuensi Responden Berdasarkan Usia", xlab = "Kategori", ylab = "Jumlah")
barplot(table(Data$`3. Jenis kelamin`), main = "Frekuensi Responden Berdasarkan Jenis Kelamin", xlab = "Kategori", ylab = "Jumlah")
barplot(table(Data$`4. Pekerjaan`), main = "Frekuensi Responden Berdasarkan Pekerjaan", xlab = "Kategori", ylab = "Jumlah")
par(mar = c(8, 5, 4, 2))  # Mengatur margin untuk memperbesar frame plot
barplot(table(Data$`6. Silakan pilih lokasi rumah anda (Kelurahan)`), main = "Frekuensi Responden Berdasarkan Lokasi Rumah", xlab = NULL, ylab = "Frekuensi", las = 2, cex.names = 0.4)

# formatting data
library(mlogit)
CM <- mlogit.data(Dataset, choice = "choice", shape = "long", 
                  chid.var = "individual", alt.var = "sales", drop.index = TRUE)
head(CM)

# Variabel yang Multikol : Shopping Frequency (Karena telah digunakan dalam pembuatan variabel choice/variabel dependennya)

# Hipotesis LRT (Likelihood Ratio Test) (Kita menggunakan model penuh karena jumlah variabel independen lebih banyak daripada opsi choice)
# H0 : Model terbatas benar (p-value > 0.05)
# H1 : Model penuh benar (p-value < 0.05)

ml.CM <- mlogit(choice ~ products.bought + spending + shopping.variations.attention +
                  amount.purchased.on.weekdays + amount.purchased.on.weekend + 
                  amount.purchased.on.holidays + shopping.amount.attention, CM)
summary(ml.CM)

ml.CM1 <- mlogit(choice ~ spending + shopping.variations.attention +
                  amount.purchased.on.weekdays + amount.purchased.on.weekend + 
                  amount.purchased.on.holidays + shopping.amount.attention, CM)
summary(ml.CM1)

ml.CM2 <- mlogit(choice ~ spending + shopping.variations.attention +
                  amount.purchased.on.weekdays + 
                  amount.purchased.on.holidays + shopping.amount.attention, CM)
summary(ml.CM2)

ml.CM3 <- mlogit(choice ~ spending + shopping.variations.attention +
                  amount.purchased.on.weekdays + shopping.amount.attention, CM)
summary(ml.CM3)

# Perbandingan antar model
Model.Comparison <- data.frame(Likelihood.Number = c(as.numeric(summary(ml.CM)$logLik),
                                                     as.numeric(summary(ml.CM1)$logLik),
                                                     as.numeric(summary(ml.CM2)$logLik),
                                                     as.numeric(summary(ml.CM3)$logLik)),
                               McFadden.R2 = c(as.numeric(summary(ml.CM)$mfR2),
                                               as.numeric(summary(ml.CM1)$mfR2),
                                               as.numeric(summary(ml.CM2)$mfR2),
                                               as.numeric(summary(ml.CM3)$mfR2)))
Model.Comparison

#how fitted choice probability match with data
apply(fitted(ml.CM, outcome=FALSE), 2, mean) # fitted mean choice probability
table(filter(Dataset, choice=="yes")$sales)/(nrow(Dataset)/2) #average frequency of choices
