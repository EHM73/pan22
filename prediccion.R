
source("D:/OneDrive - UPV/BigData/Asignaturas/15 Text mining/pan22 - ENTREGA/functions.R")

start.time <- Sys.time()

# Preparing parameters
vtn <- 10000 # Número de palabras para los vocabularios parciales de ironia y no
vn <- 8   # tamaño final del vocabulario 
k <- 10        # Number of folds in cross-validation. Usually used 10
r <- 3       # Number of repeats in cross-validation. Usually used 3

pathbase <- "D:/OneDrive - UPV/BigData/Asignaturas/15 Text mining/pan22 - ENTREGA/"

fres <- paste(vtn,"x",vn,sep="")

path_training <- paste(pathbase,"training/en/",sep="")	# Your training path
path_predict <- paste(pathbase,"predicciones/en/",sep="")			# a predecir
path_result <- paste(pathbase,"resultados/",sep="")			# El resultado
dir.create(paste(path_result,fres,sep=""))
path_result <- paste(path_result,fres,"/",sep="")
lang <- "en"

# generamos los dos vocabularios para buscar las palabras más representativas 
# (palabras que usan los ironicos que no usan los NO ironicos y las contrarias) para crear un vocabulario menos pero más representativo.
vocabularyIrony <- GenerateVocabularyIrony("I",path_training, vtn, swlang=lang)
vocabularyNOIrony <- GenerateVocabularyIrony("NI",path_training, vtn, swlang=lang)

# Frecuencias absolutas
dif <- full_join(vocabularyIrony,vocabularyNOIrony,by="WORD")
dif[is.na(dif)]<-0
dif["absol"]<-abs(dif$FREQ.x - dif$FREQ.y)
dif <- dif[order(dif$absol,decreasing = TRUE),]
vocab.dif.absol <- dif[1:vn,]

# GENERATING THE BOW FOR THE TRAINING SET

bow_training.dif.absol <- GenerateBoW(path_training, vocab.dif.absol)

# PREPARING THE VECTOR SPACE MODEL FOR THE TRAINING SET

training.dif.absol <- concat.split(bow_training.dif.absol, "V1", ",")
training.dif.absol <- cbind(training.dif.absol[,2], training.dif.absol[,4:ncol(training.dif.absol)])
names(training.dif.absol)[1] <- "theclass"

#Hemos porbado el GLM y lo descartamos
# Learning a SVM and evaluating it with k-fold cross-validation
train_control <- trainControl( method="repeatedcv", number = k , repeats = r)

model_rf.dif.absol <- train( theclass~., data= training.dif.absol, trControl = train_control, method = "rf")
print(model_rf.dif.absol)

bow_test.dif.absol <- GenerateBoW_predict(path_predict, vocab.dif.absol)
test.dif.absol <- concat.split(bow_test.dif.absol, "V1", ",")
test.dif.absol2 <- test.dif.absol[,4:ncol(test.dif.absol)]

predict_rf <- predict(model_rf.dif.absol, test.dif.absol2)
levels(predict_rf) <- c('I', 'NI')

generar_file_result(path_result,predict_rf, test.dif.absol)


end.time <- Sys.time()
time.taken <- end.time - start.time

print(time.taken)


