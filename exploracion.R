# Including needed libraries
library(XML)
library(tm)
library(splitstackshape)
library(caret)
library(tidytext)
library(dplyr)
library(ggplot2)

start.time <- Sys.time()

# Preparing parameters
n <- 10000   # Number of words in the vocabulary. Usually used 1000 or 10000
vn <- 1000
k <- 10        # Number of folds in cross-validation. Usually used 10
r <- 3       # Number of repeats in cross-validation. Usually used 3
path_training <- "D:/OneDrive - UPV/BigData/Asignaturas/15 Text mining/pan22/training/en"	# Your training path
path_test <- "D:/OneDrive - UPV/BigData/Asignaturas/15 Text mining/pan22/test/en"			# Your test path
lang <- "en"

# Auxiliar functions
# * freq_terms: Given a text, it extracts the n most frequent terms
# * Plot: Given a set of pairs term, frequency, it plots the distribution
# * GenerateVocabulary: Given a corpus (training set), obtains the n most frequent words
# * GenerateBoW: Given a corpus (training or test), and a vocabulary, obtains the bow representation

freq_terms <- function(corpus.preprocessed, n = 1000) { ###### ANTES PONIA 1000
  #https://dk81.github.io/dkmathstats_site/rtext-freq-words.html
  corpus.text <- data_frame(Text = corpus.preprocessed)
  corpus.words <- corpus.text %>% unnest_tokens(output = word, input = Text)
  corpus.wordcounts <- corpus.words %>% count(word, sort = TRUE)
  corpus.frequentterms <- corpus.wordcounts[1:n,]
  names(corpus.frequentterms) <- c("WORD", "FREQ")
  
  return (corpus.frequentterms)
}

Plot <- function(wordcounts) {
  wordcounts %>% 
    filter(FREQ > 70) %>% 
    mutate(WORD = reorder(WORD, FREQ)) %>% 
    ggplot(aes(WORD, FREQ)) + 
    geom_col() +
    coord_flip() +
    labs(x = "Word \n", y = "\n Count ", title = "Frequent Words \n") +
    geom_text(aes(label = FREQ), hjust = 1.2, colour = "white", fontface = "bold") +
    theme(plot.title = element_text(hjust = 0.5), 
          axis.title.x = element_text(face="bold", colour="darkblue", size = 12),
          axis.title.y = element_text(face="bold", colour="darkblue", size = 12))
}
swl <- c("s","amp", "m")


GenerateVocabularyIrony <- function(irony="I",path, n = 1000, lowcase = TRUE, punctuations = TRUE, numbers = TRUE, whitespaces = TRUE, swlang = "", swlist = swl, verbose = TRUE) {
  setwd(path)
  
  # Reading corpus list of files
  files = list.files(pattern="*.xml")
  
  # Reading the truth file
  truth <- read.csv("truth.txt", sep=":", header=FALSE)
  truth <- truth[,c(1,4)]
  colnames(truth) <- c("author", "class")
  
  
  # Reading files contents and concatenating into the corpus.raw variable
  corpus.raw <- NULL
  i <- 0
  for (file in files) {
    f <- strsplit(file,"\\.")[[1]][1]
    print("hola")
    print(f)
    iro<- truth[truth$author == f,2]
    print(truth[truth$author == f,2])
    if (iro==irony){
      xmlfile <- xmlTreeParse(file, useInternalNodes = TRUE)
      corpus.raw <- c(corpus.raw, xpathApply(xmlfile, "//document", function(x) xmlValue(x)))
      i <- i + 1
    }

    if (verbose) print(paste(i, " ", file))
  }
  
  # Preprocessing the corpus
  corpus.preprocessed <- corpus.raw
  
  if (lowcase) {
    if (verbose) print("Tolower...")
    corpus.preprocessed <- tolower(corpus.preprocessed)
  }
  
  if (punctuations) {
    if (verbose) print("Removing punctuations...")
    corpus.preprocessed <- removePunctuation(corpus.preprocessed)
  }
  
  if (numbers) {
    if (verbose) print("Removing numbers...")
    corpus.preprocessed <- removeNumbers(corpus.preprocessed)
  }
  
  if (whitespaces) {
    if (verbose) print("Stripping whitestpaces...")
    corpus.preprocessed <- stripWhitespace(corpus.preprocessed)
  }
  
  if (swlang!="")	{
    if (verbose) print(paste("Removing stopwords for language ", swlang , "..."))
    corpus.preprocessed <- removeWords(corpus.preprocessed, stopwords(swlang))
  }
  
  if (length(swlist)>1) {  ##### (swlist!="")
    if (verbose) print("Removing provided stopwords...")
    corpus.preprocessed <- removeWords(corpus.preprocessed, swlist)
  }
  
  # Generating the vocabulary as the n most frequent terms
  if (verbose) print("Generating frequency terms")
  
  corpus.frequentterms <- freq_terms(corpus.preprocessed, n)
  
  if (verbose) Plot(corpus.frequentterms)
  
  return (corpus.frequentterms)
}
# generamos los dos vocabularios para buscar las palabras más representativas 
# (palabras que usan los ironicos que no usan los NO ironicos y las contrarias) para crear un vocabulario menos pero más representativo.
vocabularyIrony <- GenerateVocabularyIrony("I",path_training, n, swlang=lang)
vocabularyNOIrony <- GenerateVocabularyIrony("NI",path_training, n, swlang=lang)

# Frecuencias absolutas
dif <- full_join(vocabularyIrony,vocabularyNOIrony,by="WORD")
dif[is.na(dif)]<-0
dif["diff"]<-abs(dif$FREQ.x - dif$FREQ.y)
# Frecuencias relativas para que las palabras más frecuentes no tengan mayor peso
total.ironia = sum(dif$FREQ.x)
total.no.ironia = sum(dif$FREQ.y)
dif["Frec.relativa.irony"] <- (dif$FREQ.x/total.ironia) * 100
dif["Frec.relativa.no.irony"] <- (dif$FREQ.y/total.no.ironia) * 100
dif["dif.frec.relativa"]<-abs(dif$Frec.relativa.irony - dif$Frec.relativa.no.irony)

# Ordenamos por la diferencia de freciancias relativas y nos quedamos con 50 más altas (diferentes)
#dif <- sort(dif$dif.frec.relativa, decreasing = TRUE) #este ordenar no va
dif <- dif[order(dif$dif.frec.relativa,decreasing = TRUE),]
vocab.dif <- dif[1:vn,]

# GenerateVocabulary: Given a corpus (training set), obtains the n most frequent words
GenerateVocabulary <- function(path, n = 1000, lowcase = TRUE, punctuations = TRUE, numbers = TRUE, whitespaces = TRUE, swlang = "", swlist = swl, verbose = TRUE) {
  setwd(path)
  
  # Reading corpus list of files
  files = list.files(pattern="*.xml")
  
  # Reading files contents and concatenating into the corpus.raw variable
  corpus.raw <- NULL
  i <- 0
  for (file in files) {
    xmlfile <- xmlTreeParse(file, useInternalNodes = TRUE)
    corpus.raw <- c(corpus.raw, xpathApply(xmlfile, "//document", function(x) xmlValue(x)))
    i <- i + 1
    if (verbose) print(paste(i, " ", file))
  }
  
  # Preprocessing the corpus
  corpus.preprocessed <- corpus.raw
  
  if (lowcase) {
    if (verbose) print("Tolower...")
    corpus.preprocessed <- tolower(corpus.preprocessed)
  }
  
  if (punctuations) {
    if (verbose) print("Removing punctuations...")
    corpus.preprocessed <- removePunctuation(corpus.preprocessed)
  }
  
  if (numbers) {
    if (verbose) print("Removing numbers...")
    corpus.preprocessed <- removeNumbers(corpus.preprocessed)
  }
  
  if (whitespaces) {
    if (verbose) print("Stripping whitestpaces...")
    corpus.preprocessed <- stripWhitespace(corpus.preprocessed)
  }
  
  if (swlang!="")	{
    if (verbose) print(paste("Removing stopwords for language ", swlang , "..."))
    corpus.preprocessed <- removeWords(corpus.preprocessed, stopwords(swlang))
  }
  
  if (length(swlist)>1) {  ##### (swlist!="")
    if (verbose) print("Removing provided stopwords...")
    corpus.preprocessed <- removeWords(corpus.preprocessed, swlist)
  }
  
  # Generating the vocabulary as the n most frequent terms
  if (verbose) print("Generating frequency terms")
  
  corpus.frequentterms <- freq_terms(corpus.preprocessed, n)
  
  if (verbose) Plot(corpus.frequentterms)
  
  return (corpus.frequentterms)
}

# GenerateBoW: Given a corpus (training or test), and a vocabulary, obtains the bow representation
GenerateBoW <- function(path, vocabulary, n = 100000, lowcase = TRUE, punctuations = TRUE, numbers = TRUE, whitespaces = TRUE, swlang = "", swlist = swl, verbose = TRUE) {
  setwd(path)
  
  # Reading the truth file
  truth <- read.csv("truth.txt", sep=":", header=FALSE)
  truth <- truth[,c(1,4)]
  colnames(truth) <- c("author", "class")
  
   i <- 0
  bow <- NULL
  # Reading the list of files in the corpus
  files = list.files(pattern="*.xml")
  for (file in files) {
    # Obtaining truth information for the current author
    author <- gsub(".xml", "", file)
    class <- truth[truth$author==author,"class"]
    
    if (class=="I") {
      class = "ironic"
    } else {
      class = "normal"
    }
    
    # Reading contents for the current author
    xmlfile <- xmlTreeParse(file, useInternalNodes = TRUE)
    txtdata <- xpathApply(xmlfile, "//document", function(x) xmlValue(x))
    
    # Preprocessing the text
    if (lowcase) {
      txtdata <- tolower(txtdata)
    }
    
    if (punctuations) {
      txtdata <- removePunctuation(txtdata)
    }
    
    if (numbers) {
      txtdata <- removeNumbers(txtdata)
    }
    
    if (whitespaces) {
      txtdata <- stripWhitespace(txtdata)
    }
    
    # Building the vector space model. For each word in the vocabulary, it obtains the frequency of occurrence in the current author.
    line <- author
    freq <- freq_terms(txtdata, n)
    for (word in vocabulary$WORD) {
      thefreq <- 0
      #if (length(freq[freq$WORD==word,"FREQ"])>0) {
      if (is.na(freq[freq$WORD==word, "FREQ"]$FREQ[1])) {
        0
      } else {
        thefreq <- freq[freq$WORD==word,"FREQ"]$FREQ[1]
      }
      
      line <- paste(line, ",", thefreq, sep="")
    }
    
    line <- paste(class, ",", line, sep="")
    
    # New row in the vector space model matrix
    bow <- rbind(bow, line)
    i <- i + 1
    
    if (verbose) {
      print(paste(i, author, class))
    }
  }
  
  return (bow)
}

# GENERATE VOCABULARY
vocabulary <- GenerateVocabulary(path_training, n, swlang=lang)

# GENERATING THE BOW FOR THE TRAINING SET
bow_training <- GenerateBoW(path_training, vocabulary)
bow_training.dif <- GenerateBoW(path_training, vocab.dif)

# PREPARING THE VECTOR SPACE MODEL FOR THE TRAINING SET
training <- concat.split(bow_training, "V1", ",")
training <- cbind(training[,2], training[,4:ncol(training)])
names(training)[1] <- "theclass"

# PREPARING THE VECTOR SPACE MODEL FOR THE TRAINING SET
training.dif <- concat.split(bow_training.dif, "V1", ",")
training.dif <- cbind(training.dif[,2], training.dif[,4:ncol(training.dif)])
names(training.dif)[1] <- "theclass"




# generación de varios modelos para compararlos
train_control <- trainControl( method="repeatedcv", number = k , repeats = r)
model_SVM <- train( theclass~., data= training, trControl = train_control, method = "svmLinear")
model_rf <- train( theclass~., data= training, trControl = train_control, method = "rf")
#model_adaboost <- train( theclass~., data= training, trControl = train_control, method = "adaboost")
#model_amdai <- train( theclass~., data= training, trControl = train_control, method = "amdai")
print(model_SVM)
print(model_rf)
#print(model_adaboost)
#print(model_amdai)


# Learning a SVM and evaluating it with k-fold cross-validation
train_control <- trainControl( method="repeatedcv", number = k , repeats = r)
model_SVM.dif <- train( theclass~., data= training.dif, trControl = train_control, method = "svmLinear")
model_rf.dif <- train( theclass~., data= training.dif, trControl = train_control, method = "rf")
#model_adaboost.50 <- train( theclass~., data= training.50, trControl = train_control, method = "adaboost")
#model_amdai.50 <- train( theclass~., data= training.50, trControl = train_control, method = "amdai")
print(model_SVM.dif)
print(model_rf.dif)
#print(model_adaboost.50)
#print(model_amdai.50)


end.time <- Sys.time()
time.taken <- end.time - start.time

print(time.taken)


