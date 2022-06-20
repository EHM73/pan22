#para hacer el bucle de pruebas


# Including needed libraries
library(XML)
library(tm)
library(splitstackshape)
library(caret)
library(tidytext)
library(dplyr)
library(ggplot2)

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
    if (verbose) print(f)
    iro<- truth[truth$author == f,2]
    if (verbose) print(truth[truth$author == f,2])
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
GenerateBoW <- function(path, vocabulary, n = 100000, lowcase = TRUE, punctuations = TRUE, numbers = TRUE, whitespaces = TRUE, swlang = "", swl = "", verbose = TRUE) {
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



# GenerateBoW: Given a corpus (training or test), and a vocabulary, obtains the bow representation
GenerateBoW_predict <- function(path, vocabulary, n = 100000, lowcase = TRUE, punctuations = TRUE, numbers = TRUE, whitespaces = TRUE, swlang = "", swl = "", verbose = TRUE) {
  setwd(path)
  
  i <- 0
  bow <- NULL
  # Reading the list of files in the corpus
  files = list.files(pattern="*.xml")
  for (file in files) {
    # Obtaining truth information for the current author
    author <- gsub(".xml", "", file)
    
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
    
    line <- paste("class", ",", line, sep="")
    
    # New row in the vector space model matrix
    bow <- rbind(bow, line)
    i <- i + 1
    
    if (verbose) {
      print(paste(i, author))
    }
  }
  
  return (bow)
}

generar_file_result <- function(path, prediccion,autors)
{
  for(i in 1:nrow(autors)) 
    { 
    tipo = paste('type="',prediccion[i],'"',sep='')
    autor = autors[i,3]
    id = paste('id="',autor,'"',sep='')
    linea=paste('<author' , id, 'lang="en"',tipo,'/>',sep =' ')
    print(linea)
    filename = paste(path,autor,".xml",sep ="")
    file.create(filename,overwrite=TRUE)
    write(linea,filename)  
    }
}

ejecutarmodelo <- function(vtn,vn,k,r,path_training,path_test,lang,swl,verbose = TRUE)
{
  start.time <- Sys.time()
 
  
  # generamos los dos vocabularios para buscar las palabras más representativas 
  # (palabras que usan los ironicos que no usan los NO ironicos y las contrarias) para crear un vocabulario menos pero más representativo.
  print("vocIronico")
  vocabularyIrony <- GenerateVocabularyIrony("I",path_training, vtn, swlang=lang,swl=swl,verbose = verbose)
  print("vocNOIronico")
  vocabularyNOIrony <- GenerateVocabularyIrony("NI",path_training, vtn, swlang=lang,swl=swl,verbose = verbose)
  
  # Frecuencias absolutas
  dif <- full_join(vocabularyIrony,vocabularyNOIrony,by="WORD")
  dif[is.na(dif)]<-0
  dif["absol"]<-abs(dif$FREQ.x - dif$FREQ.y)
  dif <- dif[order(dif$absol,decreasing = TRUE),]
  vocabulary <- dif[1:vn,]
  
  # GENERATING THE BOW FOR THE TRAINING SET
  print("BagOfWords")
  bow_training <- GenerateBoW(path_training, vocabulary,swl=swl,verbose = verbose)
  
  # PREPARING THE VECTOR SPACE MODEL FOR THE TRAINING SET
  
  training <- concat.split(bow_training, "V1", ",")
  training <- cbind(training[,2], training[,4:ncol(training)])
  names(training)[1] <- "theclass"
  
  #Hemos porbado el GLM y lo descartamos
  # Learning a SVM and evaluating it with k-fold cross-validation
  
  train_control <- trainControl( method="repeatedcv", number = k , repeats = r)
  print("Training")
  model_rf <- train( theclass~., data= training, trControl = train_control, method = "rf")
  #print(model_rf)
  
  print(model_rf$results[model_rf$results$mtry==model_rf$finalModel$mtry,])
  bow_test <- GenerateBoW(path_test, vocabulary, swl=swl,verbose = verbose)
  
  # Preparing the vector space model and truth for the test set
  test <- concat.split(bow_test, "V1", ",")
  truth <- as.factor(unlist(test[,2]))
  test <- test[,4:ncol(test)]
  
  # Predicting and evaluating the prediction
  print("predict")
  pred_rf <- predict(model_rf, test)
  print(confusionMatrix(pred_rf, truth))
  
  
  end.time <- Sys.time()
  time.taken <- end.time - start.time
  
  print(time.taken)
}



