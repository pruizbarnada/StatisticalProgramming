#Corinne Phillips (s1852622), Caitlin Mitchell (s1837060), Pablo Ruiz Barnada (s1854579)
#We worked together for almost all the time. Outside of this, Pablo wrote the functions for generating the text fragments,
#and Corinne and Caitlin did Q6 and Q7. We feel we all had equal contributions.


setwd("~/Documents/MSc/Statistical Programming/Practical-1")    #user needs to change working directory
a <- scan("pg10.txt",what="character",skip=104) ## skip contents
n <- length(a)
a <- a[-((n-2886):n)] ## strip license
a <- a[-grep("[0123456789]:[0123456789]",a)] ## strip out verse numbers



##4


#create function split_punct, takes slightly less than a minute

punctuation <- c("\\.",",",":",";","\\?","\\!")                     #create a string of characters containing the punctuation marks we want to separate in the bible text

split_punct <- function(text_to_transform, punctuation){            #function has the text we want to transform, and the punctuation we want to separate as inputs
  x <- text_to_transform                                            #assign the original text to x
  for (punc in punctuation){                                        #looping through each type of punctuation
    text_to_transform <- x                                          #ensures the current text being worked with is the contains the most recently added punctuation marks and therefore new length
    position <- grep(punc, text_to_transform)                       #create a vector containing the indexes of each word containing the punctuation mark in the text 
    text_to_transform <- gsub(punc, "", text_to_transform)          #remove the punctuation marks
    x <-  numeric(length(text_to_transform)+length(position))       #create a vector with length of the current text + the number of that punctuation mark (this current text updates with each loop)
    ind <- position + order(position)                               #indexes to insert punctuation mark, taking into account the shifting of later indexes once punctuation marks are added in the text 
    x[ind] <- punc                                                  #assign the vector entry at these indexes to be the punctuation mark
    x[-ind] <- text_to_transform                                    #assign all other entries to be the current text 
    
    
  }  
  x <- gsub("\\\\", "", x) #account for R special characters
  return(x)}  


a <- split_punct(a,punctuation)

#6a 
lowercase <- tolower(a)         #all words in Bible transformed to lowercase
unique <- unique(lowercase)     #do we use this?
b <- unique(lowercase)          #remove repeated words from the lowercase Bible text

#6b
matches <- match(lowercase, b)  #find the index, from the unique lowercase text, 
                                #of every word in the full lowercase text 

#6c 
counts <- tabulate(matches)     #number of times each word in the unique word list 
                                #appears in the lowercase text

#6d
ordered <- sort.int(counts, decreasing=TRUE)       #ranks counts in decreasing order 



# Code for deciding threshold
decide_threshold <- function(counts, n=500){      # n will be the rough number of most common words we want
  ordered <- sort.int(counts, decreasing=TRUE)    #Order the vector counting no. of times each word appears in text
  threshold <- ordered[n]-1
  max_indexes <- which(counts > threshold)        #obtains all indexes of b corresponding to --
  if (length(max_indexes) - n < 10){
  return(threshold)
  }
  else{
    return("The number of words is not good to define a threshold.")
  }
}

threshold <- decide_threshold(counts)             # Store this threshold



# want to chop b to the 500 most common words 
max_indexes <- which(counts > threshold)          #obtains all indexes of b corresponding to --
                                                  #words that appear more than 162 times in the lowercase text (the found threshold)
size <- length(max_indexes)                       # determining no. of most common words we will be working with, to create matrices
b <- b[max_indexes]                               # returns the words of b corresponding to the max indexes


#7a
updated_matches <- match(lowercase, b)      #index of the most common words vector 
                                            #of the lowrcase Bible

#7b
shifted <- append(updated_matches[-1], NA) 
shifted_twice <- append(shifted[-1], NA)

#matrix with the index of a word + the indices of the next 2 words 
#(NA is displayed if the word is not part of the most common words list)
matrix <- cbind(updated_matches, shifted, shifted_twice) 
print(matrix[790004:790013,])

# 7c 
#index of matrix rows of 3 common words that appear in a row in the text
common_triplets <- which(!is.na(rowSums(matrix, na.rm=FALSE))) 

#matrix of common words in each triplet
common_matrix <- matrix[common_triplets,] 

# 7d 
T <- array(0, dim=c(size, size, size))    #make an empty array T (500 x 500 x 500) according to our threshold


#for loop to build array T 
for(x in 1:nrow(common_matrix)){      #for all the rows in the common matrix
  r <- common_matrix[x,]              # a = x^th row in the matrix
  ai <- r[[1]]   
  aj <- r[[2]]
  ak <- r[[3]]
  T[ai, aj, ak] <- T[ai, aj, ak] + 1  #+1 to the matrix position ai is the row, aj is the column and ak is the matrix
}


#7f

#for loop to build array A
A <- array(0, dim=c(size, size))       
for(x in 1:nrow(common_matrix)){    #A == T, only with back-to-back words
  r <- common_matrix[x,1:2]
  ai <- r[[1]]
  aj <- r[[2]]
  A[ai, aj] <- A[ai, aj] + 1
}

#To build matrix S, select new random word if the previous chosen word is never followed by a "common" word
S <- array(0, dim=c(1, size))       
for(x in 1:nrow(common_matrix)){
  r <- common_matrix[x,1]
  S[r] <- S[r] + 1
}



# 8 
sample_indexes <- c(1:length(max_indexes)) #create a list of 1:500 to sample random indexes from

first_word <- function(){
  first_words_index <- sample(sample_indexes, 1, replace=TRUE, prob=S)       #select a common word's index
  word <- b[first_words_index]                                               #transform index to word
  return(word)
}


second_word <- function(sentence){
  previous_word <- tail(sentence,1)                                       #gets last word of the sentence built
  previous_word_index <- match(previous_word,b)                           #find index (in b) of previous word
  second_words_index <- sample(sample_indexes, 1, replace=TRUE, prob=A[previous_word_index,])       #select a common word's index
  word <- b[second_words_index]
  return(word)
}


third_word <- function(sentence){
  previous_words <- tail(sentence,2)                                       #gets last 2 words of the sentence built
  ultimate_word_index <- match(previous_words[2],b)                        #find index (in b) of the ultimate word in the sentence
  penultimate_word_index <- match(previous_words[1],b)                     #find index (in b) of penultimate word in the sentence   
  third_words_index <- sample(sample_indexes, 1, replace=TRUE, prob=T[penultimate_word_index, ultimate_word_index, ]) #randomly select a common word's index basing probabilties off T
  word <- b[third_words_index]                                             #transform index to a word
  return(word)
}


common_matrix <- data.frame(common_matrix)     #allows us to isolate the columns 


#Function to generate random section of text, specify the number of words 
sentence_generator <- function(number_of_words){
  if (number_of_words < 2){                              #make sure the sentence is long enough
    return('The sentence needs to be longer.')
  }
  else{
    word <- first_word()                  #use the function defined above to randomly select a first word for the text segment
    final_sentence <- word                #initialize the text fragment
    
    for (x in (1:(number_of_words-1))){                      #loops to generate the remaining number of words
      previous_words <- tail(final_sentence,2)                              #gets last 2 words of the sentence built, if there is only one word it returns NA for the missing word
      ultimate_word_index <- match(previous_words[2],b)                     #find index (in b) of ultimate word in the text fragment
      penultimate_word_index <- match(previous_words[1],b)                  #find index (in b) of penultimate word    
      if (TRUE %in% (common_matrix$updated_matches==penultimate_word_index & common_matrix$shifted == ultimate_word_index)){
        final_sentence <- append(final_sentence, third_word(final_sentence))    #generate a new word based on the two previous, if that pattern exists in the common matrix
      }
      else if (TRUE %in% (common_matrix$shifted == ultimate_word_index)){
        final_sentence <- append(final_sentence, second_word(final_sentence))   #otherwise, generate a new word based on the previous, if that word exists in the common matrix
      }
      else{
        final_sentence <- append(final_sentence, first_word())                  #otherwise, generate a random word basing probabilities off vector S
      }
      
    }
    
    return(cat(final_sentence,sep=" ", fill=TRUE))    #return the text fragment using the function cat
  }
}

sentence_generator(50)                                #print a random text fragment with 50 words 



#9

sentence_generator_for_S <- function(number_of_words){      #similar to sentence_generator, but using S to randomly generate each word independently               
  if (number_of_words < 2){
    return('The sentence needs to be longer.')
  }
  else{
    word <- first_word()
    final_sentence <- word                #initialize sentence
    for (x in (1:(number_of_words-1))){
      final_sentence <- append(final_sentence, first_word())
    }
    
    return(cat(final_sentence,sep=" ", fill=TRUE))
    
  }
}

sentence_generator_for_S(50)                     #print a random text fragment with 50 words




# 10 
library(stringr)                                                #load stringr package for the str_to_title function 

for (i in b){                                                   #for each of the common words stored in b
  lowercase_counter <- length(which(a == i))                    #find number of times the word appears as fully lowercase in the original Bible
  all_counter <- length(which(lowercase == i))                  #find number of times the word appears in the Bible (not case sensitive)
  proportion <- lowercase_counter/all_counter                   #find the proportion of times that the word is fully lowercase
  if (proportion < 0.5){                                        #if it appears as fully lowercase less than half of the times, we capitalize the first letter
    index <- which(b==i)
    b[index] <- str_to_title(i)                                 #update the spelling of the word in b
  }
}

print(b)                                                        #print b showing that now some words have been updated to begin with capitals, for instance names like "Jesus".
sentence_generator(50)                                          #print a random text fragment with 50 words



