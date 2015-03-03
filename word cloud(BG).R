# Following is the step by step algorithm of creating a word cloud on a bunch of text files. 
# For simplicity, we are using files in .txt format.
#****ORIGINAL-> http://www.analyticsvidhya.com/blog/2014/05/build-word-cloud-text-mining-tools/
# Step 1 : Identiy & create text files to turn into a cloud (my experiance is to use *.txt type files)
# The first step is to identify & create text files on which you want to create the word cloud.  Store these files in the location “dir path (using /)”. 
# Make sure that you do not have any other file in this location. 
# You can use any location to do this exercise, but for simplicity, try it with this location for the first time.
# 
# Step 2 : Create a corpus from the collection of text files
# The second step is to transform these text files into a R – readable format. 
# The package TM and other text mining packages operate on a format called corpus. 
# Corpus is just a way to store a collection of documents in a R software readable format.

library(tm)
library(wordcloud)
stopwordsBG<-c("при","които","този","един","една", "едно", "едни", "така","когато","който", "тези","кой","ако", "една", "както", "било", "други", "само", "чиято", "към", "на","със","тъй", "през", "към", "с","е","от","за","по","се","която", "и","между","то","като","или","без","около","това","много","малко","освен","във","в","което","над","под","след","може","не","да","тях","нас","бил","били","от","до")
cname<- file.path("C:/Users/Deyan/Documents/R/my library/LordBG")
docs <- Corpus(DirSource(cname))

# Step 3 : Data processing on the text files
# This is the most critical step in the entire process. Here, we will decode the text file by selecting some keywords, which builds up the meaning of the sentence or the sentiments of the author. 
# R makes this step really easy. Here we will make 3 important transformations.
# i. Replace symbols like “/” or “@” with a blank space
# ii. Remove words like “a”, “an”, “the”, “I”, “He” and numbers. This is done to remove any skewness caused by these commonly occurring words.
# iii.  Remove punctuation and finally whitespaces. Note that we are not replacing these with blanks because grammatically they will have an additional blank.

library (SnowballC)
for (j in seq(docs))
{docs[[j]] <- gsub("/"," ",docs[[j]])
 docs[[j]] <- gsub("@"," ",docs[[j]])}


docs <- tm_map(docs,tolower)
docs <- tm_map(docs,removeWords, stopwordsBG)
docs <- tm_map(docs,removeNumbers)
docs <- tm_map(docs,removePunctuation)
docs <- tm_map(docs,stripWhitespace)
docs <- Corpus(VectorSource(docs))

# Step 4 : Create structured data from the text file
# Now is the time to convert this entire corpus into a structured dataset. 
# Note that we have removed all filler words. 
# This is done by a command “DocumentTermMatrix” in R. 
# Execute the following line in your R session to make this conversion.

dtm <- DocumentTermMatrix(docs)


# Step 5 : Making the word cloud using the structured form of the data
# Once, we have the structured format of the text file content, 
# we now make a matrix of word and their frequencies. 
# This matrix will be finally put into the function to build wordcloud.

library(wordcloud)
m <- as.matrix(dtm)
v <- sort(colSums(m),decreasing=TRUE)
head(v,15)
words <- names(v)
d <- data.frame(word=words, freq=v)

# select the min.fric = ??? to change the min friq you want to disply
library(ColorBrewer)
wordcloud(d$word,d$freq,min.freq=4,scale=c(2,0.3),rot.per=0.35,colors=c("grey","dark green", "brown", "dark orange","red"))

wordcloud(d$word,d$freq,min.freq=4,scale=c(2,0.4),colors=c("grey","dodgerblue3", "brown", "dark orange","red"))
#OR
wordcloud(d$word,d$freq, scale=c(2,0.5), max.words=100, random.order=FALSE, rot.per=0.35, use.r.layout=FALSE, colors=brewer.pal(8, "Dark2"))
# http://cran.r-project.org/web/packages/RColorBrewer/RColorBrewer.pdf other palletes
?RColorBrewer 
# in colors=brewer.pal(8, "Set2")
# Accent   8
# Dark2   8
# Paired	 12
# Pastel1	 9
# Pastel2	 8
# Set1	 9
# Set2	 8
# Set3	 12

wordcloud(d$word,d$freq, scale=c(2,0.2), max.words=100, random.order=FALSE, rot.per=0.35, use.r.layout=FALSE, colors=brewer.pal(8, "Set2"))




