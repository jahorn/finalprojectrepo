# This is the code to my Shiny word cloud app

library(shiny)
library(tm)
library(memoise)
library(wordcloud)
library(stringr)
library(devtools)
library(twitteR)

api_key <- 	"KX9Sk6lCenrrn2yHCe2XKIVKi"
api_secret <- "TNbtVYjfzEfhWKKeKCG4MWbnzLapTyrCP5JhKx5pelVWvbwBO4"
access_token <- "927639225461366785-ct1z0xLv20fctGskDaVUwdBncX3udNw"
access_token_secret <- "l4C0SHwagaACa21SfejxSW79s1fg22Wm3vUamaJrh7Lkl"



setup_twitter_oauth(api_key, api_secret, access_token, access_token_secret)

# I am getting tweets about chipotle from the beginning of the year and on in "tweets"
# I then use the commented code below to clean the text of the tweets before I write them into a table to save and use for the word cloud

# tweets <- searchTwitter("chipotle", n=7000, lang="en", since="2017-01-01")

# Transform tweets list into a data frame
# tweets.df <- twListToDF(tweets)

# look at tweets about their queso; are people actually happy??:
# tweets.chipotle <- searchTwitter("chipotle", n=2000, lang="en", since="2017-09-12")
# tweets.chipotle <- twListToDF(tweets.chipotle)
# tweets.queso <- tweets.chipotle[grep("queso", tweets.chipotle$text),]

# clean_tweet = gsub("&amp", "", tweets.df$text)
# clean_tweet = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", clean_tweet)
# clean_tweet = gsub("@\\w+", "", clean_tweet)
# clean_tweet = gsub("[[:punct:]]", "", clean_tweet)
# clean_tweet = gsub("[[:digit:]]", "", clean_tweet)
# clean_tweet = gsub("http\\w+", "", clean_tweet)
# clean_tweet = gsub("[ \t]{2,}", "", clean_tweet)
# clean_tweet = gsub("^\\s+|\\s+$", "", clean_tweet)
# clean_tweet <- str_replace_all(clean_tweet," "," ")
# #clean_tweet <- str_replace_all(clean_tweet, "http://t.co/[a-z,A-Z,0-9]*{8}","")
# clean_tweet <- str_replace(clean_tweet,"RT @[a-z,A-Z]*: ","")
# clean_tweet <- str_replace_all(clean_tweet,"#[a-z,A-Z]*","")
# clean_tweet <- str_replace_all(clean_tweet,"@[a-z,A-Z]*","")  
# clean_tweet <- gsub("@\\w+ *", "", clean_tweet)
# clean_tweet <- gsub("#\\w+ *", "", clean_tweet)
# clean_tweet <- gsub("\n", " ", clean_tweet)
# clean_tweet <- gsub("[^a-zA-Z #]","",clean_tweet)    # "a-zA-Z #" are the things we need
# clean_tweet <- gsub("https\\w+ *", "", clean_tweet)
# clean_tweet <- tolower(clean_tweet)
# 
# 
# 
# clean_tweet_queso = gsub("&amp", "", tweets.queso$text)
# clean_tweet_queso = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", clean_tweet_queso)
# clean_tweet_queso = gsub("@\\w+", "", clean_tweet_queso)
# clean_tweet_queso = gsub("[[:punct:]]", "", clean_tweet_queso)
# clean_tweet_queso = gsub("[[:digit:]]", "", clean_tweet_queso)
# clean_tweet_queso = gsub("http\\w+", "", clean_tweet_queso)
# clean_tweet_queso = gsub("[ \t]{2,}", "", clean_tweet_queso)
# clean_tweet_queso = gsub("^\\s+|\\s+$", "", clean_tweet_queso)
# clean_tweet_queso <- str_replace_all(clean_tweet_queso," "," ")
# #clean_tweet_queso <- str_replace_all(clean_tweet_queso, "http://t.co/[a-z,A-Z,0-9]*{8}","")
# clean_tweet_queso <- str_replace(clean_tweet_queso,"RT @[a-z,A-Z]*: ","")
# clean_tweet_queso <- str_replace_all(clean_tweet_queso,"#[a-z,A-Z]*","")
# clean_tweet_queso <- str_replace_all(clean_tweet_queso,"@[a-z,A-Z]*","")  
# clean_tweet_queso <- gsub("@\\w+ *", "", clean_tweet_queso)
# clean_tweet_queso <- gsub("#\\w+ *", "", clean_tweet_queso)
# clean_tweet_queso <- gsub("\n", " ", clean_tweet_queso)
# clean_tweet_queso <- gsub("[^a-zA-Z #]","",clean_tweet_queso)    # "a-zA-Z #" are the things we need
# clean_tweet_queso <- gsub("https\\w+ *", "", clean_tweet_queso)
# clean_tweet_queso <- tolower(clean_tweet_queso)

# setwd("C:/RStudio & Git/Chipotle shiny app")


# Create a table with the clean tweets about Chipotle
#write.table(clean_tweet, "clean_tweet.txt")
#read.table("clean_tweets.txt")


# Create a table with the clean tweets about Chipotle's queso
#write.table(clean_tweet_queso, "clean_tweets_queso.txt")
#read.table("clean_tweets_queso.txt")

# Now I used these .txt files to create Corpus for each one and in the end create a word cloud, I used some of the code from the Shiny gallery

books <<- list("Chipotle tweets" = "clean_tweet", "Chipotle queso tweets" ="clean_tweets_queso")

getTermMatrix <- memoise(function(book) {
  # Careful not to let just any name slip in here; a
  # malicious user could manipulate this value.
  if (!(book %in% books))
    stop("Unknown book")
  
  text <- readLines(sprintf("./%s.txt", book))#,
                    #encoding="UTF-8")



#not sure what to put in clean_tweet spot so also looks at tweets.queso (should say text but doesn't work that way)
wish.Corpus <- Corpus(VectorSource(text)) #works with text = clean_tweet
wish.Corpus <- tm_map(wish.Corpus, removeWords, stopwords('english'))
wish.Corpus <- tm_map(wish.Corpus, removeWords, 
                      c('just', 'like', 'dont', 'get', 'one', 'amp', stopwords('english')))
myCorpus = Corpus(VectorSource(wish.Corpus))
myCorpus = tm_map(myCorpus, content_transformer(tolower))
myCorpus = tm_map(myCorpus, removePunctuation)
myCorpus = tm_map(myCorpus, removeNumbers)
myCorpus = tm_map(myCorpus, removeWords,
                  c(stopwords("SMART"), "thy", "thou", "thee", "the", "and", "but"))
myDTM = TermDocumentMatrix(myCorpus,
                           control = list(minWordLength = 1))
m = as.matrix(myDTM)

sort(rowSums(m), decreasing = TRUE)
})


ui <- fluidPage( 
                titlePanel("Word Cloud"),
                 
                 sidebarLayout(
                   # Sidebar with a slider and selection inputs
                   sidebarPanel(
                     selectInput("selection", "Choose word(s):",
                                 choices = books),
                     actionButton("update", "Change"),
                     hr(),
                     sliderInput("freq",
                                 "Minimum Frequency:",
                                 min = 1,  max = 50, value = 15),
                     sliderInput("max",
                                 "Maximum Number of Words:",
                                 min = 1,  max = 300,  value = 100)
                   ),
                   
                   # Show Word Cloud
                   mainPanel(
                     plotOutput("plot")
                   )
                 )
)
  

server <- function(input, output, session) { terms <- reactive({
  # Change when the "update" button is pressed...
  input$update
  # ...but not for anything else
  isolate({
    withProgress({
      setProgress(message = "Processing corpus...")
      getTermMatrix(input$selection)
    })
  })
})

# Make the wordcloud drawing predictable during a session
wordcloud_rep <- repeatable(wordcloud)

output$plot <- renderPlot({
  v <- terms()
  wordcloud_rep(names(v), v, scale=c(4,0.5),
                min.freq = input$freq, max.words=input$max,
                colors=brewer.pal(8, "Dark2"))
})
}

shinyApp(ui = ui, server = server)
#