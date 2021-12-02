library("rtweet")

app = "YOUR_APP_HERE"
key = "YOUR_KEY-HERE"
secret = "YOUR_KEY-HERE"
access_token = "YOUR_KEY-HERE"
access_secret = "YOUR_KEY-HERE"

twitter_token = create_token(app, key, secret, access_token, access_secret, set_renv= FALSE)
tweets = search_tweets('Ryan Reynolds', n = 2000, type = "recent", token = twitter_token, include_rts= FALSE)

library("tm")
library("SnowballC")
#Create a corpus
corpus = Corpus(VectorSource(tweets$text))
#Covert the characters to ASCII
corpus = tm_map(corpus, function(x) iconv(x, to='ASCII'))
#Remove unwanted characters, stopwards and stem the words
corpus = tm_map(corpus, removeNumbers)
corpus = tm_map(corpus, removePunctuation)
corpus = tm_map(corpus, stripWhitespace)
corpus = tm_map(corpus, tolower)
corpus = tm_map(corpus, removeWords, stopwords('english'))
corpus = tm_map(corpus, stemDocument)
#Create a weighted document term matrix
tweets.tdm = TermDocumentMatrix(corpus)
tweets.tdmw = weightTfIdf(tweets.tdm)
tweets.matrix = as.matrix(tweets.tdmw)

library("wordcloud")
freqs = rowSums(tweets.matrix)
wordcloud(names(freqs), freqs, random.order = FALSE, min.freq = 3)


#find the number of clusters by using cosine distance
##normalise all tweets to unit length
norm.tweets.matrix = diag(1/sqrt(rowSums(tweets.matrix^2))) %*% tweets.matrix
##create the distance matrix
D = dist(norm.tweets.matrix, method = "euclidean")^2/2
##perform DMS using 100 dimensions
mds.tweets.matrix = cmdscale(D, k = 100)
n = 15
SSW = rep(0, n)
for (a in 1:n) {
  K = kmeans(mds.tweets.matrix, a, nstart = 20)
  SSW[a] = K$tot.withinss
}
##plot the result
plot(1:15, SSW, type = "b")

#cluster the tweets using K-means clustering
K = kmeans(mds.tweets.matrix, 4, nstart = 20)

#the number of tweets in each cluster
table(K$cluster)

#perform DMS using 2 dimensions
mds2.tweet.matrix = cmdscale(D, k=2)
plot(mds2.tweet.matrix, col = K$cluster)

#create a dendrogram of words in cluster 3
cluster.number = 3
##find position of tweets in cluster 3
cluster3TweetsId = which(K$cluster == cluster.number)
##extract tweets vectors for cluster 3
cluster3Tweets = tweets.matrix[cluster3TweetsId,]
##find only the terms that appear in at least 3 tweets
cluster3.frequent.words = which(colSums(cluster3Tweets > 0) > 3)
cluster3.term.matrix = cluster3Tweets[,cluster3.frequent.words]
##make sure that all term vectors (columns of matrix) have a norm of 1
cluster3.norm.term.matrix = cluster3.term.matrix %*% diag(1/sqrt(colSums(cluster3.term.matrix^2)))
##preserve column names
colnames(cluster3.norm.term.matrix) = colnames(cluster3.term.matrix)
##compute the Euclidean distance for cluster3.norm.term.matrix
cluster3.D = dist(t(cluster3.norm.term.matrix), method = "euclidean")^2/2
##hierarchical clustering
cluster3.h = hclust(cluster3.D, method="complete")
plot(cluster3.h)


# Find position of tweets in cluster 4.
cluster4TweetsId = which(K$cluster == 4)
# Extract tweets vectors for cluster 4.
cluster4Tweets = tweets.matrix[cluster4TweetsId,]

# Find only the terms that appear in at least 20 tweets.
cluster4.frequent.words = which(colSums(cluster4Tweets > 0) > 25)
cluster4.term.matrix = cluster4Tweets[,cluster4.frequent.words]

# Make sure that all term vectors (columns of matrix) have a norm of 1.
cluster4.norm.term.matrix = cluster4.term.matrix %*% diag(1/sqrt(colSums(cluster4.term.matrix^2)))
# Preserve column names.
colnames(cluster4.norm.term.matrix) = colnames(cluster4.term.matrix)
# Compute the Euclidean distance for cluster4.norm.term.matrix.
cluster4.D = dist(t(cluster4.norm.term.matrix), method = "euclidean")^2/2
# Hierarchical clustering.
cluster4.h = hclust(cluster4.D, method="complete")
plot(cluster4.h, main = "Cluster 4 Dendrogram")

#sort the tweets file based on the retweet_count column in the descending order
sortedtweets = tweets[order( -tweets[,14] ),]
#extract the top 100 rows which are top 100 tweets that are retweeted the most
retweets = head(sortedtweets, n = 100)
#identify the users of these tweets
users = users_data(retweets)
users_handle = as.matrix(retweets$screen_name)
#get the follower count
followers = retweets$followers_count
#get the statuses count
statuses = retweets$statuses_count

#covert these two variables into a data frame
statistic = data.frame(followers, statuses)
#group these data into groups and count the number of each group
statistic1 = sum(statistic$followers < 10000 & statistic$statuses < 10000)
statistic2 = sum(statistic$followers < 10000 
                 & statistic$statuses >= 10000 & statistic$statuses < 100000)
statistic3 = sum(statistic$followers < 10000 & statistic$statuses >= 100000)
statistic4 = sum(statistic$followers >= 10000 & statistic$followers <100000
                 & statistic$statuses < 10000)
statistic5 = sum(statistic$followers >= 10000 & statistic$followers <100000
                 & statistic$statuses >= 10000 & statistic$statuses < 100000)
statistic6 = sum(statistic$followers >= 10000 & statistic$followers <100000
                 & statistic$statuses >= 100000)
statistic7 = sum(statistic$followers >= 100000 & statistic$statuses < 10000)
statistic8 = sum(statistic$followers >= 100000 
                 & statistic$statuses >= 10000 & statistic$statuses < 100000)
statistic9 = sum(statistic$followers >= 100000 & statistic$statuses >= 100000)
#create a new table with new data
tab = matrix(data = c(statistic1, statistic2, statistic3, statistic4, 
                      statistic5, statistic6, statistic7, statistic8, statistic9), 
             nrow = 3, byrow = TRUE)
colnames(tab) = c("0 - 9999", "10000 - 99999", "100000+")
rownames(tab) = c("0 - 9999", "10000 - 99999", "100000+")

stretchTable = function(tab, variableNames) {
     tabx = rep(rownames(tab), rowSums(tab))
     l = ncol(tab)
     m = nrow(tab)
     cn = colnames(tab)
     taby = c()
     for (a in 1:m) {
       for (b in 1:l) {
         taby = c(taby, rep(cn[b], tab[a,b]))
       }
     }
 
     d = data.frame(x = tabx, y = taby)
     colnames(d) = variableNames
     return(d)
}

Z = stretchTable(tab, c("Followers","Statuses"))

## define the functions to use
expectedIndependent = function(X) {
     n = sum(X)
     p = rowSums(X)/sum(X)
     q = colSums(X)/sum(X)
     return(p %o% q * n) # outer product creates table
}
 
chiSquaredStatistic = function(X, E) {
     return(sum((X - E)^2/E))
}
 
## compute the expected table
E = expectedIndependent(table(Z)) # compute expected counts if independent
 
## compute the randomisation distribution
x2dist = replicate(1000, { # compute 1000 randomised chi-squared statistics
   followersShuffle = sample(Z$Followers)
   statusesShuffle = sample(Z$Statuses)
   Yindep = table(followersShuffle, statusesShuffle)
   chiSquaredStatistic(Yindep, E)
   })
hist(x2dist)
x2 = chiSquaredStatistic(table(Z), E)

## pval is the proportion of x2dist that is greater than x2
pval = mean(x2dist > x2)
print(pval)

library("twitteR")
library("igraph")
app = "YOUR_APP_HERE"
key = "YOUR_KEY-HERE"
secret = "YOUR_KEY-HERE"
access_token = "YOUR_KEY-HERE"
access_secret = "YOUR_KEY-HERE"
setup_twitter_oauth(key, secret, access_token, access_secret)

user = getUser("VancityReynolds")
#identify how many friends he has
infor = user$toDataFrame()
infor$friendsCount
#download his friends
friends = user$getFriends(999)
#examine how many followers do these friends have
follower.count = c()
for (a in c(1:length(friends))){
  follower.count[a] = friends[[a]]$getFollowersCount()
}
#find the top 10 most popular friends - 10 friends that have the most followers
count.followers = function(friends) {
  follower.count = c()
  for (a in c(1:length(friends))) {
    follower.count[a] = friends[[a]]$getFollowersCount()
  }
  return(follower.count)
}
friendFollowCount = count.followers(friends)
friendPosition = order(friendFollowCount, decreasing = TRUE)[1:10]
topFriends = friends[friendPosition]
#get their name
friend.names = c()
n = length(topFriends)
for (a in 1:n) {
  friend.names[a] = topFriends[[a]]$getScreenName()
}

#download 100 friends from each of the 10 most popular friends
more.friends = list()
n = length(topFriends)
for (a in 1:n) {
  more.friends[[a]] = topFriends[[a]]$getFriends(100)
}
#store all 100 screen names in the variable friend.names.
friend.names = c()
n = length(friends)
for (a in 1:n) {
  friend.names[a] = friends[[a]]$getScreenName()
}
#write the function to build the edge list
user.to.edgelist <- function(user, friends) {
  
  # create the list of friend screen names
  friend.names = c()        
  for (a in c(1:length(friends))) {
    friend.names[a] = friends[[a]]$getScreenName()
  } 
  
  user.name = rep(user$getScreenName(), length(friends)) # repeat user's name
  el = cbind(user.name,friend.names) # bind the columns to create a matrix
  
  return(el)
}
el.ryan = user.to.edgelist(user, friends)
for (a in c(1:length(more.friends))) {
  el.friend = user.to.edgelist(topFriends[[a]], more.friends[[a]])
  el.ryan = rbind(el.ryan, el.friend)  # append the new edge list to the old one.
}
#create the graph
g = graph.edgelist(el.ryan)
plot(g, layout = layout.fruchterman.reingold, vertex.size = 5)
g2 = subgraph(g, which(degree(g, mode = "all") > 4))
plot(g2)


#obtain the probaility transition matrix T
A = t(as.matrix(get.adjacency(g2)))
print(A)
normalise = function(x) {
  if(sum(x)!=0){
    return(x/sum(x))
  } else return(0)
}
adjacency.to.probability = function(A) {
  cols = ncol(A)
  for (a in 1:cols) {
    A[, a] = normalise(A[, a])
  }
  return(A)
}

T = adjacency.to.probability(A)

#create the random jump matrix J
J = matrix(rep(1/11, 11 * 11), 11, 11)

#Combine the T and J to obtain the matrix M
alpha = 0.8
M = alpha * T + (1 - alpha) * J
#Normalise the columns of M to obtain an ergodic transition probability matrix.
M = adjacency.to.probability(M)
difference = function(x,y) {
  return(sqrt(sum((x - y)^2)))
}
stationary.distribution = function(T) {
  # first create the initial state distribution
  n = ncol(T)
  p = rep(0, n)
  p[1] = 1
  
  # now take a random walk until the state distribution reaches the
  # stationary distribution.
  p.old = rep(0, n)
  while (difference(p, p.old) > 1e-06) {
    p.old = p
    p = T %*% p.old
  }
  return(p)
}
p = stationary.distribution(M)