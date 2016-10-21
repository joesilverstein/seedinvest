# create some files
td = tempfile()
dir.create(td)
write( c("dog", "cat", "mouse"), file=paste(td, "D1", sep="/") )
write( c("hamster", "mouse", "sushi"), file=paste(td, "D2", sep="/") )
write( c("dog", "monster", "monster"), file=paste(td, "D3", sep="/") )
# read them, create a document-term matrix
textmatrix(td)
# read them, drop german stopwords
data(stopwords_de)
textmatrix(td, stopwords=stopwords_de)
# read them based on a controlled vocabulary
voc = c("dog", "mouse")
textmatrix(td, vocabulary=voc, minWordLength=1)
# clean up
unlink(td, recursive=TRUE)

###########################

# http://meefen.github.io/blog/2013/03/11/analyze-text-similarity-in-r-latent-semantic-analysis-and-multidimentional-scaling/

# load required libraries
library(tm)
library(ggplot2)
library(lsa)

# 1. Prepare mock data
text <- c("transporting food by cars will cause global warming. so we should go local.",
          "we should try to convince our parents to stop using cars because it will cause global warming.",
          "some food, such as mongo, requires a warm weather to grow. so they have to be transported to canada.",
          "a typical electronic circuit can be built with a battery, a bulb, and a switch.",
          "electricity flows from batteries to the bulb, just like water flows through a tube.",
          "batteries have chemical energe in it. then electrons flow through a bulb to light it up.",
          "birds can fly because they have feather and they are light.", "why some birds like pigeon can fly while some others like chicken cannot?",
          "feather is important for birds' fly. if feather on a bird's wings is removed, this bird cannot fly.")
view <- factor(rep(c("view 1", "view 2", "view 3"), each = 3))
df <- data.frame(text, view, stringsAsFactors = FALSE)

# prepare corpus
corpus <- Corpus(VectorSource(df$text))
# The latest version of tm (0.60) made it so you can't use functions with tm_map that operate on simple character values any more.
# http://stackoverflow.com/questions/24771165/r-project-no-applicable-method-for-meta-applied-to-an-object-of-class-charact
corpus <- tm_map(corpus, content_transformer(tolower)) # convert to lowercase
corpus <- tm_map(corpus, removePunctuation) # remove punctuation
# Remove "stopwords" from text
# Stopwords are words to remove from the text. They are given here:
# http://jmlr.csail.mit.edu/papers/volume5/lewis04a/a11-smart-stop-list/english.stop
corpus <- tm_map(corpus, function(x) removeWords(x, stopwords("english")))
# Stem words using Porter's stemming algorithm
corpus <- tm_map(corpus, stemDocument, language = "english")
corpus  # check corpus

## A corpus with 9 text documents

# 2. MDS with raw term-document matrix compute distance matrix
td.mat <- as.matrix(TermDocumentMatrix(corpus))
dist.mat <- dist(t(as.matrix(td.mat)))
dist.mat  # check distance matrix

# MDS
fit <- cmdscale(dist.mat, eig = TRUE, k = 2)
points <- data.frame(x = fit$points[, 1], y = fit$points[, 2])
ggplot(points, aes(x = x, y = y)) + geom_point(data = points, aes(x = x, y = y,
                                                                  color = df$view)) + geom_text(data = points, aes(x = x, y = y - 0.2, label = row.names(df)))

# 3. MDS with LSA
td.mat.lsa <- lw_bintf(td.mat) * gw_idf(td.mat)  # weighting
lsaSpace <- lsa(td.mat.lsa)  # create LSA space
dist.mat.lsa <- dist(t(as.textmatrix(lsaSpace)))  # compute distance matrix
dist.mat.lsa  # check distance mantrix

# MDS
fit <- cmdscale(dist.mat.lsa, eig = TRUE, k = 2)
points <- data.frame(x = fit$points[, 1], y = fit$points[, 2])
ggplot(points, aes(x = x, y = y)) + geom_point(data = points, aes(x = x, y = y,
                                                                  color = df$view)) + geom_text(data = points, aes(x = x, y = y - 0.2, label = row.names(df)))

##################

# http://crunch.kmi.open.ac.uk/w/index.php/Tutorials
# http://crunch.kmi.open.ac.uk/people/~fwild/services/lsa-indexing.Rmw




##################

### Using SeedInvest dataset

library(tm)
library(ggplot2)
library(lsa)

# prepare corpus
corpus <- Corpus(VectorSource(dfComplete$Use.of.Funds))
# The latest version of tm (0.60) made it so you can't use functions with tm_map that operate on simple character values any more.
# http://stackoverflow.com/questions/24771165/r-project-no-applicable-method-for-meta-applied-to-an-object-of-class-charact
corpus <- tm_map(corpus, content_transformer(tolower)) # convert to lowercase
corpus <- tm_map(corpus, removePunctuation) # remove punctuation
# Remove "stopwords" from text
# Stopwords are words to remove from the text. They are given here:
# http://jmlr.csail.mit.edu/papers/volume5/lewis04a/a11-smart-stop-list/english.stop
corpus <- tm_map(corpus, function(x) removeWords(x, stopwords("english")))
# Stem words using Porter's stemming algorithm
corpus <- tm_map(corpus, stemDocument, language = "english")
corpus  # check corpus

# Create Bag Of Words matrix
td.mat <- as.matrix(TermDocumentMatrix(corpus))
# Only 31 relevant word stems are used! Might not even need to reduce the dimensionality.

# create LSA space.
# This is essentially PCA to reduce the number of features, and might be unecessary since there aren't that many
lsaSpace <- lsa(td.mat, dims = dimcalc_share(share=0.5))
lsaSpace$tk # The partial matrix T contains the term loadings on the factors.
lsaSpace$dk # The partial matrix D contains the document loadings on the factors.
# The partial matrix S contains the singular values of the factors.
lsaSpace$sk # Diagonal entries of the diagonal matrix S

# Use these as the additional features in the regression?
X = as.textmatrix(lsaSpace)
X = t(lsaSpace$tk %*% diag(lsaSpace$sk) %*% t(lsaSpace$dk))

# proximity = cosine(td.mat) # raw (unprocessed) measure of proximity
# proximityLSASpace = cosine(as.textmatrix(lsaSpace)) # this measure of proximity is much better than the former






