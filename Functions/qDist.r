qdist <- function(a, b, q){
  
  # a is a chr vector with shorter text to be searched for in longer text, b
  # q is the ngram n
  
  # TODO
  # make more efficient for vectors of a and b, by using the ngram of a for all b, etc.
  
  library(stringdist)
  
  ## standardise some stuff on both texts
  a <- tolower(a)
  b <- tolower(b)
  
  qm <- qgrams(a,b, q = q)
  
  # convert to logical
  qm <- qm > 0
  
  tot <- ncol(qm) # no of total qgrams
  com <- sum(colSums(qm) == 2) # qgrams in common
  abq <- rowSums(qm) 
  
  #stringdist(a,b, method = "qgram", q = q)
  # should be same as ??? 
  ## This is distance as in no of qgrams not in common
  st.q <- tot - com
  
  # percentage in common of total
  p.tot <- com/tot
  
  # percentage in common in first title
  p.a <- unname(com/abq[1])
  
  list(dist = st.q, percentage.total= p.tot, percentage.a = p.a)
  
}