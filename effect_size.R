superiority <- function(x, y){
  if (any(is.na(x)) | any(is.na(y))) {
    nas <- unique(c(which(is.na(x)), which(is.na(y))))
    x <- x[-nas]
    y <- y[-nas]
  }
  # all favorable pairs
  P <- sum(sapply(seq_len(length(x)), function(z) sum(z > y)))
  # all unfavorable pairs
  N <- sum(sapply(seq_len(length(x)), function(z) sum(z < y)))
  # all ties
  E <- sum(sapply(seq_len(length(x)), function(z) sum(z == y)))
  # total
  Tot <- P + N + E
  r = (P - N) / (Tot)
  prob = P / Tot
  return(list(fav.pairs = P,
              tot.pairs = Tot,
              ties = E,
              rank.corr = r,
              P = prob))
}

magnitude <- function(x, method = "Cohen"){
  if (method == "Cohen") {
    if (x >= 2.0){
      ans <- 'Huge'
    } else if(x >= 1.20){
      ans <- 'Very large'
    } else if(x >= 0.80){
      ans <- 'Large'
    } else if(x >= 0.50){
      ans <- 'Medium'
    } else if(x >= 0.20){
      ans <- 'Small'
    } else if(x < 0.20){
      ans <- ('No difference')
    }
  }
  else if (method == "superiority") {
    if (x >= 0.92){
      ans <- 'Huge'
    } else if(x >= 0.80){
      ans <- 'Very large'
    } else if(x >= 0.71){
      ans <- 'Large'
    } else if(x >= 0.64){
      ans <- 'Medium'
    } else if(x >= 0.56){
      ans <- 'Small'
    } else if(x < 0.56){
      ans <- ('No difference')
    }
  } else {
    stop("Specified method not valid. Options: 'Cohen', 'superiority'")
  }
  return(ans)
}

#' Cohen's d
cohen <- function(x, y) {
  if (any(is.na(x))) {
    nas <- which(is.na(x))
    x <- x[-nas]
  }
  if (any(is.na(y))) {
    nas <- which(is.na(y))
    y <- y[-nas]
  }
  ans <- (mean(x) - mean(y)) / pooled(x, y)
  if (ans < 0) {
    message("Mean(x) < Mean(y) - taking the opposite of effect size")
    ans <- -ans
  }
  return(ans)
}
#' Pooled standard deviation
pooled <- function(x, y) {
  if (any(is.na(x))) {
    nas <- which(is.na(x))
    x <- x[-nas]
  }
  if (any(is.na(y))) {
    nas <- which(is.na(y))
    y <- y[-nas]
  }
  n.x <- length(x)
  n.y <- length(y)
  var.x <- var(x)
  var.y <- var(y)
  ans <- ((n.x - 1) * var.x + (n.y - 1) * var.y) / (n.x + n.y + 2)
  ans <- sqrt(ans)
  return(ans)
}