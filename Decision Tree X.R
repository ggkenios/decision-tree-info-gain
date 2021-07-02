###########
## INPUT ##

y <- read.csv("")

# Check if the target column is the last one:
#y <- y[,c(1, 2, 3, 5, 4)];y
target <- y[, ncol(y)]

####################
##  1. Functions  ##
####################

class1 <- unique(target)[1]
class2 <- unique(target)[2]
x <- y

#This is basically the function "table", but without alphabetical order on classes, for consistency in calculations
freq <- function(v) {
  frequency = rbind(count=sapply(unique(v),function(l)sum(v==l)))
  return(frequency)}

# Info(D)
I <- function(a,b) {
  if (a == 0 || b == 0) {INF <- 0}
  else {INF <- -a/(a+b)*log(a/(a+b), base = 2) - b/(a+b)*log(b/(a+b), base = 2)}
  return(INF)}

# Info_A(D)
infoD<- function(df1, df2) {
  ZZ <- data.frame(df1, df2)
  DDD <- c()
  for (i in (1:length(unique(df1)))) {
    DDD <- c(DDD, as.numeric(freq(df1)[i]/length(df1)*I(nrow(ZZ[which(df1 == unique(df1)[i] & df2 == class1),]), nrow(ZZ[which(df1 == unique(df1)[i] & df2 == class2),]))))
  }
  return(sum(DDD))}

# Info_D for pasting
cheat <- function(df1, df2) {
  ZZ <- data.frame(df1, df2)
  DDDD <- c()
  for (i in (1:length(unique(df1)))) {
    DDDD <- c(DDDD, paste(toString(freq(df1)[i]), "/", toString(length(df1)), sep = ""), paste( "* I(", nrow(ZZ[which(df1 == unique(df1)[i] & df2 == class1),]), ",", nrow(ZZ[which(df1 == unique(df1)[i] & df2 == class2),]), ")", sep = ""), " + ")
  }
  return(c("Info_a(D) =", DDDD, "=", infoD(df1, df2)))}

# Gain(A)
gain <- function(df11, df22) {
  GGG <- I(length(df22[which(df22 == class1)]), length(df22[which(df22 == class2)])) - infoD(df11, df22)
  return(GGG)}



###############
##  2. Tree  ##
###############

###############
#  1st Split  #
###############

if (exists('x') && is.data.frame(get('x')) == TRUE){
  cat("1st Split:", "\n")
  cat("I(",freq(target)[2],",",freq(target)[1], ") = -(", freq(target)[2], "/", freq(target)[2] + freq(target)[1], ")*log(",freq(target)[2],"/", freq(target)[2]+freq(target)[1],") - (", freq(target)[1], "/", freq(target)[2] + freq(target)[1], ")*log(",freq(target)[1],"/", freq(target)[2]+freq(target)[1],") = ",  I(freq(target)[2],freq(target)[1]), "\n", sep ="")
        
  for (i in (1:(ncol(x)-1))){
    cat(cheat(x[, i], target), "\n")}
        
  for (i in (1:(ncol(x)-1))){
    cat("Gain(A) =",I(freq(target)[2],freq(target)[1]), "-", infoD(x[, i], target), "=", gain(x[, i], target), "\n")}
        
  tt <- 0
  qq <- 0
  for (i in (1:(ncol(x)-1))){
    if (gain(x[, i], target) > tt) {
      tt <- gain(x[, i], target)
      qq <- i
    }
  }
  cat('Split by:', colnames(x[qq]), "\n")
  if (tt == I(freq(target)[2],freq(target)[1])) {cat("\n", "And its over because Info(D) = max(Gain(A)). ChecK:", "\n")
    x}
}
###############
#  2nd Split  #
###############

x <- y
for (i in (1:length(unique(x[, qq])))) {
  nam <- paste("x2", i, sep ="")
  assign(nam, x[which(x[, qq] == unique(x[, qq])[i]),])
  x <- get(paste("x2", i, sep =""))
  target <- x[, ncol(x)]
  cat("2nd Split (", i, "):", unique(x[,qq]), "\n")
  if (is.na(freq(target)[2])) {cat("Everything is:", target[1], "\n", "Stop splitting!", "\n", "\n", "----------------------------------------------------------------", "\n")
    namqq <- paste("qq2", i, sep ="")
    assign(namqq, 0)}
  else{
    cat("I(",freq(target)[2],",",freq(target)[1], ") = -(", freq(target)[2], "/", freq(target)[2] + freq(target)[1], ")*log(",freq(target)[2],"/", freq(target)[2]+freq(target)[1],") - (", freq(target)[1], "/", freq(target)[2] + freq(target)[1], ")*log(",freq(target)[1],"/", freq(target)[2]+freq(target)[1],") = ",  I(freq(target)[2],freq(target)[1]), "\n", sep ="")
      
    for (z in (1:(ncol(x)-1))){
      cat(cheat(x[, z], target), "\n")}
      
    for (z in (1:(ncol(x)-1))){
      cat("Gain(A) =",I(freq(target)[2],freq(target)[1]), "-", infoD(x[, z], target), "=", gain(x[, z], target), "\n")}
      
    namqq <- paste("qq2", i, sep ="")
    assign(namqq, 0)
    namtt <- paste("tt2", i, sep ="")
    assign(namtt, 0)
    for (j in (1:(ncol(x)-1))){
      if (gain(x[, j], target) > get(paste("tt2", i, sep =""))) {
        assign(namtt, gain(x[, j], target))
        assign(namqq, j)
      }
    }
    cat('Split by:', colnames(x[get(paste("qq2", i, sep =""))]), "\n")
    if (get(paste("tt2", i, sep ="")) == I(freq(target)[2],freq(target)[1])) {cat("\n", "And its over because Info(D) = max(Gain(A)). ChecK:", "\n")
      cat(x[,get(paste("qq2", i, sep =""))], "\n")
      cat(target, "\n", "\n", "----------------------------------------------------------------", "\n")}
    else {cat("\n", "----------------------------------------------------------------", "\n")}
  }
  x <- y
}


###############
#  3nd Split  #
###############

for (k in (1:length(unique(x[, qq])))) {
  if (get(paste("qq2", k, sep ="")) > 0) {
    x <- get(paste("x2", k, sep =""))
    qq <- get(paste("qq2", k, sep =""))
    for (i in (1:length(unique(x[, qq])))) {
      nam <- paste("x3", i, sep ="")
      assign(nam, x[which(x[, qq] == unique(x[, qq])[i]),])
      x <- get(paste("x3", i, sep =""))
      target <- x[, ncol(x)]
      cat("3nd Split (", k, "-", i, "):", unique(x[,qq]), "\n")
      if (is.na(freq(target)[2])) {cat("Everything is:", target[1], "\n", "Stop splitting!", "\n", "\n", "----------------------------------------------------------------", "\n")
        namqq <- paste("qq3", i, sep ="")
        assign(namqq, 0)}
      else{
        cat("I(",freq(target)[2],",",freq(target)[1], ") = -(", freq(target)[2], "/", freq(target)[2] + freq(target)[1], ")*log(",freq(target)[2],"/", freq(target)[2]+freq(target)[1],") - (", freq(target)[1], "/", freq(target)[2] + freq(target)[1], ")*log(",freq(target)[1],"/", freq(target)[2]+freq(target)[1],") = ",  I(freq(target)[2],freq(target)[1]), "\n", sep ="")
        
        for (i in (1:(ncol(x)-1))){
          cat(cheat(x[, i], target), "\n")}
        
        for (i in (1:(ncol(x)-1))){
          cat("Gain(A) =",I(freq(target)[2],freq(target)[1]), "-", infoD(x[, i], target), "=", gain(x[, i], target), "\n")}
        
        namqq <- paste("qq3", i, sep ="")
        assign(namqq, 0)
        namtt <- paste("tt3", i, sep ="")
        assign(namtt, 0)
        for (j in (1:(ncol(x)-1))){
          if (gain(x[, j], target) > get(paste("tt3", i, sep =""))) {
            assign(namtt, gain(x[, j], target))
            assign(namqq, j)
          }
        }
        cat('Split by:', colnames(x[get(paste("qq3", i, sep =""))]), "\n")
        if (get(paste("tt3", i, sep ="")) == I(freq(target)[2],freq(target)[1])) {cat("\n", "And its over because Info(D) = max(Gain(A)). ChecK:", "\n")
          cat(x[,get(paste("qq3", i, sep =""))], "\n")
          cat(target, "\n", "\n", "----------------------------------------------------------------", "\n")}
        else {cat("\n", "----------------------------------------------------------------", "\n")}
      }
      x <- get(paste("x2", k, sep =""))
    }
  }
}
