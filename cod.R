#################START_COD 
#vizualizam setul de date initial
View(CO2)

#modificam setul de date
#copiam setul initial
model <- CO2 

#dam fiecarui sir de caractere o valoare numerica
model$Plant <- as.character(model$Plant)
model$Plant[model$Plant=="Qn1"]<- 0
model$Plant[model$Plant=="Qn2"]<- 1
model$Plant[model$Plant=="Qn3"]<- 2

model$Plant[model$Plant=="Qc1"]<- 3
model$Plant[model$Plant=="Qc2"]<- 4
model$Plant[model$Plant=="Qc3"]<- 5

model$Plant[model$Plant=="Mn1"]<- 6
model$Plant[model$Plant=="Mn2"]<- 7
model$Plant[model$Plant=="Mn3"]<- 8

model$Plant[model$Plant=="Mc1"]<- 9
model$Plant[model$Plant=="Mc2"]<- 10
model$Plant[model$Plant=="Mc3"]<- 11
model$Plant <- as.numeric(model$Plant)

model$Type <- as.character(model$Type)
model$Type[model$Type=="Quebec"]     <- 0
model$Type[model$Type=="Mississippi"]<- 1
model$Type <- as.numeric(model$Type)

model$Treatment <- as.character(model$Treatment)
model$Treatment[model$Treatment=="nonchilled"]<- 0
model$Treatment[model$Treatment=="chilled"]   <- 1
model$Treatment <- as.numeric(model$Treatment)

#vizualizam modelul rezultat
View(model)


#construim diverse categorii de date
#Quebec
Quebec                <- subset(model,
                                Type==0, #Quebec
                                select=c(Plant, Treatment, conc, uptake))

nonchilledQuebec      <- subset(Quebec,
                                Treatment==0, #nonchilled
                                select=c(Plant, Treatment, conc, uptake))

chilledQuebec         <- subset(Quebec,
                                Treatment==1, #chilled
                                select=c(Plant, Treatment, conc, uptake))

#Mississippi
Mississippi           <- subset(model,
                                Type==1, #Mississippi
                                select=c(Plant, Treatment, conc, uptake))

nonchilledMississippi <- subset(Mississippi,
                                Treatment==0, #nonchilled
                                select=c(Plant, Treatment, conc, uptake))

chilledMississippi    <- subset(Mississippi,
                                Treatment==1, #chilled
                                select=c(Plant, Treatment, conc, uptake))

#selectam planta Qc1 din Quebec, chilled
chilledQuebec1        <- subset(chilledQuebec,
                                Plant==3, #Qc1
                                select=c(conc, uptake))

#Subiectul I
#calculam media absorbtiei in diferite cazuri
mean(model$uptake)                  # 27.21310 medie pentru toate plantele
mean(chilledMississippi$uptake)     # 15.81429
mean(nonchilledMississippi$uptake)  # 25.95238
mean(chilledQuebec$uptake)          # 31.75238
mean(nonchilledQuebec$uptake)       # 35.33333

#calculam varianta absorbtiei in cazul plantei Qc1
var(chilledQuebec1$uptake)          # 69.46571

#folosim boxplot pentru a evalua mediana
boxplot(model$uptake,
        main="Uptake",
        sub=paste("Outlier rows: ",
                  boxplot.stats(model$uptake)$out))

#folosim quantile pentru a afla mai multe informatii despre absorbtia plantelor
quantile(model$uptake)
# 0%    25%    50%    75%   100% 
#7.700 17.900 28.300 37.125 45.500


#Subiectul II
#calculam corelatia dintre conc si uptake in mai multe situatii
cor(CO2$uptake, CO2$conc)                                     # 0.4851774
cor(chilledMississippi$uptake ,chilledMississippi$conc)       # 0.5586601
cor(nonchilledMississippi$uptake ,nonchilledMississippi$conc) # 0.7019991
cor(chilledQuebec$uptake, chilledQuebec$conc)                 # 0.7422454
cor(nonchilledQuebec$uptake, nonchilledQuebec$conc)           # 0.7038936

#vizualizam compatibilitatea
scatter.smooth(x=chilledQuebec$conc, y=chilledQuebec$uptake)

#creeam regresia simpla
#construim setul de date de training si test
set.seed(100)  
trainingRowIndex <- sample(1:nrow(chilledQuebec), 0.8*nrow(chilledQuebec))  
trainingData <- chilledQuebec[trainingRowIndex, ]
testData  <- chilledQuebec[-trainingRowIndex, ]

#construim modelul folosind training data
lmMod <- lm(uptake ~ conc, data=trainingData)  
distPred <- predict(lmMod, testData)

#verificam
summary(lmMod)
AIC(lmMod)
BIC(lmMod)
actuals_preds <- data.frame(cbind(actuals=testData$uptake, predicteds=distPred))
correlation_accuracy <- cor(actuals_preds)  # 82.7%
head(actuals_preds)

#creeam regresia multipla
multLinMod <- lm(uptake ~ Type + Plant + Treatment + conc, data=model)
#o evaluam
summary(multLinMod)

#adaugam variabila noua
#folosim set.seed pentru a genera acelasi sir de numere aleatoare de fiecare data
set.seed(100)
#dam valori
moistureRate <- rnorm(84, mean=1, sd=5)
#creeam regresia multipla la care am adaugat noua variabila
multLinMod <- lm(uptake ~ Type + Plant + Treatment + conc + moistureRate, data=model)
#o evaluam
summary(multLinMod)


#Subiectul III
#vrem sa vedem doua grafice
par(mfrow=c(1, 2))
#construim secventa de numere
x_gamma <- seq(0, 10, by=0.3)

#functia de densitate
y_gamma <- dgamma(x_gamma, 10)
#afisam
plot(y_gamma,)
#functia de distributie
y_gamma <- pgamma(x_gamma, 10)
#afisam
plot(y_gamma)


###BONUS
#a
#genereaza un vector ce contine "titlul"
titlu <- function(n, last)
{
  titl <- c()
  for (val in 1:n)
  {
    titl <- c(titl, paste("",val, sep = ""))
  }
  titl <- c(titl,last)
  
}
#genereaza un vector de n numere aleatoare ce au totalul egal cu tot
genval <- function(n, tot)
{
  total <- 0
  vals <- c()
  for (val in 1:n)
  {
    rnr <- runif(1, 0.0, 100.0)
    total <- total + rnr
    vals <- c(vals, rnr)
  }
  for (i in 1:n)
  {
    vals[i] <- (vals[i]/total) * tot
  }
  
  vals
}

frepcomgen <- function(n, m)
{
  #SETARI
  precizie <- 0.1 #o zecimala
  val_min  <- 0.1 #val min
  val_max  <- 0.5 #val max
  #######
  
  #cautam valori in acest camp, pentru a limita timpul de executie, vom folosi doar 1-5
  probabilities <- seq(val_min, val_max, by=precizie)
  
  #construim "titlul" coloanelor
  lX <- titlu(n, "X/qj")
  
  #construim "titlul" coloanelor
  lY <- titlu(m, "Y/pi")
  
  #construim variabila comuna
  common_var <- matrix(0, nrow = n + 1, ncol = m + 1, dimnames = list(lX, lY))
  
  #variabila rezultata
  var_ok <- NA
  
  #shuffle
  probabilities <- list(probabilities)[[1]][sample(1:length(probabilities))]
  
  #folosim backtracking pentru a gasi o varianta
  bkt <- function(i, j, common_var) {
    if (i == n + 1) {
      row_sums <- c()
      col_sums <- c()
      for(row in 1:nrow(common_var)) {
        row_sum <- sum(common_var[row,])
        row_sums = c(row_sums, row_sum)
      }
      for(col in 1:ncol(common_var)) {
        col_sum <- sum(common_var[,col]) 
        col_sums = c(col_sums, col_sum)
      }
      if (sum(row_sums) == 1 && sum(col_sums) == 1) {
        var_ok <<- common_var
        return(1)
      }
    } else {
      for (number in probabilities) {
        common_var[i, j] <- number
        if (j == m) {
          if (bkt(i + 1, 0,common_var) == 1){
            return(1)
          }
        } else {
          if (bkt(i, j + 1, common_var) == 1) {
            return(1)
          }
        }
      }
    }
    return(0)
  }
  #daca avem matrice de 1x1, rezultatul va fi 1
  if (m*n == 1)
    var_ok <- matrix(1, nrow = n + 1, ncol = m + 1, dimnames = list(lX, lY))
  else #cauta folosind bkt
    bkt(1, 1, common_var)
  
  var_ok[[n+1,m+1]] <- 1
  
  
  #calculam suma liniilor pentru a gasi X
  for (i in 1:n)
  {
    s <- 0
    for (j in 1:m)
    {
      var_ok[[i,j]] <- var_ok[[i,j]]
      s <- s + var_ok[[i,j]]
    }
    var_ok[[i,m+1]] <- s
  }
  
  #calculam suma coloanelor pentrut Y
  for (i in 1:m)
  {
    s <- 0
    for (j in 1:n)
    {
      s <- s + var_ok[[j,i]]
    }
    var_ok[[n+1,i]] <- s
  }
  
  #stergem o valoare aleatoare pe fiecare rand
  for (i in 1:n)
  {
    rip <- sample(1:m, 1)
    var_ok[[i,rip]] <- NA
  }
  
  var_ok
}
##

#b
fcomplrepcom <- function(repCom)
{
  #obtinem dimensiunile initiale
  m <- dim(repCom)[2] - 1
  n <- dim(repCom)[1] - 1
  
  #completam interiorul
  for (i in 1:n)
  {
    s <- 0
    indLips <- 0
    for (j in 1:m)
    {
      if (is.na(repCom[[i,j]]))
        indLips <- j
      else
        s <- s + repCom[[i,j]]
    }
    if (indLips > 0)
    {
      repCom[[i,indLips]] <- repCom[[i,m + 1]] - s
    }
  }
  
  repCom
}
##

#c
#calculeaza E(X)
E <- function(va)
{
  n <- dim(va)[2]
  tot <- 0
  
  for (i in 1:n)
  {
    add <- va[[1,i]] * va[[2,i]]
    tot <- tot + add
  }
  
  tot
}
#inmulteste doua va
vaMult <- function(X,Y)
{
  n <- dim(X)[2]
  m <- dim(Y)[2]
  rez <- matrix(, nrow = 2, ncol = n*m)
  
  curent <- 1
  for (i in 1:n)
    for (j in 1:m)
    {
      rez[[1,curent]] <- X[[1, i]] * Y[[1,j]]
      rez[[2,curent]] <- X[[2, i]] * Y[[2,j]]
      curent <- curent+1
    }
  rez
}
#calculeaza cov(X, Y)
vaCov <- function(X, Y)
{
  XY <- vaMult(X, Y)
  cv <- E(XY) - E(X) * E(Y)
  cv
}

#calculeaza Var(X)
vaVar <- function(X)
{
  rett <- E(vaMult(X,X)) - E(X)^2
  rett
}

fnF <- function(X, a)
{
  n <- dim(X)[2]
  total <- 0
  for (i in 1:n)
    if (X[[1,i]] <= a)
    {
      total <- total + X[[2,i]]
    }
  total
}

covpp <- function(repartitieComuna)
{
  #obtinem dimensiunile initiale
  m <- dim(repartitieComuna)[2] - 1
  n <- dim(repartitieComuna)[1] - 1
  
  #construim X si Y
  X <- matrix(, nrow = 2, ncol = n)
  Y <- matrix(, nrow = 2, ncol = m)
  
  for (i in 1:n)
  {
    X[[1,i]] <- as.numeric(rownames(repartitieComuna)[i])
    X[[2,i]] <- repartitieComuna[[i, m+1]]
  }
  
  for (i in 1:m)
  {
    Y[[1,i]] <- as.numeric(colnames(repartitieComuna)[i])
    Y[[2,i]] <- repartitieComuna[[n+1, i]]
  }
  
  #creeam 5X
  X1 <- X
  for (i in 1:n)
  {
    X1[[1,i]] <- X1[[1,i]] * 5
  }
  #creeam -3Y
  Y1 <- Y
  for (i in 1:m)
  {
    Y1[[1,i]] <- Y1[[1,i]] * -3
  }
  #calculam Cov(5X, -3Y)
  vac <- vaCov(X1,Y1)
  
  
  
  #daca Cov(X,Y) este 0 => necorelate
  if(all.equal(vac, 0)){
    print("NECORELATE")
  }else
    print("NU SUNT NECORELATE")
  
  
  #LIPSESTE P(0<X<3/Y>2)
  
  #P(x > 6) = 1 - P(x <= 6) = 1 - F(6)
  print("P(X>6,Y<7) = ")
  print((1 - fnF(X, 6)) * fnF(Y,7))
}
##

#d
fverind <- function(repartitieComuna)
{
  #presupunem ca este
  este <- TRUE
  #obtinem dimensiunile initiale
  m <- dim(repartitieComuna)[2] - 1
  n <- dim(repartitieComuna)[1] - 1
  
  #verificam interiorul
  for (i in 1:n)
    for (j in 1:m)
    {
      rezultat <- repartitieComuna[[i, m + 1]] * repartitieComuna[[n + 1, j]]
      #daca valorile nu sunt egale, presupunerea facuta este falsa
      if (repartitieComuna[[i,j]] != rezultat)
        este <- FALSE
    }
  este
}

fvrnecor <- function(repartitieComuna)
{
  #obtinem dimensiunile initiale
  m <- dim(repartitieComuna)[2] - 1
  n <- dim(repartitieComuna)[1] - 1
  
  #construim X si Y
  X <- matrix(, nrow = 2, ncol = n)
  Y <- matrix(, nrow = 2, ncol = m)
  
  for (i in 1:n)
  {
    X[[1,i]] <- as.numeric(rownames(repartitieComuna)[i])
    X[[2,i]] <- repartitieComuna[[i, m+1]]
  }
  
  for (i in 1:m)
  {
    Y[[1,i]] <- as.numeric(colnames(repartitieComuna)[i])
    Y[[2,i]] <- repartitieComuna[[n+1, i]]
  }
  
  #calculam Cov(X, Y)
  vac <- vaCov(X,Y)
  
  rez <- all.equal(vac, 0)
  rez
}
##

#main
repcom <- frepcomgen(2, 3)
repcom
covpp(repcom)
repcom <- fcomplrepcom(repcom)
repcom
fverind(repcom)
#####