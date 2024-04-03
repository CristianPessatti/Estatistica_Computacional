##======================================================================
## CE089 - EstatÃ­stica Computacional 2
## EstatÃ­stica - UFPR
## Prof. Fernando Mayer
##======================================================================

##======================================================================
## RevisÃ£o de conceitos fundamentais da linguagem R ====================
##======================================================================

##======================================================================
## Tipos e classes de objetos
##======================================================================

## Vetores atÃ´micos ----------------------------------------------------

## double
x <- c(2, 4, 6)
typeof(x)
mode(x)
class(x)
## integer
x <- as.integer(x)
typeof(x)
mode(x)
class(x)
x <- c(2L, 4L, 6L)
typeof(x)
mode(x)
class(x)

x <- rep(1L, 100)
typeof(x)
mode(x)
class(x)
object.size(x)

x <- rep(1, 100)
typeof(x)
mode(x)
class(x)
object.size(x)


(x <- 5:9)
typeof(x)
(x <- c(5,6,7,8,9))
typeof(x)

(x <- c(2, "a"))
typeof(x)

## character
(x <- c("a", "b", "c"))
typeof(x)
mode(x)
class(x)

## logical
x <- c(TRUE, FALSE, TRUE)
typeof(x)
class(x)

## complex
x <- c(2 + 1i, 4 + 1i, 6 + 1i)
typeof(x)
class(x)

## raw
?raw
x <- raw(10)
y <- raw(5)
typeof(x)
class(x)

## tipos e hierarquia
x1 <- c(TRUE, FALSE, TRUE)
x2 <- 1L
x3 <- c(5,6,7,8,9)
x4 <- c("A", "B")

typeof(x1)
mode(x1)
typeof(x2)
mode(x2)
typeof(x3)
mode(x3)
typeof(x4)
mode(x4)

typeof(c(x1, x2))
typeof(c(x1, x3))
typeof(c(x1, x4))
typeof(c(x2, x3))
typeof(c(x2, x4))
typeof(c(x3, x4))

typeof(c(x1, x2, x3))
typeof(c(x1, x2, x4))
typeof(c(x2, x3, x4))

## vetores lÃ³gicos
(x <- c(TRUE,FALSE,TRUE,TRUE,FALSE))
as.integer(x)
as.numeric(x)
x
mean(x)

(x <- c("TRUE","FALSE","TRUE","TRUE","FALSE"))
as.integer(x)
as.numeric(x)
x
mean(x)


(x <- c(0, 1, 1, 0, 1))
as.logical(x)

(x <- c(0, 1, 5, log(12), -3.15))
as.logical(x)


(x <- c(TRUE,FALSE,TRUE,TRUE,FALSE))
(x <- c("TRUE","FALSE","TRUE","TRUE","FALSE"))
as.logical(x)
TRUE
c(TRUE)
c(T)
c(TRUE, FALSE)
c(T, F)
c(true, false)

as.logical(c("T", "F"))
as.logical(c("TRUE", "FALSE"))
as.logical(c("t", "f"))
as.logical(c("true", "false"))
as.logical(c("True", "False"))
as.logical(c(1, 0))
as.logical(c("1", "0"))
as.logical(c("1", "0", 0, 1))


## operadores lÃ³gicos
##- or: x | y
##- and: x & y
##- not: !x
##- e*x*clusive *or*: xor(x, y) # apenas 1 verdadeiro

(A <- c(F,F,T,T,T,T,F,F,F,F))
(B <- c(F,F,F,F,T,T,T,T,T,F))

A & B
A | B
!A
!B
xor(A, B)
!A | !B

U <- 1:10
U[A]
U[as.logical(1-A)]
U[B]
U[A & B]
U[A | B]
U[!A]
U[!B]
U[xor(A, B)]
U[!xor(A, B)]
U[!A | !B]


x <- c(NA, FALSE, TRUE)
names(x) <- as.character(x)
outer(x, x, `&`)
outer(x, x, `|`)

## %% e || length 1 nÃ£o sÃ£o vetorizÃ¡veis
x <- c(TRUE, TRUE, FALSE, FALSE, FALSE)
y <- c(FALSE, TRUE, FALSE, TRUE, FALSE)
x | y
x & y
x || y
FALSE && y
TRUE || y
FALSE || y
xor(x,y)

TRUE & NA
FALSE & NA
TRUE | NA
FALSE | NA


x <- c(TRUE, TRUE, FALSE, FALSE, FALSE, NA, NA, NA)
y <- c(FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, NA)
xor(x,y)

## GeneralizaÃ§Ãµes: any() e or()

## Operadores de comparaÃ§Ã£o
## == != > < >= <= %in%


"10" < "2"

##CondicÃµes: devem ter length 1 (senÃ£o considerar any() ou all()) e nÃ£o pode ser NA


## NA, NULL, NaN

NULL + TRUE
c(1, 2, 3, NULL, 4, 5)
is.na(NULL)
logic(0)


## vetores genÃ©ricos - Lista ---------------------------------------------------------------
(x <- list(1:10, letters[1:5]))
typeof(x)
class(x)
typeof(x[1])
class(x[1])
typeof(x[2])
class(x[2])
typeof(x[[1]])
class(x[[1]])
typeof(x[[2]])
class(x[[2]])

x <- c(2, 4, 6)
typeof(x)
typeof(as.integer(x))

## OBJETO -> CLASSE -> METODOS

##----------------------------------------------------------------------
## RepresentaÃ§Ã£o numÃ©rica dentro do R

set.seed(123)
(x <- runif(10))

getOption("digits")
options(digits=2)
x
options(digits=7)

set.seed(12)
(y <- runif(10))
set.seed(123)
(y <- runif(10))

RNGkind()
set.seed(123)
(y <- runif(10))

RNGkind("Super-Duper")
set.seed(123)
(y <- runif(10))

RNGkind("Marsaglia-Multicarry")
set.seed(123)
(y <- runif(10))

RNGkind("Mersenne-Twister")
set.seed(123)
(y <- runif(10))


## Erro de ponto flutuante
sqrt(2)^2 - 2
sqrt(2)^2 == 2
all.equal(sqrt(2)^2, 2)

1 == 3 / 3
.1 == .3 / 3

1 + 2 == 3
0.1 + 0.2 == 0.3

seq(0, 1, by=.1) == .3
unique(c(.3, .4 - .1, .5 - .2, .6 - .3, .7 - .4))


print(sqrt(2^2), digits = 22)
print(sqrt(2)^2, digits = 22)
print(sqrt(2)^2, digits = 23)

print(pi, digits=22)
print(1000*pi, digits=22)

0.3 + 0.6 - 0.9
print(c(0.3, 0.6, 0.9), digits = 22)

print(x, digits = 1)
print(x, digits = 7) # padrÃ£o
print(x, digits = 22)

##
##
##
## 2^(-52) . 2(-1022)
2^(-1074)
2^(-1075)

## 2(-1022)
2^(-1022)
.Machine$double.xmin

x < 2^(-1022)
print(x)
y = (1 + 2^(-52)) * 2^(-1022)
print(y)

2^(-52)
.Machine$double.eps

x = 1
print(x)
y = 1 + 2^(-52)
print(y)
x == y
y = 1 + 2^(-53)
print(y)
x == y

## 1 11111111110 1111111111111111111111111111111111111111111111111111
## 1,11...11 x 10^(1023)
(x <- sum(2 ^ (0:-52)) * 2 ^ 1023)
(y <- .Machine$double.xmax)
x == y

0.1 + 0.2 == 0.3
all.equal(0.1 + 0.2, 0.3)

(a <- 1)
(b <- 1 + 2^-52)
a == b
all.equal(a, b)
all.equal(a, b, tolerance = sqrt(.Machine$double.eps))
all.equal(a, b, tolerance = .Machine$double.eps)

(d <- 1 + 2^-26)
a == d
all.equal(a, d)
all.equal(a, d, tolerance = sqrt(.Machine$double.eps))
all.equal(a, d, tolerance = .Machine$double.eps)
isTRUE(all.equal(a, d, tolerance = .Machine$double.eps))


options(digits = 20)
x <- 9007199254740990
print(x)
x <- x+1
print(x)
x <- x+1
print(x)
x <- x+1
print(x)
##
x <- 9007199254740990
print(x)
x <- x+1
print(x)
x <- x+1
print(x)
x <- x+2
print(x)

options(digits = 7)


format(x, scientific = TRUE)
format(x, scientific = FALSE)

x <- 1/100
x
(x <- 1/100)
format(x)

x <- 1/1000
x
(x <- 1/1000)

format(x, sci=TRUE)
format(x, sci=FALSE)

1/1000
format(1/1000)
format(1/10000)

5/2
format(5/2)
format(5/2, digits=3)

format(c(0.333, 5/2))
format(c(0.333, 5/2), nsmall=3)

(x <- sample(0:9999, 10))
sprintf("%04d", x)

(x <- rnorm(10, m = 1, sd = 0.5))
sprintf("%.2f", x)

pi
(x <- print(pi, dig=5))
print(x, digits = 20)
class(x)
format(pi, dig=5)
sprintf("%.4f", pi)
sprintf("%05.2f", pi)
sprintf("%05.2f", c(pi, 10.51))

format(0.0300, dig=5)
sprintf("%.4f", 0.0300)
sprintf("%.4e", 0.0300)

format(0.0300, dig=5)
formatC(0.0300)
formatC(0.0300, dig=5)
formatC(0.0300, dig=5, flag=0)


## muitas opÃµes em format(), formatC(), sprintf(), prettyNum()

## outras opÃ§Ãµes em pacotes como stringi
## x <- c(471, 3645, 1123, 10236)

## > stringi::stri_pad_left(x, 5, 0)
## [1] "00471" "03645" "01123" "10236"
## > stringi::stri_pad_left(x, 5, 2)
## [1] "22471" "23645" "21123" "10236"
## > stringi::stri_pad_left(x, 6, 0)
## [1] "000471" "003645" "001123" "010236"
## > stringi::stri_pad_left(x/100, 6, 0)
## [1] "004.71" "036.45" "011.23" "102.36"
## >
## > stringi::stri_pad_right(x, 6, 0)
## [1] "471000" "364500" "112300" "102360"
## > stringi::stri_pad_right(x/100, 6, 0)
## [1] "4.7100" "36.450" "11.230" "102.36"
## > stringi::stri_pad_right(x/100, 7, 0)
## [1] "4.71000" "36.4500" "11.2300" "102.360"

##----------------------------------------------------------------------
## OperaÃ§Ãµes com vetores

num <- 1:6
num
num * 2
num * num
num^2
num + c(2, 4, 1) # como se chama este "mecanismo"?
num + c(2, 4, 1, 3)

##======================================================================
## IndexaÃ§Ã£o e seleÃ§Ã£o condicional
##======================================================================

##======================================================================
## IndexaÃ§Ã£o

## Vetores -------------------------------------------------------------

cont <- c(8, 4, NA, 9, 6, 1, 7, 9)
cont

cont[]
cont[4]
cont[c(1, 4, 8)]
ind <- c(1, 4, 8)
cont[ind]
cont[-4]
cont[-c(1, 4, 8)]
cont[1:5]
cont[seq(1, 8, by = 2)]
cont[-1:5]
cont[-(1:5)]
cont[cont %% 3 == 0]

is.na(cont)
cont[is.na(cont)]

!is.na(cont)

c(TRUE, FALSE)
!c(TRUE, FALSE)

cont[!is.na(cont)]

cont[is.na(cont)] <- 0
cont
cont[3] <- NA
cont

names(cont) <- letters[1:length(cont)]
cont
cont["d"]
cont[c("f", "a")]

## seleÃ§Ã£o por posiÃ§Ã£o
x <- c(3.4, 2.34, 1.50, 0.67, 4.1)
x[c(2,4)]
x[c(4,2)]

x[c(1, 2, 1, 3, 1, 4, 4)]
x[c(2.1, 3.5, 2.8)]
x[1/3]
x[10/3]
x[6]
##
x[-2]
x[-c(1,3)]
x[-c(3,1)]
#x[c(-2, 3)]
x[-2][3]

x[]
x[0]

## seleÃ§Ã£o por lÃ³gicos
x[x<3]
x[c(TRUE, FALSE, TRUE, FALSE, TRUE)]
x[c(TRUE, FALSE)]
x[c(TRUE, FALSE, NA, FALSE, TRUE)]
x[c(TRUE, NA)]
x[c(FALSE, NA)]

##Vetores nomeados e seleÃ§Ã£o por nomes

names(x)
names(x) <- letters[1:length(x)]
x
names(x) <- NULL
x
setNames(x, letters[1:length(x)])
x

x[c("a","b","d")]
x[c("a","d","a", "a")]
x[c("d", "g")]
c(a=2, b=5.3, c=1.7)

##CriaÃ§Ã£o de vetores de caracterles ("lookup table")
```{r}
x <- c("a","b","b","b","a","a","i","b","a","a")
vals <- c(a = "Alto", b="Baixo", i=NA)
vals[x]
unname(vals[x])
object.size(vals[x])
object.size(unname(vals[x]))
```



## Matrizes ------------------------------------------------------------

mat <- matrix(1:9, nrow = 3)
mat
mat[2, 3]
mat[, 1]
class(mat)
class(mat[,1])
mat[, 1, drop = FALSE]
class(mat[, 1, drop = FALSE])
mat[1, ]
mat[1, , drop = FALSE]

mat
dim(mat)

mat[ , 1]
class(mat[ , 1])
mat[ , 1, drop = FALSE]
dim(mat[ , 1, drop = FALSE])

2 + 2
"+"(2, 2)

mat[3, 2]
mat[3, 2, drop = FALSE]
mat[1, ]
mat[1, , drop = FALSE]

mat[c(1, 3), c(2, 3)]

colnames(mat) <- LETTERS[1:3]
mat[, "B"]
mat[, "B", drop = FALSE]
mat[1, "C"]

rownames(mat) <- LETTERS[24:26]
mat["X", ]
mat["X", , drop = FALSE]
mat["Y", "A"]


M <- matrix(1:12, ncol = 4)
M
x <- c(100, 200, 300)
M * x
t(x) %*% M
crossprod(x, M)


## Listas --------------------------------------------------------------

lis <- list(c(3, 8, 7, 4), mat, 5:0, "O R Ã© legal")
lis

length(lis)

lis[1]
class(lis[1])

lis[[1]]
class(lis[[1]])

lis[1:2]
class(lis[1:2])

lis[[c(1,4)]]     # nada a ver!
class(lis[[1:2]])


mean(lis[1])
mean(lis[[1]])

lis[[1]][3]
lis[[2]]
lis[[2]][2, 3]

lis <- list(vetor1 = c(3, 8, 7, 4), mat = mat,
            vetor2 = 5:0, mensagem = "O R Ã© legal")
## names(lis) <- c("vetor1", "mat", "vetor2", mensagem)
lis

lis$mensagem

lis$mat
lis$mat[2, 3]
lis[[2]][2, 3]
lis$vetor1[3]

lis[["mat"]]
lis[["vetor1"]][3]

ll <- list(a = 1, b = c("A","B","B"))
ll[3]
ll[[3]]

## data-frame ----------------------------------------------------------

da <- data.frame(A = 4:1, B = c(2, NA, 5, 8))
da

is.list(da)
is.data.frame(da)
is.list(lis)
is.data.frame(lis)

da[2, 1]
da[, 2]

da[,"B"]
da[1, ]

da["1", ]

(db <- data.frame(x = c("CTBA","Outro","CTBA","Outro"), y = c(19, 23, 18, 29)))
db[1,]
db[,1]
db[,2, drop=FALSE]

rownames(da)
rownames(da) <- c("D", "U", "T", "K")
rownames(da)
da

da["U", ]

da[, "B"]
mean(da[, "B"], na.rm = TRUE)
class(da[, "B"])
da["U", "B"]
da[, "B", drop = FALSE]
mean(da[, "B", drop = FALSE])
class(da[, "B", drop = FALSE])

da$A
da$B[3]
da$B[c(2, 4)]

da[1]
class(da[1])

da[[1]]
class(da[[1]])
da[["A"]]
da[["A"]][2:3]

da

is.na(da)
!is.na(da)
da[is.na(da), ]

da[is.na(da$A), ]
da[is.na(da$B), ]
da[!is.na(da$B), ]

## momento "e se"

is.na(da)
is.na(da$A)
is.na(da$B)

is.na(da)
complete.cases(da)
!complete.cases(da)
da[complete.cases(da), ]
da[!complete.cases(da), ]

## experimente repetir com:
## da <- data.frame(A = c(4, 3, 2, NA), B = c(2, NA, 5, 8))

summary(da)

complete.cases(da)
!complete.cases(da)
sum(!complete.cases(da))

as.logical(0:10)

airquality
head(airquality)
dim(airquality)
colMeans(airquality)
colMeans(airquality, na.rm=TRUE)
complete.cases(airquality)
sum(complete.cases(airquality))
aqc <- airquality[complete.cases(airquality), ]

colMeans(airquality)
colMeans(aqc)
colMeans(airquality, na.rm = TRUE)

aqcc <- na.omit(airquality)

head(aqc)
head(aqcc)
dim(aqc)
dim(aqcc)
all(aqcc == aqc)
all.equal(aqcc, aqc)
str(aqc)
str(aqcc)

df <- data.frame(ABC = c("A","B","B"), n = 4:6)
subset(df, n == 7)
nrow(subset(df, n == 7))
dim(subset(df, n == 7))

##======================================================================
## SeleÃ§Ã£o condicional
##======================================================================

## Vetores -------------------------------------------------------------

dados <- c(5, 15, 42, 28, 79, 4, 7, 14)

dados > 15
dados[dados > 15]

dados[dados > 15 & dados <= 35]
dados[dados > 15 | dados <= 35]

dados > 15 & dados <= 35
dados > 15 | dados <= 35

cara <- letters[1:length(dados)]
cara

cara == "c"
dados[cara == "c"]

cara == "a" & cara == "c" # porque nÃ£o funciona?
cara == "a" | cara == "c"
dados[cara == "a" | cara == "c"]
dados[cara == "a" | cara == "c" | cara == "f"]

cara == c("a", "c")
dados[cara == c("a", "c")] # errado
dados[cara %in% c("a", "c")]
cara %in% c("a", "c")

dados[cara %in% c("a", "c", "f")]
dados[!(cara %in% c("a", "c", "f"))]

cara == c("a", "c")
cara %in% c("a", "c")

cara[dados == 15]
cara[dados > 30]
cara[dados %in% c(4, 14)]

dados[dados > 15]
which(dados > 15)
which(dados <= 15)
which(!(dados > 15))

dados[dados > 15 & dados <= 35]
which(dados > 15 & dados <= 35)

dados[cara == "c"]
which(cara == "c")

dados[cara %in% c("a", "c")]
which(cara %in% c("a", "c"))

## Data frame ----------------------------------------------------------
dados <- data.frame(ano = c(2001, 2002, 2003, 2004, 2005),
                    captura = c(26, 18, 25, 32, NA),
                    porto = c("SP", "RS", "SC", "SC", "RN"))

dados[dados$ano == 2004, ]
dados[dados$porto == "SC", ]
dados[dados$captura > 20, "captura"]

dados[dados$captura > 20 & !is.na(dados$captura), ]
dados[dados$captura > 20 & complete.cases(dados), ]

dados[dados$captura > 25 & dados$porto == "SP", ]

dados[dados$porto == "SC", ]
subset(dados, porto == "SC")
dados[dados$captura > 20, ]
subset(dados, captura > 20)
dados[dados$captura > 20 & !is.na(dados$captura), ]
dados[dados$captura > 20, "captura"]
subset(dados, captura > 20, select = captura)
subset(dados, captura > 20, select = captura,  drop = TRUE)

##======================================================================
## ProgramaÃ§Ã£o

## Estruturas de repetiÃ§Ã£o ---------------------------------------------

for(i in 1:10){
  print(i)
}

x <- 100:200
for(j in 1:10){
  print(x[j])
}

for(i in c(2, 9, 4, 6)){
  print(i^2)
}

for(veiculos in c("carro", "Ã´nibus", "trem", "bicicleta")){
  print(veiculos)
}

for(veiculos in c("carro", "Ã´nibus", "trem", "bicicleta")){
  Sys.sleep(3)
  print(veiculos)
}

## Outras estruturas: while e repeat

## Calcule a soma em 1,2,3... atÃ© que a soma seja maior do que 1000
n <- 0
soma <- 0
while(soma <= 1000){
  n <- n + 1
  soma <- soma + n
}
soma

## DESAFIO
## Como fazer a soma dos 1000 primeiros numeros primos?

qnt <- 1000 #quantidade de numeros primos a ser somada
num <- 3 #comeca a testar a partir do numero 3
aux <- 0
soma <- 2 #ja considera o numero 2 na soma
cont <- 1

while(cont < qnt){
  for(i in 2:(num-1)){
    if(num %% i == 0){
      aux <- aux + 1
    }
  }
  if(aux == 0){
    soma <- soma + num
    cont <- cont + 1
    print(num) #printa os numeros primos, caso necessario
  }
  aux <- 0
  num <- num+1
}
soma

n <- 0
soma <- 0
repeat{
  n <- n + 1
  soma <- soma + n
  if(soma > 1000) break
}
soma

## Estrutura de seleÃ§Ã£o ------------------------------------------------

x <- 100:200
for(j in 1:10){
  if(x[j] <= 105){
    print("Menor ou igual a 105")
  }
}

x <- 100:200
for(j in 1:10){
  if(x[j] <= 105){
    print("Menor ou igual a 105")
  } else{
    print("Maior do que 105")
  }
}


##----------------------------------------------------------------------
## **Exemplo**: cÃ¡lculo de notas de uma disciplina

url <- "http://leg.ufpr.br/~fernandomayer/data/notas.csv"
notas <- read.table(url, header = TRUE, sep = ";", dec = ",")
str(notas)
head(notas)
summary(notas)

notas[1, c("prova1", "prova2", "prova3")]
mean(notas[1, c("prova1", "prova2", "prova3")])
class(notas[1, c("prova1", "prova2", "prova3")])
mean(as.numeric(notas[1, c("prova1", "prova2", "prova3")]))

## Antes de seguir adiante, veja o resultado de
for(i in 1:30){
  print(notas[i, c("prova1", "prova2", "prova3")])
  ##    Sys.sleep(2)
}

notas[1, c("prova1", "prova2", "prova3")]
class(notas[1, c("prova1", "prova2", "prova3")])
as.numeric(notas[1, c("prova1", "prova2", "prova3")])
class(as.numeric(notas[1, c("prova1", "prova2", "prova3")]))

## Calcula a mÃ©dia das 3 provas (para cada aluno)
notas$media <- 0
nlinhas <- nrow(notas)
provas <- c("prova1", "prova2", "prova3")
for(i in 1:nlinhas){
  notas$media[i] <- mean(as.numeric(notas[i, provas]))
}

head(notas)

notas$media <- rowMeans(notas[, provas])

## apenas comentando rapidamente
ap <- apply(notas[, provas], 1, mean)
all.equal(notas$media, ap)
rm(ap)

## e isto importa?
notas1000 <- notas[replicate(1000, seq_len(nrow(notas))), provas]
system.time({
  notas1000$media <- 0;
  for(i in 1:nrow(notas1000)){
    notas1000$media[i] <- mean(as.numeric(notas1000[i, provas]))
  }
})
system.time(apply(notas1000, 1, mean))
system.time(rowMeans(notas1000))
## .. mais detalhes sobre apply e system.time mais adiante!

## E para calcular o CV?
cv <- function(x){
  desv.pad <- sd(x)
  med <- mean(x)
  cv <- desv.pad/med
  return(cv)
}

## Cria uma nova coluna para o CV
notas$CV <- 0
for(i in 1:nlinhas){
  notas$CV[i] <- cv(as.numeric(notas[i, provas]))
}

head(notas)

## Nova coluna para armazenar a situacao
notas$situacao <- NA
for(i in 1:nlinhas){
  if(notas$media[i] >= 7){
    notas$situacao[i] <- "aprovado"
  } else{
    notas$situacao[i] <- "reprovado"
  }
}

head(notas)

##======================================================================
##  O modo R: vetorizaÃ§Ã£o

x <- 1:1e6
## Calcula o quadrado de cada nÃºmero da sequÃªncia em x usando for()
y1 <- numeric(length(x)) # vetor de mesmo comprimento de x que vai
# receber os resultados
for(i in 1:length(x)){
  y1[i] <- x[i]^2
}

## Modo R
y2 <- x^2
## Confere os resultados
identical(y1, y2)

## Tempo de execuÃ§Ã£o usando for()
y1 <- numeric(length(x))
st1 <- system.time(
  for(i in 1:length(x)){
    y1[i] <- x[i]^2
  }
)
st1

## Tempo de execuÃ§Ã£o usando a regra da reciclagem
st2 <- system.time(
  y2 <- x^2
)
st2

st1[3]/st2[3]

##----------------------------------------------------------------------
## ComparaÃ§Ãµes usando apenas variaÃ§Ãµes do for()

x <- 1:1e6

## Cria um objeto de armazenamento com o mesmo tamanho do resultado
st1 <- system.time({
  out1 <- numeric(length(x))
  for(i in 1:length(x)){
    out1[i] <- x[i]^2
  }
})
st1

## Cria um objeto de tamanho "zero" e vai "crescendo" esse vetor
st2 <- system.time({
  out2 <- numeric(0)
  for(i in 1:length(x)){
    out2[i] <- x[i]^2
  }
})
st2

## Cria um objeto de tamanho "zero" e cresce o vetor usando a funÃ§Ã£o c()
## NUNCA faÃ§a isso!!
st3 <- system.time({
  out3 <- numeric(0)
  for(i in 1:length(x)){
    out3 <- c(out3, x[i]^2)
  }
})
st3
identical(out1, out2, out3)

## knitr::include_graphics("img/R_club.jpg")
##----------------------------------------------------------------------

##----------------------------------------------------------------------
## A famÃ­lia de funÃ§Ãµes *apply()

notas$media.apply <- apply(X = notas[, provas], MARGIN = 1, FUN = mean)
head(notas)
notas$CV.apply <- apply(X = notas[, provas], MARGIN = 1, FUN = cv)
head(notas)

notas$situacao.apply <- ifelse(notas$media.apply >= 7,
                               "aprovado", "reprovado")
head(notas)

## MÃ©dias por LINHA: mÃ©dia das 3 provas para cada aluno
apply(X = notas[, provas], MARGIN = 1, FUN = mean)
## MÃ©dias por COLUNA: mÃ©dia de cada uma das 3 provas para todos os
## alunos
apply(X = notas[, provas], MARGIN = 2, FUN = mean)

mean(notas$prova1)
mean(notas$prova2)
mean(notas$prova3)

## sapply simpilifica o resultado para um vetor
sapply(notas[, provas],  mean)
## lapply retorna o resultado em formato de lista
lapply(notas[, provas],  mean)

sapply(notas[, provas],  summary)
lapply(notas[, provas],  summary)


## MÃ©dia da prova 1 por situaÃ§Ã£o
tapply(notas$prova1,  notas$situacao,  mean)
## MÃ©dia da prova 2 por situaÃ§Ã£o
tapply(notas$prova2,  notas$situacao,  mean)
## MÃ©dia da prova 3 por situaÃ§Ã£o
tapply(notas$prova3,  notas$situacao,  mean)

## Mesmo resultado da tapply, mas agora em formato de data frame
aggregate(prova1 ~ situacao, data = notas, FUN = mean)
aggregate(prova2 ~ situacao, data = notas, FUN = mean)
aggregate(prova3 ~ situacao, data = notas, FUN = mean)
## Mas aqui podemos passar as 3 colunas de uma vez
aggregate(cbind(prova1, prova2, prova3) ~ situacao,
          data = notas, FUN = mean)

##======================================================================
## FunÃ§Ãµes

##----------------------------------------------------------------------
## FunÃ§Ãµes e argumentos

runif(10, 1, 100)
runif(n = 10, min = 1, max = 100)
args(sample)
args(plot)

##----------------------------------------------------------------------
## FunÃ§Ãµes for dummies

ola.mundo <- function(){
  writeLines("OlÃ¡ mundo")
}

ola.mundo()


ola.mundo <- function(texto){
  writeLines(texto)
}

ola.mundo()

ola.mundo("FunÃ§Ãµes sÃ£o legais")

##----------------------------------------------------------------------
## FunÃ§Ã£o para fazer a soma e a mÃ©dia de dois vetores
rm(list = ls())

eu <- 50
tu <- 100

soma <- function(x, y) {
  ss <- sum(x, y)
  mm <- mean(c(x, y))
  return(c(ss, mm))
}

a <- c(2, 5, 9)
b <- c(3, 8, 4)
soma(a, b)

sum(a, b)
sum(c(a, b))
mean(a, b)
mean(c(a, b))
mean(c(a, b), trim = 1)

a <- c(2, 5, NA)
b <- c(3, 8, NA)
soma(a, b)

soma <- function(x, y, ...) {
  ss <- sum(x, y, ...)
  mm <- mean(c(x, y), ...)
  return(c(ss, mm))
}

a <- c(2, 5, 9)
b <- c(3, 8, 4)
soma(a, b)

a <- c(2, 5, NA)
b <- c(3, 8, NA)
soma(a, b)
soma(a, b, na.rm = TRUE)

## Veja que
mean(a, b, na.rm = TRUE)
mean(c(a, b), na.rm = TRUE)
mean(c(a, b), na.rm = TRUE, 0.5)
mean(c(a, b), na.rm = TRUE, trim = 0.5)

sum(c(a, b), na.rm = TRUE)
sum(a, b, na.rm = TRUE)

## MUITO CUIDADO!!!

soma <- function(x, y, ...) {
  ss <- sum(x, y, ...)
  mm <- mean(c(eu, tu), ...)
  return(c(ss, mm))
}

a <- c(2, 5, 9)
b <- c(3, 8, 4)
soma(a, b)

a <- c(2, 5, NA)
b <- c(3, 8, NA)
soma(a, b, na.rm = TRUE)

## O que aconteceu aqui?
ls()
search()
ls(".GlobalEnv")
ls("package:datasets")

## Como evitar: testando se existem no .GlobalEnv
soma <- function(x, y, ...) {
  if(exists("eu")) {eu <- x}
  if(exists("tu")) {tu <- y}
  ss <- sum(x, y, ...)
  mm <- mean(c(eu, tu), ...)
  return(c(ss, mm))
}

a <- c(2, 5, 9)
b <- c(3, 8, 4)
soma(a, b)

a <- c(2, 5, NA)
b <- c(3, 8, NA)
soma(a, b, na.rm = TRUE)

## Como evitar: usando environments
soma <- function(x, y, ...) {
  e <- rlang::env(x = x, y = y)
  ss <- sum(e$x, e$y, ...)
  mm <- mean(c(e$x, e$y), ...)
  return(c(ss, mm))
}

a <- c(2, 5, 9)
b <- c(3, 8, 4)
soma(a, b)

a <- c(2, 5, NA)
b <- c(3, 8, NA)
soma(a, b, na.rm = TRUE)

##======================================================================
## ProgramaÃ§Ã£o Orientada a Objetos

#'
#' Como vimos anteriormente, o R Ã© uma linguagem de programaÃ§Ã£o
#' orientada Ã  objetos. Dois conceitos fundamentais desse tipo de
#' linguagem sÃ£o os de **classe** e **mÃ©todo**. JÃ¡ vimos tambÃ©m que todo
#' objeto no R possui uma classe (que define sua estrutura) e analisamos
#' algumas delas. O que seria entÃ£o um mÃ©todo? Para responder essa
#' pergunta precisamos entender inicialmente os tipos de orientaÃ§Ã£o a
#' objetos que o R possui.
#'
#' O R possui 3 sitemas de orientaÃ§Ã£o a objetos: **S3**, **S4**, e
#' **RC**:
#'
#' - **S3**: implementa um estilo de programaÃ§Ã£o orientada a objeto
#'   chamada de *generic-function*. Esse Ã© o estilo mais bÃ¡sico de
#'   programaÃ§Ã£o em R (e tambÃ©m o mais utilizado). A ideia Ã© que existam
#'   **funÃ§Ãµes genÃ©ricas** que decidem qual mÃ©todo aplicar de acordo com
#'   a classe do objeto. Os mÃ©todos sÃ£o definidos da mesma forma que
#'   qualquer funÃ§Ã£o, mas chamados de maneira diferente. Ã‰ um estilo de
#'   programaÃ§Ã£o mais "informal", mas possibilita uma grande liberdade
#'   para o programador.
#'
#' - **S4**: Ã© um estilo mais formal, no sentido que que as funÃ§Ãµes
#'   genÃ©ricas devem possuir uma classe formal definida. AlÃ©m disso, Ã©
#'   possÃ­vel tambÃ©m fazer o **despacho mÃºltiplo de mÃ©todos**, ao
#'   contrÃ¡rio da classe S3.
#'
#' - **RC**: (*Reference Classes*, antes chamado de R5) Ã© o sistema mais
#'   novo implementado no R. A principal diferenÃ§a com os sistemas S3 e
#'   S4 Ã© que mÃ©todos pertencem Ã  objetos, nÃ£o Ã  funÃ§Ãµes. Isso faz com
#'   que objetos da classe RC se comportem mais como objetos da maioria
#'   das linguagens de programaÃ§Ã£o, como Python, Java, e C#.
#'
#' - **R6**:
#'
#' Nesta sessÃ£o vamos abordar como funcionam os mÃ©todos como definidos
#' pelo sistema S3, por ser o mais utilizado na prÃ¡tica para se criar
#' novas funÃ§Ãµes no R. Para saber mais sobre os outros mÃ©todos, consulte
#' o livro [Advanced R](http://adv-r.had.co.nz/OO-essentials.html).
#'
#' Vamos entender como uma funÃ§Ã£o genÃ©rica pode ser criada atravÃ©s de um
#' exemplo. Usando a funÃ§Ã£o `methods()`, podemos verificar quais mÃ©todos
#' estÃ£o disponÃ­veis para uma determinada funÃ§Ã£o, por exemplo, para a
#' funÃ§Ã£o `mean()`:
#'
methods(mean)
#'
#' O resultado sÃ£o expressÃµes do tipo `mean.<classe>`, onde `<classe>` Ã©
#' uma classe de objeto como aquelas vistas anteriormente. Isso
#' significa que a funÃ§Ã£o `mean()`, quando aplicada Ã  um objeto da
#' classe `Date`, por exemplo, pode ter um comportamento diferente
#' quando a mesma funÃ§Ã£o for aplicada Ã  um objeto de outra classe
#' (numÃ©rica).
#'
#' Suponha que temos o seguinte vetor numÃ©rico:
#'
set.seed(1)
vec <- rnorm(100)
class(vec)
#'
#' e queremos calcular sua mÃ©dia. Basta aplicar a funÃ§Ã£o `mean()` nesse
#' objeto para obtermos o resultado esperado
#'
mean(vec)
#'
#' Mas isso sÃ³ Ã© possÃ­vel porque existe um mÃ©todo definido
#' espcificamente para um vetor da classe `numeric`, que nesse caso Ã© a
#' funÃ§Ã£o `mean.default`. A funÃ§Ã£o genÃ©rica nesse caso Ã© a `mean()`, e a
#' funÃ§Ã£o mÃ©todo Ã© a `mean.default`. Veja que nÃ£o precisamos escrever
#' onome inteiro da funÃ§Ã£o genÃ©rica para que ela seja utilizada, como
#' por exemplo,
#'
mean.default(vec)
#'
#' Uma vez passado um objeto para uma funÃ§Ã£o, Ã© a classe do objeto que
#' irÃ¡ definir qual mÃ©todo utilizar, de acordo com os mÃ©todos
#' disponÃ­veis. Veja o que acontece se forcarmos o uso da funÃ§Ã£o
#' `mean.Date()` nesse vetor
mean.Date(vec)
#'
#' O resultado nÃ£o faz sentido pois ele Ã© especÃ­fico para um objeto da
#' classe `Date`.
#'
#' Tudo isso acontece por causa de um mecanismo chamado de **despacho de
#' mÃ©todos** (*method dispatch*), que Ã© responsÃ¡vel por identificar a
#' classe do objeto e utilizar ("despachar") a funÃ§Ã£o mÃ©todo correta
#' para aquela classe. Toda funÃ§Ã£o genÃ©rica possui a mesma forma: uma
#' chamada para a funÃ§Ã£o `UseMethod()`, que especifica o nome genÃ©rico e
#' o objeto a ser despachado. Por exemplo, veja o cÃ³digo fonte da funÃ§Ã£o
#' `mean()`
#'
mean
#'
#' Agora veja o cÃ³digo fonte da funÃ§Ã£o `mean.default`, que Ã© o mÃ©todo
#' especÃ­fico para vetores numÃ©ricos
#'
mean.default
#'
#' Agora suponha que vocÃª ddeseja criar uma funÃ§Ã£o que calcule a mÃ©dia
#' para um objeto de uma classe diferente daquelas previamente
#' definidas. Por exemplo, suponha que vocÃª quer que a funÃ§Ã£o `mean()`
#' retorne a mÃ©dia das linhas de uma matriz.
#'
set.seed(1)
mat <- matrix(rnorm(50), nrow = 5)
mean(mat)
rowMeans(mat)
#'
#' O resultado Ã© a mÃ©dia de todos os elementos, e nÃ£o de cada linha.
#' Nesse caso, podemos definir nossa prÃ³pria funÃ§Ã£o mÃ©todo para fazer o
#' cÃ¡lculo que precisamos. Por exemplo:
#'
mean.matrix <- function(x, ...) rowMeans(x, ...)
#'
#' Uma funÃ§Ã£o mÃ©todo Ã© sempre definida dessa forma:
#' `<funÃ§Ã£ogenÃ©rica>.<classe>`. Agora podemos ver novamente os mÃ©todos
#' disponÃ­veis para a funÃ§Ã£o `mean()`
#'
methods(mean)

#'
#' e simplesmente aplicar a funÃ§Ã£o genÃ©rica `mean()` Ã  um objeto da classe
#' `matrix` para obter o resultado que desejamos
#'
class(mat)
mean(mat)
#'
#' Esse exemplo ilustra como Ã© simples criar funÃ§Ãµes mÃ©todo para
#' diferentes classes de objetos. PoderÃ­amos fazer o mesmo para objetos
#' das classes `data.frame` e `list`
#'
mean.data.frame <- function(x, ...) sapply(x, mean, ...)
mean.list <- function(x, ...) lapply(x, mean)
#'
#' Aplicando em objetos dessas classes especÃ­ficas, obtemos:
#'
## Data frame
set.seed(1)
da <- data.frame(c1 = rnorm(10),
                 c2 = runif(10))
class(da)
mean(da)
## Lista
set.seed(1)
dl <- list(rnorm(10), runif(50))
class(dl)
mean(dl)



#'
#' Obviamente esse processo todo Ã© extremamente importante ao se criar
#' novas funÃ§Ãµes no R. Podemos tanto criar uma funÃ§Ã£o genÃ©rica (como a
#' `mean()`) e diversos mÃ©todos para ela usando classes de objetos
#' existentes, quanto (inclusive) criar novas classes e funÃ§Ãµes mÃ©todo
#' para elas. Essa Ã© uma das grandes liberdades que o mÃ©todo S3 de
#' orientaÃ§Ã£o Ã  objetos permite, e possivelmente um dos motivos pelos
#' quais Ã© relativamente simples criar pacotes inteiros no R.
#'
#' # ReferÃªncias
#'
#' Para mais detalhes e exemplos dos assuntos abordados aqui, veja
#' @Grolemund2014. Uma abordagem mais avanÃ§ada e detalhada sobre
#' programaÃ§Ã£o orientada a objetos no R pode ser consultada em
#' @Wickham2015.
