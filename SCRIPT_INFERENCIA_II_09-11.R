#Exemplos retirados do site abaixo
#https://www.est.ufmg.br/~cristianocs/Pacotes2021/InfEst.html#16

# Exemplo 1:

renda=c(9.8, 8.5, 7.0, 10.4, 8.9, 7.9, 5.9, 7.7, 8.8, 7.7, 7.9, 5.1, 9.9, 7.6, 8.4, 3.7, 6.8, 7.5, 8.5, 5.1, 7.6, 5.0, 6.1, 6.1, 10.2) 
summary(renda)
hist(renda)

#install.packages("moments")
library(moments)
skewness(renda)  # na distribuição normal é zero
kurtosis(renda)  # na distribuição normal é três
shapiro.test(renda)  # Hipótese nula é que os dados seguem distribuição normal

# https://www.rdocumentation.org/packages/BSDA/versions/1.2.2/topics/z.test
# This function is based on the standard normal distribution and creates confidence intervals and tests hypotheses for both one and two sample problems.
# z.test(
#  x,
# y = NULL,
# alternative = "two.sided",
#  mu = 0,
# sigma.x = NULL,
# sigma.y = NULL,
# conf.level = 0.95
# )

install.packages("BSDA")
library(BSDA)
z.test( x=renda,  y = NULL,  alternative = "two.sided",  mu = 0,  sigma.x =2,  sigma.y = NULL,  conf.level = 0.95)

##############################################################################################################
# Exemplo 2:

renda=c(9.8, 8.5, 7.0, 10.4, 8.9, 7.9, 5.9, 7.7, 8.8, 7.7, 7.9, 5.1, 9.9, 7.6, 8.4, 3.7, 6.8, 7.5, 8.5, 5.1, 7.6, 5.0, 6.1, 6.1, 10.2) 
boxplot(renda)
z.test( x=renda,  y = NULL,  alternative = "less",  mu = 8,  sigma.x =2,  sigma.y = NULL,  conf.level = 0.95)

##############################################################################################################
# Exemplo 3:

tempo=c(2.9, 3.4, 3.5, 4.1, 4.6, 4.7, 4.5, 3.8, 5.3, 4.9, 4.8, 5.7, 5.8, 5.0, 3.4, 5.9, 6.3, 4.6, 5.5, 6.2)
summary(tempo)
boxplot(tempo)
shapiro.test(tempo)

#https://www.rdocumentation.org/packages/stats/versions/3.6.2/topics/t.test
# t.test(x, …)
# S3 method for default
# t.test(x, y = NULL,
#      alternative = c("two.sided", "less", "greater"),
#     mu = 0, paired = FALSE, var.equal = FALSE,
#    conf.level = 0.95, …)

# S3 method for formula
#t.test(formula, data, subset, na.action, …)

# install.packages("stats")
# librarry(stats)
t.test(tempo)

##############################################################################################################
# Exemplo 4:

amostra = c(198, 185, 302, 205.2, 302, 225.3, 206.7, 201.85, 200, 189, 192.1, 200.4, 195, 215.4, 235.3, 216.3, 191.85, 174.6, 199, 195.4, 202.9)
hist(amostra)
shapiro.test(amostra) # nao deu normal

# Nao pode fazer o teste abaixo
# t.test(amostra, mu=195,alternative="greater",conf.level=0.90)

amostra2 = c(198, 185, 205.2, 225.3, 206.7, 201.85, 200, 189, 192.1, 200.4, 195, 215.4, 235.3, 216.3, 191.85, 174.6, 199, 195.4, 202.9)
# obs 302 foi retirada

shapiro.test(amostra2) # normal

# Alternativa para 'amostra': Bootstrap
sample(amostra, size = 20, replace = T)
medias = replicate(100, mean(sample(amostra,size=20, replace = T)))
quantile(medias,0.025)
quantile(medias,0.075)
hist(medias)

##############################################################################################################
# Exemplo 5:

#https://www.rdocumentation.org/packages/stats/versions/3.6.2/topics/prop.test
#prop.test(x, n, p = NULL,
#        alternative = c("two.sided", "less", "greater"),
#      conf.level = 0.95, correct = TRUE)
n = 50
phat = 0.34
k = n*phat
#teste assintótico
prop.test(x = k, n = n, p=0.7,correct = FALSE, conf.level = 0.94)   # usa a qui-quadrado
?prop.test()

#teste exato
binom.test(x = k, n=n, alternative="two.sided", conf.level = 0.94, p=0.7)

################################################################################################################
# Exemplo 6:

# ver arquivo pdf

#install.packages("OneTwoSamples")
#library(OneTwoSamples)

dados_peso <- c(98, 97, 102, 100, 98, 101, 102, 105, 95, 102, 100)
shapiro.test(dados_peso)
var_test1(x=dados_peso,side=0,sigma=10)  # side=0 (two-sided), side=1 (greater) e side=-1 (less)

#Acima, estamos testando se a variância é igual a 10. Então, pelo valor P ao nível de significância de 5%, não rejeitamos a hipótese nula, ou seja, não temos evidências amostrais que a variância é diferente de 10.

interval_var3(x=dados_peso,side=0,alpha=0.05)

################################################################################################################
# Exemplo 9.7 página 326 Livro Devore
# Teste t para amostras independentes

fundido=c(2748, 2700, 2655, 2822, 2511,3149, 3257, 3213, 3220, 2753)
nfundido=c(3027, 3356, 3359, 3297, 3125, 2910, 2889, 2902)

shapiro.test(fundido)
shapiro.test(nfundido)

summary(fundido)
summary(nfundido)

sd(fundido)
sd(nfundido)

# https://www.rdocumentation.org/packages/stats/versions/3.6.2/topics/var.test

#var.test(x, …)
# S3 method for default
#var.test(x, y, ratio = 1,
#       alternative = c("two.sided", "less", "greater"),
#     conf.level = 0.95, …)

# S3 method for formula
#var.test(formula, data, subset, na.action, …)

var.test(x=fundido,y=nfundido, ratio=1,alternative="two.sided",conf.level=0.95)

dados=c(fundido,nfundido)
tipo=c(rep('f',10),rep('nf',8))
boxplot(dados~tipo)

t.test(x=fundido,y=nfundido, mu=0,alternative="less",conf.level=0.95,paired=F,var.equal=T)

################################################################################################################
#Exemplo 9.9 página 334 Livro Devore
#Teste t para amostras dependentes

antes=c(81, 87, 86, 82, 90, 86, 96, 73,74, 75, 72, 80, 66, 72, 56, 82)
depois=c(78, 91, 78, 78, 84, 67, 92, 70,58, 62, 70, 58, 66, 60, 65, 73)
d=antes-depois

boxplot(d)
hist(d)
summary(d)
skewness(d)
kurtosis(d)

shapiro.test(d)

t.test(x=antes,y=depois, mu=0,alternative="two.sided",conf.level=0.95,paired=T)

################################################################################################################
# Exemplo - exercício 16 Livro Bussab - página 389

tabela16<-matrix(c(100, 100, 120, 80), nrow=2, 
                 dimnames = list(c("Fiel",  "Não fiel"), 
                                 c("Homens", "Mulheres"))) 

chisq.test(tabela16,correct=F)
prop.test(tabela16,correct=F)
prop.test(x = c(100, 120), n = c(200, 200),correct=F)  #### testar para p1 - p2 = valor != 0

?prop.test

################################################################################################################

#Exemplo - exercício 18 Livro Bussab - página 389


prop.test(x = c(170, 194), n = c(400, 625),correct=F,alternative="greater")   # testando igualdade ou  maior

