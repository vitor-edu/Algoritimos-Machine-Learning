base = read.csv('credito.csv')
base$clientid = NULL
summary(base)  
base[base$age < 0,]
base[base$age < 0,1:2]
base[base$age < 0 & !is.na(base$age),]
idades_inv = base[base$age < 0 & !is.na(base$age),]

base1 = base
base1$age = NULL

base2 = base
base2 = base2[base2$age > 0,]

base3 = base
base3[16,2] = 28

base4 = base
mean(base4$age, na.rm = TRUE)
media = mean(base4$age[base4$age>0],na.rm = TRUE)

base4$age = ifelse(base4$age<0,media,base4$age)

summary(base4)

base5 = base4
base5[is.na(base5$age),]

base5$age = ifelse(is.na(base5$age),media,base5$age)

summary(base5)

# Fazendo escalonamento de atributos
base6 = base5
#base6 = scale(base6)     #CUIDADO! Não escalonar TODA a base
base6[,1:3] = scale(base6[,1:3])

summary(base6)

# Encode da classe(transforma valores categóricos em valores discretos)
base6$default = factor(base6$default,levels = c(0,1))

#Instalando o pacote
install.packages('caTools')
library(caTools)

#Dividindo a base 
set.seed(1)
divisao = sample.split(base6$default,SplitRatio = 0.75)
divisao

base_treino = subset(base6,divisao == TRUE)
base_teste = subset(base6,divisao == FALSE)

install.packages('e1071')
library(e1071)

classif_bayesiano = naiveBayes(x=base_treino[-4],y = base_treino$default)
print(classif_bayesiano)

#Avaliando o modelo
prev_bayesiano = predict(classif_bayesiano, newdata = base_teste[-4])
print(prev_bayesiano)

install.packages('caret')
library(caret)

matrix_confusao = confusionMatrix(matriz_confusao)
