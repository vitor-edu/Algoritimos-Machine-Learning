base = read.csv('credit-data.csv')
base$clientid = NULL
base
summary(base) # Retorna os dados estatisticos básicos.

#valores média
media = mean(base$age[base$age>0],na.rm = TRUE)


# Substituir valores negaativo pela média
base$age = ifelse(base$age<0,media,base$age)

# Substituir valores n/a pela media
base$age = ifelse(is.na(base$age),media,base$age)

# escalonamento  
base[,1:3] = scale(base[,1:3])

base$default = factor(base$default,levels = c(0,1))

library(caTools)
library(e1071)
library(caret)

classIdx <- ncol(base)
folds <- creatFolds(base[, classIdx],10,FALSE)

# Variáveis

melhor_acuracia = 0
fold_melhor_acuracia = -1
melhor_base_treino = 0
melhor_base_teste = 0
melhor_modelo = 0

for (i in 1:10){ 

 base_treino <- (data[folds!=i,]) # Atributos descritores de treino. 
 base_teste <- (data[folds==i,]) # Atributos descritores de teste. 

 #cria o classificador svm e faz as previsões
 classif_svm = svm(formula = default ~ ., data = base_treino,
                  type = 'Cclassification', kernel = 'linear', cost = 10)

 prev_svm = predict(classif_svm, newdata = base_teste[-4])
 matriz_confusao = table(base_teste[ , 4], prev_svm)
 result_matrix_confusao = confusionMatrix(matriz_confusao)
 print(result_matrix_confusao)


 result_cm = confusionMatrix(matriz_confusao)
 resumo_cm = result_matrix_confusao$overall
 print(resumo_cm) 

 acuracia_cm = resumo_matrix_confusao['Accuracy']
 print(acuracia_cm)  

# super importante

 if(acuracia_cm > melhor_acuracia){
 melhor_acuracia = acuracia_cm
 fold_melhor_acuracia = i
 melhor_base_teste = base_teste
 melhor_base_treino = base_treino
 melhor_modelo = classif_svm
}}


            