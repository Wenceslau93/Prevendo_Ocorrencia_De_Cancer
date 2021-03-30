#Prevendo a ocorrência de cancer

#Definindo o diretório de trabalho

setwd("C:/FCD/ReAzure/Cap11/Classificacao")
getwd()

#Carregando o arquivo

?read.csv
prev <- read.csv("dataset.csv", header = TRUE, stringsAsFactors = FALSE)
View(prev)

#Resumo do dataset

str(prev)
summary(prev)

#Excluindo a coluna ID

prev$id = NULL

#Nomeando M e F

prev$diagnosis <- ifelse(prev$diagnosis == 'M', 'Maligno', 'Benigno')
View(prev)

#Transformando a variável acima em factor

prev$diagnosis <- factor(prev$diagnosis, levels = c('Benigno', 'Maligno'), 
                         labels = c("Benigno", "Maligno"))

str(prev$diagnosis)

#Verificando a divisão em porcentagem

round(prop.table(table(prev$diagnosis)) * 100, digit = 1)

count1 <- 0
count2 <- 0

for(i in 1:length(prev$diagnosis)){
  
  ifelse(prev$diagnosis[i] == 'Benigno', count1 <- count1 + 1, count2 <- count2 + 1)
}

print(count1)
print(count2)

count100 <- count1 + count2
count100

ben100 <- (count1 * 100) / count100
mal100 <- (count2 * 100) / count100

ben100
mal100

# Criando um função de normalização
normalizar <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

prev_norm <- as.data.frame(lapply(prev[ , 2:31], normalizar))
View(prev_norm)


#Treinando o modelo com KNN

library(class)
library(caret)

prev_treino <- prev_norm[1:49, ]
prev_teste <- prev_norm[1:49, ]

prev_treino_labels <- prev[1:49, 1]
prev_teste_labels <- prev[1:49, 1]

# Criando o modelo
modelo_knn_v1 <- knn(train = prev_treino, 
                     test = prev_teste,
                     cl = prev_treino_labels, 
                     k = 21)
View(modelo_knn_v1)

# A função knn() retorna um objeto do tipo fator 
#com as previsões para cada exemplo no dataset de teste

summary(modelo_knn_v1)

# Carregando o gmodels
library(gmodels)

# Criando uma tabela cruzada dos dados previstos x dados atuais
# Usaremos amostra com 100 observações: length(dados_teste_labels)
CrossTable(x = prev_teste_labels, y = modelo_knn_v1, prop.chisq = FALSE)

# Carregando segundo dataset

prev2 <- read.csv("dataset2.csv", header = TRUE, sep = ';',stringsAsFactors = FALSE)
View(prev2)

# Normalizando o segundo dataset

prev_norm2 <- as.data.frame(lapply(prev2, normalizar))

View(prev_norm2)

# Nova previsão

prev_treino2 <- prev_norm2[1:49, ]
prev_teste2 <- prev_norm2[40:49, ]

prev_treino_labels2 <- prev[1:49, 1]
prev_teste_labels2 <- prev[40:49, 1]


modelo_knn_v2 <- knn(train = prev_treino2, 
                     test = prev_norm2,
                     cl = modelo_knn_v1, 
                     k = 21)

summary(modelo_knn_v2)


View(modelo_knn_v2)

prev2$previsto <- modelo_knn_v2
View(prev2)
