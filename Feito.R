library(quantmod)
library(zoo)
library(ggplot2)

#Download dos Dados do Yahoo Finance 

getSymbols("GC=F", src = "yahoo", from ="2020-01-01", to = "2025-02-17")

#Remove a string "GC=F." do nome das colunas

colnames(`GC=F`) <- gsub("GC=F.", "", colnames(`GC=F`))

#Cria um DataFrame com os dados

DF <- as.data.frame(`GC=F`)

#Remove a coluna Volume

DF$Volume <- NULL

#Cria uma coluna Alvo, que é a diferença percentual do preço dois dias no futuro

DF$Target <- c(DF$Adjusted[-(1:2)], NA, NA)

DF$Target <- ((DF$Target - DF$Adjusted) / DF$Adjusted) * 100

#Ajusta o numero de decimais para 2

DF$Target <- round(DF$Target, digits = 2 )

#Cria um Alvo binario, se Alvo > 0 é 1 se não 0

DF$Target_Bin <- ifelse(DF$Target > 0, 1, 0)

#Criação da Media Movel

DF$MA_20 <- rollmean(DF$Adjusted, k = 20, fill = NA, align = "right")

#Criação das Variaveis

#Retorno

DF$Return <- c(NA, diff(DF$Adjusted) / DF$Adjusted[-length(DF$Adjusted)] * 100)

DF$Return <- round(DF$Return, digits = 2)

#Desvio Padrão 5  

DF$STD_RET_05 <- rollapply(DF$Return, width = 5, FUN = sd, fill = NA, align = "right" )

DF$STD_RET_05 <- round(DF$STD_RET_05, digits = 2)

#Desvio Padrão 20  

DF$STD_RET_20 <- rollapply(DF$Return, width = 20, FUN = sd, fill = NA, align = "right" )

DF$STD_RET_20 <- round(DF$STD_RET_20, digits = 2)

#Desvio Padrão 50 

DF$STD_RET_50 <- rollapply(DF$Return, width = 50, FUN = sd, fill = NA, align = "right" )

DF$STD_RET_50 <- round(DF$STD_RET_50, digits = 2)

#Desvio Padrão 100

DF$STD_RET_100 <- rollapply(DF$Return, width = 100, FUN = sd, fill = NA, align = "right" )

DF$STD_RET_100 <- round(DF$STD_RET_50, digits = 2)

DF$W1 <- 0

DF$W2 <- 0

DF$W3 <- 0

DF$W4 <- 0

DF$W5 <- 0

DF$X <- 0

######################################################
###################REDE NEURAL########################
######################################################

DF <- na.omit(DF)

DF$`F(X)` <- 0

DF$COMPRA <- 0

DF$VENDA <- 0

DF$RESULTADO <- 0

# Definir uma semente para reproducibilidade


# Inicializar os pesos apenas na primeira linha
DF$W1 <- -0.08928413
DF$W2 <- 0.6385809
DF$W3 <- 0.4484259
DF$W4 <- 0.1745209
DF$W5 <- 0.4205419

calcular_X <- function(linha) {
  # Produto das variáveis
  produto_variaveis <- linha["Return"] * linha["STD_RET_05"] * linha["STD_RET_20"] * linha["STD_RET_50"] * linha["STD_RET_100"]
  
  # Produto dos pesos
  produto_pesos <- linha["W1"] * linha["W2"] * linha["W3"] * linha["W4"] * linha["W5"]
  
  # Calcular X
  X <- produto_variaveis * produto_pesos
  
  return(X)
}

DF$X <- apply(DF, 1, calcular_X)

calcular_y <- function(linha) {
  #calcular y
  y <- (1 / (1 + exp(- linha["X"])))
  return(y)
}

DF$`F(X)` <- apply(DF, 1, calcular_y)

DF$COMPRA <- ifelse(DF$`F(X)` >= 0.5 & DF$`F(X)` <= 0.51, DF$Target, 0)

DF$VENDA <- ifelse(DF$`F(X)` >= 0.49 & DF$`F(X)` <= 0.5, -DF$Target, 0)

DF$RESULTADO <- cumsum(DF$COMPRA + DF$VENDA)


DF$Index <- 1:nrow(DF)

# Criar o gráfico de linha
ggplot(DF, aes(x = Index, y = RESULTADO)) +
  geom_line(color = "blue", size = 1) +  
  labs(
    title = "Resultado Acumulado ao Longo do Tempo",
    x = "Tempo",
    y = "Resultado Acumulado"
  ) +
  theme_minimal()  



