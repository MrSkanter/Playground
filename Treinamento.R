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
  
  DF <- na.omit(DF)
  
  DF$`F(X)` <- 0
  
  DF$W1_U <- 0
  
  DF$W2_U <- 0
  
  DF$W3_U <- 0
  
  DF$W4_U <- 0
  
  DF$W5_U <- 0
  
  DF$COMPRA <- 0
  
  DF$VENDA <- 0
  
  DF$RESULTADO <- 0
  
  ######################################################
  ###################REDE NEURAL########################
  ######################################################
  

  # Definir uma semente para reproducibilidade
  
  
  # Inicializar os pesos apenas na primeira linha
  DF$W1[1] <- round(runif(1, min = 0, max = 1), digits = 6)
  DF$W2[1] <- round(runif(1, min = 0, max = 1), digits = 6)
  DF$W3[1] <- round(runif(1, min = 0, max = 1), digits = 6)
  DF$W4[1] <- round(runif(1, min = 0, max = 1), digits = 6)
  DF$W5[1] <- round(runif(1, min = 0, max = 1), digits = 6)
  
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
  
  LR <- 0.01
  
  # Função sigmoide
  sigmoid <- function(x) {
    return(1 / (1 + exp(-x)))
  }
  
  # Loop para atualizar os pesos, X e Y sequencialmente
  for (i in 1:nrow(DF)) {
    # 1. Calcular o termo comum: (TARGET_BIN - Y) * Return * Y * (1 - Y) * LR
    termo_comum <- (DF$Target_Bin[i] - DF$`F(X)`[i]) * DF$Return[i] * DF$`F(X)`[i] * (1 - DF$`F(X)`[i]) * LR
    
    # 2. Atualizar os pesos da linha atual (W1_U, W2_U, etc.)
    DF$W1_U[i] <- DF$W1[i] + termo_comum
    DF$W2_U[i] <- DF$W2[i] + termo_comum
    DF$W3_U[i] <- DF$W3[i] + termo_comum
    DF$W4_U[i] <- DF$W4[i] + termo_comum
    DF$W5_U[i] <- DF$W5[i] + termo_comum
    
    # 3. Atualizar os pesos da próxima linha (W1, W2, etc.) com os valores atualizados da linha atual
    if (i < nrow(DF)) {
      DF$W1[i + 1] <- DF$W1_U[i]
      DF$W2[i + 1] <- DF$W2_U[i]
      DF$W3[i + 1] <- DF$W3_U[i]
      DF$W4[i + 1] <- DF$W4_U[i]
      DF$W5[i + 1] <- DF$W5_U[i]
    }
    
    # 4. Calcular o novo X para a próxima linha (se não for a última linha)
    if (i < nrow(DF)) {
      DF$X[i + 1] <- DF$Return[i + 1] * DF$STD_RET_05[i + 1] * DF$STD_RET_20[i + 1] * DF$STD_RET_50[i + 1] * DF$STD_RET_100[i + 1] *
        DF$W1[i + 1] * DF$W2[i + 1] * DF$W3[i + 1] * DF$W4[i + 1] * DF$W5[i + 1]
    }
    
    # 5. Calcular o novo Y (F(X)) para a próxima linha (se não for a última linha)
    if (i < nrow(DF)) {
      DF$`F(X)`[i + 1] <- sigmoid(DF$X[i + 1])
    }
  }

  DF$COMPRA <- ifelse(DF$`F(X)` >= 0.501 & DF$`F(X)` <= 0.503, DF$Target, 0)
  
  DF$VENDA <- ifelse(DF$`F(X)` >= 0.491 & DF$`F(X)` <= 0.493, -DF$Target, 0)
  
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
  
  # Definir os intervalos de F(X)
  faixas_FX <- seq(0, 1, by = 0.01)
  
  # Criar um data.frame para armazenar os resultados de cada faixa
  resultados_faixas <- data.frame(
    Faixa_Inicio = faixas_FX[-length(faixas_FX)],  # Início da faixa
    Faixa_Fim = faixas_FX[-1],                     # Fim da faixa
    Resultado_Compra = 0,                          # Resultado acumulado para compra
    Resultado_Venda = 0                            # Resultado acumulado para venda
  )
  
  # Calcular o resultado acumulado para cada faixa de F(X)
  for (j in 1:nrow(resultados_faixas)) {
    inicio <- resultados_faixas$Faixa_Inicio[j]
    fim <- resultados_faixas$Faixa_Fim[j]
    
    # Filtrar as operações de compra e venda que ocorreram dentro da faixa atual
    operacoes_faixa <- DF[DF$`F(X)` >= inicio & DF$`F(X)` < fim, ]
    
    # Calcular o resultado acumulado para compra e venda
    resultados_faixas$Resultado_Compra[j] <- sum(operacoes_faixa$COMPRA)
    resultados_faixas$Resultado_Venda[j] <- sum(operacoes_faixa$VENDA)
  }
  
  # Encontrar a melhor faixa para compra
  melhor_faixa_compra <- resultados_faixas[which.max(resultados_faixas$Resultado_Compra), ]
  
  # Encontrar a melhor faixa para venda
  melhor_faixa_venda <- resultados_faixas[which.max(resultados_faixas$Resultado_Venda), ]
  
  # Exibir as melhores faixas
  print("Melhor faixa para COMPRA:")
  print(melhor_faixa_compra)
  
  print("Melhor faixa para VENDA:")
  print(melhor_faixa_venda)
  
