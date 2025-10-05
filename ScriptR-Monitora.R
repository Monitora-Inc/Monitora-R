#Importando a biblioteca ggplot2
install.packages("ggplot2")
library(ggplot2)

#Importando a biblioteca patchwork
install.packages("patchwork")
library(patchwork)

#Juntando os dados coletados dos arquivos csv em um único dataframe
df <- rbind.data.frame(captura_C0D000,captura_C0D001,captura_C0D002,captura_C0D003,captura_C0D004)

#Preparando gráficos de média de cada recurso
recursos <- c("cpu", "ram", "disco", "usoRede", "latencia", "qtd_processos")
ylabels <- c("Uso de CPU (%)", "Uso de RAM (%)", "Uso de Disco (%)", "Uso de Rede (MB)",
             "Latência (ms)", "Processos")
mainlabels <- c("Média de uso de CPU por Servidor", "Média de uso de RAM por Servidor",
                "Média de uso de Disco por Servidor", "Média de uso de Rede por Servidor",
                "Média de Latência por Servidor", "Média de Processos por Servidor")
lista_graficos <- list()

for (i in seq_along(recursos)) {
  recurso <- recursos[i]
  ylabel <- ylabels[i]
  mainlabel <- mainlabels[i]
  #Função que prepara um dataframe específico para constituir o gráfico
  #Aggregate: usada para dividir os dados em subconjuntos, aplicar uma função (como mean, sum, etc.) 
  #a cada subconjunto e, em seguida, combinar os resultados em um novo objeto.
  dados_para_grafico <- aggregate(
    #Esta é a formula: Define o que será resumido e como será agrupado
    #A variável do lado esquerdo é a que o R aplicará a função e será o eixo y no gráfico
    #"~" significa "agrupado por ou em função de"
    #A variável do lado direito é a variável de agrupamento e será o eixo x no gráfico
    #reformulate constrói fórmulas de forma dinâmica, nesse caso a fórmula é: (recurso ~ id)
    reformulate("id", recurso),
    #Define a fonte dos dados
    data = df,
    #Define a função que será aplicada na agregação
    FUN = mean
  )
  
  grafico <- ggplot(dados_para_grafico, aes(x = id, y = .data[[recurso]])) +
    geom_bar(stat = "identity", fill = "skyblue") +
    labs(
      title = mainlabel,
      x = "Servidor (ID)",
      y = ylabel,
    ) + theme_minimal()
  
  lista_graficos[[i]] <- grafico
}

wrap_plots(lista_graficos, nrow = 2, ncol = 3)

#Preparando gráficos de mediana de cada recurso
mainlabels <- c("Mediana de uso de CPU por Máquina", "Mediana de uso de RAM por Máquina",
                "Mediana de uso de Disco por Máquina", "Mediana de uso de Rede por Máquina",
                "Mediana de Latência por Máquina", "Mediana de Processos por Máquina")
lista_graficos <- list()

for (i in seq_along(recursos)) {
  recurso <- recursos[i]
  ylabel <- ylabels[i]
  mainlabel <- mainlabels[i]
  
  dados_para_grafico <- aggregate(
    reformulate("id", recurso),
    data = df,
    FUN = median
  )
  
  grafico <- ggplot(dados_para_grafico, aes(x = id, y = .data[[recurso]])) +
    geom_bar(stat = "identity", fill = "skyblue") +
    labs(
      title = mainlabel,
      x = "Servidor (ID)",
      y = ylabel,
    ) + theme_minimal()
  
  lista_graficos[[i]] <- grafico
}

wrap_plots(lista_graficos, nrow = 2, ncol = 3)

#Preparando histogramas de cada recurso por servidor
lista_histogramas = list()
labels_recursos <- c("uso de CPU", "uso de RAM", "uso de Disco", "uso de Rede", 
                     "Latência", "Número de Processos")
#Servidor C0D000
for (j in seq_along(recursos)) {
  recurso_atual <- recursos[[j]]
  histograma <- ggplot(captura_C0D000, aes(x = .data[[recurso_atual]])) +
                  geom_histogram(aes(y = after_stat(density)),bins = 30, fill = "lightblue", color = "white") +
                  geom_density(color = "black", linewidth = 0.5) +
                  labs(
                    title = paste("Distribuição de",labels_recursos[[j]],"no Servidor C0D000"),
                    x = labels_recursos[[j]],
                    y = "Frequência"
                  ) + theme_minimal()
    
  lista_histogramas[[j]] <- histograma
}
wrap_plots(lista_histogramas, nrow = 2, ncol = 3)

#Servidor C0D001
for (j in seq_along(recursos)) {
  recurso_atual <- recursos[[j]]
  histograma <- ggplot(captura_C0D001, aes(x = .data[[recurso_atual]])) +
    geom_histogram(aes(y = after_stat(density)),bins = 30, fill = "lightblue", color = "white") +
    geom_density(color = "black", linewidth = 0.5) +
    labs(
      title = paste("Distribuição de",labels_recursos[[j]],"no Servidor C0D001"),
      x = labels_recursos[[j]],
      y = "Frequência"
    ) + theme_minimal()
  
  lista_histogramas[[j]] <- histograma
}
wrap_plots(lista_histogramas, nrow = 2, ncol = 3)

#Servidor C0D002
for (j in seq_along(recursos)) {
  recurso_atual <- recursos[[j]]
  histograma <- ggplot(captura_C0D002, aes(x = .data[[recurso_atual]])) +
    geom_histogram(aes(y = after_stat(density)),bins = 30, fill = "lightblue", color = "white") +
    geom_density(color = "black", linewidth = 0.5) +
    labs(
      title = paste("Distribuição de",labels_recursos[[j]],"no Servidor C0D002"),
      x = labels_recursos[[j]],
      y = "Frequência"
    ) + theme_minimal()
  
  lista_histogramas[[j]] <- histograma
}
wrap_plots(lista_histogramas, nrow = 2, ncol = 3)

#Servidor C0D003
for (j in seq_along(recursos)) {
  recurso_atual <- recursos[[j]]
  histograma <- ggplot(captura_C0D003, aes(x = .data[[recurso_atual]])) +
    geom_histogram(aes(y = after_stat(density)),bins = 30, fill = "lightblue", color = "white") +
    geom_density(color = "black", linewidth = 0.5) +
    labs(
      title = paste("Distribuição de",labels_recursos[[j]],"no Servidor C0D003"),
      x = labels_recursos[[j]],
      y = "Frequência"
    ) + theme_minimal()
  
  lista_histogramas[[j]] <- histograma
}
wrap_plots(lista_histogramas, nrow = 2, ncol = 3)

#Servidor C0D004
for (j in seq_along(recursos)) {
  recurso_atual <- recursos[[j]]
  histograma <- ggplot(captura_C0D004, aes(x = .data[[recurso_atual]])) +
    geom_histogram(aes(y = after_stat(density)),bins = 30, fill = "lightblue", color = "white") +
    geom_density(color = "black", linewidth = 0.5) +
    labs(
      title = paste("Distribuição de",labels_recursos[[j]],"no Servidor C0D004"),
      x = labels_recursos[[j]],
      y = "Frequência"
    ) + theme_minimal()
  
  lista_histogramas[[j]] <- histograma
}
wrap_plots(lista_histogramas, nrow = 2, ncol = 3)

#Calculando correlações entre recursos
cor(df$cpu, df$ram) # Resultado: -0.4136297
cor(df$cpu, df$disco) #Resultado: -0.07895412
cor(df$cpu, df$usoRede) # Resultado: 0.1504561
cor(df$cpu, df$latencia) # Resultado: -0.08082185
cor(df$cpu, df$qtd_processos) # Resultado: 0.6834651
cor(df$disco, df$ram) # Resultado: -0.005859376
cor(df$disco, df$usoRede) # Resultado: -0.04889564
cor(df$disco, df$latencia) # Resultado: -0.03234024
cor(df$disco, df$qtd_processos) # Resultado: -0.1108789
cor(df$ram, df$usoRede) # Resultado: -0.1103081
cor(df$ram, df$latencia) # Resultado: 0.1109371
cor(df$ram, df$qtd_processos) # Resultado: -0.7293521
cor(df$usoRede, df$latencia) # Resultado: 0.09917948
cor(df$usoRede, df$qtd_processos) # Resultado: 0.05095347
cor(df$latencia, df$qtd_processos) # Resultado: -0.1087479

#Fazendo modelos lineares das correlações mais fortes
#CPU e Processos
ggplot(df, aes(x = qtd_processos, y = cpu))+
  geom_point(color = "darkblue") +
  geom_smooth(
    method = "lm",
    se = TRUE,
    color = "red",
  ) +
  labs(
    title = "Relação entre uso de CPU e Quantidade de Processos",
    x = "Processos",
    y = "CPU (%)"
  )

#RAM e Processos
ggplot(df, aes(x = qtd_processos, y = ram))+
  geom_point(color = "darkblue") +
  geom_smooth(
    method = "lm",
    se = TRUE,
    color = "red",
  ) +
  labs(
    title = "Relação entre uso de RAM e Quantidade de Processos",
    x = "Processos",
    y = "RAM (%)"
  )

#Preparando boxplots de cada recurso por servidor
#Servidor C0D000
lista_boxplots <- list()
for (j in seq_along(recursos)) {
  recurso_atual <- recursos[[j]]
  ylabel <- ylabels[j]
  histograma <- ggplot(captura_C0D000, aes(y = .data[[recurso_atual]], x = 1)) +
    geom_boxplot(fill = "lightblue", color = "black") +
    labs(
      title = paste("Distribuição de",labels_recursos[[j]],"no Servidor C0D000"),
      x = "",
      y = ylabel
    ) + theme_minimal()
  
  lista_boxplots[[j]] <- histograma
}
wrap_plots(lista_boxplots, nrow = 2, ncol = 3)

#Servidor C0D001
for (j in seq_along(recursos)) {
  recurso_atual <- recursos[[j]]
  ylabel <- ylabels[j]
  histograma <- ggplot(captura_C0D001, aes(y = .data[[recurso_atual]], x = 1)) +
    geom_boxplot(fill = "lightblue", color = "black") +
    labs(
      title = paste("Distribuição de",labels_recursos[[j]],"no Servidor C0D001"),
      x = "",
      y = ylabel
    ) + theme_minimal()
  
  lista_boxplots[[j]] <- histograma
}
wrap_plots(lista_boxplots, nrow = 2, ncol = 3)

#Servidor C0D002
for (j in seq_along(recursos)) {
  recurso_atual <- recursos[[j]]
  ylabel <- ylabels[j]
  histograma <- ggplot(captura_C0D002, aes(y = .data[[recurso_atual]], x = 1)) +
    geom_boxplot(fill = "lightblue", color = "black") +
    labs(
      title = paste("Distribuição de",labels_recursos[[j]],"no Servidor C0D002"),
      x = "",
      y = ylabel
    ) + theme_minimal()
  
  lista_boxplots[[j]] <- histograma
}
wrap_plots(lista_boxplots, nrow = 2, ncol = 3)

#Servidor C0D003
for (j in seq_along(recursos)) {
  recurso_atual <- recursos[[j]]
  ylabel <- ylabels[j]
  histograma <- ggplot(captura_C0D003, aes(y = .data[[recurso_atual]], x = 1)) +
    geom_boxplot(fill = "lightblue", color = "black") +
    labs(
      title = paste("Distribuição de",labels_recursos[[j]],"no Servidor C0D003"),
      x = "",
      y = ylabel
    ) + theme_minimal()
  
  lista_boxplots[[j]] <- histograma
}
wrap_plots(lista_boxplots, nrow = 2, ncol = 3)

#Servidor C0D004
for (j in seq_along(recursos)) {
  recurso_atual <- recursos[[j]]
  ylabel <- ylabels[j]
  histograma <- ggplot(captura_C0D004, aes(y = .data[[recurso_atual]], x = 1)) +
    geom_boxplot(fill = "lightblue", color = "black") +
    labs(
      title = paste("Distribuição de",labels_recursos[[j]],"no Servidor C0D004"),
      x = "",
      y = ylabel
    ) + theme_minimal()
  
  lista_boxplots[[j]] <- histograma
}
wrap_plots(lista_boxplots, nrow = 2, ncol = 3)
