# script_samplesize

library(tidyverse)
library(ggplot2)
library(dplyr)

# Função para calcular o coeficiente de variação
cv<- function(x) {
  
  # Desvio Padrão
  dp <- sd(x, na.rm = T)
  
  # Média
  md<- mean(x, na.rm = T)
  
  # Coeficiente de Variação (CI)
  
  cv<- (dp/md)*100
}


# Número total de elementos no conjunto de dados
total_elementos <- length(dados$V1)
#view(total_elementos)


# Número de repetições
repeticoes <- 15100


# Dataframe para armazenar os resultados
resultados <- data.frame()
#view(resultados)

# Vetor para espécies
especies <- dados %>%
  distinct(V1) %>%
  pull(V1)

# Loop sobre as espécies
for (k in 1:4) {
  dta_fil <- dados %>%
    filter(V1 == especies[k])
  
  # Loop sobre o número de valores a serem selecionados
  for (i in 1:14) {
    # Vetor para armazenar médias
    medias <- numeric(repeticoes)
    
    # Loop para cada repetição
    for (j in 1:repeticoes) {
      # Seleciona i valores aleatórios
      amostra <- sample(dta_fil$V3, i, replace = T)
      
      ci <- cv(amostra)
      # Calcula a média dos valores selecionados
      media <- mean(ci, na.rm = T)
      
      # Armazena a média no vetor de médias
      medias[j] <- media
    }
    
    # Adiciona as médias calculadas ao dataframe de resultados
    resultados <- rbind(resultados,
                        data.frame(
                          especie = especies[k],
                          amostra = i,
                          Media = medias
                        ))
  }
}


#Prochilodus nigricans
resultados %>%
  filter(especie == "Prochilodus_sp") %>%
  group_by(especie, amostra) %>%
  summarise(
    md = mean(Media)
  ) %>%
  ggplot() +
  aes(x = amostra, y = md, fill = especie) +
  geom_point(stat = "identity", color = "black") + 
  scale_x_continuous(n.breaks = 15) +
  theme_classic() +
  labs(x = "Specimens added", y = "CV mean", fill = "Prochilodus nigricans") +
  scale_fill_manual(values = c("Prochilodus_sp" = "black"), 
  labels = c("Prochilodus_sp" = "Prochilodus nigricans")) +
  scale_color_manual(values = c("Prochilodus_sp" = "black"), 
   labels = c("Prochilodus_sp" = "Prochilodus nigricans")) +
  theme(legend.position = "top", legend.title = element_blank()) +
  guides(fill = guide_legend(title = "Prochilodus nigricans"), 
         color = guide_legend(title = "Prochilodus nigricans"))


#Aspidoras albater 
resultados %>%
  filter(especie == "Aspidoras_sp") %>%
  group_by(especie, amostra) %>%
  summarise(
    md = mean(Media)
  ) %>%
  ggplot() +
  aes(x = amostra, y = md, fill = especie) +
  geom_point(stat = "identity", color = "black") + 
  scale_x_continuous(n.breaks = 15) +
  theme_classic() +
  labs(x = "Specimens added", y = "CV mean", fill = "Aspidoras albater") +
  scale_fill_manual(values = c("Aspidoras_sp" = "black"), 
  labels = c("Aspidoras_sp" = "Aspidoras albater")) +
  scale_color_manual(values = c("Aspidoras_sp" = "black"), 
  labels = c("Aspidoras_sp" = "Aspidoras albater")) +
  theme(legend.position = "top", legend.title = element_blank()) +
  guides(fill = guide_legend(title = "Aspidoras albater"), 
         color = guide_legend(title = "Aspidoras albater"))


#Cichla piquiti________________________________________________________________
resultados %>%
  filter(especie == "Cichla_sp") %>%
  group_by(especie, amostra) %>%
  summarise(
    md = mean(Media)
  ) %>%
  ggplot() +
  aes(x = amostra, y = md, fill = especie) +
  geom_point(stat = "identity", color = "black") + 
  scale_x_continuous(n.breaks = 15) +
  theme_classic() +
  labs(x = "Specimens added", y = "CV mean", fill = "Cichla piquiti") +
  scale_fill_manual(values = c("Cichla_sp" = "black"), 
  labels = c("Cichla_sp" = "Cichla piquiti")) +
  scale_color_manual(values = c("Cichla_sp" = "black"), 
  labels = c("Cichla_sp" = "Cichla piquiti")) +
  theme(legend.position = "top", legend.title = element_blank()) +
  guides(fill = guide_legend(title = "Cichla piquiti"), 
         color = guide_legend(title = "Cichla piquiti"))


#Sphoeroides aff. tocantinensis
resultados %>%
  filter(especie == "Sphoeroides_colomesus") %>%
  group_by(especie, amostra) %>%
  summarise(
    md = mean(Media)
  ) %>%
  ggplot() +
  aes(x = amostra, y = md, fill = especie) +
  geom_point(stat = "identity", color = "black") + 
  scale_x_continuous(n.breaks = 15) +
  theme_classic() +
  labs(x = "Specimens added", y = "CV mean", 
       fill = "Sphoeroides aff. tocantinensis") +
  scale_fill_manual(values = c("Sphoeroides_colomesus" = "black"), 
  labels = c("Sphoeroides_colomesus" = "Sphoeroides aff. tocantinensis")) +
  scale_color_manual(values = c("Sphoeroides_colomesus" = "black"), 
  labels = c("Sphoeroides_colomesus" = "Sphoeroides aff. tocantinensis")) +
  theme(legend.position = "top", legend.title = element_blank()) +
  guides(fill = guide_legend(title = "Sphoeroides aff. tocantinensis"), 
         color = guide_legend(title = "Sphoeroides aff. tocantinensis"))


#General analyses 

# Média geral
mean_v2 <- mean(p_est$V2)
print(paste("Média de V2:", mean_v2))
#Mean = 8.0375

# Moda geral
mode_v2 <- get_mode(p_est$V2)
print(paste("Moda de V2:", mode_v2))
#Moda = 8



#____________________________________________________________________________
#Média e moda para todas as espécies
get_mode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
results <- p_est %>%
  group_by(V1) %>%
  summarise(
    mean_V2 = mean(V2),
    mode_V2 = get_mode(V2)
  )

# Exibir os resultados
print(results)

#V1                                  mean_V2 mode_V2
#<chr>                               <dbl>   <int>
#  1 Aspidoras albater               7.67       8
#2 Cichla piquiti                    9.55       9
#3 Prochilodus nigricans             6.42       6
#4 Sphoeroides aff. tocantinensis    8.51       8



#Boxplot geral
ggplot(data = p_est, aes(x = V2, y = V1, fill = V1)) +
  geom_boxplot(width = 0.5) +
  scale_fill_manual(values = c("gray", "gray", "gray", "gray")) +
  theme_bw(base_size = 10) +
  theme(legend.position = "none") +  # Remove a legenda
  labs(title = "Inflection point by species", x = "Inflection point", 
       y = "Species")


#Frequency geral
# Calculando a frequência de cada ponto de inflexão
freq <- p_est %>%
  group_by(V2) %>%
  summarise(count = n()) %>%
  mutate(percentage = count / sum(count) * 100)

# Exibindo o dataframe de frequências para verificação
print(freq)

# Criando o gráfico de barras com as porcentagens dentro das barras
ggplot(freq, aes(x = as.factor(V2), y = percentage, fill = percentage)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = sprintf("%.1f%%", percentage)), 
            vjust = -0.5, color = "black", size = 3.5) +
  scale_fill_gradient(low = "lightgray", high = "gray50", 
                      name = "Inflection point") +
  labs(title = "Frequency of Inflection Points",
       x = "Inflection Point",
       y = "Percentage") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1),
        legend.position = "none")



# Frequênci para Prochilodus nigricans
p_estPN <- p_est %>%
  filter(V1 == "Prochilodus nigricans") %>% 
  group_by(V2) %>%
  summarise(count = n()) %>%
  mutate(percentage = count / sum(count) * 100)

grafico_prochilodus <- ggplot(data = p_estPN, aes(x = factor(V2), 
                                                  y = percentage, 
                                                  fill = percentage)) +
  geom_bar(stat = "identity") + 
  labs(x = "Inflection point", y = "Frequency (%)") + 
  ggtitle(expression(paste("Frequency of Inflection Point for ", 
                           italic("Prochilodus nigricans")))) + 
  geom_text(aes(label = paste0(round(percentage, 1), "%")), 
            position = position_stack(vjust = 0.5), 
            color = "black", size = 3) + 
  theme_minimal() + 
  scale_fill_gradient(low = "lightgray", high = "gray50", 
                      name = "Inflection point") + 
  theme(axis.text.x = element_text(angle = 30, hjust = 1),
        legend.position = "none") 

print(grafico_prochilodus)


# Frequência para Aspidoras albater
p_estAA <- p_est %>%
  filter(V1 == "Aspidoras albater") %>% 
  group_by(V2) %>%
  summarise(count = n()) %>%
  mutate(percentage = count / sum(count) * 100)

grafico_aspidoras <- ggplot(data = p_estAA, aes(x = factor(V2), 
                                                y = percentage, 
                                                fill = percentage)) +
  geom_bar(stat = "identity") + 
  labs(x = "Inflection point", y = "Frequency (%)") + 
  ggtitle(expression(paste("Frequency of Inflection Point for ", 
                           italic("Aspidoras albater")))) + 
  geom_text(aes(label = paste0(round(percentage, 1), "%")), 
            position = position_stack(vjust = 0.5), 
            color = "black", size = 3) + 
  theme_minimal() + 
  scale_fill_gradient(low = "lightgray", high = "gray50", 
                      name = "Inflection point") + 
  theme(axis.text.x = element_text(angle = 30, hjust = 1),
        legend.position = "none") 

print(grafico_aspidoras)


#_______________________________________________________________________________________
# Gráfico de pontos de estabilização para Cichla piquiti
p_estCP <- p_est %>%
  filter(V1 == "Cichla piquiti") %>% 
  group_by(V2) %>%
  summarise(count = n()) %>%
  mutate(percentage = count / sum(count) * 100)

grafico_cichla <- ggplot(data = p_estCP, aes(x = factor(V2), 
                                             y = percentage, 
                                             fill = percentage)) +
  geom_bar(stat = "identity") + 
  labs(x = "Inflection point", y = "Frequency (%)") + 
  ggtitle(expression(paste("Frequency of Inflection Point for ",
                           italic("Cichla piquiti")))) + 
  geom_text(aes(label = paste0(round(percentage, 1), "%")), 
            position = position_stack(vjust = 0.5), 
            color = "black", size = 3) + 
  theme_minimal() + 
  scale_fill_gradient(low = "lightgray", high = "gray50", 
                      name = "Inflection point") + 
  theme(axis.text.x = element_text(angle = 30, hjust = 1),
        legend.position = "none") 

print(grafico_cichla)
#_________________________________________________________________________________________
#Gráfico de pontos de estabilização para Sphoeroides tocantinensis
p_estST <- p_est %>%
  filter(V1 == "Sphoeroides aff. tocantinensis") %>% 
  group_by(V2) %>%
  summarise(count = n()) %>%
  mutate(percentage = count / sum(count) * 100)

grafico_sphoeroides <- ggplot(data = p_estST, aes(x = factor(V2), 
                                                  y = percentage, 
                                                  fill = percentage)) +
  geom_bar(stat = "identity") + 
  labs(x = "Inflection point", y = "Frequency (%)") + 
  ggtitle(expression(paste("Frequency of Inflection Point for ", 
                           italic("Sphoeroides aff. tocantinensis")))) + 
  geom_text(aes(label = paste0(round(percentage, 1), "%")), 
            position = position_stack(vjust = 0.5), 
            color = "black", size = 3) + 
  theme_minimal() + 
  scale_fill_gradient(low = "lightgray", high = "gray50", 
                      name = "Inflection point") + 
  theme(axis.text.x = element_text(angle = 30, hjust = 1),
        legend.position = "none") 

print(grafico_sphoeroides)
