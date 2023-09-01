library(dplyr)
# Carregar o conjunto de dados nym.2002 do pacote Lock5Data
data(nym.2002)

# Separar os dados por gênero
male_data <- nym.2002 %>% filter(gender == "M")
female_data <- nym.2002 %>% filter(gender == "F")

# Comparar com boxplots
boxplot(male_data$time, female_data$time,
        names = c("Masculino", "Feminino"),
        xlab = "Gênero",
        ylab = "Tempo de Finalização",
        main = "Comparação dos Tempos de Finalização por Gênero")

# Comparar com histogramas
hist(male_data$time, col = "blue", xlim = c(100, 400),
     main = "Histograma dos Tempos de Finalização - Masculino",
     xlab = "Tempo de Finalização")
hist(female_data$time, col = "pink", xlim = c(100, 400),
     main = "Histograma dos Tempos de Finalização - Feminino",
     xlab = "Tempo de Finalização")