load("skew.RData")
dim(dat)
qqnorm(dat)
par(mfrow = c(3,3))
# Fazer QQ-plots para cada coluna da matriz
for (col in 1:ncol(dat)) {
  qqnorm(dat[, col], main = paste("Coluna", col))
}
head(InsectSprays)
# Carregar o conjunto de dados InsectSprays (já está incluído no R)
data("InsectSprays")
boxplot(split(InsectSprays$count, InsectSprays$spray),
        xlab = "Inseticida",
        ylab = "Contagem de Insetos",
        main = "Boxplot das Contagens de Insetos por Inseticida")

# Criar o gráfico de boxplot usando uma fórmula
boxplot(count ~ spray, data = InsectSprays,
        xlab = "Inseticida",
        ylab = "Contagem de Insetos",
        main = "Boxplot das Contagens de Insetos por Inseticida")

