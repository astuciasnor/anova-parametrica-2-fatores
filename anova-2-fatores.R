# Carregar pacotes
library(ggplot2)
library(dplyr)
library(car)
library(multcomp)
library(multcompView)

# 1. Criando o conjunto de dados
fertilizante <- factor(rep(c("Baixo", "Médio", "Alto"), each = 15))
solo <- factor(rep(c("Arenoso", "Argiloso", "Siltoso"), times = 15))
set.seed(123)
rendimento <- rnorm(45, mean = 10, sd = 2) +
  as.numeric(fertilizante) * 2 +
  as.numeric(solo) * 1.5 +
  as.numeric(fertilizante) * as.numeric(solo) * 0.5
dados <- data.frame(Fertilizante = fertilizante, Solo = solo, Rendimento = rendimento)

# 2. Análise exploratória
head(dados)

# 3. Verificação dos pressupostos
modelo <- aov(Rendimento ~ Fertilizante * Solo, data = dados)
residuos <- residuals(modelo)
shapiro.test(residuos)
leveneTest(Rendimento ~ Fertilizante * Solo, data = dados)

# 4. Ajuste do modelo com interação
dados$Fertilizante_Solo <- interaction(dados$Fertilizante, dados$Solo)
modelo_interacao <- aov(Rendimento ~ Fertilizante_Solo, data = dados)

# 5. Análise pós-hoc para a interação
comparacao_interacao <- glht(modelo_interacao, linfct = mcp(Fertilizante_Solo = "Tukey"))
letras <- cld(comparacao_interacao, level = 0.05)$mcletters$Letters

# 6. Preparação dos dados para o gráfico
dados_resumo <- dados %>%
  group_by(Fertilizante, Solo) %>%
  summarise(
    Media = mean(Rendimento),
    EP = sd(Rendimento) / sqrt(n())
  )
dados_resumo$Fertilizante_Solo <- interaction(dados_resumo$Fertilizante, dados_resumo$Solo)
dados_resumo$Letras <- letras[match(dados_resumo$Fertilizante_Solo, names(letras))]

# 7. Criação do gráfico
ggplot(dados_resumo, aes(x = Fertilizante, y = Media, fill = Solo)) +
  geom_bar(stat = "identity", position = position_dodge(0.9)) +
  geom_errorbar(aes(ymin = Media - EP, ymax = Media + EP), width = 0.2, position = position_dodge(0.9)) +
  geom_text(aes(label = Letras, y = Media + EP + 0.5), position = position_dodge(0.9), vjust = 0) +
  theme_minimal() +
  labs(title = "Rendimento Médio por Fertilizante e Solo",
       x = "Nível de Fertilizante",
       y = "Rendimento Médio") +
  scale_fill_brewer(palette = "Pastel1")

