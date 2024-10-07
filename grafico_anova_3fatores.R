library(ggplot2)
library(dplyr)

# Dados simulados
set.seed(123)
summary_data2 <- expand.grid(
  Setor = c("A", "B", "C"),
  Trimestre = c("Q1", "Q2", "Q3", "Q4"),
  Ano = 2021:2022
) %>%
  mutate(
    média_y = runif(n(), 50, 100),
    se_y = runif(n(), 5, 10)
  )

# Gráfico com facetas por Setor
ggplot(summary_data2, aes(x = Trimestre, y = média_y, fill = factor(Ano))) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_errorbar(aes(ymin = média_y - se_y, ymax = média_y + se_y),
                width = 0.2,
                position = position_dodge(0.9)) +
  facet_wrap(~ Setor) +
  labs(x = "Trimestre", y = "Média de N_Pts", fill = "Ano") +
  theme_minimal()
