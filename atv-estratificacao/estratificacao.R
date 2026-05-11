library(dplyr)
library(ggplot2)

################################################
### Definição da base de dados
################################################
dados <- tibble(
  numero = 1:100,
  area = c(1, 1, 1, 1, 1, 5, 12, 1, 1, 1, 1, 8, 16, 4, 9, 1, 9, 4, 1, 1, 1, 4, 
           10, 5, 18, 12, 4, 5, 10, 4, 16, 5, 12, 12, 4, 4, 10, 9, 12, 8, 16, 6, 
           4, 1, 10, 3, 16, 6, 10, 1, 16, 12, 6, 3, 16, 4, 18, 4, 8, 16, 8, 3, 
           9, 1, 5, 10, 4, 12, 4, 18, 4, 12, 16, 10, 8, 18, 3, 4, 8, 2, 15, 6, 
           2, 5, 8, 5, 8, 4, 12, 16, 3, 5, 16, 3, 6, 18, 4, 6, 9, 12)
)

dados %>%
  count(area)

soma <- sum(dados$area)

set.seed(as.integer(Sys.time()))

################################################
### Categorização dos dados
################################################
dados <- dados %>%
  mutate(
    estrato = case_when(
      area <= 2 ~ "A",
      area > 2 & area <= 4 ~ "B",
      area > 4 & area <= 8 ~ "C",
      area > 8 & area <= 12 ~ "D",
      area > 12 ~ "E"
    )
  )

glimpse(dados)

################################################
### Dados da população
################################################
mediaReal <- mean(dados$area)
desvioReal <- sqrt(sum((dados$area - mean(dados$area))^2) / length(dados$area))

################################################
### Gráfico Barras de cada Estrato
################################################
graf_barra <- dados %>%
  count(estrato)

ggplot(graf_barra, aes(x = estrato, y = n)) +
  geom_col() +
  labs(
    title = "Frequência por estrato",
    x = "Estrato",
    y = "Quantidade de Elementos"
  ) +
  scale_y_continuous(
    breaks = seq(0, 100, by = 1),
    labels = scales::label_number(accuracy = 1) 
  ) +
  theme_minimal()

################################################
### Gráfico Box Plot dos Estratos
################################################
dados_plot <- bind_rows(
  dados,
  dados %>% mutate(estrato = "População")
)

ggplot(dados_plot, aes(x = estrato, y = area)) +
  geom_boxplot() +
  labs(
    title = "Boxplot dos Estratos e da População",
    x = "Estrato",
    y = "Área"
  ) +
  scale_y_continuous(
    breaks = seq(0, 100, by = 1),
    labels = scales::label_number(accuracy = 1) 
  ) +
  theme_minimal()

################################################
### Gráfico de Dispersão
################################################
dispersao <- function(tabela, mediaReal, desvioReal, cartesiano, n) {
  if (n == 5){
    titulo = "n=5 - Dispersão das médias das áreas x desvio padrão"
  }
  if (n == 10){
    titulo = "n=10 - Dispersão das médias das áreas x desvio padrão"
  }
  
  if (cartesiano == 0){
    inicioX = 0
    inicioY = 0
    tamPonto = 1
    posTextoX = 0
    posTextoYRed = 5.6
    posTextoYGreen = 6.35
  } else {
    inicioX = min(tabela$media)-0.1
    inicioY = min(tabela$desvio_padrao)-0.1
    tamPonto = 2
    posTextoX = 6.9
    posTextoYRed = 6.25
    posTextoYGreen = 6.4
  }
  
  ggplot(tabela, aes(x = media, y = desvio_padrao)) +
    geom_point(size = tamPonto) +
    geom_hline(yintercept = inicioY, linewidth = 0.5) +
    geom_vline(xintercept = inicioX, linewidth = 0.5) +
    geom_point(
      aes(x = mean(media), y = mean(desvio_padrao)),
      color = "red",
      size = tamPonto+2
    ) +
    geom_point(
      aes(x = mediaReal, y = desvioReal),
      color = "darkgreen",
      size = tamPonto+2
    ) +
    annotate(
      "text",
      x = posTextoX,
      y = posTextoYRed,
      label = paste("Média das médias =", round(mean(tabela$media), 2),
                    "\nMédia dos desvios =", round(mean(tabela$desvio_padrao), 2)),
      color = "red",
      vjust = 0,
      hjust = 0
    ) +
    annotate(
      "text",
      x = posTextoX,
      y = posTextoYGreen,
      label = paste("Média real =", mediaReal,
                    "\nDesvio real =", round(desvioReal, 2)),
      color = "darkgreen",
      vjust = 0,
      hjust = 0
    ) +
    scale_x_continuous(
      breaks = seq(min(tabela$media), max(tabela$media), by = 0.1),
      labels = scales::label_number(accuracy = 0.1)
    ) +
    scale_y_continuous(
      breaks = seq(min(tabela$desvio_padrao), max(tabela$desvio_padrao), by = 0.1),
      labels = scales::label_number(accuracy = 0.1)
    ) +
    labs(
      title = titulo,
      x = "Média",
      y = "Desvio Padrão"
    ) +
    theme_minimal() +
    theme(
      panel.grid = element_blank()
    )
}

################################################
### Cálculo por amostragem
################################################
calculoAmostragem <- function(tabela, tamN) {
  n_iteracoes <- 100
  
  if (tamN == 5){
    fatia = 1
  }
  if (tamN == 10){
    fatia = 2
  }
  
  calculos <- list()
  
  pesos_estrato <- tibble(
    estrato = c("A", "B", "C", "D", "E"),
    num_elementos = c(18, 22, 22, 22, 16)
  )
  
  for (i in 1:n_iteracoes) {
    aae <- tabela %>%
      group_by(estrato) %>%
      slice_sample(n=fatia) %>%
      ungroup()
    
    dados_pond <- aae %>%
      left_join(pesos_estrato, by = "estrato")
    
    calculos[[i]] <- tibble(
      dados_pond %>%
        summarise(media = sum(area * num_elementos) / sum(num_elementos)) %>%
        select(media),
      desvio_padrao = sd(aae$area),
      n = length(aae$area)
    )
  }

  aae <- bind_rows(calculos)
  
  return (aae)
}

################################################
### Main
################################################

aae_n5 <- calculoAmostragem(dados, 5)
aae_n10 <- calculoAmostragem(dados, 10)

min(aae_n5$media)
max(aae_n5$media)
min(aae_n5$desvio_padrao)
max(aae_n5$desvio_padrao)
sd(aae_n5$media)
sd(aae_n10$media)

dispersao(aae_n5, mediaReal, desvioReal, 0, 5)
dispersao(aae_n5, mediaReal, desvioReal, 1, 5)
dispersao(aae_n10, mediaReal, desvioReal, 0, 10)
dispersao(aae_n10, mediaReal, desvioReal, 1, 10)

