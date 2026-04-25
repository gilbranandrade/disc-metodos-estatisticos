##########################################
############## Pacotes e configurações
##########################################

library(tidyverse)
library(gt) # tabela

##########################################
############## Leitura dos dados
##########################################

setwd("/Users/gilbranandrade/SourceCodes/disc-metodos-estatisticos/atv-separacao-dos-grupos")
getwd() 

dados <- read.csv2("./perfil_turma.csv")

alunosProgramacao <- data.frame(
  nome = c("Eduardo Medeiros", "Gilbran Andrade", "Yuri Henrique")
)

glimpse(dados)
glimpse(alunosProgramacao)

head(dados, 5)
head(alunosProgramacao)

##########################################
############## Organizacao das colunas
##########################################

dados <- dados %>%
  select(-Timestamp)

colnames(dados) <- c("nome", "comunicacao", "escrita", "lideranca")

dados <- dados %>%
  mutate(grupo = NA_character_)

alunosProgramacao <- alunosProgramacao %>%
  mutate(
    comunicacao = NA_integer_,
    escrita = NA_integer_,
    lideranca = NA_integer_,
    grupo = NA_character_
  )

### Respostas extremas
## Comentário CMD SHIFT C
# dados <- dados %>%
#   mutate(
#     comunicacao = case_when(
#       comunicacao %in% c(1, 2) ~ 1,
#       comunicacao %in% c(3, 4) ~ 4
#     ),
#     escrita = case_when(
#       escrita %in% c(1, 2) ~ 1,
#       escrita %in% c(3, 4) ~ 4
#     ),
#     lideranca = case_when(
#       lideranca %in% c(1, 2) ~ 1,
#       lideranca %in% c(3, 4) ~ 4
#     ),
#   )

##########################################
############## Definição de funções
##########################################

desequilibrio <- function(dados) {
  resumo <- dados %>%
    group_by(grupo) %>%
    summarise(
      soma_comunicacao = sum(comunicacao),
      soma_escrita = sum(escrita),
      soma_lideranca = sum(lideranca),
      .groups = "drop"
    )
  
  # função objetivo do problema de otimização de minimização
  d <- var(resumo$soma_comunicacao) + var(resumo$soma_escrita) + var(resumo$soma_lideranca)
  
  return(d)
}

divisao_grupos <- function(dados) {
  grupos_pelasorte <- dados %>%
    slice_sample(n = nrow(dados)) %>%
    mutate(
      grupo = rep(c("Grupo 1", "Grupo 2", "Grupo 3"), length.out = nrow(dados))
    )
  
  return(grupos_pelasorte)
}

##########################################
############## Main
##########################################

set.seed(as.integer(Sys.time())) # as.integer(Sys.time()) - 1777121756 - 1777138940

n_iteracoes <- 100000
# 1000 - 1s
# 10000 - 12s
# 100000 - 2min
# 1000000 - 22min

melhor_desequilibrio <- Inf
melhor_divisao <- NULL

for (i in 1:n_iteracoes) {
  divisao_atual <- divisao_grupos(dados)
  
  desequilibrio_atual <- desequilibrio(divisao_atual)
  
  if (desequilibrio_atual < melhor_desequilibrio) {
    melhor_desequilibrio <- desequilibrio_atual
    melhor_divisao <- divisao_atual
    
    if (melhor_desequilibrio == 0) {
      break
    }
  }
}

divisao_programacao <- divisao_grupos(alunosProgramacao)

grupos_finais <- bind_rows(
  melhor_divisao, divisao_programacao
)

##########################################
############## Mostrar a formação dos grupos
##########################################

grupos_finais %>%
  arrange(grupo, nome) %>%
  select('NOME'=nome, 'GRUPO'=grupo) %>%
  gt()

##########################################
############## Verificação de separação equalitária
##########################################

## Somas e Médias
sera_equilibrado <- melhor_divisao %>%
  group_by(grupo) %>%
  summarise(
    soma_comunicacao = sum(comunicacao),
    soma_escrita = sum(escrita),
    soma_lideranca = sum(lideranca),
    
    media_comunicacao = round(mean(comunicacao),2),
    media_escrita = round(mean(escrita),2),
    media_lideranca = round(mean(lideranca),2),
    .groups = "drop"
  )

sera_equilibrado %>%
  select('Grupo'=grupo, 
         'sum(C)'=soma_comunicacao, 'sum(E)'=soma_escrita, 'sum(L)'=soma_lideranca, 
         'mean(C)'=media_comunicacao, 'mean(E)'=media_escrita, 'mean(L)'=media_lideranca) %>%
  gt()

## Variância
funcao_objetivo <- sera_equilibrado %>%
  summarise(
    variancia_comunicacao = var(soma_comunicacao),
    variancia_escrita = var(soma_escrita),
    variancia_lideranca = var(soma_lideranca),
    
    desequilibrio = variancia_comunicacao + variancia_escrita + variancia_lideranca
  )

funcao_objetivo %>%
  mutate(
    desequilibrio = round(desequilibrio, 2),
    variancia_comunicacao = round(variancia_comunicacao, 2),
    variancia_escrita = round(variancia_escrita, 2),
    variancia_lideranca = round(variancia_lideranca, 2)
  ) %>%
  select('D'=desequilibrio, 
         'var(C)'=variancia_comunicacao, 
         'var(E)'=variancia_escrita, 
         'var(L)'=variancia_lideranca) %>%
  gt()

