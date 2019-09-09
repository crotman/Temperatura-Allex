library(tidyverse)
library(magrittr)
library(furrr)
library(tictoc)
library(readxl)

le_um_arquivo <- function(arquivo)
{

  conteudo <- read_csv(pull(arquivo, arquivo)) %>% 
    mutate(linha = row_number()) %>%
    rename(conteudo = 1) %>% 
    mutate(inicio = str_starts(conteudo, "Estacao;")) 
  
  cabecalho <- conteudo %>% 
    filter(inicio) %>%
    select(conteudo) %>% 
    pull(conteudo) %>% 
    str_split(";") %>% 
    unlist()
    
  linha_inicio <- conteudo %>% 
      filter(inicio) %>% 
      pull(linha)
  
  cabecalho[length(cabecalho)] <- "lixo"
  
  limpa_nome <- function(x) str_replace_all(x," ", "_")
  
  conteudo %<>% filter(linha > linha_inicio) %>% 
    select(conteudo) %>% 
    separate(conteudo, into = cabecalho, sep = ";") %>% 
    rename_all(tolower) %>% 
    rename_all(limpa_nome) %>% 
    select(-lixo) %>% 
    mutate(estacao = as.numeric(estacao) ) %>% 
    filter(!is.na(estacao))
  
  conteudo
  
}

plan(multiprocess)

tic()

conteudo_inmet_diario <- list.files("INMET_diario") %>%
  enframe() %>%
  mutate(arquivo = paste0("INMET_diario/", value)) %>%
  group_by(name, value) %>%
  nest() %>%
  mutate(data = future_map(data, le_um_arquivo, .progress = TRUE)) %>%
  unnest() %>% 
  select(-name, -value)

toc()

tic()

arquivo <- tibble(arquivo = "INMET_diario_complemento/temperaturas/Estação - 82533.txt")

conteudo_inmet_diario_comp_temperatura <- list.files("INMET_diario_complemento/temperaturas") %>%
  enframe() %>%
  mutate(arquivo = paste0("INMET_diario_complemento/temperaturas/", value)) %>%
  group_by(name, value) %>%
  nest() %>%
  mutate(data = future_map(data, le_um_arquivo, .progress = TRUE)) %>%
  unnest()

conteudo_inmet_diario_umidade_e_vento <- list.files("INMET_diario_complemento/umidade_e_vento") %>%
  enframe() %>%
  mutate(arquivo = paste0("INMET_diario_complemento/umidade_e_vento/", value)) %>%
  group_by(name, value) %>%
  nest() %>%
  mutate(data = future_map(data, le_um_arquivo, .progress = TRUE)) %>%
  unnest()

toc()


conteudo_inmet_diario_comp <- conteudo_inmet_diario_comp_temperatura %>% 
  full_join(
    conteudo_inmet_diario_umidade_e_vento, 
    by = c("estacao","data","hora")
  ) %>%
  select(-name.x, -value.x, - name.y, - value.y) %T>% 
  View()
              
dados_clima <- bind_rows(conteudo_inmet_diario,
                   conteudo_inmet_diario_comp
                   )  











