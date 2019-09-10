library(tidyverse)
library(magrittr)
library(furrr)
library(tictoc)
library(readxl)
library(lubridate )


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
  select(-name.x, -value.x, - name.y, - value.y) 
              
dados_clima <- bind_rows(conteudo_inmet_diario,
                   conteudo_inmet_diario_comp
                   ) %>% 
  mutate(
    tempmaxima = as.numeric(tempmaxima) ,
    tempminima = as.numeric(tempminima) ,
    temp_comp_media = as.numeric(temp_comp_media) ,
    umidade_relativa_media = as.numeric(umidade_relativa_media) ,
    velocidade_do_vento_media = as.numeric(velocidade_do_vento_media) 
  ) %>% 
  group_by(estacao, data) %>% 
  summarise(
    tempmaxima = mean(tempmaxima, na.rm = TRUE),
    tempminima = mean(tempminima, na.rm = TRUE),
    temp_comp_media = mean(temp_comp_media, na.rm = TRUE),
    umidade_relativa_media = mean(umidade_relativa_media, na.rm = TRUE),
    velocidade_do_vento_media = mean(velocidade_do_vento_media, na.rm = TRUE)
  ) %>% 
  ungroup() %>% 
  mutate(data = dmy(data) )


dados_carga <- read_excel(
  "carga_consumo/dados.xlsx", 
  sheet = "CargaDiaria") %>% 
  mutate(Data = date(Data))

dados_estacoes <- read_excel(
  "carga_consumo/dados.xlsx", 
  sheet = "Dim_estacoes")


tudo <- dados_clima %>% 
  left_join(dados_estacoes, by = c("estacao" = "Cod.Estacao" )) %>% 
  left_join(dados_carga, 
            by = c(
              "data" = "Data",
              "Subsistema"
              )
            )

saveRDS(tudo, "tudo.rds")


principais <- tudo %>% 
  filter(`Município` %in% c("RIO DE JANEIRO", "SÃO PAULO", "CURITIBA", "PORTO ALEGRE", "MANAUS")) %>% 
  saveRDS("principais.rds")
  










