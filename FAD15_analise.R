####################### -------------------------------------- ############## 
################## Criando a primeira análise de dados com o R ##############

### PACOTES

library(sidrar)
library(tidyverse)

### COLETA

ipca_raw <- sidrar::get_sidra(api = "/t/1737/n1/all/v/2265/p/all/d/v2265%202")

### LIMPEZA

dplyr::glimpse(ipca_raw)

ipca <- ipca_raw |> 
    dplyr::select("data" = "Mês (Código)",
                  "ipca" = "Valor") |> 
    dplyr::mutate(data = lubridate::ym(data)) |> 
    dplyr::filter(data >= "2004-01-01") |> 
    dplyr::as_tibble()

### Análise Exploratória

# Gráfico de linha

ggplot2::ggplot(ipca)+
  ggplot2::aes(x = data, y = ipca)+
  ggplot2::geom_line()

# Os cinco números

summary(ipca)

# Gráfico boxplot

ggplot2::ggplot(ipca)+
  ggplot2::aes(y = ipca)+
  ggplot2::geom_boxplot()
  
# Gráfico de histograma

ggplot2::ggplot(ipca)+
  ggplot2::aes(x = ipca)+
  ggplot2::geom_histogram()


#### Taxa de desocupação 

desocupacao_raw <- sidrar::get_sidra(api = "/t/6381/n1/all/v/4099/p/all/d/v4099%201")

# Limpeza da desocupação

desocupacao <- desocupacao_raw |> 
  dplyr::select('data' = "Trimestre Móvel (Código)", "desocupacao" = "Valor") |> 
  dplyr::mutate(data = lubridate::ym(data)) |> 
  dplyr::as_tibble()

# Junta os dados

df_dados <- ipca |> 
  inner_join(desocupacao, by = "data")

# Compara os duas variáveis

df_dados |> 
  ggplot2::ggplot()+
  ggplot2::aes(x = data)+
  ggplot2::geom_line(aes(y = desocupacao, color = "Taxa de desocupação"))+
  ggplot2::geom_line(aes(y = ipca, color = "IPCA"))+
  ggplot2::scale_color_manual(values = c("#282f6b", "#b22200"))

# IPCA em função do desemprego

modelo_phillips <- lm(ipca ~ desocupacao, data = df_dados)


summary(modelo_phillips)
















