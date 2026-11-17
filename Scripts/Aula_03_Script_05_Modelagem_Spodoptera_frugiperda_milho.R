# ============================================================
# MODELAGEM MILHO × Spodoptera frugiperda
# SCRIPT 01 — AMBIENTES DE PRODUÇÃO DO MILHO
# ============================================================

library(geobr)
library(sf)
library(dplyr)
library(ggplot2)


# 1. Diretórios -----------------------------------------------

dir_projeto <- "Aula_04"

dir_dados <- file.path(
  dir_projeto,
  "dados"
)

dir_dados_brutos <- file.path(
  dir_dados,
  "brutos"
)

dir_dados_processados <- file.path(
  dir_dados,
  "processados"
)

dir_resultados <- file.path(
  dir_projeto,
  "resultados"
)

dir_figuras <- file.path(
  dir_resultados,
  "figuras"
)

dir_tabelas <- file.path(
  dir_resultados,
  "tabelas"
)

diretorios <- c(
  dir_dados_brutos,
  dir_dados_processados,
  dir_figuras,
  dir_tabelas
)

invisible(
  lapply(
    diretorios,
    dir.create,
    recursive = TRUE,
    showWarnings = FALSE
  )
)


# 2. Limites estaduais do Brasil ------------------------------

estados <- geobr::read_state(
  year = 2020,
  showProgress = FALSE
)


# 3. Classificação didática dos ambientes ---------------------

estados <- estados |>
  mutate(
    ambiente = case_when(
      
      abbrev_state %in% c("PR", "SC", "RS") ~
        "Sul subtropical",
      
      abbrev_state %in% c(
        "MT", "MS", "GO", "DF",
        "MG", "SP"
      ) ~
        "Tropical/Cerrado",
      
      TRUE ~
        "Tropical úmido"
    )
  )


# 4. Mapa dos ambientes ---------------------------------------

mapa_ambientes <- ggplot() +
  
  geom_sf(
    data = estados,
    aes(fill = ambiente),
    color = "white",
    linewidth = 0.2
  ) +
  
  scale_fill_viridis_d(
    name = "Ambientes",
    option = "D"
  ) +
  
  labs(
    title = "Ambientes de produção do milho no Brasil",
    subtitle = "Regionalização simplificada para exercício didático",
    caption = paste(
      "Classificação exploratória por unidades federativas.",
      "Não representa zoneamento agrícola oficial."
    )
  ) +
  
  theme_minimal(base_size = 12) +
  
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "bottom"
  )


print(mapa_ambientes)


# 5. Exportação -----------------------------------------------

ggsave(
  file.path(
    dir_figuras,
    "01_ambientes_milho_brasil.png"
  ),
  mapa_ambientes,
  width = 9,
  height = 8,
  dpi = 300
)


# 6. Validação -------------------------------------------------

print(
  estados |>
    st_drop_geometry() |>
    count(ambiente)
)

stopifnot(
  nrow(estados) == 27,
  length(unique(estados$ambiente)) == 3,
  file.exists(
    file.path(
      dir_figuras,
      "01_ambientes_milho_brasil.png"
    )
  )
)

message("SCRIPT 01 — AULA 04 CONCLUÍDO.")



# ============================================================
# MODELAGEM MILHO × Spodoptera frugiperda
# SCRIPT 02 — OBTENÇÃO DAS OCORRÊNCIAS NO GBIF
# ============================================================

# 1. Pacotes --------------------------------------------------

library(rgbif)
library(dplyr)
library(readr)


# 2. Parâmetros da consulta -----------------------------------

# Chave taxonômica validada no Script 00.
# O GBIF utiliza essa chave para identificar a espécie.

taxon_key <- 5109855

# Recorte geográfico e temporal do exercício

pais <- "BR"

ano_inicial <- 2015
ano_final <- 2025

# Número máximo de registros por consulta.
# A API de ocorrências do GBIF permite até 300 registros
# por requisição.

limite <- 300


# 3. Consulta inicial -----------------------------------------

# Primeiro, identificamos quantos registros possuem
# coordenadas geográficas no recorte selecionado.

consulta_inicial <- occ_search(
  taxonKey = taxon_key,
  country = pais,
  year = paste(ano_inicial, ano_final, sep = ","),
  hasCoordinate = TRUE,
  limit = 0
)

total_registros <- consulta_inicial$meta$count

message(
  "Total de registros com coordenadas: ",
  total_registros
)


# 4. Obtenção dos registros -----------------------------------

# Como o GBIF retorna até 300 registros por consulta,
# precisamos dividir a obtenção em páginas.

inicios <- seq(
  from = 0,
  to = total_registros - 1,
  by = limite
)

lista_ocorrencias <- vector(
  mode = "list",
  length = length(inicios)
)

for (i in seq_along(inicios)) {
  
  message(
    "Obtendo página ",
    i,
    " de ",
    length(inicios)
  )
  
  resultado <- occ_search(
    taxonKey = taxon_key,
    country = pais,
    year = paste(ano_inicial, ano_final, sep = ","),
    hasCoordinate = TRUE,
    limit = limite,
    start = inicios[i]
  )
  
  lista_ocorrencias[[i]] <- resultado$data
}

# Reunimos todas as páginas em uma única tabela.

ocorrencias_brutas <- bind_rows(
  lista_ocorrencias
)


# 5. Conferência dos registros --------------------------------

# Verificamos se o número de registros obtidos corresponde
# ao total informado pela consulta inicial.

message(
  "Registros obtidos: ",
  nrow(ocorrencias_brutas)
)

stopifnot(
  nrow(ocorrencias_brutas) == total_registros
)


# 6. Exportação dos dados -------------------------------------

# Salvamos os dados originais em dois formatos:
# RDS: preserva a estrutura dos objetos do R.
# CSV: facilita a leitura e o compartilhamento.

saveRDS(
  ocorrencias_brutas,
  file.path(
    dir_dados_brutos,
    "gbif_spodoptera_bruto.rds"
  )
)

write_csv(
  ocorrencias_brutas,
  file.path(
    dir_dados_brutos,
    "gbif_spodoptera_bruto.csv"
  )
)


# 7. Resumo exploratório --------------------------------------

resumo_ocorrencias <- ocorrencias_brutas |>
  summarise(
    total = n(),
    com_latitude = sum(!is.na(decimalLatitude)),
    com_longitude = sum(!is.na(decimalLongitude)),
    primeiro_ano = min(year, na.rm = TRUE),
    ultimo_ano = max(year, na.rm = TRUE)
  )

print(resumo_ocorrencias)


# 8. Validação final ------------------------------------------

stopifnot(
  file.exists(
    file.path(
      dir_dados_brutos,
      "gbif_spodoptera_bruto.rds"
    )
  ),
  file.exists(
    file.path(
      dir_dados_brutos,
      "gbif_spodoptera_bruto.csv"
    )
  )
)

message("SCRIPT 02 — AULA 04 CONCLUÍDO.")



# ============================================================
# MODELAGEM MILHO × Spodoptera frugiperda
# SCRIPT 03 — AVALIAÇÃO DA QUALIDADE DAS COORDENADAS
# ============================================================

library(dplyr)
library(readr)
library(CoordinateCleaner)


# 1. Importação dos dados -------------------------------------

# Recuperamos os dados brutos salvos no Script 02.

ocorrencias_brutas <- readRDS(
  file.path(
    dir_dados_brutos,
    "gbif_spodoptera_bruto.rds"
  )
)


# 2. Preparação dos registros ---------------------------------

# Mantemos os registros originais e selecionamos os campos
# necessários para a avaliação geográfica.

ocorrencias_triagem <- ocorrencias_brutas |>
  filter(
    !is.na(decimalLongitude),
    !is.na(decimalLatitude)
  ) |>
  mutate(
    longitude = as.numeric(decimalLongitude),
    latitude = as.numeric(decimalLatitude)
  )


# 3. Testes de qualidade --------------------------------------

# CoordinateCleaner identifica padrões espaciais suspeitos,
# como coordenadas em zeros, capitais, centros de países,
# instituições e registros com coordenadas repetidas.

avaliacao_coordenadas <- CoordinateCleaner::clean_coordinates(
  x = ocorrencias_triagem,
  lon = "longitude",
  lat = "latitude",
  species = "scientificName",
  countries = "countryCode",
  tests = c(
    "capitals",
    "centroids",
    "equal",
    "gbif",
    "institutions",
    "zeros"
  ),
  value = "clean",
  verbose = TRUE
)



# 4. Resumo da avaliação --------------------------------------

# O objeto avaliacao_coordenadas contém os registros
# aprovados nos testes, pois utilizamos value = "clean".

total_original <- nrow(ocorrencias_triagem)

total_limpo <- nrow(avaliacao_coordenadas)

total_sinalizado <- total_original - total_limpo

resumo_testes <- tibble(
  registros_originais = total_original,
  registros_limpos = total_limpo,
  registros_sinalizados = total_sinalizado,
  percentual_sinalizado = round(
    100 * total_sinalizado / total_original,
    2
  )
)

print(resumo_testes)



# 5. Exportação -----------------------------------------------

saveRDS(
  avaliacao_coordenadas,
  file.path(
    dir_dados_processados,
    "spodoptera_avaliacao_coordenadas.rds"
  )
)

write_csv(
  avaliacao_coordenadas,
  file.path(
    dir_dados_processados,
    "spodoptera_avaliacao_coordenadas.csv"
  )
)


message("SCRIPT 03 — AVALIAÇÃO CONCLUÍDA.")



# ============================================================
# MODELAGEM MILHO × Spodoptera frugiperda
# SCRIPT 04 — DISTRIBUIÇÃO ESPACIAL DAS OCORRÊNCIAS
# ============================================================

library(sf)
library(ggplot2)
library(dplyr)
library(viridis)


# 1. Importação dos dados -------------------------------------

ocorrencias_limpa <- readRDS(
  file.path(
    dir_dados_processados,
    "spodoptera_avaliacao_coordenadas.rds"
  )
)


# 2. Conversão para objeto espacial ---------------------------

# Transformamos a tabela em um objeto espacial.
# EPSG:4326 representa coordenadas geográficas em WGS84.

ocorrencias_sf <- st_as_sf(
  ocorrencias_limpa,
  coords = c("longitude", "latitude"),
  crs = 4326,
  remove = FALSE
)


# 3. Limites estaduais ----------------------------------------

# Remover o arquivo de cache corrompido e baixar novamente

estados_brasil <- geobr::read_state(
  year = 2020,
  showProgress = TRUE,
  cache = FALSE
)

# Verificar se o objeto foi carregado

class(estados_brasil)


# 4. Mapa de distribuição -------------------------------------

mapa_ocorrencias <- ggplot() +
  
  geom_sf(
    data = estados_brasil,
    fill = "gray95",
    color = "gray65",
    linewidth = 0.2
  ) +
  
  geom_sf(
    data = ocorrencias_sf,
    color = "#D95F02",
    alpha = 0.65,
    size = 1
  ) +
  
  labs(
    title = "Ocorrências de Spodoptera frugiperda no Brasil",
    subtitle = "Registros GBIF entre 2015 e 2025",
    caption = paste(
      "Fonte: GBIF.",
      "Registros aprovados na triagem inicial do CoordinateCleaner."
    )
  ) +
  
  theme_minimal(base_size = 12) +
  
  theme(
    plot.title = element_text(face = "bold")
  )


print(mapa_ocorrencias)


# 5. Exportação -----------------------------------------------

ggsave(
  file.path(
    dir_figuras,
    "02_ocorrencias_spodoptera_brasil.png"
  ),
  mapa_ocorrencias,
  width = 10,
  height = 8,
  dpi = 300
)


# 6. Validação -------------------------------------------------

message(
  "Registros representados no mapa: ",
  nrow(ocorrencias_sf)
)

stopifnot(
  nrow(ocorrencias_sf) == 818,
  file.exists(
    file.path(
      dir_figuras,
      "02_ocorrencias_spodoptera_brasil.png"
    )
  )
)

message("SCRIPT 04 — AULA 04 CONCLUÍDO.")



#
# ============================================================
# MODELAGEM MILHO × Spodoptera frugiperda
# SCRIPT 05 — DOWNLOAD DAS VARIÁVEIS BIOCLIMÁTICAS CHELSA
# ============================================================

library(terra)
library(dplyr)
library(readr)


# 1. Diretório dos dados --------------------------------------

# Os arquivos originais serão armazenados separadamente
# dos dados processados.

dir_chelsa <- file.path(
  dir_dados_brutos,
  "chelsa"
)

dir.create(
  dir_chelsa,
  recursive = TRUE,
  showWarnings = FALSE
)


# 2. Parâmetros do CHELSA -------------------------------------

# Utilizaremos as 19 variáveis bioclimáticas (BIO1–BIO19),
# referentes ao período histórico 1981–2010.

url_base <- paste0(
  "https://os.zhdk.cloud.switch.ch/",
  "chelsav2/GLOBAL/climatologies/1981-2010/bio/"
)

variaveis_bio <- paste0(
  "bio",
  1:19
)


# 3. Construção dos nomes dos arquivos ------------------------

arquivos_chelsa <- paste0(
  "CHELSA_",
  variaveis_bio,
  "_1981-2010_V.2.1.tif"
)


# 4. Download das 19 variáveis -------------------------------

for (i in seq_along(arquivos_chelsa)) {
  
  arquivo_atual <- file.path(
    dir_chelsa,
    arquivos_chelsa[i]
  )
  
  url_atual <- paste0(
    url_base,
    arquivos_chelsa[i]
  )
  
  # Evita baixar novamente arquivos já existentes.
  
  if (!file.exists(arquivo_atual)) {
    
    message(
      "Baixando variável ",
      variaveis_bio[i],
      " de 19..."
    )
    
    download.file(
      url = url_atual,
      destfile = arquivo_atual,
      mode = "wb",
      method = "libcurl"
    )
    
  } else {
    
    message(
      variaveis_bio[i],
      " já existe. Download ignorado."
    )
    
  }
  
}


# 5. Verificação dos arquivos --------------------------------

# Conferimos se os 19 arquivos estão presentes
# no diretório de dados brutos.

arquivos_existentes <- file.exists(
  file.path(
    dir_chelsa,
    arquivos_chelsa
  )
)

resumo_download <- tibble(
  variavel = variaveis_bio,
  arquivo = arquivos_chelsa,
  baixado = arquivos_existentes
)

print(resumo_download)

message(
  "Arquivos encontrados: ",
  sum(arquivos_existentes),
  " de 19"
)


# 6. Validação final ------------------------------------------

stopifnot(
  all(arquivos_existentes)
)

message(
  "SCRIPT 05 CONCLUÍDO: ",
  "19 variáveis CHELSA disponíveis."
)


# ============================================================
# MODELAGEM MILHO × Spodoptera frugiperda
# SCRIPT 06 — RECORTE ESPACIAL DAS VARIÁVEIS CHELSA
# ============================================================

library(terra)
library(sf)
library(geobr)
library(dplyr)


# 1. Diretórios -----------------------------------------------

dir_chelsa_processado <- file.path(
  dir_dados_processados,
  "chelsa_brasil"
)

dir.create(
  dir_chelsa_processado,
  recursive = TRUE,
  showWarnings = FALSE
)


# 2. Limites territoriais do Brasil ---------------------------

brasil <- geobr::read_country(
  year = 2020,
  showProgress = FALSE
)

brasil <- st_transform(
  brasil,
  4326
)

# Conversão para objeto espacial do terra

brasil_vect <- terra::vect(brasil)


# 3. Identificação dos arquivos CHELSA ------------------------

arquivos_chelsa <- list.files(
  dir_chelsa,
  pattern = "\\.tif$",
  full.names = TRUE
)

stopifnot(length(arquivos_chelsa) == 19)


# 4. Recorte das 19 variáveis ---------------------------------

for (arquivo in arquivos_chelsa) {
  
  nome_arquivo <- basename(arquivo)
  
  message("Processando: ", nome_arquivo)
  
  # Leitura do raster global
  
  raster_global <- terra::rast(arquivo)
  
  # Ajuste do polígono ao CRS do raster
  
  brasil_crs <- terra::project(
    brasil_vect,
    terra::crs(raster_global)
  )
  
  # Recorte pela extensão e máscara pelo território
  
  raster_brasil <- terra::crop(
    raster_global,
    brasil_crs
  )
  
  raster_brasil <- terra::mask(
    raster_brasil,
    brasil_crs
  )
  
  # Exportação do raster recortado
  
  arquivo_saida <- file.path(
    dir_chelsa_processado,
    nome_arquivo
  )
  
  terra::writeRaster(
    raster_brasil,
    arquivo_saida,
    overwrite = TRUE
  )
  
}


# 5. Inventário dos rasters processados -----------------------

arquivos_processados <- list.files(
  dir_chelsa_processado,
  pattern = "\\.tif$",
  full.names = TRUE
)

inventario_chelsa <- tibble(
  arquivo = basename(arquivos_processados),
  tamanho_MB = round(
    file.info(arquivos_processados)$size / 1024^2,
    2
  )
)

print(inventario_chelsa)


# 6. Validação -------------------------------------------------

stopifnot(
  length(arquivos_processados) == 19
)

message(
  "SCRIPT 06 CONCLUÍDO: ",
  length(arquivos_processados),
  " variáveis recortadas para o Brasil."
)


# ============================================================
# MODELAGEM MILHO × Spodoptera frugiperda
# SCRIPT 07 — VALIDAÇÃO DAS VARIÁVEIS BIOCLIMÁTICAS
# ============================================================

library(terra)
library(dplyr)
library(tibble)


# 1. Identificação dos arquivos -------------------------------

arquivos_processados <- list.files(
  dir_chelsa_processado,
  pattern = "\\.tif$",
  full.names = TRUE
)

arquivos_processados <- sort(arquivos_processados)


# 2. Organização em um único objeto ----------------------------

# Cada raster representa uma variável bioclimática.
# O conjunto será organizado como um SpatRaster multicamadas.

bioclim_brasil <- terra::rast(
  arquivos_processados
)

names(bioclim_brasil) <- paste0(
  "BIO",
  1:19
)


# 3. Verificação da estrutura ---------------------------------

print(bioclim_brasil)

message(
  "Número de variáveis: ",
  terra::nlyr(bioclim_brasil)
)


# 4. Inventário espacial --------------------------------------

inventario_espacial <- tibble(
  
  variavel = names(bioclim_brasil),
  
  linhas = terra::nrow(bioclim_brasil),
  
  colunas = terra::ncol(bioclim_brasil),
  
  resolucao_x = terra::res(bioclim_brasil)[1],
  
  resolucao_y = terra::res(bioclim_brasil)[2],
  
  sistema_coordenadas = terra::crs(
    bioclim_brasil,
    proj = TRUE
  ),
  
  extensao = as.character(
    terra::ext(bioclim_brasil)
  )
)

print(
  inventario_espacial |>
    select(
      variavel,
      linhas,
      colunas,
      resolucao_x,
      resolucao_y
    )
)


# 5. Estatísticas das variáveis -------------------------------

estatisticas <- terra::global(
  bioclim_brasil,
  fun = c("min", "mean", "max"),
  na.rm = TRUE
)

print(estatisticas)


# 6. Validação final ------------------------------------------

stopifnot(
  terra::nlyr(bioclim_brasil) == 19,
  length(unique(terra::nrow(bioclim_brasil))) == 1,
  length(unique(terra::ncol(bioclim_brasil))) == 1
)

message(
  "SCRIPT 07 CONCLUÍDO: ",
  "19 variáveis organizadas e verificadas."
)


# Salvar conjunto bioclimático do Brasil

terra::writeRaster(
  bioclim_brasil,
  file.path(
    dir_chelsa_processado,
    "CHELSA_bioclim_19vars_brasil.tif"
  ),
  overwrite = TRUE
)

# Conferir arquivo

file.exists(
  file.path(
    dir_chelsa_processado,
    "CHELSA_bioclim_19vars_brasil.tif"
  )
)



# ============================================================
# MODELAGEM MILHO × Spodoptera frugiperda
# SCRIPT 08 — EXTRAÇÃO DAS VARIÁVEIS AMBIENTAIS
# ============================================================

library(terra)
library(sf)
library(dplyr)
library(readr)


# 1. Importação dos dados -------------------------------------

# Recuperamos as ocorrências aprovadas na triagem espacial.

ocorrencias_limpa <- readRDS(
  file.path(
    dir_dados_processados,
    "spodoptera_avaliacao_coordenadas.rds"
  )
)

# Recuperamos o conjunto das 19 variáveis CHELSA.

bioclim_brasil <- terra::rast(
  file.path(
    dir_chelsa_processado,
    "CHELSA_bioclim_19vars_brasil.tif"
  )
)


# 2. Conversão das ocorrências em pontos espaciais ------------

# As coordenadas das ocorrências estão em longitude e latitude,
# no sistema geográfico WGS84 (EPSG:4326).

ocorrencias_sf <- st_as_sf(
  ocorrencias_limpa,
  coords = c("longitude", "latitude"),
  crs = 4326,
  remove = FALSE
)

# Conversão para o formato espacial do pacote terra.

ocorrencias_vect <- terra::vect(
  ocorrencias_sf
)


# 3. Extração das variáveis climáticas ------------------------

# Para cada ponto, extraímos o valor de cada uma das
# 19 camadas do raster.

valores_ambientais <- terra::extract(
  bioclim_brasil,
  ocorrencias_vect
)

# Removemos a coluna ID criada automaticamente pelo terra.

valores_ambientais <- valores_ambientais |>
  select(-ID)


# 4. Integração das informações -------------------------------

# Reunimos os dados originais das ocorrências e os valores
# ambientais extraídos em uma única tabela.

base_modelagem <- bind_cols(
  ocorrencias_limpa,
  valores_ambientais
)


# 5. Verificação dos resultados -------------------------------

# Conferimos o número de registros e as variáveis extraídas.

message(
  "Registros na base: ",
  nrow(base_modelagem)
)

message(
  "Variáveis ambientais: ",
  ncol(valores_ambientais)
)

# Verificamos quantos registros apresentam valores ausentes.

resumo_extracao <- base_modelagem |>
  summarise(
    registros = n(),
    registros_completos = sum(
      complete.cases(
        across(all_of(names(bioclim_brasil)))
      )
    ),
    registros_com_valores_ausentes = sum(
      !complete.cases(
        across(all_of(names(bioclim_brasil)))
      )
    )
  )

print(resumo_extracao)


# 6. Exportação -----------------------------------------------

saveRDS(
  base_modelagem,
  file.path(
    dir_dados_processados,
    "spodoptera_base_ambiental.rds"
  )
)

write_csv(
  base_modelagem,
  file.path(
    dir_dados_processados,
    "spodoptera_base_ambiental.csv"
  )
)


# 7. Validação final ------------------------------------------

stopifnot(
  nrow(base_modelagem) == 818,
  ncol(valores_ambientais) == 19,
  file.exists(
    file.path(
      dir_dados_processados,
      "spodoptera_base_ambiental.csv"
    )
  )
)

message("SCRIPT 08 — EXTRAÇÃO AMBIENTAL CONCLUÍDA.")



# ============================================================
# MODELAGEM MILHO × Spodoptera frugiperda
# SCRIPT 09 — CORRELAÇÃO DAS VARIÁVEIS BIOCLIMÁTICAS
# ============================================================

library(terra)
library(dplyr)
library(tidyr)
library(ggplot2)
library(corrplot)
library(readr)


# 1. Importação da base ambiental -----------------------------

# Recuperamos a tabela que integra as ocorrências da espécie
# com os valores das 19 variáveis bioclimáticas.

base_modelagem <- readRDS(
  file.path(
    dir_dados_processados,
    "spodoptera_base_ambiental.rds"
  )
)


# 2. Seleção das variáveis BIO --------------------------------

# Selecionamos exclusivamente as 19 variáveis ambientais.
# Os demais campos da tabela não entram na correlação.

variaveis_bio <- base_modelagem |>
  select(
    all_of(paste0("BIO", 1:19))
  )

# Conferimos se todas as variáveis são numéricas.

stopifnot(
  all(
    vapply(variaveis_bio, is.numeric, logical(1))
  )
)


# 3. Matriz de correlação -------------------------------------

# Pearson avalia a intensidade e a direção da relação linear
# entre duas variáveis.

# Pairwise.complete.obs utiliza os registros disponíveis
# para cada par de variáveis, desconsiderando valores ausentes.

matriz_cor <- cor(
  variaveis_bio,
  method = "pearson",
  use = "pairwise.complete.obs"
)

print(round(matriz_cor, 2))


# 4. Correlograma ---------------------------------------------

# Representação gráfica da matriz de correlação.
# Cores quentes indicam correlações positivas.
# Cores frias indicam correlações negativas.
# A intensidade da cor representa a magnitude da correlação.

png(
  file.path(
    dir_figuras,
    "03_correlograma_bioclim_chelsa.png"
  ),
  width = 2400,
  height = 2200,
  res = 300
)

corrplot(
  matriz_cor,
  method = "color",
  type = "upper",
  order = "original",
  addCoef.col = "black",
  number.cex = 0.55,
  tl.col = "black",
  tl.srt = 45,
  col = colorRampPalette(
    c("#2166AC", "white", "#B2182B")
  )(200),
  diag = FALSE
)

dev.off()


# 5. Identificação dos pares altamente correlacionados -------

# Vamos utilizar |r| >= 0.70 como limite exploratório
# para identificar possíveis redundâncias entre preditores.

# Extraímos apenas o triângulo superior da matriz,
# evitando repetir os mesmos pares.

pares_correlacao <- as.data.frame(
  as.table(matriz_cor)
) |>
  rename(
    variavel_1 = Var1,
    variavel_2 = Var2,
    correlacao = Freq
  ) |>
  filter(
    as.character(variavel_1) <
      as.character(variavel_2)
  ) |>
  mutate(
    correlacao = round(correlacao, 3),
    correlacao_absoluta = abs(correlacao)
  ) |>
  filter(
    correlacao_absoluta >= 0.70
  ) |>
  arrange(
    desc(correlacao_absoluta)
  )

print(pares_correlacao)


# 6. Gráfico da distribuição das correlações ------------------

# Este gráfico mostra a distribuição dos coeficientes
# de correlação entre todos os pares de variáveis.

correlacoes <- as.data.frame(
  as.table(matriz_cor)
) |>
  rename(
    variavel_1 = Var1,
    variavel_2 = Var2,
    correlacao = Freq
  ) |>
  filter(
    as.character(variavel_1) <
      as.character(variavel_2)
  )

grafico_correlacoes <- ggplot(
  correlacoes,
  aes(x = correlacao)
) +
  
  geom_histogram(
    bins = 20,
    fill = "#3B8C75",
    color = "white"
  ) +
  
  geom_vline(
    xintercept = c(-0.70, 0.70),
    linetype = "dashed",
    color = "#B2182B"
  ) +
  
  labs(
    title = "Distribuição dos coeficientes de correlação",
    subtitle = "Variáveis bioclimáticas CHELSA — BIO1 a BIO19",
    x = "Correlação de Pearson (r)",
    y = "Número de pares de variáveis"
  ) +
  
  theme_minimal(base_size = 12)


print(grafico_correlacoes)

ggsave(
  file.path(
    dir_figuras,
    "04_distribuicao_correlacoes_bioclim.png"
  ),
  grafico_correlacoes,
  width = 9,
  height = 6,
  dpi = 300
)


# 7. Exportação dos resultados -------------------------------

write_csv(
  as.data.frame(matriz_cor) |>
    tibble::rownames_to_column("variavel"),
  file.path(
    dir_tabelas,
    "matriz_correlacao_bioclim.csv"
  )
)

write_csv(
  pares_correlacao,
  file.path(
    dir_tabelas,
    "pares_alta_correlacao.csv"
  )
)


# 8. Validação ------------------------------------------------

stopifnot(
  nrow(matriz_cor) == 19,
  ncol(matriz_cor) == 19,
  all(is.finite(matriz_cor)),
  file.exists(
    file.path(
      dir_figuras,
      "03_correlograma_bioclim_chelsa.png"
    )
  ),
  file.exists(
    file.path(
      dir_figuras,
      "04_distribuicao_correlacoes_bioclim.png"
    )
  )
)

message("SCRIPT 09 — CORRELAÇÃO CONCLUÍDA.")

# ============================================================
# SCRIPT 10 — AVALIAÇÃO DE MULTICOLINEARIDADE (VIF)
# ============================================================

library(tidyverse)
library(usdm)

# 1. Carregar base ambiental
base_modelagem <- readRDS(
  file.path(
    dir_dados_processados,
    "spodoptera_base_ambiental.rds"
  )
)

# 2. Selecionar variáveis BIOCLIM e garantir formato numérico
dados_vif <- base_modelagem %>%
  select(all_of(paste0("BIO", 1:19))) %>%
  drop_na() %>%
  as.data.frame()

# Garantir que todas as variáveis sejam numéricas
dados_vif[] <- lapply(dados_vif, as.numeric)

# Conferir estrutura
str(dados_vif)

# 3. Calcular VIF
resultado_vif <- usdm::vifstep(
  dados_vif,
  th = 5
)

# 4. Visualizar resultado
print(resultado_vif)


#Interpretação didática: as oito variáveis restantes apresentam VIF inferior a 5. A BIO13 ficou próxima do limite adotado (4,81), mas não foi excluída. Um cuidado: o VIF foi calculado sobre os valores ambientais extraídos nos locais de ocorrência da praga. Portanto, descreve a multicolinearidade nesse conjunto de pontos, não necessariamente em todo o território brasileiro. Registro para nossa comparação | Pearson: identifica pares de variáveis altamente correlacionadas. | VIF: identifica redundância considerando simultaneamente as demais variáveis. | Resultado VIF: oito variáveis retidas.


# ============================================================
# SCRIPT 11 — ANÁLISE DE COMPONENTES PRINCIPAIS (PCA)
# ============================================================

library(tidyverse)
library(factoextra)

# 1. Carregar base ambiental
base_modelagem <- readRDS(
  file.path(
    dir_dados_processados,
    "spodoptera_base_ambiental.rds"
  )
)

# 2. Selecionar as 19 variáveis BIOCLIM
dados_pca <- base_modelagem %>%
  select(all_of(paste0("BIO", 1:19))) %>%
  drop_na()

# 3. Executar PCA
# scale. = TRUE padroniza as variáveis.
# Isso é importante porque as variáveis BIOCLIM
# possuem unidades e amplitudes diferentes.

resultado_pca <- prcomp(
  dados_pca,
  center = TRUE,
  scale. = TRUE
)

# 4. Variância explicada pelos componentes
variancia_pca <- factoextra::get_eigenvalue(
  resultado_pca
)

print(variancia_pca)

# 5. Gráfico da variância explicada
grafico_variancia <- factoextra::fviz_eig(
  resultado_pca,
  addlabels = TRUE,
  ylim = c(0, 50)
)

print(grafico_variancia)

ggsave(
  file.path(
    dir_figuras,
    "05_variancia_explicada_pca.png"
  ),
  grafico_variancia,
  width = 9,
  height = 6,
  dpi = 300
)

# 6. Contribuição das variáveis aos dois primeiros componentes
grafico_variaveis <- factoextra::fviz_pca_var(
  resultado_pca,
  col.var = "contrib",
  repel = TRUE
)

print(grafico_variaveis)

ggsave(
  file.path(
    dir_figuras,
    "06_contribuicao_variaveis_pca.png"
  ),
  grafico_variaveis,
  width = 9,
  height = 7,
  dpi = 300
)

# 7. Exportar variância explicada
write_csv(
  variancia_pca,
  file.path(
    dir_tabelas,
    "06_variancia_explicada_pca.csv"
  )
)

# 8. Exportar contribuições das variáveis
contribuicoes <- as.data.frame(
  resultado_pca$rotation[, 1:2]
) %>%
  rownames_to_column("variavel")

write_csv(
  contribuicoes,
  file.path(
    dir_tabelas,
    "07_contribuicao_variaveis_pca.csv"
  )
)

cat("PCA concluída.\n")



#Análise de Componentes Principais (PCA) das variáveis BIOCLIM

#1. Objetivo da análise

#A Análise de Componentes Principais (PCA) foi utilizada para avaliar a estrutura de correlação entre as 19 variáveis bioclimáticas (BIOCLIM) extraídas nos locais de ocorrência de Spodoptera frugiperda no Brasil.

#O objetivo foi identificar os principais gradientes ambientais representados pelo conjunto de variáveis e verificar quanto da variabilidade total dos dados poderia ser sintetizada por um número menor de dimensões.

#Antes da análise, as variáveis foram centralizadas e padronizadas (center = TRUE e scale. = TRUE). Essa etapa é importante porque as variáveis BIOCLIM apresentam diferentes unidades e amplitudes de valores. A padronização permite que todas contribuam para a PCA em uma escala comparável.

#2. Variância explicada pelos componentes principais

#Os resultados indicaram a seguinte distribuição da variância: Componente Variância explicada e Variância acumulada

#O primeiro componente principal (PC1) explicou aproximadamente 57% da variabilidade total dos dados. Isso indica que uma parcela expressiva da informação ambiental está associada a um único gradiente.

#O segundo componente (PC2) explicou 13,35% adicionais. Em conjunto, PC1 e PC2 representaram 70,31% da variância total.

#Com a inclusão do terceiro componente (PC3), a variância acumulada alcançou 83,13%.

#Portanto, os três primeiros componentes sintetizam mais de 80% da informação contida nas 19 variáveis originais.

#Interpretação: a redução de 19 variáveis para três dimensões representa uma síntese expressiva dos dados ambientais. Entretanto, isso não significa que três variáveis originais sejam suficientes para representar todo o ambiente. Os componentes são combinações matemáticas das variáveis originais.

#3. Interpretação do gráfico de variância explicada (Scree plot)

#O Scree plot representa a proporção de variância explicada por cada componente principal.

#Observa-se uma queda acentuada entre o primeiro e o segundo componente, seguida por uma redução progressiva da contribuição dos componentes seguintes.

#Esse padrão evidencia a predominância do PC1 na estrutura dos dados. A partir do quarto componente, a contribuição individual torna-se relativamente pequena.

#Para este exercício, os três primeiros componentes constituem uma síntese adequada para explorar os principais gradientes ambientais, pois acumulam 83,13% da variância.

#4. Interpretação do círculo de correlações

#O círculo de correlações representa a associação das variáveis originais com os dois primeiros componentes principais.

#Cada seta corresponde a uma variável BIOCLIM. Sua direção e seu comprimento ajudam a interpretar a relação com os componentes:
  
 # Setas próximas e apontando para direções semelhantes indicam associação positiva entre variáveis.

#Setas em direções opostas indicam associação negativa.

#Setas aproximadamente perpendiculares indicam associação linear relativamente fraca.

#Setas mais próximas da circunferência representam variáveis mais bem representadas pelo plano formado por PC1 e PC2.

#Setas próximas ao centro indicam menor representação nesse plano.

#Dimensão 1 (PC1 — 56,96%)

#O primeiro componente apresenta uma separação marcante entre grupos de variáveis.

#No lado positivo do eixo estão principalmente BIO6, BIO9, BIO14 e BIO17, enquanto no lado negativo aparecem BIO1, BIO2, BIO3, BIO7, BIO13, BIO15, BIO16 e BIO19, entre outras.

#Essa configuração demonstra que o principal gradiente ambiental está associado a um conjunto de variáveis que apresentam padrões opostos de variação entre os locais amostrados.

#Dimensão 2 (PC2 — 13,35%)

#O segundo componente representa uma dimensão adicional de diferenciação ambiental.

#Variáveis como BIO5, BIO8, BIO7 e BIO13 apresentam associação positiva com esse eixo, enquanto BIO15, BIO2 e BIO11 apresentam associação negativa.

#Assim, PC2 acrescenta uma dimensão de variação que não é inteiramente representada pelo primeiro componente.

#É importante lembrar que o sinal positivo ou negativo dos componentes é arbitrário: o que importa é a posição relativa das variáveis e suas relações entre si.

#5. O que a PCA acrescenta à avaliação de multicolinearidade?
  
 # A PCA complementa os resultados obtidos anteriormente com a correlação de Pearson e o VIF.

#Enquanto Pearson identifica associações lineares entre pares de variáveis, e o VIF avalia a redundância de cada variável em relação ao conjunto das demais, a PCA sintetiza a estrutura multivariada dos dados.

#Neste exercício, os três procedimentos fornecem perspectivas complementares:
  
 # Pearson: identifica quais pares de variáveis apresentam correlações elevadas.

#VIF: identifica variáveis com elevada redundância linear em relação às demais.

#PCA: identifica os principais gradientes ambientais e a proporção de variância representada por cada componente.

#A PCA não seleciona diretamente as variáveis que devem permanecer no modelo. Seu resultado ajuda a compreender a estrutura ambiental e a avaliar se um conjunto relativamente pequeno de dimensões consegue representar boa parte da informação original.

#6. Síntese dos resultados

#A PCA demonstrou que as 19 variáveis BIOCLIM apresentam uma estrutura multivariada concentrada principalmente nos três primeiros componentes, responsáveis por 83,13% da variância total.

#O primeiro componente foi predominante, explicando 56,96% da variabilidade. Os dois primeiros componentes representaram 70,31%, permitindo visualizar os principais padrões de associação entre as variáveis.

#Esses resultados reforçam a existência de redundância na informação ambiental e justificam a comparação com os resultados de Pearson e VIF antes da seleção dos preditores para a modelagem.

#Conclusão didática: a PCA não responde quais variáveis devem ser escolhidas para o modelo de distribuição potencial de Spodoptera frugiperda. Ela mostra como as variáveis se organizam, quais compartilham padrões de variação e quanto da informação ambiental pode ser resumida em poucas dimensões. A decisão final de seleção deve considerar também a relevância ecológica das variáveis e o desempenho dos modelos.


# ============================================================
# SCRIPT 12 — MATRIZ COMPARATIVA PEARSON, VIF E PCA
# ============================================================

library(tidyverse)
library(readr)

# 1. Carregar base ambiental
base_modelagem <- readRDS(
  file.path(
    dir_dados_processados,
    "spodoptera_base_ambiental.rds"
  )
)

dados <- base_modelagem %>%
  select(all_of(paste0("BIO", 1:19))) %>%
  drop_na() %>%
  as.data.frame()

# 2. PEARSON
# Identificar pares com correlação absoluta >= 0.70

matriz_cor <- cor(dados, method = "pearson")

# Identificar posições dos pares na matriz
posicoes <- which(
  upper.tri(matriz_cor) & abs(matriz_cor) >= 0.70,
  arr.ind = TRUE
)

# Organizar os pares identificados
pares_pearson <- tibble(
  Var1 = rownames(matriz_cor)[posicoes[, 1]],
  Var2 = colnames(matriz_cor)[posicoes[, 2]],
  correlacao = matriz_cor[posicoes]
)

# Contabilizar quantas associações elevadas envolvem cada variável
resumo_pearson <- bind_rows(
  pares_pearson %>% count(Var1, name = "n") %>%
    rename(variavel = Var1),
  pares_pearson %>% count(Var2, name = "n") %>%
    rename(variavel = Var2)
) %>%
  group_by(variavel) %>%
  summarise(
    n_pares_pearson = sum(n),
    .groups = "drop"
  )

# 3. VIF
# Recuperar diretamente as variáveis mantidas no objeto S4

variaveis_mantidas <- resultado_vif@results$Variables

resumo_vif <- tibble(
  variavel = paste0("BIO", 1:19)
) %>%
  mutate(
    resultado_vif = if_else(
      variavel %in% variaveis_mantidas,
      "Mantida",
      "Excluída"
    )
  )

# 4. PCA
# Recuperar associações das variáveis com PC1 e PC2

resultado_pca <- read_csv(
  file.path(
    dir_tabelas,
    "07_contribuicao_variaveis_pca.csv"
  ),
  show_col_types = FALSE
) %>%
  rename(
    PC1_loading = PC1,
    PC2_loading = PC2
  )

# 5. Integrar os resultados

matriz_comparativa <- tibble(
  variavel = paste0("BIO", 1:19)
) %>%
  left_join(resumo_pearson, by = "variavel") %>%
  left_join(resumo_vif, by = "variavel") %>%
  left_join(resultado_pca, by = "variavel") %>%
  mutate(
    n_pares_pearson = replace_na(n_pares_pearson, 0),
    pearson = if_else(
      n_pares_pearson > 0,
      "Correlação elevada",
      "Sem pares acima do limite"
    )
  ) %>%
  arrange(variavel)

# 6. Visualizar matriz
print(matriz_comparativa, n = 19)

# 7. Exportar matriz
write_csv(
  matriz_comparativa,
  file.path(
    dir_tabelas,
    "08_matriz_comparativa_pearson_vif_pca.csv"
  )
)

cat("Matriz comparativa concluída.\n")



### Comparação entre Pearson, VIF e PCA

#A matriz comparativa demonstrou que os três procedimentos fornecem informações complementares para avaliar a redundância entre variáveis ambientais. A correlação de Pearson identificou 16 variáveis com pelo menos um par apresentando |r| ≥ 0,70. Entretanto, o VIF manteve apenas oito das 19 variáveis originais, demonstrando que a avaliação conjunta da multicolinearidade pode produzir resultados diferentes daqueles obtidos pela análise de correlações pareadas.

#Entre as variáveis mantidas pelo VIF, BIO4, BIO11 e BIO12 não apresentaram pares com correlação acima do limite estabelecido. Por outro lado, BIO13 e BIO19 foram mantidas mesmo apresentando sete pares de correlação elevada cada uma.A PCA complementou essa avaliação ao demonstrar que os três primeiros componentes principais explicaram 83,13% da variância total. Os dois primeiros componentes, utilizados na representação gráfica das variáveis, explicaram conjuntamente 70,31%.

#Os resultados evidenciam que a escolha de preditores não deve depender exclusivamente de um único procedimento estatístico. A correlação de Pearson identifica associações entre pares, o VIF avalia a redundância conjunta e a PCA sintetiza os principais gradientes ambientais. Assim, a seleção final deve considerar a concordância entre os métodos, a interpretação ecológica das variáveis e a finalidade da modelagem.


# ============================================================
# SCRIPT 13 — MODELO DE ADEQUABILIDADE AMBIENTAL
# Espécie: Spodoptera frugiperda
# Método: MaxEnt - pacote maxnet
# Área de estudo: Brasil
# Período das camadas ambientais: 1981-2010
# ============================================================


# 1. PACOTES --------------------------------------------------

library(sf)
library(terra)
library(tidyverse)
library(maxnet)
library(geobr)

# sf: manipulação de dados espaciais vetoriais.
# terra: processamento de dados raster e extração ambiental.
# tidyverse: organização e manipulação dos dados.
# maxnet: implementação do algoritmo MaxEnt em R.
# geobr: obtenção dos limites territoriais brasileiros.


# 2. DEFINIR AS VARIÁVEIS AMBIENTAIS --------------------------

# Selecionamos cinco variáveis após a avaliação de
# multicolinearidade por Pearson e VIF.

# A seleção busca representar diferentes dimensões climáticas,
# evitando incorporar preditores excessivamente redundantes.

variaveis_selecionadas <- c(
  "BIO4",   # Sazonalidade da temperatura
  "BIO5",   # Temperatura máxima do mês mais quente
  "BIO10",  # Temperatura média do trimestre mais quente
  "BIO11",  # Temperatura média do trimestre mais frio
  "BIO12"   # Precipitação anual
)


# 3. CARREGAR REGISTROS DE OCORRÊNCIA -------------------------

# Utilizamos os registros previamente avaliados pelo
# CoordinateCleaner no Script 03.

# Os registros representam locais conhecidos de ocorrência
# da espécie, não necessariamente todos os locais onde ela
# está presente no território brasileiro.

ocorrencias <- readRDS(
  file.path(
    dir_dados_processados,
    "spodoptera_avaliacao_coordenadas.rds"
  )
)


# 4. CARREGAR AS CAMADAS AMBIENTAIS ---------------------------

# Carregar o conjunto de 19 variáveis CHELSA já recortadas
# para o Brasil no Script 07.

bioclim_brasil <- terra::rast(
  file.path(
    dir_chelsa_processado,
    "CHELSA_bioclim_19vars_brasil.tif"
  )
)

# Selecionar somente as cinco variáveis definidas para o modelo.

preditores <- bioclim_brasil[[variaveis_selecionadas]]

# Conferir a seleção dos preditores.

print(names(preditores))

plot(preditores)
# 5. EXTRAIR VALORES AMBIENTAIS NAS OCORRÊNCIAS -------------

# Converter os registros de ocorrência em pontos espaciais.
# As coordenadas estão em longitude e latitude (EPSG:4326).

pontos <- terra::vect(
  ocorrencias,
  geom = c("decimalLongitude", "decimalLatitude"),
  crs = "EPSG:4326"
)

# Extrair os valores das cinco variáveis ambientais
# correspondentes a cada ponto de ocorrência.

valores_presenca <- terra::extract(
  preditores,
  pontos
) %>%
  select(-ID) %>%
  drop_na()

# A função drop_na() remove registros sem valores ambientais
# completos, evitando que dados ausentes interfiram no ajuste.


# 6. PREPARAR OS PONTOS DE BACKGROUND -------------------------

# O MaxEnt utiliza registros de presença e pontos de background.
#
# Os pontos de background representam condições ambientais
# disponíveis na área de estudo, utilizadas como referência
# para comparar os ambientes associados às ocorrências.
#
# Eles NÃO representam ausências confirmadas da espécie.
#
# Neste exercício, utilizaremos o território brasileiro como
# área de referência para a amostragem.

# Obter os limites territoriais do Brasil.

brasil <- geobr::read_country(
  year = 2020,
  showProgress = FALSE
) %>%
  sf::st_transform(4326)

brasil_vect <- terra::vect(brasil)

# Gerar 10.000 pontos aleatórios de background.
#
# A escolha de 10.000 pontos é uma decisão prática para este
# exercício: proporciona uma amostragem ampla das condições
# ambientais brasileiras, mantendo o processamento viável.
#
# Não existe um número universalmente ideal de pontos.
# A quantidade necessária depende da extensão da área,
# resolução ambiental e distribuição dos ambientes disponíveis.

set.seed(123)

background <- terra::spatSample(
  brasil_vect,
  size = 10000,
  method = "random"
)

# Extrair os valores ambientais nos pontos de background.

valores_background <- terra::extract(
  preditores,
  background
) %>%
  select(-ID) %>%
  drop_na()

# Conferir quantos pontos permaneceram após a extração.
# Alguns podem ser removidos por ausência de dados ambientais.

cat(
  "Pontos de background válidos:",
  nrow(valores_background),
  "\n"
)


# 7. ORGANIZAR OS DADOS PARA O MAXNET -------------------------

# Converter os dados extraídos para data.frame.

dados_presenca <- as.data.frame(valores_presenca)
dados_background <- as.data.frame(valores_background)

# Remover eventuais combinações ambientais duplicadas
# entre os registros de presença.

dados_presenca <- distinct(dados_presenca)

# Garantir que presença e background utilizem exatamente
# as mesmas variáveis e na mesma ordem.

dados_background <- dados_background[
  , variaveis_selecionadas
]

dados_presenca <- dados_presenca[
  , variaveis_selecionadas
]


# 8. CONSTRUIR A MATRIZ DE RESPOSTAS --------------------------

# O MaxEnt diferencia dois grupos:
#
# 1 = presença: valores ambientais dos locais conhecidos
#     de ocorrência da espécie.
#
# 0 = background: valores ambientais dos locais amostrados
#     como referência ambiental.
#
# Essa estrutura permite ao algoritmo identificar combinações
# ambientais associadas às ocorrências observadas.

dados_modelo <- bind_rows(
  dados_presenca,
  dados_background
)

presenca <- c(
  rep(1, nrow(dados_presenca)),
  rep(0, nrow(dados_background))
)

# Conferir a dimensão dos dados utilizados.

cat("Presenças:", nrow(dados_presenca), "\n")
cat("Background:", nrow(dados_background), "\n")


# 9. AJUSTAR O MODELO MAXNET ----------------------------------

# maxnet.formula define as classes de características
# utilizadas pelo algoritmo para representar as respostas
# ambientais da espécie.
#
# "lqh" corresponde a:
#
# l = Linear: relações lineares entre preditores e resposta.
#
# q = Quadratic: permite representar respostas curvas,
#     como relações unimodais.
#
# h = Hinge: permite respostas flexíveis, com mudanças
#     de inclinação ao longo do gradiente ambiental.
#
# A combinação dessas classes permite representar relações
# ambientais mais flexíveis do que apenas relações lineares.

classes <- maxnet::maxnet.formula(
  p = presenca,
  data = dados_modelo,
  classes = "lqh"
)

# Ajustar o modelo.
#
# regmult = 1 define o multiplicador de regularização.
#
# A regularização penaliza a complexidade do modelo,
# reduzindo o risco de ajuste excessivo aos dados utilizados.
#
# Valores maiores tendem a produzir modelos mais simples;
# valores menores permitem respostas mais complexas.
#
# Neste exercício, utilizamos 1 como configuração inicial,
# sem realizar otimização dos parâmetros.

modelo_maxnet <- maxnet::maxnet(
  p = presenca,
  data = dados_modelo,
  f = classes,
  regmult = 1
)


# 10. PROJETAR A ADEQUABILIDADE AMBIENTAL ---------------------

# Aplicar o modelo às camadas ambientais de todo o Brasil.
#
# O resultado é um raster contínuo, no qual cada célula
# recebe um valor estimado pelo modelo.

# type = "cloglog":
# Define a transformação utilizada para apresentar a saída.
#
# Os valores variam aproximadamente entre 0 e 1.
# Valores maiores indicam maior adequabilidade relativa
# segundo o modelo ajustado.
#
# Não devem ser interpretados diretamente como probabilidade
# real de ocorrência ou risco de infestação.

mapa_adequabilidade <- terra::predict(
  preditores,
  modelo_maxnet,
  type = "cloglog",
  na.rm = TRUE
)


# 11. EXPORTAR O RASTER ---------------------------------------

# Salvar o resultado para utilização em análises posteriores,
# elaboração de mapas e comparação entre regiões.

terra::writeRaster(
  mapa_adequabilidade,
  file.path(
    dir_dados_processados,
    "mapa_adequabilidade_spodoptera_maxnet.tif"
  ),
  overwrite = TRUE
)


# 12. ELABORAR O MAPA -----------------------------------------

# Representar espacialmente a adequabilidade ambiental
# estimada para Spodoptera frugiperda no Brasil.

png(
  file.path(
    dir_figuras,
    "07_adequabilidade_spodoptera_maxnet.png"
  ),
  width = 2400,
  height = 1800,
  res = 300
)

plot(
  mapa_adequabilidade,
  main = "Adequabilidade ambiental — Spodoptera frugiperda",
  col = hcl.colors(100, "YlOrRd", rev = FALSE)
)

# Adicionar os limites territoriais brasileiros para
# facilitar a interpretação espacial do resultado.

plot(
  terra::vect(brasil),
  add = TRUE,
  border = "gray30",
  lwd = 0.5
)

dev.off()


# 13. VALIDAÇÃO BÁSICA E RESUMO DO MODELO ---------------------

# Verificar quantos registros foram efetivamente utilizados
# e quais variáveis foram incorporadas ao modelo.

cat("\n============================================\n")
cat("RESUMO DO MODELO MAXNET\n")
cat("============================================\n")

cat(
  "Registros de presença:",
  nrow(dados_presenca),
  "\n"
)

cat(
  "Pontos de background:",
  nrow(dados_background),
  "\n"
)

cat(
  "Número de preditores:",
  length(variaveis_selecionadas),
  "\n"
)

cat(
  "Variáveis:",
  paste(variaveis_selecionadas, collapse = ", "),
  "\n"
)

# Estatísticas descritivas do raster de adequabilidade.
# Permitem verificar a amplitude e a distribuição geral
# dos valores previstos pelo modelo.

print(
  terra::global(
    mapa_adequabilidade,
    fun = c("min", "mean", "max"),
    na.rm = TRUE
  )
)

cat("\nModelo MaxNet concluído.\n")



# ============================================================
# MODELAGEM MILHO × Spodoptera frugiperda
# SCRIPT 14 — SEPARAÇÃO DOS DADOS DE TREINO E TESTE
# ============================================================

library(tidyverse)

# 1. VERIFICAR OS DADOS ----------------------------------------

# Recuperamos os objetos produzidos no Script 13.
# A separação será feita sobre os mesmos dados ambientais
# utilizados na construção do primeiro modelo exploratório.

stopifnot(
  exists("dados_presenca"),
  exists("dados_background"),
  exists("variaveis_selecionadas")
)

# Garantir que os conjuntos contenham os mesmos preditores.

dados_presenca <- dados_presenca %>%
  select(all_of(variaveis_selecionadas))

dados_background <- dados_background %>%
  select(all_of(variaveis_selecionadas))

# Conferir a estrutura dos dados.

cat("Presenças disponíveis:", nrow(dados_presenca), "\n")
cat("Background disponível:", nrow(dados_background), "\n")


# 2. DEFINIR A PROPORÇÃO DE TREINO E TESTE ---------------------

# Utilizaremos 70% dos dados para treinamento e 30% para teste.
#
# Treinamento: conjunto utilizado para ajustar o modelo.
# Teste: conjunto independente do treinamento, utilizado
# posteriormente para avaliar seu desempenho.
#
# A proporção 70/30 é uma escolha didática inicial,
# não uma regra universal para todos os modelos.

proporcao_treino <- 0.70

# O set.seed garante que a divisão aleatória possa ser
# reproduzida em futuras execuções do script.

set.seed(123)


# 3. SEPARAR AS PRESENÇAS --------------------------------------

# Sorteamos aleatoriamente 70% dos registros de presença.
#
# Os registros restantes serão reservados para avaliação.

indices_presenca_treino <- sample(
  seq_len(nrow(dados_presenca)),
  size = floor(proporcao_treino * nrow(dados_presenca))
)

presencas_treino <- dados_presenca[
  indices_presenca_treino,
]

presencas_teste <- dados_presenca[
  -indices_presenca_treino,
]


# 4. SEPARAR OS PONTOS DE BACKGROUND ---------------------------

# Aplicamos o mesmo procedimento aos pontos de background.
#
# É importante reservar também parte do background para teste:
# assim, o modelo não utilizará esses pontos durante o ajuste.

indices_background_treino <- sample(
  seq_len(nrow(dados_background)),
  size = floor(proporcao_treino * nrow(dados_background))
)

background_treino <- dados_background[
  indices_background_treino,
]

background_teste <- dados_background[
  -indices_background_treino,
]


# 5. CONSTRUIR OS CONJUNTOS FINAIS -----------------------------

# Reunimos presenças e background em cada conjunto.
#
# A resposta indica a classe de cada registro:
# 1 = presença conhecida
# 0 = ponto de background

dados_treino <- bind_rows(
  presencas_treino,
  background_treino
)

resposta_treino <- c(
  rep(1, nrow(presencas_treino)),
  rep(0, nrow(background_treino))
)

dados_teste <- bind_rows(
  presencas_teste,
  background_teste
)

resposta_teste <- c(
  rep(1, nrow(presencas_teste)),
  rep(0, nrow(background_teste))
)


# 6. RESUMO DA DIVISÃO -----------------------------------------

cat("\n============================================\n")
cat("RESUMO DA SEPARAÇÃO DOS DADOS\n")
cat("============================================\n")

cat("\nTREINAMENTO\n")
cat("Presenças:", nrow(presencas_treino), "\n")
cat("Background:", nrow(background_treino), "\n")
cat("Total:", nrow(dados_treino), "\n")

cat("\nTESTE\n")
cat("Presenças:", nrow(presencas_teste), "\n")
cat("Background:", nrow(background_teste), "\n")
cat("Total:", nrow(dados_teste), "\n")


# 7. EXPORTAR OS CONJUNTOS -------------------------------------

# Salvamos os dados em um único arquivo RDS.
# Isso permite recuperar os quatro conjuntos e os vetores
# de resposta sem precisar repetir a separação.

conjuntos_modelagem <- list(
  dados_treino = dados_treino,
  resposta_treino = resposta_treino,
  dados_teste = dados_teste,
  resposta_teste = resposta_teste,
  variaveis = variaveis_selecionadas
)

saveRDS(
  conjuntos_modelagem,
  file.path(
    dir_dados_processados,
    "conjuntos_treino_teste_maxnet.rds"
  )
)


# 8. VALIDAÇÃO -------------------------------------------------

# Verificamos se os conjuntos possuem as mesmas variáveis,
# se não há sobreposição de linhas e se as classes estão
# representadas nos dois conjuntos.

stopifnot(
  ncol(dados_treino) == length(variaveis_selecionadas),
  ncol(dados_teste) == length(variaveis_selecionadas),
  length(resposta_treino) == nrow(dados_treino),
  length(resposta_teste) == nrow(dados_teste),
  all(c(0, 1) %in% resposta_treino),
  all(c(0, 1) %in% resposta_teste),
  nrow(dados_treino) + nrow(dados_teste) ==
    nrow(dados_presenca) + nrow(dados_background)
)

cat("\nSCRIPT 14 CONCLUÍDO.\n")
cat("Conjuntos de treinamento e teste salvos com sucesso.\n")



# ============================================================
# MODELAGEM MILHO × Spodoptera frugiperda
# SCRIPT 15 — AJUSTE DO MAXNET E PREDIÇÃO
# ============================================================

library(tidyverse)
library(maxnet)

# 1. RECUPERAR OS CONJUNTOS DE MODELAGEM ----------------------

# Carregar os dados preparados no Script 14.
# Assim, mantemos exatamente a mesma divisão entre treino e teste.

conjuntos_modelagem <- readRDS(
  file.path(
    dir_dados_processados,
    "conjuntos_treino_teste_maxnet.rds"
  )
)

dados_treino <- conjuntos_modelagem$dados_treino
resposta_treino <- conjuntos_modelagem$resposta_treino

dados_teste <- conjuntos_modelagem$dados_teste
resposta_teste <- conjuntos_modelagem$resposta_teste

variaveis_selecionadas <- conjuntos_modelagem$variaveis


# 2. DEFINIR AS CLASSES DE CARACTERÍSTICAS --------------------

# Utilizamos as mesmas classes do modelo exploratório:
# l = linear; q = quadrática; h = hinge.
#
# Mantemos essa configuração para que a comparação entre
# o modelo exploratório e o modelo avaliado seja consistente.

classes_maxnet <- maxnet::maxnet.formula(
  p = resposta_treino,
  data = dados_treino,
  classes = "lqh"
)


# 3. AJUSTAR O MODELO COM OS DADOS DE TREINAMENTO -------------

# Somente os dados de treinamento participam do ajuste.
#
# regmult = 1 mantém a configuração inicial de regularização.
# A penalização controla a complexidade das respostas ambientais,
# reduzindo o risco de sobreajuste.

modelo_maxnet_treino <- maxnet::maxnet(
  p = resposta_treino,
  data = dados_treino,
  f = classes_maxnet,
  regmult = 1
)

# Salvar o modelo ajustado para reutilização posterior.

saveRDS(
  modelo_maxnet_treino,
  file.path(
    dir_dados_processados,
    "modelo_maxnet_treino.rds"
  )
)


# 4. PREDIZER OS DADOS DE TESTE -------------------------------

# Aplicar o modelo aos registros que não participaram
# do treinamento.
#
# A saída cloglog fornece valores contínuos de adequabilidade
# estimada pelo modelo.

predicoes_teste <- predict(
  modelo_maxnet_treino,
  newdata = dados_teste,
  type = "cloglog"
)

# Organizar resultados observados e preditos.
#
# Observado: 1 = presença; 0 = background.
# Predito: valor estimado pelo modelo.

resultado_teste <- tibble(
  observado = resposta_teste,
  adequabilidade_predita = as.numeric(predicoes_teste)
)

# Conferir as primeiras observações.

print(head(resultado_teste))


# 5. VERIFICAR AS PREDIÇÕES -----------------------------------

# Verificar se todos os registros receberam uma predição
# e se os valores estão dentro do intervalo esperado.

stopifnot(
  length(predicoes_teste) == nrow(dados_teste),
  all(is.finite(predicoes_teste)),
  all(predicoes_teste >= 0 & predicoes_teste <= 1)
)

# Resumo das predições por classe observada.
# Esta etapa é apenas descritiva; ainda não representa
# uma avaliação formal do desempenho do modelo.

resumo_teste <- resultado_teste %>%
  group_by(observado) %>%
  summarise(
    registros = n(),
    media_predita = mean(adequabilidade_predita),
    mediana_predita = median(adequabilidade_predita),
    minimo = min(adequabilidade_predita),
    maximo = max(adequabilidade_predita),
    .groups = "drop"
  )

print(resumo_teste)


# 6. EXPORTAR AS PREDIÇÕES ------------------------------------

write_csv(
  resultado_teste,
  file.path(
    dir_tabelas,
    "09_predicoes_dados_teste.csv"
  )
)

write_csv(
  resumo_teste,
  file.path(
    dir_tabelas,
    "10_resumo_predicoes_teste.csv"
  )
)


# 7. RESUMO DA EXECUÇÃO ---------------------------------------

cat("\n============================================\n")
cat("AJUSTE DO MAXNET — TREINAMENTO E TESTE\n")
cat("============================================\n")

cat("Registros de treinamento:", nrow(dados_treino), "\n")
cat("Registros de teste:", nrow(dados_teste), "\n")

cat(
  "Variáveis utilizadas:",
  paste(variaveis_selecionadas, collapse = ", "),
  "\n"
)

cat("Classes de características: lqh\n")
cat("Multiplicador de regularização: 1\n")

cat("\nPredições dos dados de teste concluídas.\n")
cat("Modelo e resultados salvos.\n")



#No Script 15, calculamos a média e a mediana da adequabilidade para presenças e background. Essa comparação foi útil como uma primeira avaliação exploratória: as presenças apresentaram adequabilidade média de 0,665, enquanto o background apresentou média de 0,233. Isso sugere que o modelo atribuiu valores mais elevados aos locais de ocorrência conhecidos.

#Entretanto, essas médias não são métricas formais de desempenho. Elas não informam, por si só, o quanto o modelo consegue discriminar os dois grupos, nem quantos registros são corretamente classificados sob determinado limiar. Além disso, existe sobreposição entre os valores de adequabilidade dos grupos.

#Por isso, neste script utilizaremos três métricas frequentemente empregadas em Modelagem de Nicho Ecológico (ENM):
 # 1. AUC (Area Under the ROC Curve): avalia a capacidade de discriminação do modelo entre presenças e background, considerando diferentes limiares. É amplamente utilizada em ENM porque permite avaliar a capacidade discriminatória sem definir previamente um único ponto de corte. Em dados de presença-background, deve ser interpretada como discriminação entre presenças e os pontos de background utilizados, não como uma medida absoluta da distribuição real da espécie.
  #2. Sensibilidade: representa a proporção das presenças corretamente identificadas pelo modelo. É importante porque indica quanto das ocorrências conhecidas é reconhecido como adequado.
  #3. Especificidade: representa a proporção dos pontos de background classificados corretamente como não presença. Complementa a sensibilidade, permitindo avaliar o comportamento do modelo em relação aos dois grupos.

#Sensibilidade e especificidade dependem de um limiar de classificação. Para este exercício, utilizaremos o limiar de 0,5, apenas como referência didática. Esse valor não representa necessariamente o melhor limiar ecológico ou estatístico para o modelo.

# ============================================================
# SCRIPT 16 — AVALIAÇÃO DO DESEMPENHO DO MODELO MAXNET
# ============================================================

library(tidyverse)
library(pROC)

# ------------------------------------------------------------
# 1. ORGANIZAR OS DADOS DE TESTE
# ------------------------------------------------------------

# Utilizamos as predições produzidas no Script 15.
# Não utilizamos os dados de treinamento nesta avaliação.

resultado_teste <- read_csv(
  file.path(
    dir_tabelas,
    "09_predicoes_dados_teste.csv"
  ),
  show_col_types = FALSE
)

# ------------------------------------------------------------
# 2. AUC — AREA UNDER THE ROC CURVE
# ------------------------------------------------------------

# A AUC avalia a capacidade de discriminação do modelo
# entre os registros de presença e os pontos de background.
#
# Valores próximos de 1 indicam maior capacidade de
# discriminação; valores próximos de 0.5 indicam desempenho
# semelhante ao esperado ao acaso.

curva_roc <- pROC::roc(
  response = factor(
    resultado_teste$observado,
    levels = c(0, 1)
  ),
  predictor = resultado_teste$adequabilidade_predita,
  levels = c("0", "1"),
  direction = "<",
  quiet = TRUE
)

valor_auc <- as.numeric(
  pROC::auc(curva_roc)
)

# Gráfico da curva ROC

png(
  file.path(
    dir_figuras,
    "11_curva_roc_maxnet.png"
  ),
  width = 2000,
  height = 1600,
  res = 250
)

plot(
  curva_roc,
  main = paste("Curva ROC — MaxNet | AUC =", round(valor_auc, 3)),
  col = "darkblue",
  lwd = 3
)

abline(
  a = 0,
  b = 1,
  lty = 2,
  col = "gray50"
)

dev.off()

# ------------------------------------------------------------
# 3. SENSIBILIDADE E ESPECIFICIDADE
# ------------------------------------------------------------

# Para esta atividade, adotaremos 0.5 como limiar didático.
#
# Adequabilidade >= 0.5: classificado como presença.
# Adequabilidade < 0.5: classificado como background.

limiar <- 0.5

resultado_teste <- resultado_teste %>%
  mutate(
    classe_predita = if_else(
      adequabilidade_predita >= limiar,
      1,
      0
    )
  )

# Matriz de confusão

matriz_confusao <- table(
  Observado = resultado_teste$observado,
  Predito = resultado_teste$classe_predita
)

print(matriz_confusao)

# Extrair os valores da matriz

verdadeiros_positivos <- matriz_confusao["1", "1"]
falsos_negativos <- matriz_confusao["1", "0"]

verdadeiros_negativos <- matriz_confusao["0", "0"]
falsos_positivos <- matriz_confusao["0", "1"]

# Sensibilidade: proporção de presenças corretamente identificadas

sensibilidade <- verdadeiros_positivos /
  (verdadeiros_positivos + falsos_negativos)

# Especificidade: proporção de backgrounds corretamente identificados

especificidade <- verdadeiros_negativos /
  (verdadeiros_negativos + falsos_positivos)

# ------------------------------------------------------------
# 4. TSS — TRUE SKILL STATISTIC
# ------------------------------------------------------------

# O TSS combina sensibilidade e especificidade.
# Seu intervalo teórico vai de -1 a 1.
#
# Valores próximos de 1 indicam melhor desempenho.
# Valores próximos de 0 indicam desempenho semelhante ao acaso.
# Valores negativos indicam desempenho inferior ao acaso.

tss <- sensibilidade + especificidade - 1

# ------------------------------------------------------------
# 5. ORGANIZAR E EXPORTAR AS MÉTRICAS
# ------------------------------------------------------------

metricas_desempenho <- tibble(
  metrica = c(
    "AUC",
    "Sensibilidade",
    "Especificidade",
    "TSS"
  ),
  valor = c(
    valor_auc,
    sensibilidade,
    especificidade,
    tss
  ),
  limiar = c(
    NA,
    limiar,
    limiar,
    limiar
  )
)

print(metricas_desempenho)

write_csv(
  metricas_desempenho,
  file.path(
    dir_tabelas,
    "11_metricas_desempenho_maxnet.csv"
  )
)

# ------------------------------------------------------------
# 6. VALIDAÇÃO FINAL
# ------------------------------------------------------------

stopifnot(
  is.finite(valor_auc),
  sensibilidade >= 0 & sensibilidade <= 1,
  especificidade >= 0 & especificidade <= 1,
  tss >= -1 & tss <= 1
)

cat("\n============================================\n")
cat("AVALIAÇÃO DO MODELO MAXNET CONCLUÍDA\n")
cat("============================================\n")

cat("AUC:", round(valor_auc, 3), "\n")
cat("Sensibilidade:", round(sensibilidade, 3), "\n")
cat("Especificidade:", round(especificidade, 3), "\n")
cat("TSS:", round(tss, 3), "\n")

cat("\nLimiar utilizado:", limiar, "\n")
cat("Métricas e curva ROC exportadas.\n")

#Sensibilidade = 0,808: o modelo identificou corretamente aproximadamente 80,8% das presenças do conjunto de teste.

#Especificidade = 0,814: aproximadamente 81,4% dos pontos de background foram classificados abaixo do limiar de 0,5.

#TSS = 0,622: indica uma capacidade discriminatória relevante, combinando sensibilidade e especificidade. É uma medida complementar interessante porque considera simultaneamente os acertos nos dois grupos.

#AUC = 0,869: indica boa capacidade de discriminação entre os registros de presença e os pontos de background. Em termos intuitivos, há aproximadamente 86,9% de probabilidade de um registro de presença escolhido aleatoriamente receber uma pontuação de adequabilidade superior à de um ponto de background escolhido aleatoriamente.



# ============================================================
# SCRIPT 17 — DEFINIÇÃO DO LIMIAR MAXIMIZANDO O TSS
# ============================================================

library(tidyverse)
library(pROC)

# ------------------------------------------------------------
# 1. RECUPERAR AS PREDIÇÕES DO CONJUNTO DE TESTE
# ------------------------------------------------------------

resultado_teste <- read_csv(
  file.path(
    dir_tabelas,
    "09_predicoes_dados_teste.csv"
  ),
  show_col_types = FALSE
)

# ------------------------------------------------------------
# 2. CONSTRUIR A CURVA ROC
# ------------------------------------------------------------

curva_roc <- pROC::roc(
  response = factor(
    resultado_teste$observado,
    levels = c(0, 1)
  ),
  predictor = resultado_teste$adequabilidade_predita,
  levels = c("0", "1"),
  direction = "<",
  quiet = TRUE
)

# ------------------------------------------------------------
# 3. IDENTIFICAR O LIMIAR QUE MAXIMIZA O TSS
# ------------------------------------------------------------

# O método Youden identifica o limiar que maximiza:
# Sensibilidade + Especificidade - 1.
#
# Portanto, corresponde ao limiar que maximiza o TSS.

melhor_limiar <- pROC::coords(
  curva_roc,
  x = "best",
  best.method = "youden",
  ret = c(
    "threshold",
    "sensitivity",
    "specificity"
  ),
  transpose = FALSE
)

# Calcular o TSS correspondente

melhor_limiar <- as_tibble(melhor_limiar) %>%
  mutate(
    TSS = sensitivity + specificity - 1
  )

print(melhor_limiar)

# ------------------------------------------------------------
# 4. SALVAR O LIMIAR SELECIONADO
# ------------------------------------------------------------

# Caso existam vários limiares com o mesmo TSS máximo,
# utilizaremos o primeiro retornado pelo procedimento.
#
# Essa escolha será registrada para manter a reprodutibilidade.

limiar_tss <- melhor_limiar$threshold[1]

write_csv(
  melhor_limiar,
  file.path(
    dir_tabelas,
    "12_limiar_maximizando_tss.csv"
  )
)

saveRDS(
  limiar_tss,
  file.path(
    dir_dados_processados,
    "limiar_maximizando_tss.rds"
  )
)

# ------------------------------------------------------------
# 5. VALIDAÇÃO
# ------------------------------------------------------------

stopifnot(
  length(limiar_tss) == 1,
  is.finite(limiar_tss),
  limiar_tss >= 0,
  limiar_tss <= 1
)

cat("\n============================================\n")
cat("LIMIAR MAXIMIZANDO TSS\n")
cat("============================================\n")

cat("Limiar selecionado:", round(limiar_tss, 4), "\n")
cat(
  "Sensibilidade:",
  round(melhor_limiar$sensitivity[1], 3),
  "\n"
)
cat(
  "Especificidade:",
  round(melhor_limiar$specificity[1], 3),
  "\n"
)
cat(
  "TSS máximo:",
  round(melhor_limiar$TSS[1], 3),
  "\n"
)

cat("\nLimiar salvo para utilização na projeção espacial.\n")


#O resultado é coerente com o obtido anteriormente usando o limiar 0,5:
# - Limiar 0,5: TSS = 0,622.
# - Limiar 0,4883: TSS = 0,633.
#A diferença é pequena, mas o segundo limiar proporciona um equilíbrio ligeiramente melhor entre sensibilidade e especificidade para esse conjunto de teste.
#2. Interpretação
#O limiar de 0,4883 significa que os pixels com adequabilidade prevista igual ou superior a esse valor serão classificados como ambientalmente adequados, enquanto aqueles abaixo serão classificados como ambientalmente não adequados, segundo o critério adotado.

#Não significa que exista uma probabilidade de ocorrência de 48,83%.


# ============================================================
# SCRIPT 18 — PROJEÇÃO ESPACIAL CONTÍNUA DO MAXNET
# Spodoptera frugiperda — Brasil
# ============================================================

library(tidyverse)
library(terra)
library(maxnet)

# ------------------------------------------------------------
# 1. RECUPERAR O MODELO MAXNET TREINADO
# ------------------------------------------------------------

modelo_maxnet_treino <- readRDS(
  file.path(
    dir_dados_processados,
    "modelo_maxnet_treino.rds"
  )
)

# ------------------------------------------------------------
# 2. CARREGAR AS VARIÁVEIS AMBIENTAIS SELECIONADAS
# ------------------------------------------------------------

# Utilizaremos as mesmas cinco variáveis empregadas
# durante o treinamento do modelo.

variaveis_selecionadas <- c(
  "BIO4",
  "BIO5",
  "BIO10",
  "BIO11",
  "BIO12"
)

# Os arquivos são organizados explicitamente para garantir
# que cada raster corresponda à variável correta.

arquivos_preditores <- file.path(
  dir_chelsa_processado,
  paste0(
    "CHELSA_bio",
    c(4, 5, 10, 11, 12),
    "_1981-2010_V.2.1.tif"
  )
)

stopifnot(
  all(file.exists(arquivos_preditores))
)

preditores_brasil <- terra::rast(
  arquivos_preditores
)

names(preditores_brasil) <- variaveis_selecionadas

# Conferir as camadas carregadas

print(preditores_brasil)

# ------------------------------------------------------------
# 3. VALIDAR OS PREDITORES
# ------------------------------------------------------------

stopifnot(
  nlyr(preditores_brasil) == 5,
  identical(
    names(preditores_brasil),
    variaveis_selecionadas
  )
)

# ------------------------------------------------------------
# 4. PROJETAR A ADEQUABILIDADE AMBIENTAL
# ------------------------------------------------------------

# Aplicamos o modelo treinado a cada pixel do raster.
#
# type = "cloglog" mantém a mesma transformação utilizada
# na projeção exploratória anterior.
#
# O resultado representa a adequabilidade ambiental estimada.
### Pode demorar um pouco (5 min)
adequabilidade_spodoptera <- terra::predict(
  preditores_brasil,
  modelo_maxnet_treino,
  type = "cloglog",
  na.rm = TRUE
)

names(adequabilidade_spodoptera) <- "Adequabilidade"

# ------------------------------------------------------------
# 5. SALVAR O RASTER CONTÍNUO
# ------------------------------------------------------------

arquivo_raster <- file.path(
  dir_dados_processados,
  "adequabilidade_continua_spodoptera_maxnet.tif"
)

terra::writeRaster(
  adequabilidade_spodoptera,
  arquivo_raster,
  overwrite = TRUE
)

# ------------------------------------------------------------
# 6. CALCULAR ESTATÍSTICAS DESCRITIVAS
# ------------------------------------------------------------

estatisticas_adequabilidade <- terra::global(
  adequabilidade_spodoptera,
  fun = c("min", "mean", "max"),
  na.rm = TRUE
)

print(estatisticas_adequabilidade)

# ------------------------------------------------------------
# 7. GERAR O MAPA CONTÍNUO
# ------------------------------------------------------------

png(
  file.path(
    dir_figuras,
    "12_adequabilidade_continua_spodoptera.png"
  ),
  width = 2400,
  height = 2000,
  res = 300
)

terra::plot(
  adequabilidade_spodoptera,
  main = "Adequabilidade ambiental — Spodoptera frugiperda",
  col = hcl.colors(100, palette = "viridis"),
  axes = TRUE
)

dev.off()

# ------------------------------------------------------------
# 8. VALIDAÇÃO FINAL
# ------------------------------------------------------------

stopifnot(
  file.exists(arquivo_raster),
  nlyr(adequabilidade_spodoptera) == 1
)

cat("\n============================================\n")
cat("PROJEÇÃO ESPACIAL CONCLUÍDA\n")
cat("============================================\n")

cat("Variáveis utilizadas:",
    paste(variaveis_selecionadas, collapse = ", "), "\n")

cat("Modelo: MaxNet treinado\n")
cat("Transformação: cloglog\n")
cat("Raster contínuo salvo.\n")
cat("Mapa exportado.\n")


# ============================================================
# SCRIPT 19 — MAPA BINÁRIO DE ADEQUABILIDADE AMBIENTAL
# Spodoptera frugiperda — Brasil
# ============================================================

library(tidyverse)
library(terra)

# ------------------------------------------------------------
# 1. CARREGAR O RASTER CONTÍNUO DO SCRIPT 18
# ------------------------------------------------------------

adequabilidade_spodoptera <- terra::rast(
  file.path(
    dir_dados_processados,
    "adequabilidade_continua_spodoptera_maxnet.tif"
  )
)

# ------------------------------------------------------------
# 2. RECUPERAR O LIMIAR MAXIMIZANDO TSS
# ------------------------------------------------------------

limiar_tss <- readRDS(
  file.path(
    dir_dados_processados,
    "limiar_maximizando_tss.rds"
  )
)

cat("Limiar utilizado:", round(limiar_tss, 4), "\n")

# ------------------------------------------------------------
# 3. CONVERTER O RASTER CONTÍNUO EM BINÁRIO
# ------------------------------------------------------------

# Valores abaixo do limiar = classe 0.
# Valores iguais ou superiores ao limiar = classe 1.
#
# A classificação representa adequabilidade ambiental
# segundo o limiar selecionado, não presença ou ausência
# confirmada da espécie.

mapa_binario <- terra::ifel(
  adequabilidade_spodoptera >= limiar_tss,
  1,
  0
)

names(mapa_binario) <- "Adequabilidade_binaria"

# ------------------------------------------------------------
# 4. EXPORTAR O RASTER BINÁRIO
# ------------------------------------------------------------

arquivo_binario <- file.path(
  dir_dados_processados,
  "adequabilidade_binaria_spodoptera_maxnet.tif"
)

terra::writeRaster(
  mapa_binario,
  arquivo_binario,
  overwrite = TRUE
)

# ------------------------------------------------------------
# 5. CALCULAR A DISTRIBUIÇÃO DAS CLASSES
# ------------------------------------------------------------

frequencia_classes <- terra::freq(
  mapa_binario,
  digits = 0,
  value = TRUE
)

print(frequencia_classes)

# Calcular a proporção de cada classe

total_pixels <- sum(frequencia_classes$count)

resumo_classes <- as_tibble(frequencia_classes) %>%
  mutate(
    classe = if_else(
      value == 1,
      "Adequabilidade igual ou superior ao limiar",
      "Adequabilidade abaixo do limiar"
    ),
    percentual = 100 * count / total_pixels
  )

print(resumo_classes)

# Exportar resumo

write_csv(
  resumo_classes,
  file.path(
    dir_tabelas,
    "13_resumo_classes_adequabilidade_binaria.csv"
  )
)

# ------------------------------------------------------------
# 6. ELABORAR O MAPA BINÁRIO
# ------------------------------------------------------------

png(
  file.path(
    dir_figuras,
    "13_adequabilidade_binaria_spodoptera.png"
  ),
  width = 1800,
  height = 1600,
  res = 300,
  pointsize = 10
)

# Ajustar as margens para acomodar mapa, título e legenda.
# A margem direita maior reserva espaço para a legenda.

par(
  mar = c(4, 4, 5, 7),
  cex.main = 1,
  cex.axis = 0.85
)

terra::plot(
  mapa_binario,
  main = paste0(
    "Adequabilidade ambiental — Spodoptera frugiperda\n",
    "Limiar maximizing TSS = ",
    round(limiar_tss, 3)
  ),
  col = viridisLite::magma(2),
  axes = TRUE,
  plg = list(
    title = "Adequabilidade",
    cex = 0.85
  )
)

dev.off()

# ------------------------------------------------------------
# 7. VALIDAÇÃO FINAL
# ------------------------------------------------------------

stopifnot(
  file.exists(arquivo_binario),
  terra::nlyr(mapa_binario) == 1,
  all(na.omit(terra::values(mapa_binario)) %in% c(0, 1))
)

cat("\n============================================\n")
cat("MAPA BINÁRIO CONCLUÍDO\n")
cat("============================================\n")

cat("Limiar maximizing TSS:", round(limiar_tss, 4), "\n")
cat("Classes geradas: 0 e 1\n")
cat("Raster binário exportado.\n")
cat("Mapa e tabela de frequências exportados.\n")


# ============================================================
# DIAGNÓSTICO DA ÁREA DO RASTER BINÁRIO
# ============================================================

# 1. Verificar frequência das classes

print(
  terra::freq(
    mapa_binario,
    value = TRUE
  )
)

# 2. Verificar quantidade de células válidas e NA

total_celulas <- terra::ncell(mapa_binario)

celulas_validas <- terra::global(
  !is.na(mapa_binario),
  "sum",
  na.rm = TRUE
)[1, 1]

cat("Total de células:", total_celulas, "\n")
cat("Células válidas:", celulas_validas, "\n")
cat(
  "Percentual de células válidas:",
  round(100 * celulas_validas / total_celulas, 2),
  "%\n"
)

# 3. Verificar a área calculada com máscara

area_pixels_corrigida <- terra::cellSize(
  mapa_binario,
  unit = "km",
  mask = TRUE
)

area_adequada_corrigida <- terra::ifel(
  mapa_binario == 1,
  area_pixels_corrigida,
  NA
)

area_adequada_km2 <- terra::global(
  area_adequada_corrigida,
  "sum",
  na.rm = TRUE
)[1, 1]

area_total_km2 <- terra::global(
  area_pixels_corrigida,
  "sum",
  na.rm = TRUE
)[1, 1]

cat("\nÁrea adequada:", round(area_adequada_km2, 2), "km²\n")
cat("Área total válida:", round(area_total_km2, 2), "km²\n")
cat(
  "Percentual adequado:",
  round(100 * area_adequada_km2 / area_total_km2, 2),
  "%\n"
)


resumo_area <- tibble(
  area_referencia_raster_km2 = area_total_km2,
  area_adequada_km2 = area_adequada_km2,
  percentual_adequado = 100 * area_adequada_km2 / area_total_km2
)

write_csv(
  resumo_area,
  file.path(
    dir_tabelas,
    "14_area_adequabilidade_spodoptera.csv"
  )
)

print(resumo_area)

#Interpretação para o exercício
#O modelo MaxNet, ajustado com os dados de treinamento e projetado sobre as condições bioclimáticas CHELSA, classificou aproximadamente 1,70 milhão de km² do território brasileiro como ambientalmente adequado à ocorrência de Spodoptera frugiperda, considerando o limiar de 0,4883.

#Esse resultado representa cerca de 19,77% da área territorial considerada no raster.

#É importante destacar que não estamos estimando a área efetivamente infestada pela espécie, tampouco a área cultivada com milho. Estamos quantificando a extensão territorial das condições ambientais classificadas como adequadas pelo modelo.


######################## Extra - aplicações potenciais | Adequabilidade por estado

# ============================================================
# SCRIPT 22 — ADEQUABILIDADE AMBIENTAL POR ESTADO
# Spodoptera frugiperda — Brasil
# ============================================================

library(tidyverse)
library(sf)
library(terra)
library(geobr)

# ------------------------------------------------------------
# 1. CARREGAR O RASTER CONTÍNUO DO SCRIPT 18
# ------------------------------------------------------------

adequabilidade_spodoptera <- terra::rast(
  file.path(
    dir_dados_processados,
    "adequabilidade_continua_spodoptera_maxnet.tif"
  )
)

# ------------------------------------------------------------
# 2. OBTER OS LIMITES ESTADUAIS
# ------------------------------------------------------------

estados_br <- geobr::read_state(
  year = 2020,
  showProgress = FALSE
)

# Manter as informações de identificação dos estados.
# A geometria é preservada automaticamente pelo objeto sf.

estados_br <- estados_br %>%
  select(
    abbrev_state,
    name_state
  )

# Conferir a estrutura espacial

print(estados_br)
sf::st_geometry_type(estados_br)
# ------------------------------------------------------------
# 3. CALCULAR A ADEQUABILIDADE MÉDIA POR ESTADO
# ------------------------------------------------------------

# Cada estado será utilizado como zona de extração.
#
# A função mean calcula a média dos valores do raster
# dentro dos limites de cada unidade federativa.
#
# Os valores variam entre 0 e 1.

resultado_estados <- terra::extract(
  adequabilidade_spodoptera,
  terra::vect(estados_br),
  fun = mean,
  na.rm = TRUE
)

# Associar os resultados aos estados

estados_br$adequabilidade_media <- resultado_estados[, 2]

# ------------------------------------------------------------
# 4. CONSTRUIR O ÍNDICE DE ADEQUABILIDADE (0–100)
# ------------------------------------------------------------

estados_br <- estados_br %>%
  mutate(
    indice_adequabilidade = round(
      adequabilidade_media * 100
    )
  )

# ------------------------------------------------------------
# 5. CLASSIFICAR OS ESTADOS
# ------------------------------------------------------------

estados_br <- estados_br %>%
  mutate(
    classe_adequabilidade = case_when(
      indice_adequabilidade <= 25 ~ "0–25",
      indice_adequabilidade <= 50 ~ "26–50",
      indice_adequabilidade <= 75 ~ "51–75",
      indice_adequabilidade > 75 ~ ">75"
    ),
    classe_adequabilidade = factor(
      classe_adequabilidade,
      levels = c(
        "0–25",
        "26–50",
        "51–75",
        ">75"
      )
    )
  )

# ------------------------------------------------------------
# 6. ORGANIZAR E RANQUEAR OS ESTADOS
# ------------------------------------------------------------

ranking_estados <- estados_br %>%
  st_drop_geometry() %>%
  select(
    estado = name_state,
    sigla = abbrev_state,
    adequabilidade_media,
    indice_adequabilidade,
    classe_adequabilidade
  ) %>%
  arrange(desc(adequabilidade_media))

print(ranking_estados)

# Exportar tabela

write_csv(
  ranking_estados,
  file.path(
    dir_tabelas,
    "15_ranking_adequabilidade_estados.csv"
  )
)

# ------------------------------------------------------------
# 7. CONSTRUIR O MAPA TEMÁTICO COM GGPLOT2
# ------------------------------------------------------------

mapa_estados <- ggplot(estados_br) +
  
  geom_sf(
    aes(fill = classe_adequabilidade),
    color = "gray40",
    linewidth = 0.2
  ) +
  
  scale_fill_viridis_d(
    option = "C",
    direction = -1,
    name = "Índice de adequabilidade"
  ) +
  
  labs(
    title = "Adequabilidade ambiental de Spodoptera frugiperda",
    subtitle = "Índice médio por estado brasileiro (0–100)",
    caption = paste(
      "Modelo MaxNet | CHELSA 1981–2010 |",
      "Resolução aproximada de 1 km"
    )
  ) +
  
  theme_minimal() +
  
  theme(
    plot.title = element_text(
      face = "bold",
      size = 14
    ),
    plot.subtitle = element_text(
      size = 11
    ),
    legend.position = "right",
    panel.grid = element_blank()
  )

print(mapa_estados)

# Exportar mapa

ggsave(
  file.path(
    dir_figuras,
    "14_adequabilidade_estados_spodoptera.png"
  ),
  plot = mapa_estados,
  width = 11,
  height = 9,
  dpi = 300
)

# ------------------------------------------------------------
# 8. VALIDAÇÃO FINAL
# ------------------------------------------------------------

stopifnot(
  nrow(ranking_estados) == 27,
  all(is.finite(ranking_estados$adequabilidade_media)),
  all(ranking_estados$indice_adequabilidade >= 0),
  all(ranking_estados$indice_adequabilidade <= 100)
)

cat("\n============================================\n")
cat("ADEQUABILIDADE POR ESTADO CONCLUÍDA\n")
cat("============================================\n")

cat("Estados avaliados:", nrow(ranking_estados), "\n")

cat("\nCinco estados com maior adequabilidade média:\n")

print(
  ranking_estados %>%
    select(estado, sigla, indice_adequabilidade) %>%
    slice_head(n = 5)
)

cat("\nTabela e mapa exportados.\n")

# ============================================================
# SALVAR AMBIENTE DE TRABALHO — AULA 04
# ============================================================

save.image(
  file = file.path(
    dir_projeto,
    "aula_04_modelagem_spodoptera_save.RData"
  )
)

cat("Ambiente de trabalho salvo com sucesso!\n")

