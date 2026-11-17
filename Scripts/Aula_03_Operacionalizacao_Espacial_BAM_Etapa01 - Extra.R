# =============================================================================
# AULA 03 — OPERACIONALIZAÇÃO ESPACIAL DOS COMPONENTES BAM
# Araucaria angustifolia (sistema do pinhão) e Triticum aestivum (trigo)
# Área: Paraná, Santa Catarina e Rio Grande do Sul
# Referência climática planejada: 2001–2020
#
# ETAPA 1 DE 7 — CONFIGURAÇÃO ESPACIAL E OCORRÊNCIAS DA ARAUCÁRIA
#
# Nesta primeira etapa:
#   1. configuramos o projeto e instalamos/carregamos os pacotes;
#   2. baixamos os limites estaduais do Sul do Brasil;
#   3. construímos a área de estudo;
#   4. consultamos ocorrências de Araucaria angustifolia no GBIF;
#   5. fazemos uma triagem inicial e documentada dos registros;
#   6. exportamos dados e mapas para inspeção.
#
# IMPORTANTE:
# - A consulta síncrona do GBIF é apropriada para exploração e conjuntos
#   relativamente pequenos. Para uma base definitiva/publicável, recomenda-se
#   um download GBIF (occ_download), com DOI e citação do conjunto derivado.
# - O filtro temporal 2001–2020 aproxima o período dos dados climáticos.
#   Registros sem ano ou anteriores/posteriores serão mantidos em arquivo
#   separado para auditoria, mas não entram na base temporal principal.
# - A triagem automática não substitui validação taxonômica/geográfica manual.
# - Ainda não calculamos A, B ou M. Primeiro preparamos dados e avaliamos
#   qualidade. Nenhuma variável será convertida em adequabilidade sem critério.
# =============================================================================

B
# -----------------------------------------------------------------------------
# 0. Pacotes
# -----------------------------------------------------------------------------

pacotes <- c(
  "tidyverse",
  "sf",
  "geobr",
  "rgbif",
  "janitor",
  "terra"
)

pacotes_ausentes <- pacotes[
  !pacotes %in% rownames(installed.packages())
]

if (length(pacotes_ausentes) > 0) {
  install.packages(pacotes_ausentes)
}

invisible(
  lapply(pacotes, library, character.only = TRUE)
)


# -----------------------------------------------------------------------------
# 1. Pastas do projeto e parâmetros gerais
# -----------------------------------------------------------------------------

# Crie uma pasta de projeto no computador e defina-a como diretório de trabalho.
# Exemplo Windows:
# setwd("C:/Users/SeuUsuario/Documents/Aula_03_BAM")

# Se o diretório de trabalho ainda não foi definido, o R usará o diretório atual.
# getwd()

pastas <- c(
  "./aula_03/dados",
  "./aula_03/dados/brutos",
  "./aula_03/dados/processados",
  "./aula_03/resultados",
  "./aula_03/resultados/figuras",
  "./aula_03/resultados/tabelas"
)

for (pasta in pastas) {
  if (!dir.exists(pasta)) {
    dir.create(pasta, recursive = TRUE)
  }
}

# Definimos os parâmetros centrais da atividade.
estados_sul <- c("PR", "SC", "RS")

ano_inicial <- 2001
ano_final   <- 2020

especie_alvo <- "Araucaria angustifolia"

# Sistema de coordenadas geográficas usado pelos dados de ocorrência.
crs_geografico <- 4326

# Sistema projetado adequado para medições de área/distância no Sul do Brasil.
# SIRGAS 2000 / UTM 22S é adequado como referência inicial para a região,
# mas a área atravessa zonas UTM; para análises de área regionais, avaliaremos
# uma projeção equivalente apropriada na etapa de métricas.
crs_projetado <- 31982


# -----------------------------------------------------------------------------
# 2. Limites estaduais do Sul do Brasil
# -----------------------------------------------------------------------------

# geobr fornece limites administrativos oficiais em formato espacial.
# Usamos a malha de 2020 para manter uma referência territorial consistente.
estados_br <- geobr::read_state(
  year = 2020,
  showProgress = TRUE
)

# Mantemos somente Paraná, Santa Catarina e Rio Grande do Sul.
estados_sul_sf <- estados_br |>
  dplyr::filter(abbrev_state %in% estados_sul) |>
  sf::st_make_valid() |>
  sf::st_transform(crs_geografico)

# Conferência: devemos ter exatamente três unidades da federação.
stopifnot(nrow(estados_sul_sf) == 3)

print(
  estados_sul_sf |>
    sf::st_drop_geometry() |>
    dplyr::select(name_state, abbrev_state)
)

# Dissolvemos as três unidades federativas em um único polígono.
# Essa geometria será usada para recortar e filtrar as bases posteriores.
area_estudo <- estados_sul_sf |>
  dplyr::summarise(
    regiao = "Sul do Brasil",
    .groups = "drop"
  ) |>
  sf::st_make_valid()

# Exportamos as geometrias para reutilização em etapas futuras.
sf::st_write(
  estados_sul_sf,
  "./aula_03/dados/processados/estados_sul_2020.gpkg",
  delete_dsn = TRUE,
  quiet = TRUE
)

sf::st_write(
  area_estudo,
  "./aula_03/dados/processados/area_estudo_sul.gpkg",
  delete_dsn = TRUE,
  quiet = TRUE
)


# -----------------------------------------------------------------------------
# 3. Mapa da área de estudo
# -----------------------------------------------------------------------------

mapa_area <- ggplot() +
  geom_sf(
    data = estados_sul_sf,
    aes(fill = abbrev_state),
    color = "grey20",
    linewidth = 0.45
  ) +
  geom_sf(
    data = area_estudo,
    fill = NA,
    color = "black",
    linewidth = 0.8
  ) +
  scale_fill_viridis_d(
    name = "Estado",
    option = "D"
  ) +
  labs(
    title = "Área de estudo — Sul do Brasil",
    subtitle = "Paraná, Santa Catarina e Rio Grande do Sul",
    caption = "Limites estaduais: malha geográfica do IBGE via geobr (2020)."
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "bottom"
  )

print(mapa_area)

ggsave(
  "./aula_03/resultados/figuras/01_area_estudo_sul.png",
  mapa_area,
  width = 8,
  height = 7,
  dpi = 300
)


# -----------------------------------------------------------------------------
# 4. Identificação taxonômica da espécie no GBIF
# -----------------------------------------------------------------------------

# A função name_backbone consulta a taxonomia do GBIF e retorna o identificador
# taxonKey associado ao nome informado.
#
# Não devemos presumir que qualquer grafia retornada em uma ocorrência seja
# automaticamente uma identificação correta. Guardaremos o resultado da
# identificação taxonômica para documentar a consulta.

taxon_gbif <- rgbif::name_backbone(
  name = especie_alvo,
  rank = "SPECIES"
)

print(taxon_gbif)

if (is.null(taxon_gbif$usageKey)) {
  stop(
    "O GBIF não retornou um usageKey para a espécie. ",
    "Verifique a grafia e a resposta de name_backbone()."
  )
}

taxon_key <- taxon_gbif$usageKey

# Salvamos a resposta taxonômica para auditoria.
readr::write_csv(
  tibble::tibble(
    nome_pesquisado = especie_alvo,
    taxon_key = taxon_key,
    nome_cientifico_gbif = taxon_gbif$scientificName,
    rank_gbif = taxon_gbif$rank,
    status_taxonomico = taxon_gbif$status
  ),
  "./aula_03/resultados/tabelas/02_taxonomia_gbif_araucaria.csv"
)


# -----------------------------------------------------------------------------
# 5. Consulta exploratória de ocorrências no GBIF
# -----------------------------------------------------------------------------

# occ_search é uma consulta síncrona. A API permite paginação, mas a consulta
# tem limites operacionais. Para bases grandes e uso científico definitivo,
# prefira occ_download(), que requer credenciais GBIF e fornece um DOI.
#
# Aqui consultamos apenas:
#   - o taxonKey confirmado pelo backbone do GBIF;
#   - ocorrências com coordenadas;
#   - registros dentro do polígono da região Sul;
#   - registros datados entre 2001 e 2020.
#
# A geometria é enviada como WKT no sistema geográfico EPSG:4326.
# Se a consulta espacial falhar por causa do tamanho/complexidade do WKT,
# interrompa e use uma consulta por estado ou um download GBIF com filtro
# espacial apropriado; não remova o filtro geográfico silenciosamente.

# Criamos uma caixa geográfica simples que envolve os três estados.
# Essa geometria tem apenas quatro vértices principais e é muito mais leve
# do que enviar o contorno completo da região Sul ao GBIF.

bbox_sul <- sf::st_as_sfc(
  sf::st_bbox(area_estudo)
)

wkt_bbox <- sf::st_as_text(bbox_sul)

print(wkt_bbox)


# Primeira página: além dos dados, a resposta informa o total de registros.
resposta_inicial <- rgbif::occ_search(
  taxonKey = taxon_key,
  hasCoordinate = TRUE,
  geometry = wkt_bbox,
  year = paste0(ano_inicial, ",", ano_final),
  limit = limite_pagina,
  start = 0
)

total_gbif <- resposta_inicial$meta$count

message(
  "Registros encontrados na caixa geográfica: ",
  total_gbif
)


lista_paginas <- list(resposta_inicial$data)

if (total_gbif > limite_pagina) {
  
  inicios <- seq(
    from = limite_pagina,
    to = min(total_gbif - 1, 99999),
    by = limite_pagina
  )
  
  for (start_atual in inicios) {
    
    message(
      "Consultando a partir do registro ",
      start_atual
    )
    
    resposta <- rgbif::occ_search(
      taxonKey = taxon_key,
      hasCoordinate = TRUE,
      geometry = wkt_bbox,
      year = paste0(ano_inicial, ",", ano_final),
      limit = limite_pagina,
      start = start_atual
    )
    
    lista_paginas[[length(lista_paginas) + 1]] <- resposta$data
  }
}

ocorrencias_brutas <- dplyr::bind_rows(lista_paginas) |>
  janitor::clean_names()

# -----------------------------------------------------------------------------
# 6. Triagem inicial e controle de qualidade
# -----------------------------------------------------------------------------

# A consulta já exigiu coordenadas, mas ainda precisamos verificar:
#   - coordenadas ausentes;
#   - coordenadas fora dos limites plausíveis;
#   - coordenadas iguais a zero;
#   - ano de ocorrência;
#   - incerteza espacial informada;
#   - tipo de registro.
#
# Não vamos excluir automaticamente registros apenas por terem incerteza
# espacial ausente. A ausência dessa informação será registrada e poderá ser
# tratada em uma análise de sensibilidade.

colunas_essenciais <- c(
  "decimal_longitude",
  "decimal_latitude",
  "year"
)

colunas_ausentes <- setdiff(
  colunas_essenciais,
  names(ocorrencias_brutas)
)

if (length(colunas_ausentes) > 0) {
  stop(
    "Colunas esperadas não encontradas no retorno GBIF: ",
    paste(colunas_ausentes, collapse = ", ")
  )
}

# Função auxiliar para obter uma coluna quando ela não estiver presente.
# Isso evita falhas em registros que não possuem todos os metadados.
obter_coluna <- function(dados, coluna) {
  if (coluna %in% names(dados)) {
    dados[[coluna]]
  } else {
    rep(NA, nrow(dados))
  }
}

ocorrencias_triagem <- ocorrencias_brutas |>
  dplyr::mutate(
    longitude = as.numeric(decimal_longitude),
    latitude = as.numeric(decimal_latitude),
    ano = as.integer(year),
    incerteza_m = as.numeric(
      obter_coluna(ocorrencias_brutas, "coordinate_uncertainty_in_meters")
    ),
    nome_registro = as.character(
      obter_coluna(ocorrencias_brutas, "scientific_name")
    ),
    base_registro = as.character(
      obter_coluna(ocorrencias_brutas, "basis_of_record")
    ),
    occurrence_id = as.character(
      obter_coluna(ocorrencias_brutas, "occurrence_id")
    ),
    dataset_key = as.character(
      obter_coluna(ocorrencias_brutas, "dataset_key")
    )
  ) |>
  dplyr::mutate(
    coordenada_valida = !is.na(longitude) &
      !is.na(latitude) &
      longitude != 0 &
      latitude != 0 &
      dplyr::between(longitude, -180, 180) &
      dplyr::between(latitude, -90, 90),

    ano_no_periodo = !is.na(ano) &
      dplyr::between(ano, ano_inicial, ano_final),

    incerteza_ate_10km = is.na(incerteza_m) |
      incerteza_m <= 10000
  )

# Criamos uma tabela com todos os registros e indicadores de triagem.
readr::write_csv(
  ocorrencias_triagem,
  "./aula_03/dados/processados/gbif_araucaria_triagem_completa.csv"
)

# Base principal:
# Mantemos coordenadas válidas e incerteza <= 10 km.
# Registros sem incerteza informada são mantidos, mas identificados.
ocorrencias_candidatas <- ocorrencias_triagem |>
  dplyr::filter(
    coordenada_valida,
    incerteza_ate_10km
  )

# Transformamos as coordenadas em objeto espacial.
ocorrencias_sf <- sf::st_as_sf(
  ocorrencias_candidatas,
  coords = c("longitude", "latitude"),
  crs = crs_geografico,
  remove = FALSE
)

# Conferimos novamente se os pontos estão dentro da área de estudo.
# Essa verificação é importante porque a consulta remota pode interpretar
# geometrias ou limites de modo diferente do esperado.
dentro_area <- sf::st_within(
  ocorrencias_sf,
  area_estudo,
  sparse = FALSE
)

ocorrencias_sf$dentro_area <- apply(
  dentro_area,
  1,
  any
)

ocorrencias_sf <- ocorrencias_sf |>
  dplyr::filter(dentro_area)

# Removemos duplicatas exatas de coordenadas + ano + nome de registro.
# Não removemos automaticamente registros próximos, pois isso exigiria uma
# decisão explícita sobre distância mínima e esforço amostral.
ocorrencias_sf <- ocorrencias_sf |>
  dplyr::distinct(
    longitude,
    latitude,
    ano,
    nome_registro,
    .keep_all = TRUE
  )

# Separar os registros sem incerteza espacial permite inspecioná-los depois.
ocorrencias_sem_incerteza <- ocorrencias_sf |>
  dplyr::filter(is.na(incerteza_m))

ocorrencias_com_incerteza <- ocorrencias_sf |>
  dplyr::filter(!is.na(incerteza_m))

# Exportamos a base espacial candidata.
sf::st_write(
  ocorrencias_sf,
  "./aula_03/dados/processados/gbif_araucaria_candidatas.gpkg",
  delete_dsn = TRUE,
  quiet = TRUE
)

# Exportamos também a tabela sem geometria.
readr::write_csv(
  sf::st_drop_geometry(ocorrencias_sf),
  "./aula_03/dados/processados/gbif_araucaria_candidatas.csv"
)


# -----------------------------------------------------------------------------
# 7. Relatório de qualidade da base de ocorrências
# -----------------------------------------------------------------------------

resumo_qualidade <- tibble::tibble(
  indicador = c(
    "Registros retornados pelo GBIF",
    "Registros com coordenadas válidas e incerteza <= 10 km ou ausente",
    "Registros após conferência espacial e remoção de duplicatas",
    "Registros sem incerteza espacial informada",
    "Registros com incerteza espacial informada"
  ),
  n = c(
    nrow(ocorrencias_brutas),
    nrow(ocorrencias_candidatas),
    nrow(ocorrencias_sf),
    nrow(ocorrencias_sem_incerteza),
    nrow(ocorrencias_com_incerteza)
  )
)

print(resumo_qualidade)

readr::write_csv(
  resumo_qualidade,
  "./aula_03/resultados/tabelas/03_relatorio_qualidade_gbif.csv"
)

# Resumo por estado informado pelo provedor.
# Esta classificação é apenas descritiva; a atribuição espacial final poderá
# ser refeita por interseção com os polígonos estaduais.
if ("state_province" %in% names(ocorrencias_sf)) {

  resumo_estado_gbif <- ocorrencias_sf |>
    sf::st_drop_geometry() |>
    dplyr::count(state_province, sort = TRUE)

  print(resumo_estado_gbif)

  readr::write_csv(
    resumo_estado_gbif,
    "./aula_03/resultados/tabelas/04_ocorrencias_por_estado_gbif.csv"
  )
}


# -----------------------------------------------------------------------------
# 8. Mapa exploratório das ocorrências de araucária
# -----------------------------------------------------------------------------

mapa_ocorrencias <- ggplot() +

  geom_sf(
    data = estados_sul_sf,
    fill = "grey96",
    color = "grey45",
    linewidth = 0.4
  ) +

  geom_sf(
    data = ocorrencias_sf,
    color = "#238A8D",
    alpha = 0.65,
    size = 1.1
  ) +

  labs(
    title = "Ocorrências candidatas de Araucaria angustifolia",
    subtitle = "GBIF — registros georreferenciados entre 2001 e 2020",
    caption = paste(
      "Consulta exploratória GBIF; filtros e limitações documentados no script.",
      "Os pontos não representam abundância nem ausência da espécie."
    )
  ) +

  theme_minimal(base_size = 12) +

  theme(
    plot.title = element_text(face = "bold"),
    panel.grid = element_line(color = "grey90")
  )

print(mapa_ocorrencias)

ggsave(
  "./aula_03/resultados/figuras/05_ocorrencias_araucaria_gbif.png",
  mapa_ocorrencias,
  width = 9,
  height = 7,
  dpi = 300
)


# -----------------------------------------------------------------------------
# 9. Encerramento da etapa 1 — validações
# -----------------------------------------------------------------------------

# Testes simples para detectar problemas antes de avançar.
stopifnot(
  nrow(estados_sul_sf) == 3,
  nrow(area_estudo) == 1,
  all(sf::st_is_valid(estados_sul_sf)),
  all(sf::st_is_valid(area_estudo))
)

if (nrow(ocorrencias_sf) == 0) {
  warning(
    "Nenhuma ocorrência candidata foi mantida. ",
    "Revise a consulta GBIF e os filtros antes de avançar."
  )
}

message("ETAPA 1 concluída.")
message("Área de estudo e base candidata de ocorrências preparadas.")
message("Não interpretar ainda os pontos como distribuição potencial.")

#### Parar exercício da disciplina aqui! Abaixo é extra e requer muito tempo e poder computacional

# =============================================================================
# AULA 03 — MODELAGEM DE NICHO ECOLÓGICO APLICADA À PRODUÇÃO SUSTENTÁVEL
# ETAPA 03 — PROPRIEDADES DO SOLO: COMPONENTE ABIÓTICO (A)
#
# Espécies de referência:
#   - Araucaria angustifolia
#   - Triticum aestivum
#
# Área de estudo: Paraná, Santa Catarina e Rio Grande do Sul
# Fonte: SoilGrids 2.0 / ISRIC
# Resolução espacial nominal: 250 m
# Profundidades: 0–5, 5–15 e 15–30 cm
# Estatística: Q0.5 (mediana predita)
#
# ESTRATÉGIA DE AQUISIÇÃO
#   Os testes desta aula mostraram que:
#   (1) o VRT remoto do SoilGrids responde;
#   (2) terra::rast("/vsicurl/...") abre o VRT;
#   (3) crop() e mask() funcionam após transformar a área de estudo
#       para o CRS do raster.
#
#   Por isso, este script acessa diretamente os VRTs remotos com terra.
#   Não utiliza soilDB::fetchSoilGrids(), cuja rotina de criação de
#   GeoTIFF temporário falhou no ambiente testado.
#
# CUIDADO COM AS UNIDADES
#   Os arquivos serão inicialmente preservados em seus valores brutos.
#   Não aplicar fatores de escala antes de conferir os metadados oficiais
#   de cada propriedade. A camada sand_0-5cm_Q0.5 foi validada em aula:
#   valores brutos 32–879; para essa propriedade, a conversão demonstrada
#   foi g/kg para %, dividindo por 10.
#
# IMPORTANTE
#   SoilGrids é um produto global de predição, não uma amostragem de campo.
#   A resolução nominal de 250 m não equivale à precisão local de uma
#   análise laboratorial ou de um levantamento pedológico detalhado.
# =============================================================================


# =============================================================================
# 0. CONFIGURAÇÃO INICIAL
# =============================================================================

# Limpar objetos da sessão é opcional.
# Descomente a linha abaixo se desejar iniciar uma sessão completamente limpa.
# rm(list = ls())

# Definir a semente não é necessário para a aquisição dos rasters,
# pois não há procedimento aleatório neste script.

# Pacotes necessários:
# - sf: leitura e transformação de dados vetoriais;
# - terra: leitura, recorte, máscara e exportação dos rasters;
# - tidyverse: organização dos inventários e tabelas;
# - readr: exportação de tabelas CSV.

pacotes <- c("sf", "terra", "tidyverse", "readr")

pacotes_faltantes <- pacotes[
  !pacotes %in% rownames(installed.packages())
]

if (length(pacotes_faltantes) > 0) {
  install.packages(pacotes_faltantes)
}

invisible(
  lapply(pacotes, library, character.only = TRUE)
)


# =============================================================================
# 1. DEFINIÇÃO DO PROJETO E DOS DIRETÓRIOS
# =============================================================================

# O diretório de trabalho deve ser a pasta que contém a pasta "aula_03".
# Exemplo:
# setwd("C:/Users/mario/Documents/...")
#
# Confira o diretório atual:
getwd()

# Diretório principal da aula.
dir_projeto <- file.path("aula_03")

# Diretórios de entrada e saída.
dir_dados_processados <- file.path(
  dir_projeto, "dados", "processados"
)

dir_solo <- file.path(
  dir_dados_processados, "solo"
)

dir_resultados <- file.path(
  dir_projeto, "resultados"
)

dir_tabelas <- file.path(
  dir_resultados, "tabelas"
)

dir_figuras <- file.path(
  dir_resultados, "figuras"
)

# Criar os diretórios caso ainda não existam.
diretorios <- c(
  dir_solo,
  dir_tabelas,
  dir_figuras
)

invisible(
  lapply(
    diretorios,
    dir.create,
    recursive = TRUE,
    showWarnings = FALSE
  )
)

# Arquivo vetorial produzido na Etapa 01.
arquivo_area <- file.path(
  dir_dados_processados,
  "area_estudo_sul.gpkg"
)

if (!file.exists(arquivo_area)) {
  stop(
    "Arquivo da área de estudo não encontrado: ",
    arquivo_area,
    "\nExecute a Etapa 01 ou corrija o caminho."
  )
}


# =============================================================================
# 2. LEITURA E CONFERÊNCIA DA ÁREA DE ESTUDO
# =============================================================================

# Ler o polígono regional.
area_estudo <- sf::st_read(
  arquivo_area,
  quiet = TRUE
)

# Verificar se a geometria possui CRS definido.
if (is.na(sf::st_crs(area_estudo))) {
  stop("A área de estudo não possui sistema de referência definido.")
}

# Verificar validade geométrica.
validade_area <- sf::st_is_valid(area_estudo)

if (any(is.na(validade_area) | !validade_area)) {
  warning(
    "Há geometrias inválidas. Verifique o arquivo da área de estudo."
  )
}

# Transformar para longitude/latitude apenas para padronizar
# a representação vetorial de entrada.
area_estudo <- sf::st_transform(
  area_estudo,
  4326
)

# Conferência básica.
print(area_estudo)
print(sf::st_crs(area_estudo))


# =============================================================================
# 3. PARÂMETROS DO SOILGRIDS
# =============================================================================

# Propriedades edáficas selecionadas.
propriedades <- c(
  "phh2o",
  "soc",
  "clay",
  "sand",
  "cec",
  "nitrogen"
)

# Intervalos de profundidade disponibilizados pelo SoilGrids.
profundidades <- c(
  "0-5",
  "5-15",
  "15-30"
)

# Q0.5 corresponde à mediana da distribuição predita.
estatistica <- "Q0.5"

# Endereço-base dos arquivos VRT.
url_base <- "https://files.isric.org/soilgrids/latest/data"

# Número esperado de combinações.
n_camadas_esperadas <- length(propriedades) *
  length(profundidades)

cat(
  "\nNúmero de propriedades:", length(propriedades),
  "\nNúmero de profundidades:", length(profundidades),
  "\nNúmero esperado de camadas:", n_camadas_esperadas,
  "\n"
)


# =============================================================================
# 4. DICIONÁRIO DAS PROPRIEDADES
# =============================================================================

# Este dicionário descreve o significado das variáveis.
# As unidades e fatores de escala serão registrados separadamente,
# após conferência dos metadados oficiais de cada camada.

dicionario_solo <- tibble::tribble(
  ~variavel, ~descricao,
  "phh2o",   "pH em água",
  "soc",     "Carbono orgânico do solo",
  "clay",    "Conteúdo de argila",
  "sand",    "Conteúdo de areia",
  "cec",     "Capacidade de troca catiônica",
  "nitrogen","Nitrogênio total"
)

readr::write_csv(
  dicionario_solo,
  file.path(dir_tabelas, "dicionario_soilgrids.csv")
)


# =============================================================================
# 5. PREPARAÇÃO DA ÁREA NO CRS DO SOILGRIDS
# =============================================================================

# O SoilGrids utiliza a projeção Interrupted Goode Homolosine.
# Não devemos comparar coordenadas ou executar crop/mask entre objetos
# em sistemas de referência diferentes.
#
# Primeiro abriremos uma camada de referência para obter o CRS real
# diretamente dos metadados do raster.

url_referencia <- paste0(
  url_base,
  "/sand/sand_0-5cm_Q0.5.vrt"
)

url_referencia_gdal <- paste0(
  "/vsicurl/",
  url_referencia
)

raster_referencia <- tryCatch(
  terra::rast(url_referencia_gdal),
  error = function(e) {
    stop(
      "Não foi possível abrir o VRT de referência do SoilGrids.\n",
      "Verifique a conexão e o endereço remoto.\n",
      conditionMessage(e)
    )
  }
)

# Obter o CRS do raster, sem assumir um código EPSG.
crs_soilgrids <- terra::crs(raster_referencia)

if (is.na(crs_soilgrids) || crs_soilgrids == "") {
  stop("O raster de referência não apresentou CRS válido.")
}

# Transformar o polígono para o CRS do raster.
area_solo <- sf::st_transform(
  area_estudo,
  crs = crs_soilgrids
)

# Converter sf para SpatVector, utilizado pelas funções do terra.
area_solo_vect <- terra::vect(area_solo)

# Registrar informações espaciais de referência.
cat("\nCRS do SoilGrids:\n")
print(crs_soilgrids)

cat("\nExtensão da área no CRS do SoilGrids:\n")
print(terra::ext(area_solo_vect))


# =============================================================================
# 6. FUNÇÃO DE AQUISIÇÃO, RECORTE E EXPORTAÇÃO
# =============================================================================

# A função abaixo executa a mesma sequência para cada combinação:
# propriedade × profundidade.
#
# Etapas:
#   1. montar o endereço do VRT;
#   2. abrir o raster remoto;
#   3. recortar pela extensão da área de estudo;
#   4. aplicar a máscara do polígono;
#   5. salvar GeoTIFF local;
#   6. retornar metadados básicos.
#
# A função não converte unidades. Os arquivos são preservados em
# escala bruta para permitir auditoria posterior.

baixar_recortar_soilgrids <- function(
    variavel,
    profundidade,
    estatistica,
    area_vect,
    url_base,
    dir_saida
) {
  
  # Montar nome e endereço do arquivo.
  nome_vrt <- paste0(
    variavel,
    "_",
    profundidade,
    "cm_",
    estatistica,
    ".vrt"
  )
  
  url_vrt <- paste(
    url_base,
    variavel,
    nome_vrt,
    sep = "/"
  )
  
  url_vrt_gdal <- paste0(
    "/vsicurl/",
    url_vrt
  )
  
  # Nome padronizado para o arquivo local.
  nome_saida <- paste0(
    "soilgrids_",
    variavel,
    "_",
    gsub("-", "_", profundidade),
    "cm_",
    estatistica,
    "_raw.tif"
  )
  
  arquivo_saida <- file.path(
    dir_saida,
    nome_saida
  )
  
  message(
    "\nProcessando: ",
    variavel,
    " | ",
    profundidade,
    " cm | ",
    estatistica
  )
  
  message("VRT: ", url_vrt)
  
  # Abrir o VRT remoto.
  raster_remoto <- tryCatch(
    terra::rast(url_vrt_gdal),
    error = function(e) {
      stop(
        "Falha ao abrir o VRT para ",
        variavel, " / ", profundidade,
        " cm.\n",
        "Endereço: ", url_vrt, "\n",
        conditionMessage(e)
      )
    }
  )
  
  # Recortar pela extensão da área de estudo.
  raster_crop <- tryCatch(
    terra::crop(
      raster_remoto,
      area_vect,
      snap = "out"
    ),
    error = function(e) {
      stop(
        "Falha no crop para ",
        variavel, " / ", profundidade,
        " cm.\n",
        conditionMessage(e)
      )
    }
  )
  
  # Aplicar máscara para remover pixels externos ao polígono.
  raster_mascarado <- tryCatch(
    terra::mask(
      raster_crop,
      area_vect
    ),
    error = function(e) {
      stop(
        "Falha na máscara para ",
        variavel, " / ", profundidade,
        " cm.\n",
        conditionMessage(e)
      )
    }
  )
  
  # Nome interno da camada.
  names(raster_mascarado) <- paste0(
    variavel,
    "_",
    gsub("-", "_", profundidade),
    "cm_raw"
  )
  
  # Salvar arquivo local.
  terra::writeRaster(
    raster_mascarado,
    filename = arquivo_saida,
    overwrite = TRUE,
    gdal = c("COMPRESS=LZW")
  )
  
  # Verificar persistência do arquivo.
  if (!file.exists(arquivo_saida)) {
    stop(
      "O arquivo não foi encontrado após a exportação: ",
      arquivo_saida
    )
  }
  
  # Reabrir o arquivo salvo para verificar integridade.
  raster_local <- terra::rast(arquivo_saida)
  
  # Estatísticas básicas calculadas pelo terra.
  estatisticas <- terra::global(
    raster_local,
    fun = c("min", "mean", "max"),
    na.rm = TRUE
  )
  
  # Registrar informações da camada.
  resultado <- tibble::tibble(
    variavel = variavel,
    profundidade_cm = profundidade,
    estatistica = estatistica,
    arquivo = arquivo_saida,
    url_origem = url_vrt,
    n_linhas = terra::nrow(raster_local),
    n_colunas = terra::ncol(raster_local),
    resolucao_x = terra::res(raster_local)[1],
    resolucao_y = terra::res(raster_local)[2],
    crs = terra::crs(raster_local),
    minimo_raw = as.numeric(estatisticas[1, 1]),
    media_raw = as.numeric(estatisticas[1, 2]),
    maximo_raw = as.numeric(estatisticas[1, 3]),
    arquivo_existe = file.exists(arquivo_saida)
  )
  
  message("Concluído: ", arquivo_saida)
  
  return(resultado)
}


# =============================================================================
# 7. AQUISIÇÃO DAS 18 CAMADAS # vai demorar início 10h23, término no meu pc 11h45
# =============================================================================

# Criar a grade de combinações.
combinacoes <- tidyr::expand_grid(
  variavel = propriedades,
  profundidade = profundidades
)

# Conferir a tabela de combinações antes de iniciar.
print(combinacoes)

# Lista para armazenar os resultados de auditoria.
resultados_aquisicao <- vector(
  mode = "list",
  length = nrow(combinacoes)
)

# Executar uma combinação por vez.
# A aquisição sequencial reduz o consumo simultâneo de memória
# e facilita identificar qual camada apresentou eventual falha.

for (i in seq_len(nrow(combinacoes))) {
  
  resultados_aquisicao[[i]] <- baixar_recortar_soilgrids(
    variavel = combinacoes$variavel[i],
    profundidade = combinacoes$profundidade[i],
    estatistica = estatistica,
    area_vect = area_solo_vect,
    url_base = url_base,
    dir_saida = dir_solo
  )
  
}

# Reunir os resultados em uma tabela.
inventario_soilgrids <- dplyr::bind_rows(
  resultados_aquisicao
)

# Conferir se todas as camadas foram geradas.
print(inventario_soilgrids)

if (
  nrow(inventario_soilgrids) != n_camadas_esperadas ||
  any(!inventario_soilgrids$arquivo_existe)
) {
  stop(
    "A aquisição não gerou todas as camadas esperadas. ",
    "Verifique o inventário e as mensagens anteriores."
  )
}

# Salvar inventário.
readr::write_csv(
  inventario_soilgrids,
  file.path(
    dir_tabelas,
    "inventario_soilgrids_aquisicao.csv"
  )
)


# =============================================================================
# 8. CONFERÊNCIA DOS RASTERS LOCAIS
# =============================================================================

# Reabrir os arquivos a partir do disco.
# Isso comprova que os dados foram efetivamente salvos e não dependem
# apenas dos objetos existentes na memória do R.

arquivos_solo <- inventario_soilgrids$arquivo

rasters_locais <- lapply(
  arquivos_solo,
  terra::rast
)

# Conferir o número de arquivos e camadas.
stopifnot(
  length(rasters_locais) == n_camadas_esperadas
)

# Conferir resolução espacial.
resolucoes <- lapply(
  rasters_locais,
  terra::res
)

resolucoes_matriz <- do.call(
  rbind,
  resolucoes
)

print(resolucoes_matriz)

# Conferir sistemas de referência.
crs_camadas <- vapply(
  rasters_locais,
  terra::crs,
  FUN.VALUE = character(1)
)

if (length(unique(crs_camadas)) != 1) {
  warning(
    "As camadas não apresentam todas o mesmo CRS."
  )
}

# Conferir nomes e arquivos.
auditoria_arquivos <- tibble::tibble(
  arquivo = arquivos_solo,
  existe = file.exists(arquivos_solo),
  tamanho_MB = file.info(arquivos_solo)$size / (1024^2)
)

print(auditoria_arquivos)

readr::write_csv(
  auditoria_arquivos,
  file.path(
    dir_tabelas,
    "auditoria_arquivos_soilgrids.csv"
  )
)


# =============================================================================
# 9. RESUMO ESTATÍSTICO DAS CAMADAS
# =============================================================================

# As estatísticas são calculadas sobre os valores brutos armazenados.
# Elas servem para detectar:
#   - camadas vazias;
#   - valores extremos inesperados;
#   - problemas de leitura;
#   - diferenças entre propriedades e profundidades.
#
# Não interpretar ainda as médias como unidades físicas finais.


resumo_soilgrids <- purrr::map_dfr(
  
  arquivos_solo,
  
  function(arquivo_i) {
    
    raster_i <- terra::rast(arquivo_i)
    
    minimo <- terra::global(
      raster_i,
      fun = "min",
      na.rm = TRUE
    )
    
    media <- terra::global(
      raster_i,
      fun = "mean",
      na.rm = TRUE
    )
    
    maximo <- terra::global(
      raster_i,
      fun = "max",
      na.rm = TRUE
    )
    
    valores <- terra::values(
      raster_i,
      mat = FALSE
    )
    
    mediana <- stats::median(
      valores,
      na.rm = TRUE
    )
    
    tibble::tibble(
      arquivo = basename(arquivo_i),
      camada = names(raster_i),
      minimo_raw = as.numeric(minimo[1, 1]),
      media_raw = as.numeric(media[1, 1]),
      mediana_raw = mediana,
      maximo_raw = as.numeric(maximo[1, 1])
    )
    
  }
  
)

dim(resumo_soilgrids)

print(resumo_soilgrids)

summary(resumo_soilgrids)
# =============================================================================
# 10. MAPA EXPLORATÓRIO DE UMA CAMADA
# =============================================================================

# Para evitar criar tabelas gigantes com todos os pixels das 18 camadas,
# a visualização inicial é feita apenas com a camada sand 0–5 cm.
#
# A visualização é exploratória: a legenda representa valores brutos.

arquivo_sand <- inventario_soilgrids |>
  dplyr::filter(
    variavel == "sand",
    profundidade_cm == "0-5"
  ) |>
  dplyr::pull(arquivo)

raster_sand <- terra::rast(arquivo_sand)

# Converter para data.frame apenas a camada selecionada.
df_sand <- terra::as.data.frame(
  raster_sand,
  xy = TRUE,
  na.rm = TRUE
)

names(df_sand)[3] <- "valor_raw"

mapa_sand <- ggplot2::ggplot(
  df_sand,
  ggplot2::aes(
    x = x,
    y = y,
    fill = valor_raw
  )
) +
  ggplot2::geom_raster() +
  ggplot2::coord_equal() +
  ggplot2::scale_fill_viridis_c(
    name = "Valor bruto"
  ) +
  ggplot2::labs(
    title = "SoilGrids — areia, 0–5 cm",
    subtitle = "Q0.5; valores brutos, sem conversão de escala",
    x = "Coordenada X",
    y = "Coordenada Y"
  ) +
  ggplot2::theme_minimal(
    base_size = 11
  )

print(mapa_sand)

ggplot2::ggsave(
  filename = file.path(
    dir_figuras,
    "soilgrids_sand_0_5cm_raw.png"
  ),
  plot = mapa_sand,
  width = 8,
  height = 7,
  dpi = 300
)


# =============================================================================
# 11. INVENTÁRIO DAS VARIÁVEIS ABIÓTICAS (COMPONENTE A)
# =============================================================================

# Este inventário integra as propriedades do solo com as variáveis
# climáticas e topográficas geradas nas etapas anteriores.
#
# A resolução de cada fonte é mantida conforme o produto original.
# Não se deve interpretar a resolução nominal de 250 m do SoilGrids
# como aumento de resolução dos dados climáticos TerraClimate.

inventario_A <- tibble::tribble(
  ~componente, ~variavel, ~fonte, ~resolucao_nominal, ~periodo,
  "A", "Temperatura média", "TerraClimate", "~4 km", "2001–2020",
  "A", "Precipitação média anual", "TerraClimate", "~4 km", "2001–2020",
  "A", "Sazonalidade da precipitação", "TerraClimate", "~4 km", "2001–2020",
  "A", "Altitude", "DEM/elevatr", "conforme produto", "estático",
  "A", "Declividade", "Derivada do DEM", "conforme DEM", "estático",
  "A", "pH em água", "SoilGrids 2.0", "250 m", "produto de referência",
  "A", "Carbono orgânico", "SoilGrids 2.0", "250 m", "produto de referência",
  "A", "Argila", "SoilGrids 2.0", "250 m", "produto de referência",
  "A", "Areia", "SoilGrids 2.0", "250 m", "produto de referência",
  "A", "CTC", "SoilGrids 2.0", "250 m", "produto de referência",
  "A", "Nitrogênio total", "SoilGrids 2.0", "250 m", "produto de referência"
)

readr::write_csv(
  inventario_A,
  file.path(
    dir_tabelas,
    "inventario_variaveis_abioticas.csv"
  )
)


# =============================================================================
# 12. REGISTRO DE PARÂMETROS PARA REPRODUTIBILIDADE
# =============================================================================

# Registrar parâmetros utilizados na aquisição.
# A data de execução é registrada para documentar quando os arquivos
# foram acessados. O endereço "latest" pode apontar para versões futuras;
# por isso, guardar também os arquivos locais e este registro.

parametros_execucao <- tibble::tibble(
  parametro = c(
    "Fonte",
    "URL_base",
    "Area_estudo",
    "Propriedades",
    "Profundidades_cm",
    "Estatistica",
    "Resolucao_nominal_m",
    "CRS_raster",
    "Data_execucao",
    "R_version",
    "terra_version",
    "sf_version"
  ),
  valor = c(
    "SoilGrids 2.0 / ISRIC",
    url_base,
    "Paraná, Santa Catarina e Rio Grande do Sul",
    paste(propriedades, collapse = "; "),
    paste(profundidades, collapse = "; "),
    estatistica,
    "250",
    crs_soilgrids,
    as.character(Sys.Date()),
    as.character(getRversion()),
    as.character(packageVersion("terra")),
    as.character(packageVersion("sf"))
  )
)

readr::write_csv(
  parametros_execucao,
  file.path(
    dir_tabelas,
    "parametros_execucao_soilgrids.csv"
  )
)


# =============================================================================
# 13. VALIDAÇÃO FINAL
# =============================================================================

cat("\n")
cat("============================================================\n")
cat("VALIDAÇÃO FINAL — ETAPA 03: SOILGRIDS\n")
cat("============================================================\n")

cat(
  "Camadas esperadas: ",
  n_camadas_esperadas,
  "\n",
  sep = ""
)

cat(
  "Camadas registradas: ",
  nrow(inventario_soilgrids),
  "\n",
  sep = ""
)

cat(
  "Arquivos existentes: ",
  sum(file.exists(arquivos_solo)),
  "\n",
  sep = ""
)

cat(
  "Todos os arquivos existem? ",
  all(file.exists(arquivos_solo)),
  "\n",
  sep = ""
)

cat(
  "Todas as camadas têm valores válidos? ",
  all(
    is.finite(resumo_soilgrids$minimo_raw) &
      is.finite(resumo_soilgrids$maximo_raw)
  ),
  "\n",
  sep = ""
)

stopifnot(
  nrow(inventario_soilgrids) == n_camadas_esperadas,
  all(file.exists(arquivos_solo)),
  all(is.finite(resumo_soilgrids$minimo_raw)),
  all(is.finite(resumo_soilgrids$maximo_raw))
)

cat("\nETAPA 03 CONCLUÍDA.\n")
cat("Os rasters foram salvos em escala bruta.\n")
cat("Consulte os inventários e resumos antes da conversão de unidades.\n")


# =============================================================================
# REFERÊNCIAS
# =============================================================================
#
# Poggio, L. et al. (2021). SoilGrids 2.0: producing soil information for
# the globe with quantified spatial uncertainty. SOIL, 7, 217–240.
# https://doi.org/10.5194/soil-7-217-2021
#
# SoilGrids documentation:
# https://docs.isric.org/globaldata/soilgrids/
#
# SoilGrids WCS:
# https://docs.isric.org/globaldata/soilgrids/wcs.html
#
# =============================================================================

