################################################################################
# DISCIPLINA: Modelagem de Nicho Ecológico Aplicada à Produção Sustentável
# PPGAG - UTFPR, Campus Pato Branco
# Docente: Mário S. M. Tagliari
#
# AULA PRÁTICA 01
# Da ocorrência geográfica ao espaço ambiental: Ilex paraguariensis no Paraná
#
# OBJETIVOS
# 1. Consultar registros de ocorrência no GBIF.
# 2. Filtrar registros com coordenadas válidas e recortá-los ao Paraná.
# 3. Produzir mapas de ocorrências e buffers geográficos.
# 4. Extrair temperatura, precipitação e altitude nos pontos de ocorrência.
# 5. Construir curvas de densidade para descrever os valores ambientais.
#
# IMPORTANTE
# Esta atividade descreve ocorrências conhecidas e as condições ambientais
# associadas a elas. Ela ainda NÃO ajusta um MEN-ENM ou SDM e não estima a
# distribuição potencial da espécie.
################################################################################


# ==============================================================================
# 0. PREPARAÇÃO DO AMBIENTE
# ==============================================================================

# Instale os pacotes somente se ainda não estiverem instalados.
# A função requireNamespace() verifica a instalação sem carregar os pacotes.
pacotes <- c(
  "rgbif",     # Consulta de dados de ocorrência do GBIF
  "sf",        # Dados espaciais vetoriais
  "geobr",     # Limites territoriais brasileiros
  "terra",     # Dados espaciais raster
  "geodata",   # Download de dados climáticos e de elevação
  "dplyr",     # Organização e transformação dos dados
  "ggplot2",   # Gráficos e mapas
  "readr",     # Exportação de tabelas
  "viridis"    # Paletas de cores acessíveis
)

pacotes_ausentes <- pacotes[
  !vapply(pacotes, requireNamespace, logical(1), quietly = TRUE)
]

if (length(pacotes_ausentes) > 0) {
  install.packages(pacotes_ausentes)
}

# Carregamos explicitamente as bibliotecas usadas ao longo do script.
library(rgbif)
library(sf)
library(geobr)
library(terra)
library(geodata)
library(dplyr)
library(ggplot2)
library(readr)
library(viridis)


# ==============================================================================
# 1. PASTA DE TRABALHO E ORGANIZAÇÃO DOS RESULTADOS
# ==============================================================================

# Escolha uma pasta de projeto no seu computador.
# Aqui usamos uma pasta chamada "aula01_erva_mate" dentro do diretório atual.
# getwd() mostra o diretório atual; setwd() pode ser usado se necessário.
pasta_saida <- "resultados_aula01_erva_mate"

if (!dir.exists(pasta_saida)) {
  dir.create(pasta_saida, recursive = TRUE)
}

# Subpasta para armazenar os arquivos baixados e evitar downloads desnecessários.
pasta_dados <- file.path(pasta_saida, "dados")
if (!dir.exists(pasta_dados)) {
  dir.create(pasta_dados, recursive = TRUE)
}

# Para reprodutibilidade, registramos informações da sessão do R.
writeLines(
  capture.output(sessionInfo()),
  con = file.path(pasta_saida, "informacoes_sessao_R.txt")
)


# ==============================================================================
# 2. CONSULTA DE OCORRÊNCIAS NO GBIF
# ==============================================================================

# A consulta retorna registros indexados pelo GBIF.
# "scientificName" solicita o nome científico; "hasCoordinate = TRUE" restringe
# a consulta aos registros que possuem coordenadas.
#
# occ_search() permite até 100.000 registros por consulta, mas a API pode impor
# limites e o resultado pode ser menor que o total disponível.
# Para esta aula, limitamos a consulta a 10.000 registros.
#
# Em pesquisas formais, prefira um download GBIF com identificador/DOI,
# documente a data de acesso e cite os conjuntos de dados utilizados.
consulta_gbif <- rgbif::occ_search(
  scientificName = "Ilex paraguariensis",
  hasCoordinate = TRUE,
  limit = 10000
)

# A resposta contém metadados e uma tabela chamada "data".
ocorrencias_brutas <- consulta_gbif$data

# Conferimos quantos registros foram retornados antes de filtrar.
cat("Registros retornados pelo GBIF:", nrow(ocorrencias_brutas), "\n")
cat("Colunas disponíveis:\n")
print(names(ocorrencias_brutas))

# Salvamos a tabela bruta para documentar o que a consulta retornou.
# Ela pode conter campos que serão descartados nas próximas etapas.
readr::write_csv(
  ocorrencias_brutas,
  file.path(pasta_dados, "gbif_erva_mate_consulta_bruta.csv")
)


# ==============================================================================
# 3. LIMPEZA INICIAL DOS REGISTROS
# ==============================================================================

# Selecionamos somente as colunas necessárias nesta aula.
# Algumas colunas podem não estar presentes em todas as respostas; por isso,
# verificamos sua existência antes de selecioná-las.
colunas_desejadas <- c(
  "key", "scientificName", "decimalLatitude", "decimalLongitude",
  "countryCode", "stateProvince", "locality", "basisOfRecord",
  "eventDate", "coordinateUncertaintyInMeters", "institutionCode",
  "collectionCode", "catalogNumber", "issues"
)

colunas_disponiveis <- intersect(colunas_desejadas, names(ocorrencias_brutas))

ocorrencias <- ocorrencias_brutas |>
  dplyr::select(dplyr::all_of(colunas_disponiveis))

# Removemos registros sem latitude ou longitude.
# Uma coordenada ausente impede localizar o registro no mapa.
ocorrencias <- ocorrencias |>
  dplyr::filter(
    !is.na(decimalLatitude),
    !is.na(decimalLongitude)
  )

# Excluímos coordenadas fora dos limites numéricos possíveis do planeta.
# Esta checagem detecta valores impossíveis, mas não garante que toda
# coordenada restante esteja geograficamente correta.
ocorrencias <- ocorrencias |>
  dplyr::filter(
    decimalLatitude >= -90,
    decimalLatitude <= 90,
    decimalLongitude >= -180,
    decimalLongitude <= 180
  )

# Removemos coordenadas repetidas exatas.
# Atenção: duas observações legítimas podem compartilhar a mesma coordenada.
# Portanto, esta etapa remove duplicatas espaciais para a demonstração, mas
# não substitui uma avaliação taxonômica e de qualidade registro a registro.
ocorrencias <- ocorrencias |>
  dplyr::distinct(decimalLongitude, decimalLatitude, .keep_all = TRUE)

cat("Registros com coordenadas válidas e sem duplicatas exatas:",
    nrow(ocorrencias), "\n")

# Exportamos os registros após a limpeza inicial.
readr::write_csv(
  ocorrencias,
  file.path(pasta_dados, "gbif_erva_mate_coordenadas_limpeza_inicial.csv")
)


# ==============================================================================
# 4. OBTER O LIMITE DO ESTADO DO PARANÁ
# ==============================================================================

# geobr fornece limites territoriais brasileiros como dados espaciais.
# Usamos um limite estadual oficial para recortar os registros por geometria,
# em vez de filtrar apenas pela coluna textual stateProvince do GBIF.
parana <- geobr::read_state(
  code_state = "PR",
  year = 2020,
  simplified = FALSE,
  showProgress = FALSE
)

# Padronizamos o sistema de coordenadas para longitude/latitude (WGS84).
# O EPSG:4326 é comum em dados de ocorrência e em serviços de biodiversidade.
parana <- sf::st_transform(parana, crs = 4326)


# ==============================================================================
# 5. CONVERTER AS COORDENADAS EM PONTOS ESPACIAIS
# ==============================================================================

# st_as_sf() converte a tabela em um objeto espacial.
# coords indica quais colunas guardam longitude e latitude, nesta ordem.
# crs = 4326 informa que as coordenadas estão em longitude/latitude WGS84.
ocorrencias_sf <- sf::st_as_sf(
  ocorrencias,
  coords = c("decimalLongitude", "decimalLatitude"),
  crs = 4326,
  remove = FALSE
)

# Mantemos somente os pontos que intersectam o polígono do Paraná.
# O recorte geométrico é mais confiável do que depender apenas de stateProvince,
# que pode estar ausente, escrito de maneiras diferentes ou conter erros.
ocorrencias_pr <- sf::st_filter(
  ocorrencias_sf,
  parana,
  .predicate = sf::st_intersects
)

cat("Ocorrências dentro do Paraná:", nrow(ocorrencias_pr), "\n")

if (nrow(ocorrencias_pr) == 0) {
  stop(
    paste(
      "Nenhuma ocorrência foi encontrada dentro do Paraná.",
      "Verifique a consulta GBIF, as colunas retornadas e a conexão com a internet."
    )
  )
}

# Exportamos os pontos filtrados como CSV e GeoPackage.
# O CSV facilita a leitura; o GeoPackage preserva a geometria espacial.
readr::write_csv(
  sf::st_drop_geometry(ocorrencias_pr),
  file.path(pasta_saida, "ocorrencias_erva_mate_parana.csv")
)

sf::st_write(
  ocorrencias_pr,
  file.path(pasta_saida, "ocorrencias_erva_mate_parana.gpkg"),
  delete_dsn = TRUE,
  quiet = TRUE
)


# ==============================================================================
# 6. MAPA 1 — OCORRÊNCIAS GEOGRÁFICAS NO PARANÁ
# ==============================================================================

# geom_sf() desenha os limites estaduais e os pontos espaciais.
# O mapa apresenta ocorrências conhecidas, não uma estimativa de adequabilidade.
mapa_ocorrencias <- ggplot() +
  geom_sf(
    data = parana,
    fill = "grey97",
    color = "grey35",
    linewidth = 0.5
  ) +
  geom_sf(
    data = ocorrencias_pr,
    color = "#238A8D",
    alpha = 0.75,
    size = 1.8
  ) +
  labs(
    title = expression("Ocorrências de " * italic("Ilex paraguariensis")),
    subtitle = "Registros georreferenciados do GBIF recortados ao Paraná",
    x = "Longitude",
    y = "Latitude",
    caption = paste(
      "Fonte: GBIF; limite estadual: geobr.",
      "Pontos representam registros disponíveis, sujeitos a vieses e incertezas."
    )
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid.major = element_line(color = "grey90")
  )

print(mapa_ocorrencias)

ggsave(
  filename = file.path(pasta_saida, "01_ocorrencias_parana.png"),
  plot = mapa_ocorrencias,
  width = 8,
  height = 7,
  dpi = 300
)


# ==============================================================================
# 7. MAPA 2 — BUFFERS AO REDOR DAS OCORRÊNCIAS
# ==============================================================================

# Um buffer de 10 km é uma vizinhança geométrica criada ao redor de cada ponto.
# Ele NÃO significa que a espécie ocupe toda essa área.
#
# Para calcular distâncias em metros, transformamos os dados para um CRS
# projetado em metros. EPSG:5880 (SIRGAS 2000 / Brazil Polyconic) é utilizado
# aqui para fins didáticos em território brasileiro.
#
# Em análises de alta precisão, escolha e justifique a projeção apropriada
# à extensão espacial da área de estudo.
crs_metros <- 5880

parana_m <- sf::st_transform(parana, crs = crs_metros)
ocorrencias_pr_m <- sf::st_transform(ocorrencias_pr, crs = crs_metros)

# A distância do buffer é definida em metros: 10.000 m = 10 km.
raio_buffer_m <- 10000

buffers <- sf::st_buffer(
  ocorrencias_pr_m,
  dist = raio_buffer_m
)

# Para visualização conjunta, todos os objetos precisam compartilhar o CRS.
mapa_buffers <- ggplot() +
  geom_sf(
    data = parana_m,
    fill = "grey98",
    color = "grey35",
    linewidth = 0.5
  ) +
  geom_sf(
    data = buffers,
    fill = "#FDE725",
    color = "#7F7F00",
    alpha = 0.25,
    linewidth = 0.2
  ) +
  geom_sf(
    data = ocorrencias_pr_m,
    color = "#440154",
    size = 1.1,
    alpha = 0.8
  ) +
  labs(
    title = expression("Buffers ilustrativos de " * italic("Ilex paraguariensis")),
    subtitle = "Raio de 10 km ao redor de cada registro",
    x = "Coordenada projetada (m)",
    y = "Coordenada projetada (m)",
    caption = paste(
      "Buffers são vizinhanças geométricas, não áreas de ocupação confirmada",
      "nem estimativas de nicho ecológico."
    )
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"))

print(mapa_buffers)

ggsave(
  filename = file.path(pasta_saida, "02_buffers_10km_parana.png"),
  plot = mapa_buffers,
  width = 8,
  height = 7,
  dpi = 300
)

# Opcional: dissolver os buffers em uma única geometria.
# Isso ajuda a mostrar a união espacial das vizinhanças, mas pode ocultar
# quantos registros existem em cada região.
buffers_unidos <- sf::st_union(buffers)


# ==============================================================================
# 8. DOWNLOAD DAS VARIÁVEIS AMBIENTAIS
# ==============================================================================

# WorldClim 2.1 fornece dados climáticos de referência de 1970-2000.
# Aqui "presente" significa o período de referência climático do produto,
# não observações meteorológicas em tempo real nem especificamente o ano 2026.
#
# BIO1 = temperatura média anual (WorldClim costuma armazená-la em décimos de °C)
# BIO12 = precipitação anual (mm)
#
# Resolução de 2.5 minutos de arco é um compromisso entre detalhamento,
# tamanho dos arquivos e desempenho computacional para a aula.
#
# Os arquivos serão armazenados na pasta de dados para facilitar a reutilização.
clima_bio <- geodata::worldclim_country(
  country = "BRA",
  var = "bio",
  res = 2.5,
  path = pasta_dados,
  version = "2.1"
)

# Selecionamos apenas as camadas necessárias: BIO1 e BIO12.
# No produto WorldClim, a camada 1 é BIO1 e a camada 12 é BIO12.
temperatura_media_anual <- clima_bio[[1]]
precipitacao_anual <- clima_bio[[12]]

# A função elevation_30s() obtém um produto global de elevação com resolução
# nominal de 30 segundos de arco. O download pode ser grande.
# O recorte ao Brasil é solicitado para reduzir o volume de dados.
altitude <- geodata::elevation_30s(
  country = "BRA",
  path = pasta_dados
)

# Recortamos os rasters para uma área de interesse ao redor do Paraná.
# O recorte reduz o custo computacional; o buffer espacial serve apenas como
# margem para não cortar células próximas à fronteira do estado.
parana_4326 <- sf::st_transform(parana, crs = 4326)
ext_parana <- terra::ext(terra::vect(parana_4326))
margem_graus <- 0.5

ext_parana_ampliada <- terra::ext(
  xmin(ext_parana) - margem_graus,
  xmax(ext_parana) + margem_graus,
  ymin(ext_parana) - margem_graus,
  ymax(ext_parana) + margem_graus
)

temperatura_media_anual <- terra::crop(
  temperatura_media_anual,
  ext_parana_ampliada
)
precipitacao_anual <- terra::crop(
  precipitacao_anual,
  ext_parana_ampliada
)
altitude <- terra::crop(
  altitude,
  ext_parana_ampliada
)


# ==============================================================================
# 9. EXTRAIR OS VALORES AMBIENTAIS NOS PONTOS DE OCORRÊNCIA
# ==============================================================================

# terra::extract() consulta o valor do pixel/célula raster em que cada ponto cai.
# Não é uma medição feita exatamente no local de coleta: é um valor associado
# à célula espacial do produto ambiental.
#
# Antes de extrair, transformamos os pontos para o CRS de cada raster, se
# necessário. As funções extract() do terra também lidam com CRS diferentes,
# mas explicitar a compatibilidade facilita o aprendizado.
pontos_v <- terra::vect(ocorrencias_pr)

# Extraímos BIO1 e BIO12 separadamente para que os nomes das colunas fiquem claros.
valores_temp <- terra::extract(
  temperatura_media_anual,
  pontos_v
)

valores_prec <- terra::extract(
  precipitacao_anual,
  pontos_v
)

valores_alt <- terra::extract(
  altitude,
  pontos_v
)

# Criamos uma tabela com um registro por ponto e as três variáveis.
# A primeira coluna de cada extração é o ID da feição, por isso extraímos
# explicitamente a segunda coluna, que contém o valor raster.
dados_ambientais <- sf::st_drop_geometry(ocorrencias_pr) |>
  dplyr::mutate(
    temperatura_media_anual_C = as.numeric(valores_temp[[2]]),
    precipitacao_anual_mm = as.numeric(valores_prec[[2]]),
    altitude_m = as.numeric(valores_alt[[2]])
  )

# Valores NA podem ocorrer quando um ponto não coincide com uma célula válida.
# Primeiro contabilizamos a ausência de dados, antes de decidir como proceder.
cat("\nValores ausentes por variável ambiental:\n")
print(
  sapply(
    dados_ambientais[
      c(
        "temperatura_media_anual_C",
        "precipitacao_anual_mm",
        "altitude_m"
      )
    ],
    function(x) sum(is.na(x))
  )
)

# Salvamos a tabela completa, inclusive eventuais NA, para manter transparência.
readr::write_csv(
  dados_ambientais,
  file.path(pasta_saida, "dados_ambientais_ocorrencias_completos.csv")
)

# Para os gráficos, removemos apenas registros sem valor para a variável
# representada. Não imputamos valores ausentes nesta atividade introdutória.
dados_temp <- dados_ambientais |>
  dplyr::filter(is.finite(temperatura_media_anual_C))

dados_prec <- dados_ambientais |>
  dplyr::filter(is.finite(precipitacao_anual_mm))

dados_alt <- dados_ambientais |>
  dplyr::filter(is.finite(altitude_m))

# Resumo descritivo para conectar gráficos e estatística básica.
resumo_ambiental <- dados_ambientais |>
  dplyr::summarise(
    n_registros = dplyr::n(),
    n_temp = sum(is.finite(temperatura_media_anual_C)),
    media_temp_C = mean(temperatura_media_anual_C, na.rm = TRUE),
    mediana_temp_C = median(temperatura_media_anual_C, na.rm = TRUE),
    dp_temp_C = sd(temperatura_media_anual_C, na.rm = TRUE),
    n_prec = sum(is.finite(precipitacao_anual_mm)),
    media_prec_mm = mean(precipitacao_anual_mm, na.rm = TRUE),
    mediana_prec_mm = median(precipitacao_anual_mm, na.rm = TRUE),
    dp_prec_mm = sd(precipitacao_anual_mm, na.rm = TRUE),
    n_alt = sum(is.finite(altitude_m)),
    media_alt_m = mean(altitude_m, na.rm = TRUE),
    mediana_alt_m = median(altitude_m, na.rm = TRUE),
    dp_alt_m = sd(altitude_m, na.rm = TRUE)
  )

print(resumo_ambiental)

readr::write_csv(
  resumo_ambiental,
  file.path(pasta_saida, "resumo_estatistico_ambiental.csv")
)


# ==============================================================================
# 10. CURVA DE DENSIDADE 1 — TEMPERATURA MÉDIA ANUAL
# ==============================================================================

# Uma curva de densidade é uma estimativa suavizada da distribuição dos valores.
# O eixo x representa os valores da variável; o eixo y representa densidade,
# e não o número bruto de ocorrências.
#
# A curva descreve a distribuição dos valores ambientais associados aos pontos
# amostrados. Ela NÃO é, por si só, uma estimativa completa do nicho fundamental
# ou realizado: faltam, entre outros elementos, disponibilidade ambiental,
# esforço amostral, interações bióticas e processos de dispersão.
grafico_temperatura <- ggplot(
  dados_temp,
  aes(x = temperatura_media_anual_C)
) +
  geom_density(
    fill = "#21918C",
    alpha = 0.35,
    color = "#225EA8",
    linewidth = 1
  ) +
  labs(
    title = expression("Temperatura média anual associada a " * italic("Ilex paraguariensis")),
    subtitle = "WorldClim 2.1; período de referência 1970–2000",
    x = "Temperatura média anual (°C)",
    y = "Densidade estimada",
    caption = "A curva descreve os valores nas ocorrências amostradas; não é um ENM ajustado."
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"))

print(grafico_temperatura)

ggsave(
  file.path(pasta_saida, "03_densidade_temperatura.png"),
  grafico_temperatura,
  width = 8,
  height = 5,
  dpi = 300
)


# ==============================================================================
# 11. CURVA DE DENSIDADE 2 — PRECIPITAÇÃO ANUAL
# ==============================================================================

grafico_precipitacao <- ggplot(
  dados_prec,
  aes(x = precipitacao_anual_mm)
) +
  geom_density(
    fill = "#3B528B",
    alpha = 0.35,
    color = "#3B528B",
    linewidth = 1
  ) +
  labs(
    title = expression("Precipitação anual associada a " * italic("Ilex paraguariensis")),
    subtitle = "WorldClim 2.1; período de referência 1970–2000",
    x = "Precipitação anual (mm/ano)",
    y = "Densidade estimada",
    caption = "Os valores refletem a resolução espacial do raster climático."
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"))

print(grafico_precipitacao)

ggsave(
  file.path(pasta_saida, "04_densidade_precipitacao.png"),
  grafico_precipitacao,
  width = 8,
  height = 5,
  dpi = 300
)


# ==============================================================================
# 12. CURVA DE DENSIDADE 3 — ALTITUDE
# ==============================================================================

grafico_altitude <- ggplot(
  dados_alt,
  aes(x = altitude_m)
) +
  geom_density(
    fill = "#5EC962",
    alpha = 0.35,
    color = "#238A8D",
    linewidth = 1
  ) +
  labs(
    title = expression("Altitude associada a " * italic("Ilex paraguariensis")),
    subtitle = "Valores extraídos de um modelo digital de elevação",
    x = "Altitude (m)",
    y = "Densidade estimada",
    caption = "A altitude é topografia; não é uma variável climática."
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"))

print(grafico_altitude)

ggsave(
  file.path(pasta_saida, "05_densidade_altitude.png"),
  grafico_altitude,
  width = 8,
  height = 5,
  dpi = 300
)

# Instalar, se necessário
#install.packages("plotly")

library(plotly)

# Espaço ambiental 3D da erva-mate
plot_ly(
  dados_ambientais,
  x = ~temperatura_media_anual_C,
  y = ~precipitacao_anual_mm,
  z = ~altitude_m,
  type = "scatter3d",
  mode = "markers",
  marker = list(size = 3, color = "#238A8D", opacity = 0.7)
) %>%
  layout(
    scene = list(
      xaxis = list(title = "Temperatura média anual (°C)"),
      yaxis = list(title = "Precipitação anual (mm)"),
      zaxis = list(title = "Altitude (m)")
    )
  )

### OPÇÕES ALTERNATIVAS
# ============================================================
# GRÁFICOS DO ESPAÇO AMBIENTAL DE Ilex paraguariensis
# ============================================================

library(ggplot2)
library(dplyr)
library(viridis)

# Manter apenas registros que tenham valores válidos
# para as três variáveis ambientais.
# Isso garante que os gráficos utilizem o mesmo conjunto
# de registros, facilitando a comparação entre eles.

dados_3vars <- dados_ambientais |>
  filter(
    is.finite(temperatura_media_anual_C),
    is.finite(precipitacao_anual_mm),
    is.finite(altitude_m)
  )

# Conferir quantos registros serão utilizados.
cat("Registros utilizados nos gráficos:", nrow(dados_3vars), "\n")

# ============================================================
# GRÁFICO 1: TEMPERATURA MÉDIA ANUAL × PRECIPITAÇÃO ANUAL
# ============================================================

grafico_temp_prec <- ggplot(
  dados_3vars,
  aes(
    x = temperatura_media_anual_C,
    y = precipitacao_anual_mm
  )
) +
  
  # Cada ponto representa uma ocorrência georreferenciada
  # e seus respectivos valores ambientais.
  geom_point(
    color = "#238A8D",
    alpha = 0.45,
    size = 2
  ) +
  
  # Estimar a densidade bidimensional e desenhar
  # curvas de nível ao redor das concentrações de registros.
  stat_density_2d(
    aes(color = after_stat(level)),
    bins = 7,
    linewidth = 0.8,
    na.rm = TRUE
  ) +
  
  # Aplicar uma escala contínua de cores às curvas.
  scale_color_viridis_c(
    option = "D",
    name = "Densidade"
  ) +
  
  # Definir títulos e unidades dos eixos.
  labs(
    title = expression(
      "Espaço ambiental de " * italic("Ilex paraguariensis")
    ),
    subtitle = "Temperatura média anual × precipitação anual",
    x = "Temperatura média anual (°C)",
    y = "Precipitação anual (mm)",
    caption = paste(
      "Pontos: ocorrências do GBIF.",
      "Linhas: contornos da densidade bidimensional."
    )
  ) +
  
  theme_minimal(base_size = 13) +
  
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "right"
  )

# Exibir o gráfico.
print(grafico_temp_prec)

# Salvar a figura em alta resolução.
ggsave(
  filename = file.path(
    pasta_saida,
    "06_contornos_temperatura_precipitacao.png"
  ),
  plot = grafico_temp_prec,
  width = 9,
  height = 7,
  dpi = 300
)
# ============================================================
# GRÁFICO 2b: TEMPERATURA MÉDIA ANUAL × ALTITUDE
# ============================================================

grafico_temp_alt <- ggplot(
  dados_3vars,
  aes(
    x = temperatura_media_anual_C,
    y = altitude_m
  )
) +
  
  # Representar individualmente os registros de ocorrência.
  geom_point(
    color = "#3B528B",
    alpha = 0.45,
    size = 2
  ) +
  
  # Adicionar curvas de nível da densidade bidimensional.
  stat_density_2d(
    aes(color = after_stat(level)),
    bins = 7,
    linewidth = 0.8,
    na.rm = TRUE
  ) +
  
  # Utilizar a mesma escala de cores do primeiro gráfico.
  scale_color_viridis_c(
    option = "D",
    name = "Densidade"
  ) +
  
  # Definir títulos e unidades.
  labs(
    title = expression(
      "Espaço ambiental de " * italic("Ilex paraguariensis")
    ),
    subtitle = "Temperatura média anual × altitude",
    x = "Temperatura média anual (°C)",
    y = "Altitude (m)",
    caption = paste(
      "Pontos: ocorrências do GBIF.",
      "Linhas: contornos da densidade bidimensional."
    )
  ) +
  
  theme_minimal(base_size = 13) +
  
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "right"
  )

# Exibir o gráfico.
print(grafico_temp_alt)

# Exportar a figura.
ggsave(
  filename = file.path(
    pasta_saida,
    "07_contornos_temperatura_altitude.png"
  ),
  plot = grafico_temp_alt,
  width = 9,
  height = 7,
  dpi = 300
)
# ==============================================================================
# 13. EXPORTAÇÃO FINAL E VERIFICAÇÕES
# ==============================================================================

# Tabela final simplificada para os estudantes.
tabela_final <- dados_ambientais |>
  dplyr::select(
    dplyr::any_of(c(
      "key", "scientificName", "decimalLatitude", "decimalLongitude",
      "temperature", "temperatura_media_anual_C",
      "precipitacao_anual_mm", "altitude_m"
    ))
  )

readr::write_csv(
  tabela_final,
  file.path(pasta_saida, "tabela_final_ocorrencias_variaveis_ambientais.csv")
)

# Lista os arquivos gerados para facilitar a conferência da execução.
cat("\nArquivos gerados na pasta:", normalizePath(pasta_saida), "\n")
print(list.files(pasta_saida, recursive = TRUE))

################################################################################
# #
# 1. Os pontos do GBIF representam a distribuição completa da espécie no Paraná?
# 2. Por que o número de registros não equivale à abundância da espécie?
# 3. O que os buffers representam? O que eles NÃO representam?
# 4. Por que precisamos de um CRS projetado para calcular buffers em metros?
# 5. O que uma curva de densidade mostra, e por que o eixo y não é uma contagem?
# 6. A distribuição de temperatura nos pontos é igual à disponibilidade de
#    temperaturas na área de estudo? Por quê?
# 7. Que efeitos podem surgir de vieses geográficos, duplicatas ou erros de
#    identificação e georreferenciamento nos dados de ocorrência?
# 8. Por que não podemos chamar essas três curvas, isoladamente, de modelo de
#    nicho ecológico?
#
################################################################################
