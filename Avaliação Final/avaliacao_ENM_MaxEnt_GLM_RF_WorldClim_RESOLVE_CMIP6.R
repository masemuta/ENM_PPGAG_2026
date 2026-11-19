# =====================================================================
# AVALIAÇÃO FINAL — MODELAGEM DE NICHO ECOLÓGICO / DISTRIBUIÇÃO DE ESPÉCIES
# Algoritmos: MaxEnt (maxnet), GLM e Random Forest
# Clima: WorldClim v2.1 | 1970–2000 | 2,5 minutos de arco
# Validação: blocos espaciais (5 folds)
#
# IMPORTANTE
# 1) A área geográfica do exercício é o BRASIL.
# 2) A máscara ecológica pode ser construída com ecorregiões RESOLVE 2017 recortadas
#    pelo limite do Brasil. O aluno pode usar todas ou selecionar ECO_ID(s).
# 3) RESOLVE pode ser baixado automaticamente ou lido de um ZIP local
#    previamente baixado da pasta compartilhada pelo docente.
# 4) MaxEnt usa PRESENÇA-BACKGROUND; GLM/RF usam PRESENÇA-PSEUDOAUSÊNCIA.
#    Os desenhos amostrais permanecem separados em todas as etapas.
# 5) Projeção futura: WorldClim CMIP6, SSP5-8.5 (SSP585), 2081-2100.
#    SSP585 representa uma trajetória de alta emissão/forçamento, não uma previsão.
# 6) Cada decisão metodológica/operacional é acompanhada de DOI para leitura.
#
# REFERÊNCIAS-CHAVE (DOI):
# Área acessível/M: 10.1111/j.1365-2699.2010.02456.x
# Ecorregiões RESOLVE: 10.1093/biosci/bix014
# Limpeza de ocorrências: 10.1111/ecog.01132
# MaxEnt: 10.1016/j.ecolmodel.2005.03.026
# GLM/SDM e teoria ecológica: 10.1111/j.1466-8238.2007.00357.x
# Random Forest: 10.1023/A:1010933404324
# Validação espacial: 10.1111/ecog.02881
# Métricas/limiar TSS: 10.1111/j.1365-2664.2006.01214.x
# WorldClim 2: 10.1002/joc.5086
# CMIP6: 10.5194/gmd-9-1937-2016
# SSPs: 10.1016/j.gloenvcha.2016.05.009
#
# =====================================================================

# ---------------------------------------------------------------------
# 00. PACOTES E CONFIGURAÇÕES
# ---------------------------------------------------------------------

pacotes <- c(
  "tidyverse", "terra", "sf", "geodata", "rgbif",
  "CoordinateCleaner", "maxnet", "randomForest",
  "blockCV", "pROC", "viridis", "corrplot"
)

instalar <- pacotes[!pacotes %in% rownames(installed.packages())]
if (length(instalar) > 0) install.packages(instalar, dependencies = TRUE)

invisible(lapply(pacotes, library, character.only = TRUE))

set.seed(0800) #Semente geral para permitir reprodução

# >>> PERSONALIZAR PELO ALUNO <<<

especie <- "DEFINIR"  # Nome científico aceito pelo GBIF

# O Brasil é a extensão geográfica fixa do exercício.
# A máscara M é definida pelas ecorregiões RESOLVE que o aluno justificar.

# Origem dos dados RESOLVE:
# "download" = baixa automaticamente o ZIP oficial;
# "arquivo_local" = usa um ZIP previamente baixado (p.ex. da pasta compartilhada).
# Pasta compartilhada indicada pode ser baixada no linl do One Drive meu:
# https://1drv.ms/f/c/547d3c1176e36632/IgDufRkPWzTmRL-xEQlpGmWdAQux9oNpp9pINspnx0DFDvg?e=gJx93h

metodo_resolve <- "download"
url_resolve <- "https://storage.googleapis.com/teow2016/Ecoregions2017.zip" #não garanto que vá resolver
arquivo_resolve_local <- "dados/originais/Ecoregions2017.zip"

# M inicial ampla: todas as ecorregiões que intersectam o Brasil.
# Para uma M mais específica, altere para FALSE e informe ECO_ID(s) em eco_ids_M.
usar_todas_ecorregioes_Brasil <- TRUE
eco_ids_M <- NULL  # Exemplo: c(341, 342); consulte a tabela exportada pelo script.

# Diretório principal do projeto.
pasta_projeto <- "Projeto_ENM"

# Parâmetros de download e amostragem.
limite_gbif <- 100000       # Limite máximo consultável em uma busca GBIF
n_background <- 5000        # Número de pontos de background do MaxEnt
proporcao_candidatos_PA <- 15 # Candidatos sorteados por pseudoausência desejada
buffer_PA_km <- 5           # Distância mínima das presenças para pseudoausências

# Validação espacial.
k_folds <- 5
bloco_km <- 100              # Ajustar à escala ecológica e à extensão de M
iteracoes_blocos <- 100

# Modelagem.
classes_maxent <- "lq"       # Linear + quadrática (configuração didática)
regularizacao_maxent <- 1
n_arvores_RF <- 500

# Preditores candidatos (BIO1–BIO19). A seleção final será feita após
# inspeção de correlação e VIF. Esta lista inicial é um ponto de partida.
variaveis_candidatas <- c("bio4", "bio10", "bio12", "bio18")
limite_correlacao <- 0.70
limite_vif <- 5

# Resolução de WorldClim definida para toda a avaliação.
worldclim_versao <- "2.1"
worldclim_resolucao <- 2.5

# Projeção futura: WorldClim CMIP6 na mesma resolução de 2,5 minutos.
# SSP585 = SSP5-8.5 (trajetória de alta emissão/forçamento).
# 2081-2100 é o horizonte de fim de século; altere se a pergunta exigir.
# Um único GCM não representa toda a incerteza entre modelos climáticos.
ssp_futuro <- "585" # este é o cenário mais pessimista
modelo_cmip6 <- "MIROC6" # tem muitos para escolher
periodo_futuro <- "2081-2100" # apenas uma modelagem no "longo prazo"

# Criar estrutura de pastas.
pastas <- c(
  "dados/originais", "dados/processados", "resultados/tabelas",
  "resultados/figuras", "resultados/mapas", "resultados/modelos"
)
for (p in pastas) dir.create(file.path(pasta_projeto, p), recursive = TRUE,
                             showWarnings = FALSE)

setwd(pasta_projeto)

cat("\nProjeto:", especie, "\n")
cat("WorldClim:", worldclim_versao, "| resolução:",
    worldclim_resolucao, "minutos de arco\n")

# ---------------------------------------------------------------------
# 01. BRASIL + ECORREGIÕES RESOLVE 2017 = MÁSCARA FINAL
# ---------------------------------------------------------------------

# 01A. Obter o limite nacional diretamente no R.
# geodata::gadm baixa o limite administrativo do Brasil (nível 0).
# Área acessível/M: DOI 10.1111/j.1365-2699.2010.02456.x
brasil_v <- geodata::gadm(country = "BRA", level = 0,
                          path = "dados/originais/GADM")
brasil_sf <- sf::st_as_sf(brasil_v) |>
  sf::st_make_valid() |>
  sf::st_transform(4326)
brasil_sf <- sf::st_union(brasil_sf) |> sf::st_as_sf()

# 01B. Obter RESOLVE Ecoregions 2017: download automático ou ZIP local.
# Para usar o arquivo compartilhado pelo docente, baixe Ecoregions2017.zip
# e salve no caminho definido em arquivo_resolve_local.
# Fonte/DOI: 10.1093/biosci/bix014
pasta_resolve <- "dados/originais/RESOLVE_2017"
dir.create(pasta_resolve, recursive = TRUE, showWarnings = FALSE)

if (metodo_resolve == "download") {
  zip_resolve <- file.path(pasta_resolve, "Ecoregions2017.zip")
  if (!file.exists(zip_resolve)) {
    utils::download.file(url_resolve, zip_resolve, mode = "wb", quiet = FALSE)
  }
} else if (metodo_resolve == "arquivo_local") {
  if (!file.exists(arquivo_resolve_local)) {
    stop("Não encontrei o ZIP RESOLVE em: ", arquivo_resolve_local,
         "\nBaixe o arquivo compartilhado e ajuste o caminho.")
  }
  zip_resolve <- arquivo_resolve_local
} else {
  stop("metodo_resolve deve ser 'download' ou 'arquivo_local'.")
}

utils::unzip(zip_resolve, exdir = pasta_resolve)
arquivo_shp_resolve <- list.files(pasta_resolve, pattern = "\\.shp$",
                                  recursive = TRUE, full.names = TRUE,
                                  ignore.case = TRUE)
if (length(arquivo_shp_resolve) == 0) {
  stop("Não encontrei um shapefile .shp dentro do ZIP RESOLVE.")
}
resolve_sf <- sf::st_read(arquivo_shp_resolve[1], quiet = TRUE) |>
  sf::st_make_valid() |>
  sf::st_transform(4326)

# Recortar as ecorregiões pelo limite nacional.
ecorregioes_brasil <- sf::st_intersection(resolve_sf, brasil_sf)
if (nrow(ecorregioes_brasil) == 0) stop("Nenhuma ecorregião RESOLVE intersectou o Brasil.")

# Exportar IDs e nomes para inspeção/seleção pelo aluno.
campos_eco <- intersect(c("ECO_ID", "ECO_NAME", "BIOME_NAME", "REALM"),
                        names(ecorregioes_brasil))
readr::write_csv(sf::st_drop_geometry(ecorregioes_brasil) |>
                   dplyr::select(dplyr::all_of(campos_eco)) |>
                   dplyr::distinct(),
                 "resultados/tabelas/ecorregioes_RESOLVE_Brasil.csv")
sf::st_write(ecorregioes_brasil,
             "dados/processados/ecorregioes_RESOLVE_Brasil.gpkg",
             delete_dsn = TRUE, quiet = TRUE)

# A opção TRUE é uma M nacional ampla para o exercício-piloto.
# Para uma M ecologicamente mais defensável, selecione e justifique ECO_ID(s).
if (usar_todas_ecorregioes_Brasil) {
  area_M <- ecorregioes_brasil
} else {
  if (is.null(eco_ids_M) || length(eco_ids_M) == 0) {
    stop("Informe eco_ids_M com um ou mais ECO_ID da tabela exportada.")
  }
  area_M <- ecorregioes_brasil |>
    dplyr::filter(.data$ECO_ID %in% eco_ids_M)
  if (nrow(area_M) == 0) stop("Nenhum ECO_ID selecionado foi encontrado no Brasil.")
}

area_M <- sf::st_union(area_M) |> sf::st_as_sf()
area_M <- sf::st_make_valid(area_M)
area_M <- sf::st_intersection(area_M, brasil_sf)

plot(sf::st_geometry(brasil_sf), col = "grey96", border = "grey40",
     main = "Brasil e máscara ecológica RESOLVE 2017")
plot(sf::st_geometry(area_M), add = TRUE, col = "darkseagreen3",
     border = "darkgreen")

# ---------------------------------------------------------------------
# 02. DOWNLOAD E LIMPEZA DAS OCORRÊNCIAS (GBIF)
# ---------------------------------------------------------------------

# occ_search faz a consulta no GBIF e pagina internamente quando necessário.
# A busca é limitada a registros com coordenadas e sem sinalização geográfica
# de erro pelo GBIF. A limpeza posterior continua sendo necessária.

busca_gbif <- rgbif::occ_search(
  scientificName = especie,
  hasCoordinate = TRUE,
  hasGeospatialIssue = FALSE,
  country = "BR",
  limit = limite_gbif
)

ocorrencias_raw <- busca_gbif$data

if (is.null(ocorrencias_raw) || nrow(ocorrencias_raw) == 0) {
  stop("A consulta ao GBIF não retornou registros. Confira o nome científico.")
}

readr::write_csv(ocorrencias_raw,
                 "dados/originais/ocorrencias_GBIF_brutas.csv")

# Manter campos úteis e padronizar coordenadas.
campos <- c("gbifID", "scientificName", "acceptedScientificName",
            "decimalLongitude", "decimalLatitude",
            "coordinateUncertaintyInMeters", "basisOfRecord",
            "year", "countryCode")
campos_presentes <- intersect(campos, names(ocorrencias_raw))

ocorrencias <- ocorrencias_raw |>
  dplyr::select(dplyr::all_of(campos_presentes)) |>
  dplyr::filter(
    is.finite(decimalLongitude), is.finite(decimalLatitude),
    decimalLongitude >= -180, decimalLongitude <= 180,
    decimalLatitude >= -90, decimalLatitude <= 90
  )

# Excluir registros com incerteza espacial muito elevada (> 10 km), quando
# essa informação estiver disponível. O limite deve ser justificado no relatório.
# DOI (qualidade/avaliação de registros): 10.1111/ecog.01132
if ("coordinateUncertaintyInMeters" %in% names(ocorrencias)) {
  ocorrencias <- ocorrencias |>
    dplyr::filter(is.na(coordinateUncertaintyInMeters) |
                    coordinateUncertaintyInMeters <= 10000)
}

n_gbif_inicial <- nrow(ocorrencias_raw)
n_coordenadas <- nrow(ocorrencias)

# CoordinateCleaner sinaliza registros potencialmente problemáticos.
# Não usamos o teste de outliers automaticamente: pontos extremos podem
# representar ocorrências biologicamente verdadeiras.
resultado_cc <- CoordinateCleaner::clean_coordinates(
  x = as.data.frame(ocorrencias),
  lon = "decimalLongitude",
  lat = "decimalLatitude",
  species = "scientificName",
  tests = c("capitals", "centroids", "equal", "gbif", "institutions", "zeros"),
  value = "clean"
)

ocorrencias_cc <- as_tibble(resultado_cc)

# Restringir os registros ao Brasil e, em seguida, à máscara M RESOLVE.
# DOI para limpeza/rarefação espacial de ocorrências: 10.1111/ecog.01132
ocorrencias_sf <- sf::st_as_sf(
  ocorrencias_cc,
  coords = c("decimalLongitude", "decimalLatitude"),
  crs = 4326,
  remove = FALSE
)

ocorrencias_sf <- sf::st_filter(ocorrencias_sf, area_M, .predicate = sf::st_intersects)

if (nrow(ocorrencias_sf) < 20) {
  stop("Restaram menos de 20 ocorrências após a limpeza e recorte.\n",
       "Reavalie a área M, a taxonomia e os filtros. Não prossiga automaticamente.")
}

# Remover duplicatas exatas de coordenadas.
coords <- sf::st_coordinates(ocorrencias_sf)
ocorrencias_sf$coord_id <- paste(round(coords[, 1], 5),
                                 round(coords[, 2], 5), sep = "_")
ocorrencias_sf <- ocorrencias_sf |>
  dplyr::distinct(coord_id, .keep_all = TRUE)

n_area_M <- nrow(ocorrencias_sf)

readr::write_csv(sf::st_drop_geometry(ocorrencias_sf),
                 "dados/processados/ocorrencias_limpeza.csv")

# ---------------------------------------------------------------------
# 03. DOWNLOAD WORLDCLIM v2.1 — BIOCLIM 1970–2000 / 2,5 MINUTOS (aumentei a resolução um pouco para os mapas ficarem melhores)
# ---------------------------------------------------------------------

# WorldClim v2.1 fornece 19 variáveis bioclimáticas para 1970–2000.
# geodata baixa os arquivos oficiais e os disponibiliza como SpatRaster.

pasta_worldclim <- "dados/originais/WorldClim_v2.1"
dir.create(pasta_worldclim, recursive = TRUE, showWarnings = FALSE)

bio_global <- geodata::worldclim_global(
  var = "bio",
  res = worldclim_resolucao,
  path = pasta_worldclim,
  version = worldclim_versao
)

names(bio_global) <- paste0("bio", 1:terra::nlyr(bio_global))

# BIO1, BIO2 e BIO5–BIO11 (exceto BIO3 e BIO4) são representadas em
# décimos de °C nos rasters bioclimáticos WorldClim. BIO3 e BIO4 são índices.
# A mesma conversão será aplicada ao baseline e ao CMIP6 futuro.
variaveis_temp_decimos <- c("bio1", "bio2", "bio5", "bio6", "bio7",
                            "bio8", "bio9", "bio10", "bio11")
converter_unidades_bioclim <- function(r) {
  nomes_temp <- intersect(variaveis_temp_decimos, names(r))
  if (length(nomes_temp) > 0) r[[nomes_temp]] <- r[[nomes_temp]] / 10
  r
}
bio_global <- converter_unidades_bioclim(bio_global)

# Conferir resolução e CRS antes de qualquer processamento.
print(bio_global)
cat("Resolução do raster (graus):", terra::res(bio_global), "\n")
cat("CRS:", terra::crs(bio_global), "\n")

# Recortar e mascarar pela área M. Não alterar a resolução nativa.
area_v <- terra::vect(area_M)
bio_M <- terra::crop(bio_global, area_v)
bio_M <- terra::mask(bio_M, area_v)

# Salvar o raster recortado para reutilização.
terra::writeRaster(
  bio_M,
  "dados/processados/WorldClim_BIO_areaM.tif",
  overwrite = TRUE
)

# Conferir valores e camadas.
print(names(bio_M))
print(terra::global(bio_M, fun = c("min", "mean", "max"), na.rm = TRUE))

# ---------------------------------------------------------------------
# 04. EXTRAÇÃO DOS PREDITORES E CONTROLE DE COLINEARIDADE
# ---------------------------------------------------------------------

variaveis_candidatas <- tolower(variaveis_candidatas)
if (!all(variaveis_candidatas %in% names(bio_M))) {
  stop("Uma ou mais variáveis candidatas não existem no raster WorldClim.")
}

# Extrair as variáveis climáticas nos locais de ocorrência.
pres_env_todos <- terra::extract(bio_M[[variaveis_candidatas]],
                                  terra::vect(ocorrencias_sf)) |>
  as_tibble() |>
  dplyr::select(-ID) |>
  dplyr::filter(dplyr::if_all(dplyr::everything(), is.finite))

# Identificar células raster repetidas: apenas uma presença por célula.
xy_pres <- sf::st_coordinates(ocorrencias_sf)
cells_pres <- terra::cellFromXY(bio_M[[1]], xy_pres)
keep_cell <- !duplicated(cells_pres) & !is.na(cells_pres)

ocorrencias_sf <- ocorrencias_sf[keep_cell, ]
xy_pres <- sf::st_coordinates(ocorrencias_sf)
pres_env <- terra::extract(bio_M[[variaveis_candidatas]],
                            terra::vect(ocorrencias_sf)) |>
  as_tibble() |>
  dplyr::select(-ID) |>
  dplyr::filter(dplyr::if_all(dplyr::everything(), is.finite))

# Alinhar o objeto espacial ao conjunto de valores ambientais válidos.
valid_pres <- terra::extract(bio_M[[variaveis_candidatas]],
                              terra::vect(ocorrencias_sf)) |>
  dplyr::select(-ID) |>
  as.data.frame() |>
  stats::complete.cases()
ocorrencias_sf <- ocorrencias_sf[valid_pres, ]
pres_env <- terra::extract(bio_M[[variaveis_candidatas]],
                            terra::vect(ocorrencias_sf)) |>
  as_tibble() |>
  dplyr::select(-ID) |>
  tidyr::drop_na()

if (nrow(pres_env) < 20) stop("Poucas presenças com dados ambientais completos.")

# Correlação de Pearson: relatório para apoiar a seleção das variáveis.
# |r| >= 0,70 e VIF < 5 são referências heurísticas, não leis universais.
# DOI: 10.1111/j.1600-0587.2012.07348.x
mat_cor <- stats::cor(pres_env, use = "pairwise.complete.obs",
                      method = "pearson")
readr::write_csv(as.data.frame(mat_cor) |> tibble::rownames_to_column("variavel"),
                 "resultados/tabelas/matriz_correlacao.csv")

png("resultados/figuras/matriz_correlacao.png", width = 1800,
    height = 1600, res = 200)
corrplot::corrplot(mat_cor, method = "color", type = "upper",
                             tl.col = "black", tl.cex = 0.8,
                             addCoef.col = "black", number.cex = 0.6)
dev.off()

# Seleção automática didática: remove iterativamente a variável com maior
# correlação absoluta acima do limite. A decisão final deve ser ecológica.
selecionar_cor <- function(dados, limite = 0.70) {
  vars <- names(dados)
  repeat {
    if (length(vars) <= 2) break
    m <- abs(stats::cor(dados[, vars, drop = FALSE], use = "pairwise.complete.obs"))
    diag(m) <- 0
    maior <- max(m, na.rm = TRUE)
    if (!is.finite(maior) || maior < limite) break
    par <- which(m == maior, arr.ind = TRUE)[1, ]
    # Remove a segunda variável do par; o aluno pode alterar a decisão
    # considerando a relevância ecológica de cada preditor.
    vars <- setdiff(vars, vars[par[2]])
  }
  vars
}

vars_cor <- selecionar_cor(pres_env, limite_correlacao)

# Calcular VIF por regressões auxiliares e remover iterativamente a variável
# com maior VIF até atingir o limite definido.
calcular_vif <- function(dados) {
  vars <- names(dados)
  if (length(vars) < 2) return(tibble::tibble(variavel = vars, VIF = 1))
  purrr::map_dfr(vars, function(v) {
    outros <- setdiff(vars, v)
    mod <- stats::lm(stats::reformulate(outros, response = v), data = dados)
    r2 <- summary(mod)$r.squared
    tibble::tibble(variavel = v, VIF = 1 / (1 - r2))
  })
}

selecionar_vif <- function(dados, limite = 5) {
  vars <- names(dados)
  repeat {
    vif_atual <- calcular_vif(dados[, vars, drop = FALSE])
    if (nrow(vif_atual) < 2 || max(vif_atual$VIF, na.rm = TRUE) <= limite) break
    remover <- vif_atual$variavel[which.max(vif_atual$VIF)]
    vars <- setdiff(vars, remover)
  }
  list(variaveis = vars, vif = calcular_vif(dados[, vars, drop = FALSE]))
}

resultado_vif <- selecionar_vif(pres_env[, vars_cor, drop = FALSE], limite_vif)
variaveis_modelo <- resultado_vif$variaveis

if (length(variaveis_modelo) < 2) {
  stop("Menos de duas variáveis restaram após o controle de colinearidade.",
       " Reavalie as variáveis candidatas e os limites definidos.")
}

readr::write_csv(resultado_vif$vif, "resultados/tabelas/VIF_final.csv")
readr::write_csv(tibble::tibble(variavel = variaveis_modelo),
                 "resultados/tabelas/variaveis_selecionadas.csv")

cat("\nVariáveis selecionadas:\n")
print(variaveis_modelo)

# Manter somente os preditores selecionados no raster e nas presenças.
env <- bio_M[[variaveis_modelo]]
pres_env <- terra::extract(env, terra::vect(ocorrencias_sf)) |>
  as_tibble() |>
  dplyr::select(-ID) |>
  tidyr::drop_na()

# ---------------------------------------------------------------------
# 05. CONSTRUIR OS DOIS DESENHOS AMOSTRAIS INDEPENDENTES
# ---------------------------------------------------------------------

# 05A. MAXENT: presença-background -----------------------------------

# Sortear background em células válidas dentro da área M.
# Background representa o ambiente disponível, não uma ausência confirmada.
# DOI: 10.1890/07-2153.1
background_pts <- terra::spatSample(
  env[[1]], size = n_background, method = "random",
  na.rm = TRUE, as.points = TRUE, values = FALSE
)

# Evitar que um ponto de background coincida com célula de presença.
cell_background <- terra::cellFromXY(env[[1]], terra::crds(background_pts))
cell_presence <- terra::cellFromXY(env[[1]], sf::st_coordinates(ocorrencias_sf))
keep_bg <- !cell_background %in% cell_presence & !is.na(cell_background)
background_pts <- background_pts[keep_bg]

bg_extraido <- terra::extract(env, background_pts) |>
  as_tibble()
keep_bg_valido <- stats::complete.cases(bg_extraido[, -1, drop = FALSE])
background_pts <- background_pts[keep_bg_valido]
background_env <- bg_extraido[keep_bg_valido, , drop = FALSE] |>
  dplyr::select(-ID)

if (nrow(background_env) < 100) stop("Poucos pontos de background disponíveis.")

# Conjunto MaxEnt: 1 = presença; 0 = background.
dados_maxent <- dplyr::bind_rows(
  pres_env |> dplyr::mutate(resposta = 1L),
  background_env |> dplyr::mutate(resposta = 0L)
) |>
  tidyr::drop_na() |>
  dplyr::mutate(resposta = as.integer(resposta))

# 05B. GLM e RANDOM FOREST: presença-pseudoausência ---------------

# Gerar candidatos a pseudoausência na área M e afastá-los das presenças.
# Razão 1:1 e buffer são decisões didáticas que precisam ser justificadas.
# DOI: 10.1111/j.1466-8238.2007.00357.x
# A distância é calculada em CRS projetado métrico (EPSG:6933).
n_pa_desejado <- nrow(pres_env)
n_candidatos <- min(n_pa_desejado * proporcao_candidatos_PA, 50000)

candidatos_pa <- terra::spatSample(
  env[[1]], size = n_candidatos, method = "random",
  na.rm = TRUE, as.points = TRUE, values = FALSE
)

cand_sf <- sf::st_as_sf(candidatos_pa) |> sf::st_transform(6933)
pres_sf_m <- sf::st_transform(ocorrencias_sf, 6933)

proximos <- sf::st_is_within_distance(
  cand_sf, pres_sf_m, dist = buffer_PA_km * 1000
)
keep_pa <- lengths(proximos) == 0
cand_sf <- cand_sf[keep_pa, ]

if (nrow(cand_sf) < n_pa_desejado) {
  stop("Não há pseudoausências suficientes após aplicar o buffer.\n",
       "Reduza a distância, amplie a área M ou revise o número de presenças.")
}

cand_sf <- cand_sf[sample(seq_len(nrow(cand_sf)), n_pa_desejado), ]

cand_sf_4326 <- sf::st_transform(cand_sf, 4326)
pa_extraido <- terra::extract(env, terra::vect(cand_sf_4326)) |>
  as_tibble()
keep_pa_valido <- stats::complete.cases(pa_extraido[, -1, drop = FALSE])
cand_sf_4326 <- cand_sf_4326[keep_pa_valido, ]
pseudo_env <- pa_extraido[keep_pa_valido, , drop = FALSE] |>
  dplyr::select(-ID)

# Conjunto GLM/RF: 1 = presença; 0 = pseudoausência.
dados_pa <- dplyr::bind_rows(
  pres_env |> dplyr::mutate(resposta = 1L),
  pseudo_env |> dplyr::mutate(resposta = 0L)
) |>
  tidyr::drop_na() |>
  dplyr::mutate(resposta = as.integer(resposta))

# Exportar amostras para auditoria e reprodutibilidade.
readr::write_csv(dados_maxent, "dados/processados/amostras_MaxEnt_presenca_background.csv")
readr::write_csv(dados_pa, "dados/processados/amostras_GLM_RF_presenca_pseudoausencia.csv")

cat("\nAmostras MaxEnt — presença:", sum(dados_maxent$resposta == 1),
    "| background:", sum(dados_maxent$resposta == 0), "\n")
cat("Amostras GLM/RF — presença:", sum(dados_pa$resposta == 1),
    "| pseudoausência:", sum(dados_pa$resposta == 0), "\n")

# ---------------------------------------------------------------------
# 06. FUNÇÕES DE VALIDAÇÃO ESPACIAL E MÉTRICAS
# ---------------------------------------------------------------------

criar_folds_espaciais <- function(dados, coords_sf, nome_desenho) {
  if (nrow(dados) != nrow(coords_sf)) {
    stop("Dados e coordenadas não têm o mesmo número de linhas em ", nome_desenho)
  }
  if (length(unique(dados$resposta)) < 2) stop("Amostra sem as duas classes.")

  # A validação espacial reduz a dependência entre treino e teste causada
  # pela autocorrelação espacial; justificar o tamanho do bloco.
  # DOI: 10.1111/ecog.02881
  cv <- blockCV::cv_spatial(
    x = coords_sf,
    column = "resposta",
    k = k_folds,
    size = bloco_km * 1000,
    selection = "random",
    iteration = iteracoes_blocos,
    seed = 0800,
    biomod2 = FALSE,
    progress = TRUE,
    report = TRUE,
    plot = TRUE
  )

  if (length(cv$folds_ids) != nrow(dados)) {
    stop("A quantidade de IDs de folds não corresponde às observações.")
  }
  if (length(unique(stats::na.omit(cv$folds_ids))) < 2) {
    stop("A validação espacial produziu menos de dois folds utilizáveis.")
  }

  readr::write_csv(
    tibble::tibble(id = seq_along(cv$folds_ids), fold = cv$folds_ids),
    paste0("resultados/tabelas/folds_", nome_desenho, ".csv")
  )
  cv$folds_ids
}

# Criar coordenadas correspondentes a cada amostra.
# As presenças são representadas pelos pontos de ocorrência e os pontos
# de background/pseudoausência pelos pontos amostrados nos rasters.

# Coordenadas ambientais de presenças e pontos negativos.
xy_pres <- sf::st_coordinates(ocorrencias_sf)
xy_bg <- terra::crds(background_pts)
xy_pa <- sf::st_coordinates(cand_sf_4326)

coords_maxent <- sf::st_as_sf(
  data.frame(x = c(xy_pres[, 1], xy_bg[, 1]),
             y = c(xy_pres[, 2], xy_bg[, 2])),
  coords = c("x", "y"), crs = 4326
) |>
  dplyr::mutate(resposta = dados_maxent$resposta)

coords_glmrf <- sf::st_as_sf(
  data.frame(x = c(xy_pres[, 1], xy_pa[, 1]),
             y = c(xy_pres[, 2], xy_pa[, 2])),
  coords = c("x", "y"), crs = 4326
) |>
  dplyr::mutate(resposta = dados_pa$resposta)

folds_maxent <- criar_folds_espaciais(dados_maxent, coords_maxent, "MaxEnt")
folds_glmrf <- criar_folds_espaciais(dados_pa, coords_glmrf, "GLM_RF")

# Calcular AUC, sensibilidade, especificidade e TSS.
calcular_metricas <- function(observado, probabilidade, limiar) {
  observado <- as.integer(observado)
  classe <- ifelse(probabilidade >= limiar, 1L, 0L)
  tab <- table(
    factor(observado, levels = c(0, 1)),
    factor(classe, levels = c(0, 1))
  )
  tn <- tab[1, 1]; fp <- tab[1, 2]
  fn <- tab[2, 1]; tp <- tab[2, 2]
  sens <- ifelse((tp + fn) > 0, tp / (tp + fn), NA_real_)
  espec <- ifelse((tn + fp) > 0, tn / (tn + fp), NA_real_)
  tss <- sens + espec - 1
  auc <- tryCatch(
    as.numeric(pROC::auc(pROC::roc(observado, probabilidade,
                                  levels = c(0, 1), direction = "<",
                                  quiet = TRUE))),
    error = function(e) NA_real_
  )
  tibble::tibble(AUC = auc, sensibilidade = sens,
                 especificidade = espec, TSS = tss,
                 limiar = limiar, TN = tn, FP = fp, FN = fn, TP = tp)
}

# Escolher limiar maximizando TSS nos dados de treinamento.
# TSS combina sensibilidade e especificidade; o limiar não é universal.
# DOI: 10.1111/j.1365-2664.2006.01214.x
# O teste espacial permanece intocado até a avaliação final do fold.
escolher_limiar_tss <- function(observado, probabilidade) {
  limiares <- seq(0.05, 0.95, by = 0.01)
  valores <- purrr::map_dfr(limiares, function(l) {
    m <- calcular_metricas(observado, probabilidade, l)
    tibble::tibble(limiar = l, TSS = m$TSS)
  })
  valores$limiar[which.max(valores$TSS)]
}

# ---------------------------------------------------------------------
# 07. AJUSTE E VALIDAÇÃO DOS TRÊS ALGORITMOS
# ---------------------------------------------------------------------

# MaxEnt: presença-background. lq e regularização 1 são parâmetros iniciais
# para ensino, não uma combinação ótima universal.
# DOI: 10.1016/j.ecolmodel.2005.03.026
ajustar_maxent <- function(dados) {
  x <- dados |> dplyr::select(dplyr::all_of(variaveis_modelo))
  p <- dados$resposta == 1
  f <- maxnet::maxnet.formula(p, x, classes = classes_maxent)
  maxnet::maxnet(p = p, data = x, f = f,
                 regmult = regularizacao_maxent)
}

pred_maxent <- function(modelo, dados) {
  as.numeric(stats::predict(modelo,
                            newdata = dados |> dplyr::select(dplyr::all_of(variaveis_modelo)),
                            type = "cloglog"))
}

ajustar_glm <- function(dados) {
  formula <- stats::reformulate(variaveis_modelo, response = "resposta")
  stats::glm(formula, data = dados, family = stats::binomial())
}

pred_glm <- function(modelo, dados) {
  as.numeric(stats::predict(modelo, newdata = dados, type = "response"))
}

# Random Forest: 500 árvores como configuração inicial reproduzível.
# DOI: 10.1023/A:1010933404324
ajustar_rf <- function(dados) {
  dados_rf <- dados |>
    dplyr::mutate(resposta = factor(resposta, levels = c(0, 1)))
  formula <- stats::reformulate(variaveis_modelo, response = "resposta")
  randomForest::randomForest(
    formula, data = dados_rf, ntree = n_arvores_RF,
    importance = TRUE
  )
}

pred_rf <- function(modelo, dados) {
  as.numeric(stats::predict(modelo, newdata = dados,
                            type = "prob")[, "1"])
}

validar_algoritmo <- function(dados, folds, algoritmo) {
  resultados <- list()
  modelos <- list()

  for (f in sort(unique(stats::na.omit(folds)))) {
    idx_treino <- which(folds != f & !is.na(folds))
    idx_teste <- which(folds == f)
    treino <- dados[idx_treino, , drop = FALSE]
    teste <- dados[idx_teste, , drop = FALSE]

    if (length(unique(treino$resposta)) < 2 ||
        length(unique(teste$resposta)) < 2) {
      warning("Fold ", f, " ignorado: uma das partições não contém as duas classes.")
      next
    }

    modelo <- switch(
      algoritmo,
      MaxEnt = ajustar_maxent(treino),
      GLM = ajustar_glm(treino),
      RF = ajustar_rf(treino)
    )

    pred_treino <- switch(
      algoritmo,
      MaxEnt = pred_maxent(modelo, treino),
      GLM = pred_glm(modelo, treino),
      RF = pred_rf(modelo, treino)
    )
    pred_teste <- switch(
      algoritmo,
      MaxEnt = pred_maxent(modelo, teste),
      GLM = pred_glm(modelo, teste),
      RF = pred_rf(modelo, teste)
    )

    limiar <- escolher_limiar_tss(treino$resposta, pred_treino)
    met <- calcular_metricas(teste$resposta, pred_teste, limiar) |>
      dplyr::mutate(algoritmo = algoritmo, fold = f,
                    n_treino = nrow(treino), n_teste = nrow(teste))

    resultados[[as.character(f)]] <- met
    modelos[[as.character(f)]] <- modelo
  }

  list(metricas = dplyr::bind_rows(resultados), modelos = modelos)
}

# MaxEnt: somente presença-background.
resultado_maxent <- validar_algoritmo(dados_maxent, folds_maxent, "MaxEnt")

# GLM e RF: somente presença-pseudoausência.
resultado_glm <- validar_algoritmo(dados_pa, folds_glmrf, "GLM")
resultado_rf <- validar_algoritmo(dados_pa, folds_glmrf, "RF")

metricas_cv <- dplyr::bind_rows(
  resultado_maxent$metricas,
  resultado_glm$metricas,
  resultado_rf$metricas
)

readr::write_csv(metricas_cv, "resultados/tabelas/metricas_validacao_espacial.csv")

resumo_cv <- metricas_cv |>
  dplyr::group_by(algoritmo) |>
  dplyr::summarise(
    dplyr::across(c(AUC, sensibilidade, especificidade, TSS),
                  list(media = ~mean(.x, na.rm = TRUE),
                       desvio = ~sd(.x, na.rm = TRUE))),
    n_folds = dplyr::n(), .groups = "drop"
  )
readr::write_csv(resumo_cv, "resultados/tabelas/resumo_metricas_validacao.csv")
print(resumo_cv)

# ---------------------------------------------------------------------
# 08. AJUSTE FINAL COM TODOS OS DADOS E MAPAS DE ADEQUABILIDADE
# ---------------------------------------------------------------------

# O ajuste final usa todos os dados do desenho amostral correspondente.
modelo_maxent_final <- ajustar_maxent(dados_maxent)
modelo_glm_final <- ajustar_glm(dados_pa)
modelo_rf_final <- ajustar_rf(dados_pa)

saveRDS(modelo_maxent_final, "resultados/modelos/modelo_MaxEnt_final.rds")
saveRDS(modelo_glm_final, "resultados/modelos/modelo_GLM_final.rds")
saveRDS(modelo_rf_final, "resultados/modelos/modelo_RF_final.rds")

# Predição espacial. As funções garantem que cada algoritmo receba apenas
# as variáveis selecionadas e devolva um valor contínuo por célula.
r_maxent <- terra::predict(
  env, modelo_maxent_final,
  fun = function(model, data) stats::predict(model, newdata = data,
                                              type = "cloglog"),
  na.rm = TRUE
)

r_glm <- terra::predict(
  env, modelo_glm_final,
  type = "response", na.rm = TRUE
)

r_rf <- terra::predict(
  env, modelo_rf_final,
  fun = function(model, data) stats::predict(model, newdata = data,
                                              type = "prob")[, "1"],
  na.rm = TRUE
)

names(r_maxent) <- "MaxEnt"
names(r_glm) <- "GLM"
names(r_rf) <- "RandomForest"

terra::writeRaster(r_maxent, "resultados/mapas/adequabilidade_MaxEnt.tif", overwrite = TRUE)
terra::writeRaster(r_glm, "resultados/mapas/adequabilidade_GLM.tif", overwrite = TRUE)
terra::writeRaster(r_rf, "resultados/mapas/adequabilidade_RF.tif", overwrite = TRUE)

# ---------------------------------------------------------------------
# 08B. PROJEÇÃO CLIMÁTICA FUTURA — CMIP6 / SSP5-8.5
# ---------------------------------------------------------------------

# WorldClim fornece projeções CMIP6 downscaled e calibradas com WorldClim v2.1.
# Usamos os mesmos preditores, resolução e máscara do período de referência.
# DOI CMIP6: 10.5194/gmd-9-1937-2016 | DOI SSP: 10.1016/j.gloenvcha.2016.05.009

pasta_cmip6 <- "dados/originais/WorldClim_CMIP6"
dir.create(pasta_cmip6, recursive = TRUE, showWarnings = FALSE)

bio_futuro_global <- geodata::cmip6_world(
  model = modelo_cmip6,
  ssp = ssp_futuro,
  time = periodo_futuro,
  var = "bioc",
  res = worldclim_resolucao,
  path = pasta_cmip6
)
if (is.null(bio_futuro_global)) {
  stop("O download CMIP6 retornou NULL. Confira GCM, SSP e período disponíveis.")
}
names(bio_futuro_global) <- paste0("bio", seq_len(terra::nlyr(bio_futuro_global)))
bio_futuro_global <- converter_unidades_bioclim(bio_futuro_global)

if (!all(variaveis_modelo %in% names(bio_futuro_global))) {
  stop("Nem todas as variáveis do modelo estão disponíveis no raster futuro.")
}

bio_futuro_M <- terra::crop(bio_futuro_global[[variaveis_modelo]],
                            terra::vect(area_M))
bio_futuro_M <- terra::mask(bio_futuro_M, terra::vect(area_M))

# Transferir os modelos calibrados no clima de referência para o clima futuro.
# As projeções são condicionais ao GCM/SSP/período, não previsões determinísticas.
r_maxent_futuro <- terra::predict(
  bio_futuro_M, modelo_maxent_final,
  fun = function(model, data) stats::predict(model, newdata = data,
                                              type = "cloglog"),
  na.rm = TRUE
)
r_glm_futuro <- terra::predict(
  bio_futuro_M, modelo_glm_final, type = "response", na.rm = TRUE
)
r_rf_futuro <- terra::predict(
  bio_futuro_M, modelo_rf_final,
  fun = function(model, data) stats::predict(model, newdata = data,
                                              type = "prob")[, "1"],
  na.rm = TRUE
)

names(r_maxent_futuro) <- "MaxEnt"
names(r_glm_futuro) <- "GLM"
names(r_rf_futuro) <- "RandomForest"
prefixo_futuro <- paste0("CMIP6_SSP", ssp_futuro, "_", modelo_cmip6, "_", periodo_futuro)
terra::writeRaster(r_maxent_futuro,
  file.path("resultados/mapas", paste0("adequabilidade_MaxEnt_", prefixo_futuro, ".tif")),
  overwrite = TRUE)
terra::writeRaster(r_glm_futuro,
  file.path("resultados/mapas", paste0("adequabilidade_GLM_", prefixo_futuro, ".tif")),
  overwrite = TRUE)
terra::writeRaster(r_rf_futuro,
  file.path("resultados/mapas", paste0("adequabilidade_RF_", prefixo_futuro, ".tif")),
  overwrite = TRUE)

# ---------------------------------------------------------------------
# 09. LIMIAR FINAL, MAPAS BINÁRIOS E CONSENSO
# ---------------------------------------------------------------------

# Limiar representativo: mediana dos limiares encontrados nos folds
# de validação. Os mapas binários são produtos auxiliares; os mapas
# contínuos devem permanecer disponíveis para interpretação.
limiares_finais <- metricas_cv |>
  dplyr::group_by(algoritmo) |>
  dplyr::summarise(limiar = median(limiar, na.rm = TRUE), .groups = "drop")
readr::write_csv(limiares_finais, "resultados/tabelas/limiares_finais.csv")

lim_maxent <- limiares_finais$limiar[limiares_finais$algoritmo == "MaxEnt"]
lim_glm <- limiares_finais$limiar[limiares_finais$algoritmo == "GLM"]
lim_rf <- limiares_finais$limiar[limiares_finais$algoritmo == "RF"]

bin_maxent <- r_maxent >= lim_maxent
bin_glm <- r_glm >= lim_glm
bin_rf <- r_rf >= lim_rf

names(bin_maxent) <- "MaxEnt_binario"
names(bin_glm) <- "GLM_binario"
names(bin_rf) <- "RF_binario"

terra::writeRaster(bin_maxent, "resultados/mapas/binario_MaxEnt.tif", overwrite = TRUE)
terra::writeRaster(bin_glm, "resultados/mapas/binario_GLM.tif", overwrite = TRUE)
terra::writeRaster(bin_rf, "resultados/mapas/binario_RF.tif", overwrite = TRUE)

# Frequência de concordância: 0 = nenhum algoritmo classificou como adequado;
# 1/3, 2/3 ou 1 = concordância crescente. Não é probabilidade de ocorrência.
consenso <- (bin_maxent + bin_glm + bin_rf) / 3
names(consenso) <- "frequencia_concordancia"
terra::writeRaster(consenso, "resultados/mapas/consenso_tres_algoritmos.tif",
                   overwrite = TRUE)

# ---------------------------------------------------------------------
# 10. IMPORTÂNCIA DAS VARIÁVEIS E FIGURAS FINAIS
# ---------------------------------------------------------------------

# Importância do Random Forest (Mean Decrease in Accuracy / Gini).
imp_rf <- randomForest::importance(modelo_rf_final) |>
  as.data.frame() |>
  tibble::rownames_to_column("variavel")
readr::write_csv(imp_rf, "resultados/tabelas/importancia_RF.csv")

# Curvas de resposta simples: variar um preditor mantendo os demais
# na mediana observada no conjunto de treinamento completo.
curvas_resposta <- function(modelo, dados, algoritmo, n = 100) {
  purrr::map_dfr(variaveis_modelo, function(v) {
    grade <- tibble::as_tibble(
      as.list(stats::setNames(as.list(vapply(dados[variaveis_modelo],
        stats::median, numeric(1), na.rm = TRUE)), variaveis_modelo))
    )
    grade <- grade[rep(1, n), , drop = FALSE]
    grade[[v]] <- seq(min(dados[[v]], na.rm = TRUE),
                      max(dados[[v]], na.rm = TRUE), length.out = n)
    pred <- switch(
      algoritmo,
      MaxEnt = pred_maxent(modelo, grade),
      GLM = pred_glm(modelo, grade),
      RF = pred_rf(modelo, grade)
    )
    tibble::tibble(algoritmo = algoritmo, variavel = v,
                   valor = grade[[v]], adequabilidade = pred)
  })
}

curvas <- dplyr::bind_rows(
  curvas_resposta(modelo_maxent_final, dados_maxent, "MaxEnt"),
  curvas_resposta(modelo_glm_final, dados_pa, "GLM"),
  curvas_resposta(modelo_rf_final, dados_pa, "RF")
)
readr::write_csv(curvas, "resultados/tabelas/curvas_resposta.csv")

fig_curvas <- ggplot2::ggplot(curvas,
  ggplot2::aes(x = valor, y = adequabilidade, colour = algoritmo)) +
  ggplot2::geom_line(linewidth = 0.8) +
  ggplot2::facet_wrap(~variavel, scales = "free_x") +
  ggplot2::scale_colour_viridis_d(option = "D", end = 0.9) +
  ggplot2::labs(x = "Valor do preditor", y = "Adequabilidade predita",
                colour = "Algoritmo") +
  ggplot2::theme_bw()

ggplot2::ggsave("resultados/figuras/curvas_resposta.png", fig_curvas,
                width = 10, height = 7, dpi = 300)

# Mapa de consenso para inspeção rápida.
png("resultados/figuras/consenso.png", width = 1800, height = 1400, res = 200)
plot(consenso, col = viridis::viridis(4), main = "Concordância entre algoritmos",
     axes = TRUE)
plot(terra::vect(area_M), add = TRUE, border = "grey20", lwd = 0.7)
dev.off()

# ---------------------------------------------------------------------
# 11. AUDITORIA, RESUMO E EXPORTAÇÃO
# ---------------------------------------------------------------------

resumo_dados <- tibble::tibble(
  etapa = c("Registros retornados pelo GBIF", "Registros com coordenadas válidas",
            "Registros após limpeza, área M e duplicatas", "Presenças com dados ambientais"),
  n = c(n_gbif_inicial, n_coordenadas, n_area_M, nrow(pres_env))
)
readr::write_csv(resumo_dados, "resultados/tabelas/auditoria_ocorrencias.csv")

configuracao <- tibble::tibble(
  especie = especie,
  worldclim = paste0("v", worldclim_versao),
  periodo = "1970-2000",
  resolucao_minutos = worldclim_resolucao,
  area_geografica = "Brasil",
  mascara = "RESOLVE Ecoregions 2017 recortadas pelo Brasil",
  todas_ecorregioes_brasil = usar_todas_ecorregioes_Brasil,
  eco_ids_M = ifelse(is.null(eco_ids_M), "todas", paste(eco_ids_M, collapse = ";")),
  ssp_futuro = paste0("SSP", ssp_futuro),
  gcm_futuro = modelo_cmip6,
  periodo_futuro = periodo_futuro,
  variaveis = paste(variaveis_modelo, collapse = "; "),
  n_background = n_background,
  proporcao_presenca_pseudoausencia = "1:1",
  buffer_pseudoausencia_km = buffer_PA_km,
  folds = k_folds,
  tamanho_bloco_km = bloco_km,
  classes_maxent = classes_maxent,
  regularizacao_maxent = regularizacao_maxent,
  arvores_random_forest = n_arvores_RF,
  seed = 0800
)
readr::write_csv(configuracao, "resultados/tabelas/configuracao_modelagem.csv")

cat("\n============================================================\n")
cat("AVALIAÇÃO FINAL — EXECUÇÃO CONCLUÍDA\n")
cat("============================================================\n")
cat("Espécie:", especie, "\n")
cat("Ocorrências após limpeza:", nrow(ocorrencias_sf), "\n")
cat("Variáveis ambientais:", paste(variaveis_modelo, collapse = ", "), "\n")
cat("Amostras MaxEnt:", nrow(dados_maxent), "\n")
cat("Amostras GLM/RF:", nrow(dados_pa), "\n")
cat("Modelos ajustados: MaxEnt, GLM e Random Forest\n")
cat("Projeção futura: CMIP6 / SSP", ssp_futuro, " / ", modelo_cmip6,
    " / ", periodo_futuro, "\n", sep = "")
cat("Resultados salvos em:", normalizePath("resultados"), "\n")

# FIM DO SCRIPT
