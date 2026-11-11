# =============================================================================
# AULA 02 — DIMENSÕES DO NICHO ECOLÓGICO E DIAGRAMA BAM
# Comparação: Araucaria angustifolia (pinhão) e Triticum aestivum (trigo)
# Mário S. M. Tagliari | PPGAG — UTFPR, Campus Pato Branco
#
# OBJETIVO DESTA PRIMEIRA ETAPA
# Construir uma comparação conceitual entre:
#   A = condições abióticas
#   B = condições/interações bióticas
#   M = áreas acessíveis à espécie por dispersão ou colonização
#
# IMPORTANTE:
# O objeto do BAM é a distribuição geográfica do organismo, não diretamente
# a distribuição do produto. Assim, estudamos Araucaria angustifolia para
# discutir o sistema do pinhão e Triticum aestivum para discutir o trigo.
#
# A dimensão antropogênica/manejo será apresentada separadamente. Ela não é
# um quarto componente do BAM clássico.
#
# Os diagramas são ESQUEMÁTICOS: não representam áreas geográficas, proporções,
# probabilidades ou interseções estimadas.
# =============================================================================


# 0. PREPARAÇÃO DO AMBIENTE ----------------------------------------------------

# Instale somente os pacotes que ainda não estiverem disponíveis.
pacotes <- c("tidyverse", "ggplot2", "patchwork")
pacotes_ausentes <- pacotes[!pacotes %in% rownames(installed.packages())]
if (length(pacotes_ausentes) > 0) install.packages(pacotes_ausentes)

library(tidyverse)
library(ggplot2)
library(patchwork)


# 1. ORGANIZAÇÃO DO PROJETO ----------------------------------------------------

# Defina o diretório de trabalho antes de executar, se desejar.
# Exemplo: setwd("C:/Users/SeuUsuario/Documents/Aula_02_BAM")

# Criamos uma pasta para guardar as tabelas e figuras geradas.
if (!dir.exists("./Aula_02/resultados_aula02")) {
  dir.create("./Aula_02/resultados_aula02")
}


# 2. DEFINIÇÃO DOS SISTEMAS ----------------------------------------------------

# Nesta etapa não atribuímos valores numéricos às variáveis.
# Organizamos os processos que serão discutidos em cada sistema.
sistemas <- tibble::tribble(
  ~sistema, ~especie, ~produto, ~contexto,
  "Araucária/pinhão", "Araucaria angustifolia", "Pinhão",
  "Espécie arbórea nativa; coleta de sementes em populações naturais ou manejadas",
  "Trigo", "Triticum aestivum", "Grãos",
  "Espécie domesticada; cultivo agrícola conduzido por decisões humanas"
)
print(sistemas)


# 3. MATRIZ CONCEITUAL DOS COMPONENTES A, B E M -------------------------------

# A — Abiotic: condições ambientais que permitem ou limitam a ocorrência.
# B — Biotic: interações com outros organismos que podem favorecer ou limitar.
# M — Movement: acessibilidade por dispersão ou colonização.
#
# No trigo, a introdução/transporte de sementes por pessoas é um mecanismo
# antropogênico de acesso. Não deve ser confundido com dispersão natural.
# A tabela é uma síntese didática, não uma mensuração empírica.

matriz_bam <- tibble::tribble(
  ~sistema, ~componente, ~processo, ~exemplos,
  "Araucária/pinhão", "A — Abiótico", "Condições ambientais",
  "Temperatura; precipitação; sazonalidade; altitude; solo",
  "Araucária/pinhão", "B — Biótico", "Interações ecológicas",
  "Dispersores; predadores de sementes; competidores; interações na regeneração",
  "Araucária/pinhão", "M — Movimento", "Dispersão e acessibilidade",
  "Dispersão de sementes; conectividade entre fragmentos; barreiras à colonização",
  "Trigo", "A — Abiótico", "Condições ambientais",
  "Temperatura durante o ciclo; precipitação; déficit hídrico; solo; altitude",
  "Trigo", "B — Biótico", "Interações ecológicas",
  "Patógenos; insetos-praga; plantas daninhas; microrganismos do solo",
  "Trigo", "M — Movimento", "Introdução e acessibilidade",
  "Transporte de sementes; introdução por ação humana; disponibilidade de sementes"
)
print(matriz_bam)
readr::write_csv(matriz_bam, "resultados_aula02/matriz_conceitual_BAM.csv")


# 4. DIMENSÃO ANTROPOGÊNICA E DE MANEJO ---------------------------------------

# Registramos separadamente fatores que influenciam o aproveitamento dos
# recursos, a distribuição observada e o desempenho produtivo.
# Não os inserimos automaticamente em A, B ou M: essa separação é deliberada.
manejo_humano <- tibble::tribble(
  ~sistema, ~dimensao, ~exemplos,
  "Araucária/pinhão", "Coleta e manejo",
  "Época da coleta; intensidade; quantidade de sementes removidas",
  "Araucária/pinhão", "Manejo e conservação",
  "Proteção de árvores reprodutivas; condução da regeneração; conservação florestal",
  "Araucária/pinhão", "Acesso e comercialização",
  "Acesso aos remanescentes; mão de obra; transporte; canais de comercialização",
  "Trigo", "Manejo agrícola",
  "Cultivar; época de semeadura; fertilização; irrigação; controle fitossanitário",
  "Trigo", "Tecnologia e infraestrutura",
  "Mecanização; armazenamento; assistência técnica; infraestrutura logística",
  "Trigo", "Decisões econômicas",
  "Preço; custo de produção; disponibilidade de crédito; demanda de mercado"
)
print(manejo_humano)
readr::write_csv(manejo_humano, "resultados_aula02/dimensao_antropogenica_manejo.csv")



# -----------------------------------------------------------------------------
# 5. Preparação dos círculos para os diagramas BAM
# -----------------------------------------------------------------------------

criar_circulo <- function(x0, y0, raio, componente, n = 300) {
  
  angulos <- seq(0, 2 * pi, length.out = n)
  
  tibble(
    x = x0 + raio * cos(angulos),
    y = y0 + raio * sin(angulos),
    componente = componente
  )
}

circulos <- bind_rows(
  criar_circulo(-0.65, 0.15, 1.25, "A"),
  criar_circulo( 0.65, 0.15, 1.25, "B"),
  criar_circulo( 0.00,-0.65, 1.25, "M")
)


# -----------------------------------------------------------------------------
# 6. Função para construir diagramas BAM específicos
# -----------------------------------------------------------------------------

plotar_bam <- function(
    titulo,
    subtitulo,
    texto_A,
    texto_B,
    texto_M
) {
  
  # Organizamos os textos em posições específicas dos círculos.
  textos <- tibble(
    x = c(-1.05, 1.05, 0),
    y = c(0.55, 0.55, -1.28),
    componente = c("A", "B", "M"),
    descricao = c(texto_A, texto_B, texto_M)
  )
  
  ggplot() +
    
    # Desenhamos os três conjuntos do BAM.
    geom_polygon(
      data = circulos,
      aes(
        x = x,
        y = y,
        group = componente,
        fill = componente
      ),
      alpha = 0.32,
      color = "grey25",
      linewidth = 0.7
    ) +
    
    # Inserimos a identificação de cada componente e sua descrição.
    geom_text(
      data = textos,
      aes(x = x, y = y, label = componente),
      size = 6,
      fontface = "bold"
    ) +
    
    geom_text(
      data = textos,
      aes(x = x, y = y - 0.28, label = descricao),
      size = 3.1,
      lineheight = 1.05
    ) +
    
    # Indicamos a interseção conceitual dos três conjuntos.
    annotate(
      "text",
      x = 0,
      y = -0.12,
      label = "A ∩ B ∩ M",
      size = 3.6,
      fontface = "bold"
    ) +
    
    scale_fill_manual(
      values = c(
        "A" = "#440154",
        "B" = "#21908C",
        "M" = "#FDE725"
      ),
      guide = "none"
    ) +
    
    coord_equal(
      xlim = c(-2.1, 2.1),
      ylim = c(-2.1, 1.8),
      expand = FALSE
    ) +
    
    labs(
      title = titulo,
      subtitle = subtitulo
    ) +
    
    theme_void(base_size = 12) +
    
    theme(
      plot.title = element_text(
        face = "bold",
        size = 12,
        hjust = 0.5
      ),
      plot.subtitle = element_text(
        size = 9,
        hjust = 0.5
      ),
      plot.margin = margin(10, 10, 10, 10)
    )
}


# -----------------------------------------------------------------------------
# 7. Diagrama BAM — Araucaria angustifolia
# -----------------------------------------------------------------------------

bam_araucaria <- plotar_bam(
  
  titulo = "BAM — Araucaria angustifolia",
  
  subtitulo = "Sistema nativo associado ao pinhão",
  
  texto_A = "Clima\nSolo\nAltitude",
  
  texto_B = "Dispersores\nPredadores\nCompetidores",
  
  texto_M = "Dispersão\nnatural\nConectividade\nflorestal"
)

print(bam_araucaria)


# -----------------------------------------------------------------------------
# 8. Diagrama BAM — Triticum aestivum
# -----------------------------------------------------------------------------

bam_trigo <- plotar_bam(
  
  titulo = "BAM — Triticum aestivum",
  
  subtitulo = "Sistema agrícola associado ao trigo",
  
  texto_A = "Temperatura\nÁgua\nSolo",
  
  texto_B = "Patógenos\nInsetos-praga\nPlantas daninhas",
  
  texto_M = "Transporte\nhumano de\nsementes\nIntrodução"
)

print(bam_trigo)


# -----------------------------------------------------------------------------
# 9. Comparação visual dos dois sistemas
# -----------------------------------------------------------------------------

comparacao_bam <- bam_araucaria + bam_trigo +
  
  plot_layout(ncol = 2) +
  
  plot_annotation(
    title = "Comparação dos componentes BAM",
    subtitle = paste(
      "Araucaria angustifolia (pinhão) versus",
      "Triticum aestivum (trigo)"
    ),
    caption = paste(
      "Representação conceitual. Os círculos não representam",
      "áreas geográficas ou interseções quantitativamente estimadas."
    ),
    theme = theme(
      plot.title = element_text(
        face = "bold",
        size = 16,
        hjust = 0.5
      ),
      plot.subtitle = element_text(
        size = 11,
        hjust = 0.5
      ),
      plot.caption = element_text(
        size = 8,
        color = "grey35"
      )
    )
  )

print(comparacao_bam)

ggsave(
  filename = "./Aula_02/resultados/figuras/comparacao_BAM_araucaria_trigo.png",
  plot = comparacao_bam,
  width = 13,
  height = 7.5,
  dpi = 300
)




# 10. QUESTÕES PARA DISCUSSÃO --------------------------------------------------

questoes <- tibble::tribble(
  ~numero, ~questao,
  1, "O que significa adequabilidade abiótica para uma araucária e para uma planta de trigo?",
  2, "Quais interações bióticas podem favorecer ou limitar cada espécie?",
  3, "Por que a dispersão natural é central para a araucária, enquanto o transporte humano de sementes é central para o trigo cultivado?",
  4, "Por que a área potencialmente adequada ao trigo não corresponde necessariamente à área efetivamente cultivada?",
  5, "Como a coleta de pinhão pode alterar a disponibilidade do recurso sem necessariamente alterar a distribuição geográfica das árvores adultas?",
  6, "Quais processos importantes ficam de fora do BAM clássico quando estudamos sistemas produtivos?",
  7, "Que dados seriam necessários para transformar cada diagrama conceitual em uma análise espacial?"
)
print(questoes)
readr::write_csv(questoes, "resultados_aula02/questoes_discussao.csv")


# 11. PRÓXIMA ETAPA ------------------------------------------------------------

#
# Próximo passo: operacionalizar os componentes com camadas e dados espaciais.
#
# Araucária:
#   - ocorrências validadas de Araucaria angustifolia;
#   - variáveis climáticas e topográficas;
#   - cobertura florestal e fragmentação;
#   - dados de interações ecológicas, quando disponíveis;
#   - dados de coleta/produção de pinhão, sem confundi-los com a distribuição
#     geográfica da espécie.
#
# Trigo:
#   - dados municipais de área plantada e produção (IBGE/PAM);
#   - variáveis climáticas e edáficas;
#   - informações sobre pragas e doenças, quando disponíveis;
#   - distinção entre área climaticamente adequada e área efetivamente cultivada.
#
# Não atribua valores arbitrários a B ou M apenas para completar o diagrama.
# A operacionalização exige dados, critérios explícitos e discussão de limites.
# =============================================================================
