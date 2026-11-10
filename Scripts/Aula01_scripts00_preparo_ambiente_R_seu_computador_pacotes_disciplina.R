
# ============================================================
# DISCIPLINA: MODELAGEM DE NICHO ECOLÓGICO APLICADA À
# PRODUÇÃO SUSTENTÁVEL
#
# SCRIPT 00 — ORGANIZAÇÃO DO PROJETO E DIRETÓRIOS
#
# PPGAG — UTFPR, Campus Pato Branco
# ============================================================

# OBJETIVOS
#
# 1. Compreender a importância de organizar um projeto no RStudio.
# 2. Criar um projeto R (arquivo .Rproj).
# 3. Criar um diretório principal para a disciplina.
# 4. Organizar os arquivos em pastas específicas para cada aula.
# 5. Padronizar o armazenamento de dados brutos, processados,
#    resultados, figuras e tabelas.
# 6. Compreender o funcionamento do diretório de trabalho
#    (working directory) e do comando setwd().
#
# IMPORTANTE
#
# A organização dos diretórios é uma etapa fundamental para
# garantir a reprodutibilidade, a organização e a continuidade
# das análises ao longo da disciplina.
#
# Execute este script a partir do projeto R criado no RStudio.


# ============================================================
# 1. POR QUE UTILIZAR UM PROJETO R (RPROJECT)?
# ============================================================

# Um projeto RStudio é uma estrutura de trabalho que reúne
# os scripts, dados, resultados e demais arquivos de uma análise.
#
# Ao criar um projeto, o RStudio gera um arquivo com extensão:
#
# .Rproj
#
# Esse arquivo identifica o projeto e permite que o RStudio
# reconheça automaticamente seu diretório principal.
#
# PRINCIPAIS VANTAGENS:
#
# - Organização dos arquivos em um único local;
# - Facilidade para retomar análises em diferentes momentos;
# - Uso de caminhos relativos, sem depender do computador;
# - Maior facilidade para compartilhar o projeto;
# - Reprodutibilidade dos procedimentos;
# - Integração com sistemas de controle de versão, como GitHub.
#
# RECOMENDAÇÃO:
#
# Cada aluno deverá criar um único projeto R para esta disciplina.
# Todas as atividades serão organizadas dentro desse projeto.


# ============================================================
# 2. CRIAÇÃO DO PROJETO NO RSTUDIO
# ============================================================

# Antes de executar o restante deste script, crie o projeto:
#
# 1. Abra o RStudio.
# 2. Clique em File > New Project.
# 3. Selecione New Directory.
# 4. Selecione New Project.
# 5. Defina o nome do diretório:
#
#    Modelagem_Nicho_Ecologico_PPGAG
#
# 6. Escolha o local onde deseja armazenar a disciplina.
# 7. Clique em Create Project.
#
# O RStudio criará o diretório principal e o arquivo:
#
# Modelagem_Nicho_Ecologico_PPGAG.Rproj
#
# O arquivo .Rproj deverá permanecer na raiz do projeto.
#
# ATENÇÃO:
#
# Não crie um novo projeto para cada aula.
# Todas as aulas deverão permanecer dentro do mesmo RProject.


# ============================================================
# 3. VERIFICAÇÃO DO DIRETÓRIO DE TRABALHO
# ============================================================

# getwd() informa o diretório de trabalho atualmente utilizado
# pelo R.
#
# Quando o projeto R é aberto corretamente, esse diretório
# deverá corresponder à pasta principal da disciplina.

cat("\nDIRETÓRIO DE TRABALHO ATUAL:\n")

print(getwd())

# Verificar se o RStudio está utilizando um projeto.

arquivo_projeto <- list.files(
  path = getwd(),
  pattern = "\\.Rproj$",
  full.names = TRUE
)

if (length(arquivo_projeto) > 0) {
  
  cat("\nProjeto R identificado:\n")
  print(basename(arquivo_projeto))
  
} else {
  
  warning(
    "Nenhum arquivo .Rproj foi identificado no diretório atual. ",
    "Verifique se o projeto foi criado e aberto corretamente."
  )
  
}


# ============================================================
# 4. ORGANIZAÇÃO GERAL DO PROJETO
# ============================================================

# A estrutura principal da disciplina será organizada da
# seguinte maneira:
#
# Modelagem_Nicho_Ecologico_PPGAG/
#
#   Modelagem_Nicho_Ecologico_PPGAG.Rproj
#
#   Aula_01/
#   Aula_02/
#   Aula_03/
#   Aula_04/
#   Aula_05/
#   Aula_06/
#
# Cada aula terá sua própria estrutura de armazenamento:
#
#   Aula_XX/
#
#       dados/
#           brutos/
#           processados/
#
#       resultados/
#           figuras/
#           tabelas/
#
#       scripts/
#
# Essa organização permite manter os arquivos de cada atividade
# separados, evitando misturar dados e resultados de aulas
# diferentes.


# ============================================================
# 5. DEFINIÇÃO DAS AULAS
# ============================================================

# Neste primeiro momento, criaremos as pastas das quatro
# atividades já previstas.
#
# Novas aulas poderão ser acrescentadas posteriormente,
# bastando atualizar esta lista.

aulas <- c(
  "Aula_01",
  "Aula_02",
  "Aula_03",
  "Aula_04"
)

# IMPORTANTE:
#
# A numeração das pastas deverá acompanhar a sequência
# efetivamente adotada no cronograma da disciplina.


# ============================================================
# 6. CRIAÇÃO DOS DIRETÓRIOS
# ============================================================

# Criar as pastas principais de cada aula.

for (aula in aulas) {
  
  dir.create(
    aula,
    showWarnings = FALSE
  )
  
  # Diretórios de dados
  
  dir.create(
    file.path(aula, "dados", "brutos"),
    recursive = TRUE,
    showWarnings = FALSE
  )
  
  dir.create(
    file.path(aula, "dados", "processados"),
    recursive = TRUE,
    showWarnings = FALSE
  )
  
  # Diretórios de resultados
  
  dir.create(
    file.path(aula, "resultados", "figuras"),
    recursive = TRUE,
    showWarnings = FALSE
  )
  
  dir.create(
    file.path(aula, "resultados", "tabelas"),
    recursive = TRUE,
    showWarnings = FALSE
  )
  
  # Diretório para os scripts da atividade
  
  dir.create(
    file.path(aula, "scripts"),
    recursive = TRUE,
    showWarnings = FALSE
  )
  
}


# ============================================================
# 7. FUNÇÃO DE CADA DIRETÓRIO
# ============================================================

# DADOS/BRUTOS
#
# Armazenar os arquivos originais obtidos de fontes externas,
# como GBIF, WorldClim, CHELSA, TerraClimate e outras bases.
#
# Os arquivos originais não deverão ser modificados diretamente.
#
#
# DADOS/PROCESSADOS
#
# Armazenar os dados após procedimentos de limpeza,
# padronização, recorte espacial, transformação e organização.
#
#
# RESULTADOS/FIGURAS
#
# Armazenar mapas, gráficos, diagramas e demais figuras
# produzidas durante as análises.
#
#
# RESULTADOS/TABELAS
#
# Armazenar tabelas, métricas, estatísticas e sínteses
# produzidas pelos scripts.
#
#
# SCRIPTS
#
# Armazenar os códigos R específicos de cada aula.
#
# Exemplo:
#
# Aula_04/
#
#   scripts/
#       Script_01_ambientes.R
#       Script_02_ocorrencias_GBIF.R
#       Script_03_limpeza_coordenadas.R
#       Script_04_mapa_ocorrencias.R


# ============================================================
# 8. VERIFICAÇÃO DA ESTRUTURA CRIADA
# ============================================================

# Listar os diretórios existentes dentro de cada aula.

for (aula in aulas) {
  
  cat("\n========================================\n")
  cat("ESTRUTURA:", aula, "\n")
  cat("========================================\n")
  
  print(
    list.dirs(
      aula,
      recursive = TRUE,
      full.names = FALSE
    )
  )
  
}


# ============================================================
# 9. SOBRE O COMANDO setwd()
# ============================================================

# O comando setwd() modifica o diretório de trabalho do R.
#
# Exemplo de caminho absoluto no Windows:
#
# setwd("C:/Users/Aluno/Documents/Modelagem_Nicho_Ecologico_PPGAG")
#
# Entretanto, quando utilizamos um projeto R (.Rproj),
# normalmente NÃO precisamos utilizar setwd().
#
# Ao abrir o arquivo .Rproj, o RStudio já define a raiz
# do projeto como diretório de trabalho.
#
# Por isso, recomendamos utilizar caminhos relativos.
#
# EXEMPLO:
#
# Para acessar um arquivo dentro da Aula_04:
#
# read.csv(
#   "Aula_04/dados/processados/arquivo.csv"
# )
#
# Para salvar uma figura:
#
# ggplot2::ggsave(
#   "Aula_04/resultados/figuras/mapa.png"
# )
#
# Esses caminhos funcionam a partir da raiz do projeto,
# independentemente do nome de usuário ou computador.
#
# O uso de setwd() com caminhos absolutos deve ser evitado
# nos scripts compartilhados entre os alunos.


# ============================================================
# 10. PADRONIZAÇÃO DOS NOMES DOS ARQUIVOS
# ============================================================

# Para facilitar a identificação e a recuperação dos arquivos,
# recomendamos algumas regras:
#
# - Utilizar nomes descritivos;
# - Evitar espaços e caracteres especiais;
# - Utilizar sublinhado (_) para separar palavras;
# - Manter a numeração dos scripts;
# - Não sobrescrever os dados brutos;
# - Manter os resultados organizados por aula.
#
# EXEMPLOS:
#
# Script_01_ambientes.R
# Script_02_ocorrencias_GBIF.R
# Script_03_limpeza_coordenadas.R
# Script_04_mapa_ocorrencias.R
#
# ocorrencias_gbif_brutas.csv
# ocorrencias_coordenadas_limpas.csv
# adequabilidade_continua.tif
# curva_ROC.png
# metricas_modelo.csv


# ============================================================
# 11. VALIDAÇÃO FINAL DO PROJETO
# ============================================================

# Verificar se todas as pastas esperadas foram criadas.

pastas_necessarias <- unlist(
  lapply(
    aulas,
    function(aula) {
      file.path(
        aula,
        c(
          "dados/brutos",
          "dados/processados",
          "resultados/figuras",
          "resultados/tabelas",
          "scripts"
        )
      )
    }
  )
)

pastas_existentes <- dir.exists(pastas_necessarias)

cat("\nPASTAS CRIADAS:", sum(pastas_existentes), "\n")
cat("PASTAS ESPERADAS:", length(pastas_necessarias), "\n")

if (all(pastas_existentes)) {
  
  cat("\n========================================\n")
  cat("SCRIPT 00 — CONCLUÍDO COM SUCESSO\n")
  cat("========================================\n")
  cat("Projeto R identificado.\n")
  cat("Diretórios das aulas organizados.\n")
  cat("Estrutura de dados e resultados criada.\n")
  cat("Projeto preparado para as atividades.\n")
  cat("========================================\n")
  
} else {
  
  stop(
    "A estrutura de diretórios está incompleta. ",
    "Verifique as pastas que não foram criadas."
  )
  
}


# ============================================================
# FIM DO SCRIPT 00
# ============================================================



# ============================================================
# DISCIPLINA: MODELAGEM DE NICHO ECOLÓGICO APLICADA À
# PRODUÇÃO SUSTENTÁVEL
#
# SCRIPT 00 — PREPARAÇÃO DO AMBIENTE COMPUTACIONAL
# PPGAG — UTFPR, Campus Pato Branco
# ============================================================

# OBJETIVOS
#
# 1. Verificar a versão do R e a utilização do RStudio.
# 2. Instalar os pacotes utilizados nas atividades da disciplina.
# 3. Verificar se todos os pacotes foram instalados corretamente.
# 4. Registrar as versões dos pacotes e informações da sessão.
#
# IMPORTANTE
#
# Execute este script antes de iniciar as atividades práticas.
# Não é necessário reinstalar os pacotes a cada aula.
#
# A atualização do R e do RStudio Desktop é realizada
# separadamente da instalação dos pacotes.


# ============================================================
# 1. VERIFICAÇÃO DO AMBIENTE
# ============================================================

cat("\n========================================\n")
cat("VERIFICAÇÃO DO AMBIENTE COMPUTACIONAL\n")
cat("========================================\n")

# Versão do R

cat("\nVersão do R:\n")
print(R.version.string)

# Verificar se o código está sendo executado no RStudio

if (Sys.getenv("RSTUDIO") == "1") {
  
  cat("\nRStudio detectado.\n")
  
} else {
  
  cat("\nRStudio não detectado.\n")
  
}


# ============================================================
# 2. PACOTES NECESSÁRIOS
# ============================================================

# Lista consolidada dos pacotes utilizados nos exercícios
# já desenvolvidos e nas atividades previstas.

pacotes <- c(
  
  # Dados de biodiversidade
  "rgbif",
  
  # Dados espaciais
  "sf",
  "terra",
  "geobr",
  "geodata",
  
  # Manipulação e organização de dados
  "tidyverse",
  "dplyr",
  "tidyr",
  "tibble",
  "readr",
  "janitor",
  
  # Visualização gráfica
  "ggplot2",
  "viridis",
  "patchwork",
  "plotly",
  "corrplot",
  
  # Avaliação de coordenadas
  "CoordinateCleaner",
  
  # Análises estatísticas
  "usdm",
  "factoextra",
  "pROC",
  
  # Modelagem de distribuição de espécies
  "maxnet"
  
)


# Remover eventuais duplicações

pacotes <- unique(pacotes)

cat("\nTotal de pacotes necessários:", length(pacotes), "\n")


# ============================================================
# 3. IDENTIFICAÇÃO DOS PACOTES AUSENTES
# ============================================================

# requireNamespace() verifica se um pacote está instalado
# sem precisar carregá-lo na sessão.

pacotes_instalados <- vapply(
  pacotes,
  requireNamespace,
  logical(1),
  quietly = TRUE
)

pacotes_faltantes <- pacotes[!pacotes_instalados]

cat(
  "\nPacotes já instalados:",
  sum(pacotes_instalados),
  "\n"
)

cat(
  "Pacotes que precisam ser instalados:",
  length(pacotes_faltantes),
  "\n"
)


# ============================================================
# 4. INSTALAÇÃO DOS PACOTES AUSENTES
# ============================================================

# Definir o repositório CRAN.

options(
  repos = c(CRAN = "https://cloud.r-project.org")
)

# Instalar somente os pacotes ausentes.

if (length(pacotes_faltantes) > 0) {
  
  cat("\nIniciando a instalação dos pacotes...\n")
  
  install.packages(
    pacotes_faltantes,
    dependencies = TRUE
  )
  
} else {
  
  cat("\nTodos os pacotes já estão instalados.\n")
  
}


# ============================================================
# 5. VERIFICAÇÃO FINAL
# ============================================================

# Repetir a verificação após a instalação.

verificacao_final <- vapply(
  pacotes,
  requireNamespace,
  logical(1),
  quietly = TRUE
)

tabela_pacotes <- data.frame(
  pacote = pacotes,
  instalado = verificacao_final
)

print(tabela_pacotes)

# Identificar eventuais falhas.

pacotes_com_erro <- pacotes[!verificacao_final]

if (length(pacotes_com_erro) > 0) {
  
  cat("\nATENÇÃO: alguns pacotes não foram instalados.\n")
  
  print(pacotes_com_erro)
  
} else {
  
  cat("\nTodos os pacotes foram instalados corretamente.\n")
  
}


# ============================================================
# 6. REGISTRO DAS VERSÕES DOS PACOTES
# ============================================================

# Registrar as versões efetivamente instaladas.

versoes_pacotes <- data.frame(
  pacote = pacotes[verificacao_final],
  versao = vapply(
    pacotes[verificacao_final],
    function(x) {
      as.character(packageVersion(x))
    },
    character(1)
  )
)

print(versoes_pacotes)


# ============================================================
# 7. EXPORTAÇÃO DAS INFORMAÇÕES
# ============================================================

# Criar uma pasta para armazenar os registros do ambiente.

dir_saida <- "preparacao_disciplina"

if (!dir.exists(dir_saida)) {
  
  dir.create(
    dir_saida,
    recursive = TRUE
  )
  
}

# Salvar a tabela de verificação.

write.csv(
  tabela_pacotes,
  file.path(
    dir_saida,
    "verificacao_pacotes.csv"
  ),
  row.names = FALSE
)

# Salvar as versões dos pacotes.

write.csv(
  versoes_pacotes,
  file.path(
    dir_saida,
    "versoes_pacotes.csv"
  ),
  row.names = FALSE
)

# Salvar informações completas da sessão.

writeLines(
  capture.output(sessionInfo()),
  file.path(
    dir_saida,
    "informacoes_sessao_R.txt"
  )
)


# ============================================================
# 8. VALIDAÇÃO FINAL
# ============================================================

if (all(verificacao_final)) {
  
  cat("\n========================================\n")
  cat("SCRIPT 00 — CONCLUÍDO COM SUCESSO\n")
  cat("========================================\n")
  cat("Todos os pacotes estão disponíveis.\n")
  cat("Informações da sessão foram registradas.\n")
  cat("Ambiente preparado para as atividades.\n")
  cat("========================================\n")
  
} else {
  
  stop(
    "A instalação não foi concluída. ",
    "Verifique os pacotes indicados na tabela."
  )
  
}