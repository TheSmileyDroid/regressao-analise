# Script para Análise de Correlação e Regressão

# Função para instalar e carregar pacotes
install_and_load <- function(package) {
  if (!require(package, character.only = TRUE)) {
    install.packages(package, dependencies = TRUE, repos = "http://cran.us.r-project.org")
    library(package, character.only = TRUE)
  }
}

packages <- c("ggplot2", "corrplot", "GGally", "dplyr", "readr")
sapply(packages, install_and_load)

# Função auxiliar para salvar gráficos
save_plots <- function(data, prefix) {
  # Selecionar apenas numéricos
  data_num <- data %>% select_if(is.numeric) %>% na.omit()
  
  # 1. Correlograma
  png(paste0(prefix, "_correlograma.png"), width = 800, height = 800)
  cor_matrix <- cor(data_num)
  corrplot(cor_matrix, method = "color", type = "upper", 
           order = "hclust", addCoef.col = "black",
           tl.col = "black", tl.srt = 45,
           title = paste("Correlograma -", prefix), mar = c(0,0,1,0))
  dev.off()
  
  # 2. Matriz de Dispersão (selecionar top 5 variáveis se houver muitas)
  if(ncol(data_num) > 6) {
    data_subset <- data_num[, 1:6]
  } else {
    data_subset <- data_num
  }
  
  png(paste0(prefix, "_dispersao.png"), width = 1000, height = 1000)
  p <- ggpairs(data_subset, title = paste("Matriz de Dispersão -", prefix))
  print(p)
  dev.off()
  
  cat(paste("Gráficos gerados para:", prefix, "\n"))
}

# --- CARREGAMENTO E PREPARAÇÃO DO DATASET (IDHM) ---
cat("\n--- Carregando Dataset: Cidades (IDHM 2010) ---\n")

# URL do dataset real de IDHM (Fonte: GitHub mauriciocramos/IDHM)
url_idhm <- "https://raw.githubusercontent.com/mauriciocramos/IDHM/main/municipal.csv"
file_idhm <- "idhm_municipios.csv"

# Baixar o arquivo se não existir
if (!file.exists(file_idhm)) {
  cat("Baixando dataset de IDHM...\n")
  download.file(url_idhm, destfile = file_idhm, mode = "wb")
}

# Ler dataset com formatação correta (pt-BR)
tryCatch({
  # read_delim é mais flexível. Locale define decimal como vírgula.
  all_data <- read_delim(file_idhm, delim = ";", locale = locale(decimal_mark = ","), show_col_types = FALSE)
  
  # Filtrar apenas para o ano mais recente (2010)
  data_2010 <- all_data %>% filter(ANO == 2010)
  
  cat("Dados carregados. Total de municípios em 2010:", nrow(data_2010), "\n")
  
  # --- CENÁRIO 1: ECONOMIA E DESENVOLVIMENTO ---
  cat("\n--- Gerando Gráficos: Cenário 1 - Economia (IDHM, Renda, Gini) ---\n")
  
  econ_data <- data_2010 %>%
    select(IDHM, IDHM_R, RDPC, GINI, PPOB) %>%
    na.omit() %>%
    rename(
      IDH_Municipal = IDHM,
      IDH_Renda = IDHM_R,
      Renda_Per_Capita = RDPC,
      Indice_Gini = GINI,
      Populacao_Total = PPOB
    )
  
  save_plots(econ_data, "1_Economia_Real_2010")
  
  
  # --- CENÁRIO 2: EDUCAÇÃO ---
  cat("\n--- Gerando Gráficos: Cenário 2 - Educação (IDHM-E, Analfabetismo, Frequência) ---\n")
  
  edu_data <- data_2010 %>%
    select(IDHM_E, T_ANALF18M, T_FREQ6A14, E_ANOSESTUDO, RDPC) %>%
    na.omit() %>%
    rename(
      IDH_Educacao = IDHM_E,
      Taxa_Analfabetismo_18mais = T_ANALF18M,
      Freq_Escolar_6a14 = T_FREQ6A14,
      Exp_Anos_Estudo = E_ANOSESTUDO,
      Renda_Per_Capita = RDPC
    )
  
  save_plots(edu_data, "2_Educacao_Real_2010")
  
  
  # --- CENÁRIO 3: SAÚDE E SANEAMENTO ---
  cat("\n--- Gerando Gráficos: Cenário 3 - Saúde (Mortalidade, Longevidade, Saneamento) ---\n")
  
  health_data <- data_2010 %>%
    select(MORT1, ESPVIDA, IDHM_L, T_AGUA, T_LIXO, RDPC) %>%
    na.omit() %>%
    rename(
      Mortalidade_Infantil = MORT1,
      Esperanca_Vida = ESPVIDA,
      IDH_Longevidade = IDHM_L,
      Agua_Encanada_pct = T_AGUA,
      Coleta_Lixo_pct = T_LIXO,
      Renda_Per_Capita = RDPC
    )
  
  save_plots(health_data, "3_Saude_Real_2010")
  
  cat("\nTodos os gráficos foram gerados com sucesso usando dados do Censo 2010!\n")

}, error = function(e) {
  cat("ERRO CRÍTICO: Não foi possível processar o arquivo de dados.\n")
  cat("Detalhes do erro:", e$message, "\n")
  cat("Verifique se o arquivo 'idhm_municipios.csv' está íntegro e no formato CSV (ponto e vírgula).\n")
})
