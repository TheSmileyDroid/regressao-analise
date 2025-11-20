# Script para Análise Abrangente de Correlação (Dados Brutos - Censo 2010)
# Objetivo: Relacionar a maior quantidade possível de variáveis brutas (excluindo índices compostos como IDHM)

# Função para instalar e carregar pacotes
install_and_load <- function(package) {
  if (!require(package, character.only = TRUE)) {
    install.packages(package, dependencies = TRUE, repos = "http://cran.us.r-project.org")
    library(package, character.only = TRUE)
  }
}

packages <- c("ggplot2", "corrplot", "dplyr", "readr", "stringr")
sapply(packages, install_and_load)

# --- CARREGAMENTO DOS DADOS ---
file_idhm <- "idhm_municipios.csv"
if (!file.exists(file_idhm)) {
  stop("Arquivo 'idhm_municipios.csv' não encontrado. Execute o script anterior para baixar.")
}

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

# Ler dataset
cat("Carregando dados...\n")
all_data <- read_delim(file_idhm, delim = ";", locale = locale(decimal_mark = ","), show_col_types = FALSE)
data_2010 <- all_data %>% filter(ANO == 2010)

# --- SELEÇÃO E LIMPEZA DE VARIÁVEIS ---
# O usuário pediu para evitar "indicadores" (IDHM) e focar nos dados brutos.
# Vamos selecionar variáveis numéricas e excluir códigos e índices compostos.

# Identificar colunas numéricas
numeric_cols <- data_2010 %>% select_if(is.numeric)

# Remover colunas indesejadas (Códigos, Ano, Índices Compostos IDHM)
raw_data <- numeric_cols %>%
  select(-any_of(c("ANO", "Codmun6", "Codmun7", "pesoRUR", "pesotot", "pesourb", "peso1", "peso4", "peso5", "peso6", "peso13", "peso15", "peso18"))) %>% # Remover pesos e códigos com segurança
  select(-starts_with("IDHM")) %>% # Remover índices compostos
  select(-starts_with("PESO")) %>% # Remover colunas de peso amostral
  select(-starts_with("CORTE"))    # Remover cortes de renda (redundante com Gini e Renda média)

# Selecionar um subconjunto representativo de variáveis brutas para não tornar o gráfico ilegível
# Focando em: Demografia, Educação (Taxas), Trabalho/Renda, Saúde/Saneamento, Vulnerabilidade
selected_vars <- raw_data %>%
  select(
    # Renda e Desigualdade
    RDPC, GINI, PMPOB, RIND, THEIL,
    # Saúde e Saneamento
    ESPVIDA, MORT1, T_AGUA, T_LIXO, T_LUZ, T_DENS,
    # Educação (Taxas de Analfabetismo e Frequência)
    T_ANALF18M, T_FREQ6A14, T_MED18M, E_ANOSESTUDO,
    # Trabalho
    T_DES18M, T_ATIV18M, P_FORMAL,
    # Demografia / Estrutura
    RAZDEP, T_ENV, PPOB,
    # Vulnerabilidade Social
    T_MULCHEFEFIF014, T_CRIFUNDIN_TODOS
  ) %>%
  rename(
    Renda_Per_Capita = RDPC,
    Indice_Gini = GINI,
    Pct_Pobreza = PMPOB,
    Renda_Ind_Trabalho = RIND,
    Indice_Theil = THEIL,
    Esperanca_Vida = ESPVIDA,
    Mortalidade_Infantil = MORT1,
    Agua_Encanada_pct = T_AGUA,
    Coleta_Lixo_pct = T_LIXO,
    Eletricidade_pct = T_LUZ,
    Densidade_Moradia = T_DENS,
    Analfabetismo_18m = T_ANALF18M,
    Freq_Escolar_6a14 = T_FREQ6A14,
    Ensino_Medio_Completo_18m = T_MED18M,
    Exp_Anos_Estudo = E_ANOSESTUDO,
    Desemprego_18m = T_DES18M,
    Atividade_Econ_18m = T_ATIV18M,
    Trabalho_Formal_pct = P_FORMAL,
    Razao_Dependencia = RAZDEP,
    Taxa_Envelhecimento = T_ENV,
    Populacao_Total = PPOB,
    Mulheres_Chefes_Filhos = T_MULCHEFEFIF014,
    Criancas_Pobreza = T_CRIFUNDIN_TODOS
  ) %>%
  na.omit()

cat("Variáveis selecionadas para análise:", ncol(selected_vars), "\n")

# --- GERAÇÃO DE VISUALIZAÇÕES AVANÇADAS (Estilo R Graph Gallery) ---
cat("Gerando visualizações avançadas inspiradas no R Graph Gallery...\n")

# Calcular matriz de correlação
M <- cor(selected_vars)

# Função para cálculo de p-valor (para significância)
cor.mtest <- function(mat, ...) {
    mat <- as.matrix(mat)
    n <- ncol(mat)
    p.mat<- matrix(NA, n, n)
    diag(p.mat) <- 0
    for (i in 1:(n - 1)) {
        for (j in (i + 1):n) {
            tmp <- cor.test(mat[, i], mat[, j], ...)
            p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
        }
    }
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
  p.mat
}
# Calcular p-valores
p.mat <- cor.mtest(selected_vars)

# Definição de cores personalizadas
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))

# 1. Correlograma Misto (Superior com Números, Inferior com Círculos) + Clustering
png("5_Correlograma_Misto_Cluster.png", width = 1200, height = 1200)
corrplot(M, method="color", col=col(200),  
         type="upper", order="hclust", 
         addCoef.col = "black", # Adicionar coeficiente de correlação
         tl.col="black", tl.srt=45, # Cor e rotação do texto
         # Combinar com círculo no fundo? Não, vamos fazer limpo.
         diag=FALSE,
         title="Correlação Hierárquica (Numérica)", mar=c(0,0,2,0)
)
dev.off()

# 2. Correlograma com Teste de Significância (X nos não significantes)
png("6_Correlograma_Significancia.png", width = 1200, height = 1200)
corrplot(M, method="color", col=col(200),  
         type="upper", order="hclust", 
         addCoef.col = "black", # Adicionar coeficientes
         tl.col="black", tl.srt=45,
         p.mat = p.mat, sig.level = 0.01, insig = "blank", # Deixar em branco os não significantes (p > 0.01)
         title="Correlações Significantes (p < 0.01)", mar=c(0,0,2,0)
)
dev.off()

# 3. Correlograma Estilo 'Elipse' (Visualização geométrica da força)
png("7_Correlograma_Elipses.png", width = 1200, height = 1200)
corrplot(M, method = "ellipse", col = col(200),
         type = "upper", order = "hclust",
         tl.col = "black", tl.srt = 45,
         title="Força da Correlação (Elipses)", mar=c(0,0,2,0))
dev.off()

# 4. Matriz de Dispersão Estilizada (GGally)
# Selecionar um subconjunto menor para o ggpairs não ficar ilegível (ex: top 8 variáveis mais interessantes)
vars_ggpairs <- selected_vars %>% 
  select(Renda_Per_Capita, Indice_Gini, Mortalidade_Infantil, 
         Esperanca_Vida, Analfabetismo_18m, Agua_Encanada_pct, 
         Trabalho_Formal_pct, Populacao_Total)

png("8_Matriz_Dispersao_Estilizada.png", width = 1400, height = 1400)
# Função personalizada para o painel inferior (pontos com transparência)
lower_fn <- function(data, mapping, ...){
  p <- ggplot(data = data, mapping = mapping) + 
    geom_point(color = "darkblue", alpha = 0.3, size = 0.5) + 
    geom_smooth(method = "lm", color = "red", se = FALSE, ...)
  p
}

ggpairs(vars_ggpairs, 
        lower = list(continuous = lower_fn),
        diag = list(continuous = wrap("densityDiag", fill = "lightblue", alpha = 0.5)),
        upper = list(continuous = wrap("cor", size = 4, color = "black")),
        title = "Matriz de Dispersão Detalhada (Top 8 Variáveis)"
) + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
dev.off()

cat("Todas as visualizações avançadas foram geradas com sucesso!\n")

