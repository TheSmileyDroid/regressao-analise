shell_call <- function(command, ...) {
  result <- system(command, intern = TRUE, ...)
  cat(paste0(result, collapse = "\n"))
}

shell_call("curl -L -o ./star-dataset.zip https://www.kaggle.com/api/v1/datasets/download/deepu1109/star-dataset")
shell_call("unzip ./star-dataset.zip")
shell_call("mv '6 class csv.csv' 'data.csv'")
# Função para instalar e carregar pacotes necessários
install <- function(package) {
  if (!require(package, character.only = TRUE)) {
    install.packages(package)
  }
}

# Lista de pacotes necessários
packages <- c("ggplot2", "corrplot", "GGally", "dplyr")
sapply(packages, install)
sapply(packages, library, character.only = TRUE)

# --- CARREGAMENTO DOS DADOS ---
# Substitua 'mtcars' pelo seu conjunto de dados.
# Exemplo para ler CSV: dados <- read.csv("seu_arquivo.csv")
dados <- read.csv("data.csv")
dados

# Selecionar apenas colunas numéricas para correlação
dados_num <- dados %>% select_if(is.numeric)

# --- 1. MATRIZ DE CORRELAÇÃO (CORRELOGRAMA) ---
# Calcular a matriz de correlação
cor_matrix <- cor(dados_num, use = "complete.obs")

# Salvar o correlograma como imagem
png("correlograma.png", width = 800, height = 800)
corrplot(cor_matrix, method = "color", type = "upper", 
         order = "hclust", 
         addCoef.col = "black", # Adicionar coeficientes de correlação
         tl.col = "black", tl.srt = 45, # Cor e rotação dos rótulos
         title = "Correlograma das Variáveis", mar = c(0,0,1,0))
dev.off()

print("Correlograma salvo como 'correlograma.png'")

# --- 2. MATRIZ DE DIAGRAMAS DE DISPERSÃO ---
# Usando GGally para visualizar pares de variáveis
# Nota: Se o dataset for muito grande, selecione apenas as variáveis mais importantes
# Exemplo: dados_subset <- dados_num[, c("mpg", "disp", "hp", "drat", "wt")]

png("matriz_dispersao.png", width = 1000, height = 1000)
ggpairs(dados_num, title = "Matriz de Dispersão e Correlação") 
dev.off()

print("Matriz de dispersão salva como 'matriz_dispersao.png'")

print("Concluído! Verifique os arquivos gerados.")
