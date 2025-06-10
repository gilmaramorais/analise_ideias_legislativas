##########################################
### Emendas Parlamentares - Download API
##########################################

# Carregando os pacotes necessários:
library(httr)         # Para fazer requisições HTTP (API)
library(jsonlite)     # Para lidar com dados JSON
library(dplyr)        # Manipulação de dados
library(stringr)      # Manipulação de strings
library(tidyr)        # Transformações de dados
library(tidytext)     # Análise de texto
library(stopwords)    # Stopwords em português
library(echarts4r)    # Visualizações interativas (nuvem de palavras)
library(scales)       # Formatação de números
library(cluster)      # Análise de cluster
library(factoextra)   # Visualização de agrupamentos

# É necessário obter uma chave da API do Portal da Transparência:
# Cadastre-se em https://portaldatransparencia.gov.br/api-de-dados/cadastrar-email
# e insira a chave abaixo:
token <- ""  # Coloque sua chave da API aqui

# Função que baixa todas as páginas de emendas para um determinado ano
baixar_todas_paginas <- function(ano, token) {
  pagina <- 1
  resultados <- list()  # Lista que vai armazenar os dados
  
  repeat {
    # Monta a URL para cada página
    url_emendas <- paste0("https://api.portaldatransparencia.gov.br/api-de-dados/emendas?ano=", ano, "&pagina=", pagina)
    
    # Faz a requisição GET para a API
    resposta <- GET(url_emendas, add_headers(`chave-api-dados` = token))
    
    # Verifica se houve erro na resposta
    if (status_code(resposta) != 200) {
      warning(paste("Erro na requisição: ano", ano, "página", pagina))
      break
    }
    
    # Converte a resposta para texto e depois para data.frame
    texto <- content(resposta, "text", encoding = "UTF-8")
    dados <- fromJSON(texto, flatten = TRUE)
    
    # Se não vierem dados, termina o loop
    if (length(dados) == 0) {
      cat("Sem mais dados na página", pagina, "do ano", ano, "\n")
      break
    }
    
    resultados[[pagina]] <- as.data.frame(dados)
    
    cat("Página", pagina, "do ano", ano, "baixada com", nrow(resultados[[pagina]]), "registros\n")
    
    pagina <- pagina + 1
    Sys.sleep(1)  # Pausa para evitar sobrecarregar a API
  }
  
  if (length(resultados) == 0) return(NULL)
  
  bind_rows(resultados)  # Junta tudo em um único data.frame
}

# Baixando os dados de emendas para diferentes anos
emendas_2024 <- baixar_todas_paginas(2024, token)
emendas_2023 <- baixar_todas_paginas(2023, token)


# Unindo os anos mais recentes para análise
emendas <- bind_rows(
  emendas_2023,
  emendas_2024
)

##########################################
### E-Cidadania - Importação dos dados
##########################################

# Baixando os dados públicos de participação social do Senado (consultas públicas)
url_ecidadania <- "https://www12.senado.leg.br/ecidadania/documentos/home/resultados/todas-as-proposicoes-com-votos-na-consulta-publica-csv"

# Lendo o CSV diretamente da URL (com encoding Windows-1252)
e_cid <- readr::read_csv2(url_ecidadania, locale = locale(encoding = "Windows-1252"), skip = 1)

# Filtrando as matérias que foram propostas entre 2022 e 2024
e_cid_filtrado <- e_cid %>%
  filter(str_detect(`NOME DA MATÉRIA`, "2022|2023|2024"))

##########################################
### Análise de Texto: Palavras mais faladas
##########################################

# Stopwords customizadas (em português) para limpar as palavras irrelevantes
stopwords_pt <- c(stopwords::stopwords("pt"), "de", "a", "nacional", "pública")

# Extraindo palavras únicas das "funções" das emendas (ex: Saúde, Educação)
palavras_funcoes <- emendas %>%
  distinct(funcao) %>%
  unnest_tokens(word, funcao) %>%
  filter(!word %in% stopwords_pt) %>%
  pull(word) %>%
  unique()

# Tokenizando as ementas e filtramos apenas palavras que aparecem nas funções
palavras_ementa_filtradas <- e_cid %>%
  unnest_tokens(word, EMENTA) %>%
  filter(!word %in% stopwords_pt) %>%
  filter(word %in% palavras_funcoes)

# Contando as palavras mais frequentes
palavras_top <- palavras_ementa_filtradas %>%
  count(word, sort = TRUE) %>%
  head(50)

# Criando uma nuvem de palavras com as palavras mais frequentes
palavras_top %>%
  e_color_range(n, color) %>%
  e_charts() %>%
  e_cloud(word, n, color,
          shape = "circle",
          rotationRange = c(0, 0),
          sizeRange = c(8, 110)) %>%
  e_tooltip() %>%
  e_toolbox_feature(feature = "saveAsImage", title = "Download")

##########################################
### Tratamento e agregação dos gastos por função
##########################################

# Limpando os valores financeiros e somamos por função e ano
resultado_gastos <- emendas %>%
  mutate(
    valorEmpenhado = str_replace_all(valorEmpenhado, "\\.", "") %>%
      str_replace(",", ".") %>%
      as.numeric(),
    valorLiquidado = str_replace_all(valorLiquidado, "\\.", "") %>%
      str_replace(",", ".") %>%
      as.numeric(),
    valorPago = str_replace_all(valorPago, "\\.", "") %>%
      str_replace(",", ".") %>%
      as.numeric()
  ) %>%
  group_by(funcao, ano) %>%
  summarise(
    valorEmpenhado = sum(valorEmpenhado, na.rm = TRUE),
    valorLiquidado = sum(valorLiquidado, na.rm = TRUE),
    valorPago = sum(valorPago, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    valorEmpenhado = scales::comma(valorEmpenhado, big.mark = ".", decimal.mark = ",", accuracy = 0.01),
    valorLiquidado = scales::comma(valorLiquidado, big.mark = ".", decimal.mark = ",", accuracy = 0.01),
    valorPago = scales::comma(valorPago, big.mark = ".", decimal.mark = ",", accuracy = 0.01)
  )

##########################################
### Classificação temática das proposições (função)
##########################################

# Criando uma coluna "funcao" a partir de palavras-chave presentes na EMENTA das proposições
e_cid_filtrado <- e_cid_filtrado %>%
  mutate(funcao = case_when(
    str_detect(str_to_lower(EMENTA), "saúde|sus|hospital|ubs|posto de saúde|enferm") ~ "Saúde",
    str_detect(str_to_lower(EMENTA), "educação|escola|ensino|creche|universidade|aluno|professor") ~ "Educação",
    str_detect(str_to_lower(EMENTA), "assistência|cras|creas|vulnerável|social") ~ "Assistência social",
    str_detect(str_to_lower(EMENTA), "segurança|polícia|militar|penal|crime|prisão") ~ "Segurança pública",
    str_detect(str_to_lower(EMENTA), "esporte|lazer|quadra|campo|ginásio|atividade física") ~ "Desporto e lazer",
    str_detect(str_to_lower(EMENTA), "direitos|cidadania|igualdade|liberdade") ~ "Direitos da cidadania",
    str_detect(str_to_lower(EMENTA), "urbanismo|infraestrutura urbana|praça|iluminação pública") ~ "Urbanismo",
    str_detect(str_to_lower(EMENTA), "agricultura|rural|agro|produtor|agrícola") ~ "Agricultura",
    str_detect(str_to_lower(EMENTA), "ambiente|meio ambiente|sustentável|resíduo|ecologia") ~ "Gestão ambiental",
    str_detect(str_to_lower(EMENTA), "habitação|moradia|residencial|casa popular") ~ "Habitação",
    str_detect(str_to_lower(EMENTA), "trabalho|emprego|renda|qualificação profissional") ~ "Trabalho",
    str_detect(str_to_lower(EMENTA), "cultura|evento cultural|patrimônio|museu|teatro") ~ "Cultura",
    str_detect(str_to_lower(EMENTA), "transporte|estrada|rodovia|asfalto|pavimentação|ônibus") ~ "Transporte",
    str_detect(str_to_lower(EMENTA), "administração|gestão pública|governança") ~ "Administração",
    str_detect(str_to_lower(EMENTA), "ciência|tecnologia|inovação|pesquisa") ~ "Ciência e Tecnologia",
    str_detect(str_to_lower(EMENTA), "comunicação|internet|telecomunicação|wi-fi") ~ "Comunicações",
    str_detect(str_to_lower(EMENTA), "comércio|serviço|negócio|mercado") ~ "Comércio e serviços",
    str_detect(str_to_lower(EMENTA), "indústria|industrial|fábrica") ~ "Indústria",
    str_detect(str_to_lower(EMENTA), "defesa|militar|exército|marinha|aeronáutica") ~ "Defesa nacional",
    str_detect(str_to_lower(EMENTA), "organização agrária|reforma agrária|assentamento") ~ "Organização agrária",
    str_detect(str_to_lower(EMENTA), "relações exteriores|cooperação internacional|diplomacia") ~ "Relações exteriores",
    str_detect(str_to_lower(EMENTA), "encargo|precatório|dívida pública") ~ "Encargos especiais",
    str_detect(str_to_lower(EMENTA), "múltiplo|intersetorial|interdisciplinar") ~ "Múltiplo",
    TRUE ~ "Outros"
  ))

# Contando quantas proposições há por função
cont_fun <- e_cid_filtrado %>% count(funcao)

##########################################
### Análise de Cluster (Demanda x Valor Pago)
##########################################

# Quantidade de matérias (proposições) por função
dados_por_funcao <- e_cid_filtrado %>%
  group_by(funcao) %>%
  summarise(
    total_materias = n(),
    .groups = "drop"
  )

# Somando os valores liquidados por função
resultado_gastos_agg <- resultado_gastos %>%
  mutate(valorLiquidado = str_replace_all(valorLiquidado, "\\.", "") %>%
           str_replace(",", ".") %>%
           as.numeric()) %>%
  group_by(funcao) %>%
  summarise(valorLiquidado = sum(valorLiquidado, na.rm = TRUE), .groups = "drop")

# Juntando as duas informações: demanda (proposições) e valor pago
base_completa <- dados_por_funcao %>%
  left_join(resultado_gastos_agg, by = "funcao") %>%
  drop_na(funcao, valorLiquidado)

# Padronizando os dados para análise de agrupamento
base_num <- base_completa %>%
  select(-funcao) %>%
  mutate(across(everything(), as.numeric))

dados_std <- scale(base_num)
rownames(dados_std) <- base_completa$funcao

# Criando os clusters hierárquicos
dist_matrix <- dist(dados_std)
hc <- hclust(dist_matrix, method = "ward.D2")

# Plotando o dendrograma
plot(hc, main = "Dendrograma das Funções", xlab = "", sub = "")

# Cortando o dendrograma em 3 grupos
grupos <- cutree(hc, k = 3)

# Adicionando os clusters à base original
base_completa$cluster <- as.factor(grupos)

# Visualizando os clusters com PCA
fviz_cluster(list(data = dados_std, cluster = grupos),
             labelsize = 10, repel = TRUE,
             main = "Cluster de Funções (Demanda x Valor Liquidado)")

# Resumo estatístico dos grupos
base_completa %>%
  group_by(cluster) %>%
  summarise(across(where(is.numeric), mean, na.rm = TRUE))
