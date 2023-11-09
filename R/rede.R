#Instação de pacotes
install.packages("pacman")
pacman::p_load(
  tidyverse, igraph, remotes, abjutils, tm, stopwords)
if (!"stf" %in% installed.packages()){
  remotes::install_github("jjesusfilho/stf")
}
library(stf)
#
# Download dos acórdãos do STF com o termo "Direitos Fundamentais"
dir.create("Raw")

stf::stf_baixar_cjsg("internacional", 
                     base = "acordaos",
                     dir = "Raw")

base_original <- stf::stf_read_cjsg(dir = "Raw")
dir.create("Bases")
save(base_original, file = "Bases/00_base_original.rdata")

#Tirando colunas com NA da base
colunas_com_todos_na <- colSums(is.na(base_original)) == nrow(base_original)
base_sem_na <- base_original[, !colunas_com_todos_na]
rm(colunas_com_todos_na)
rm(base_original)
# Removendo acentos e deixando tudo em maiúsculo
base_sem_acento <- base_sem_na
for (coluna in colnames(base_sem_acento)) {
  base_sem_acento[[coluna]] <- abjutils::rm_accent(base_sem_acento[[coluna]])
}
base_sem_acento <- as.data.frame(lapply(base_sem_acento, toupper))
# Criar uma variável com observações únicas das colunas
observacoes_unicas <- as.data.frame(unique(c(base_sem_acento$relator_processo_nome,
                                             base_sem_acento$relator_acordao_nome,
                                             base_sem_acento$revisor_processo_nome)))

# Removendo espaços duplicados
for (coluna in colnames(base_sem_acento)) {
  base_sem_acento[[coluna]] <- gsub("\\s+", " ", base_sem_acento[[coluna]])
}
# Removendo espaço no começo ou final
for (coluna in colnames(base_sem_acento)) {
  base_sem_acento[[coluna]] <- trimws(base_sem_acento[[coluna]])
}

# Remove as linhas duplicadas
base_sem_acento <- distinct(base_sem_acento)
base_sem_acento <- distinct(base_sem_acento, id, julgamento_data, .keep_all = TRUE)
save(base_sem_acento, file = "Bases/01_base_sem_acento.rdata")
rm(base_sem_na)
rm(observacoes_unicas)
rm(coluna)
#
# Escolher as colunas do df
sub_internacional <- base_sem_acento[, c(10, 26, 27, 45)]
rm(base_sem_acento)

# Removendo observações que não tenham LEG-INT
sub_internacional <- sub_internacional %>%
  filter(str_detect(documental_legislacao_citada_texto, "LEG-INT"))
save(sub_internacional, file = "Bases/02_sub_internacional.rdata")

# Definir a expressão regular
regex <- 'LEG-INT[^"]*"'

# Criar um dataframe temporário com as ocorrências separadas
temp_df <- sub_internacional %>%
  mutate(matches = str_extract_all(documental_legislacao_citada_texto, regex)) %>%
  unnest_longer(matches)

# Selecionar apenas as colunas de interesse
sub_internacional <- temp_df %>% select(id, matches, titulo, julgamento_data)

# Removendo infos irrelavantes
sub_internacional <- sub_internacional %>%
  mutate(matches = str_replace_all(matches, "\\\\N", ""))
sub_internacional <- sub_internacional %>%
  mutate(matches = str_replace_all(matches, "\\\\R", ""))

# Dividir a coluna matches em várias colunas
sub_internacional <- sub_internacional %>%
  mutate(
    tipo = str_extract(matches, "(?<=LEG-INT[- ]{1})\\b.{3}\\b"),
    ano = str_extract(matches, "(?<=ANO-)[0-9]+"),
    nome = str_replace(matches, "LEG-INT[- ]{1}\\b.{3}\\b ANO-[0-9]+ ", "")
  ) %>%
  select(-matches)

sub_internacional <- sub_internacional %>%
  mutate(tipo = ifelse(is.na(tipo) & id == "SJUR199542", "CVC", tipo))

sub_internacional <- sub_internacional %>%
  filter(tipo != "LEI")
sub_internacional <- sub_internacional %>%
  filter(tipo != "PRT")
sub_internacional <- sub_internacional %>%
  filter(tipo != "DEL")
sub_internacional <- sub_internacional %>%
  filter(tipo != "DEC")
sub_internacional <- sub_internacional %>%
  arrange(nome, ano) %>%
  fill(ano, .direction = "down")
sub_internacional <- sub_internacional %>%
  mutate(nome = str_replace(nome, paste0("LEG-INT ", tipo), ""))
sub_internacional <- sub_internacional %>%
  mutate(nome = str_replace_all(nome, "\\d+ ANO-\\d+", ""))
sub_internacional <- sub_internacional %>%
  mutate(nome = str_replace_all(nome, "[()]", ""))
sub_internacional <- sub_internacional %>%
  mutate(nome = str_replace_all(nome, "-", " "))
sub_internacional <- sub_internacional %>%
  mutate(nome = str_replace_all(nome, "ART [0-9]+", ""))
sub_internacional <- sub_internacional %>%
  mutate(nome = str_replace_all(nome, "ITEM [0-9]+", ""))
sub_internacional <- sub_internacional %>%
  mutate(nome = str_replace_all(nome, "INC [0-9]+", ""))
sub_internacional <- sub_internacional %>%
  mutate(nome = str_replace_all(nome, "PAR [0-9]+", ""))
sub_internacional <- sub_internacional %>%
  mutate(nome = str_replace_all(nome, "CAPITULO [0-9]+", ""))
sub_internacional <- sub_internacional %>%
  mutate(nome = str_replace_all(nome, "LET [0-9]{4}[A-Za-z]", ""))
sub_internacional <- sub_internacional %>%
  mutate(nome = str_replace_all(nome, "ANEXO [0-9]+", ""))
sub_internacional <- sub_internacional %>%
  mutate(nome = str_replace_all(nome, "LET [A-Za-z]+", ""))
sub_internacional <- sub_internacional %>%
  mutate(nome = str_replace_all(nome, "ITEM [A-Za-z][0-9]{2}", ""))
sub_internacional <- sub_internacional %>%
  mutate(nome = str_replace_all(nome, "LETRA [A-Za-z]+", ""))
sub_internacional <- sub_internacional %>%
  mutate(nome = str_replace_all(nome, "ANEXO [A-Za-z]+", ""))
sub_internacional <- sub_internacional %>%
  mutate(nome = str_replace_all(nome, "ALINEA [A-Za-z]+", ""))
sub_internacional <- sub_internacional %>%
  mutate(nome = str_replace_all(nome, "NUMERO [0-9]+", ""))
sub_internacional <- sub_internacional %>%
  mutate(nome = str_replace(nome, "^6", ""))
sub_internacional <- sub_internacional %>%
  mutate(nome = str_replace_all(nome, "^(\\.1|\\.1 ,|\\.1 , .2|\\.3|6|70/)", ""))
sub_internacional <- sub_internacional %>%
  mutate(nome = str_replace_all(nome, "(\\.1)", ""))
sub_internacional <- sub_internacional %>%
  mutate(nome = str_replace_all(nome, "(\\.2)", ""))
sub_internacional <- sub_internacional %>%
  mutate(nome = str_replace_all(nome, "000", ""))
sub_internacional <- sub_internacional %>%
  mutate(nome = str_replace(nome, "^A ", ""))
sub_internacional <- sub_internacional %>%
  mutate(nome = str_replace(nome, "^B ", ""))
sub_internacional <- sub_internacional %>%
  mutate(nome = str_replace(nome, "^E ", ""))
sub_internacional <- sub_internacional %>%
  mutate(nome = str_replace(nome, "^(\\d{3})(.*CONVENCAO)", "\\2 \\1"))
sub_internacional <- sub_internacional %>%
  mutate(nome = str_replace_all(nome, "ITEM .A", ""))
sub_internacional <- sub_internacional %>%
  mutate(nome = str_replace_all(nome, "REDACAO DADA PELO ", ""))
sub_internacional <- sub_internacional %>%
  mutate(nome = str_replace_all(nome, "^RATIFICADA PELO BRASIL EM 1992 ", ""))
sub_internacional <- sub_internacional %>%
  mutate(nome = str_replace_all(nome, "^REDACAO DADA PELOS PLT 1/75, PLT 2/75 E PLT 4/7", ""))
sub_internacional <- sub_internacional %>%
  mutate(nome = str_replace_all(nome, "PAR UNICO ", ""))
sub_internacional <- sub_internacional %>%
  mutate(nome = str_replace_all(nome, "PACTO PACTO ", "PACTO "))
sub_internacional <- sub_internacional %>%
  mutate(nome = str_replace_all(nome, "TRATADO TRATADO ", "TRATADO "))
sub_internacional <- sub_internacional %>%
  mutate(nome = str_replace(nome, "\"$", ""))
sub_internacional <- sub_internacional %>%
  mutate(nome = str_replace(nome, "^Nº1 ", ""))
sub_internacional <- sub_internacional %>%
  mutate(nome = str_replace(nome, "^Nº 7 ", ""))
sub_internacional <- sub_internacional %>%
  mutate(nome = str_replace(nome, "^LEG INT CVC ANO 1969 ", ""))
sub_internacional <- sub_internacional %>%
  mutate(nome = str_replace(nome, "^ITEM I ITEM II ITEM I ITEM II ITEM I ITEM II ", ""))
sub_internacional <- sub_internacional %>%
  mutate(nome = str_replace(nome, "^5 ", ""))
sub_internacional <- sub_internacional %>%
  mutate(nome = str_replace_all(nome, "[.,]", ""))
sub_internacional <- sub_internacional %>%
  mutate(nome = str_replace_all(nome, "O GOVERNO DA REPUBLICA FEDERATIVA DO ", ""))
sub_internacional <- sub_internacional %>%
  mutate(nome = str_replace_all(nome, "A REPUBLICA FEDERATIVA DO ", ""))
sub_internacional <- sub_internacional %>%
  mutate(nome = str_replace_all(nome, "OGOVERNO DOS ", ""))
sub_internacional <- sub_internacional %>%
  mutate(nome = str_replace_all(nome, "FIRMADO NO RIO E JANEIRO ", ""))
sub_internacional <- sub_internacional %>%
  mutate(nome = str_replace_all(nome, "RATIFICADO PELO BRASIL EM \\d+", ""))
sub_internacional <- sub_internacional %>%
  mutate(nome = str_replace_all(nome, "ASSINADA EM SAO JOSE DA COSTA RICA OEA", ""))
sub_internacional <- sub_internacional %>%
  mutate(nome = str_replace_all(nome, "ASSINADA EM VARSOVIA", ""))
sub_internacional <- sub_internacional %>%
  mutate(nome = str_replace_all(nome, "ORGANIZACAO DAS NACOES UNIDAS", "ONU"))
sub_internacional <- sub_internacional %>%
  mutate(nome = str_replace_all(nome, "AGENCIA INTERNACIONAL DE ENERGIA ATOMICA", "AIEA"))
sub_internacional <- sub_internacional %>%
  mutate(nome = str_replace_all(nome, "ESTADOS UNIDOS DA AMERICA", "EUA"))
sub_internacional <- sub_internacional %>%
  mutate(nome = str_replace_all(nome, "REPUBLICA DA", ""))
sub_internacional <- sub_internacional %>%
  mutate(nome = str_replace_all(nome, "REPUBLICA DO", ""))
sub_internacional <- sub_internacional %>%
  mutate(nome = ifelse(str_starts(nome, "CONVENCAO AMERICANA SOBRE DIREITOS HUMANOS"), "CONVENCAO AMERICANA SOBRE DIREITOS HUMANOS", nome))
sub_internacional <- sub_internacional %>%
  mutate(nome = str_replace_all(nome, "DE 10 DE DEZEMBRO DE 1998|CELEBRADO EM PARIS EM 12 DE DEZEMBRO DE 2015 E FIRMADO EM NOVA IORQUE EM 22 DE ABRIL DE 2016|FIRMADO NA CIDADE DO VATICANO EM 13 DE NOVEMBRO DE 2008|FIRMADO EM BRASILIA EM 23 DE SETEMBRO DE 2014|FIRMADO EM BRASILIA EM 20 DE MARCO DE 2007|EM 6 DE DEZEMBRO DE 2005|O GOVERNO DA|RATIFICADA PELO BRASIL EM 1992|ASSINADA EM SAO JOSE DA COSTA RICA", ""))
sub_internacional <- sub_internacional %>%
  mutate(nome = str_replace_all(nome, "ORGANIZACAO INTERNACIONAL DO TRABALHO OIT", "OIT"))
sub_internacional <- sub_internacional %>%
  mutate(nome = str_replace_all(nome, "ENTRE", ""))
# Removendo espaços duplicados
for (coluna in colnames(sub_internacional)) {
  sub_internacional[[coluna]] <- gsub("\\s+", " ", sub_internacional[[coluna]])
}
# Removendo espaço no começo ou final
for (coluna in colnames(sub_internacional)) {
  sub_internacional[[coluna]] <- trimws(sub_internacional[[coluna]])
}
rm(coluna)
rm(regex)
rm(temp_df)
save(sub_internacional, file = "Bases/02_sub_internacional.rdata")
#
tipos_unicos <- as.data.frame(unique(sub_internacional$nome))



