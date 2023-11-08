#Instação de pacotes
install.packages("pacman")
pacman::p_load(
  tidyverse, igraph, remotes, abjutils, tm, stopwords)
remotes::install_github("jjesusfilho/stf")
library(stf)
#
# Download dos acórdãos do STF com o termo "Direitos Fundamentais"
dir.create("Raw")

stf::stf_baixar_cjsg("estrangeira citada", 
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
#
# Escolher as colunas do df
sub_internacional <- base_sem_acento[, c(10, 25, 33, 44)]
rm(base_sem_acento)
rm(base_sem_na)
rm(observacoes_unicas)
rm(coluna)
save(sub_internacional, file = "Bases/02_sub_internacional.rdata")
