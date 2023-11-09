#Instação de pacotes
install.packages("pacman")
pacman::p_load(
  tidyverse, igraph, remotes, abjutils, tm, stopwords)
if (!"stf" %in% installed.packages()){
  remotes::install_github("jjesusfilho/stf")
}
library(stf)
#
# Download dos acórdãos do STF com o termo "internacional"
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

#Limpando dados
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
sub_internacional <- sub_internacional %>%
  mutate(nome = str_replace_all(nome, "ASSINADO EM LONDRES MOSCOU E WASHINGTON EM 1º DE JULHO DE 1968", ""))
sub_internacional <- sub_internacional %>%
  mutate(nome = str_replace_all(nome, "O GOVERNO DOS EUA", "EUA"))
sub_internacional <- sub_internacional %>%
  mutate(nome = str_replace_all(nome, "CONCLUIDO EM MONTEVIDEU A 18 DE FEVEREIRO DE 1960  ARGENTINA BRASIL CHILE MEXICO PARAGUAI PERU E URUGUAI", ""))
sub_internacional <- sub_internacional %>%
  mutate(nome = str_replace_all(nome, "FIRMADO EM HAIA EM 23 DE JANEIRO DE 2009", ""))
sub_internacional <- sub_internacional %>%
  mutate(nome = str_replace_all(nome, "TRATADO DE EXTRADICAO COM OS EUA E RESPECTIVO PROTOCOLO ADICIONAL", "TRATADO DE EXTRADICAO COM OS EUA"))
sub_internacional <- sub_internacional %>%
  mutate(nome = str_replace_all(nome, "DA CHILE", "DO CHILE"))
sub_internacional <- sub_internacional %>%
  mutate(nome = str_replace_all(nome, "O REINO DA ", ""))
sub_internacional <- sub_internacional %>%
  mutate(nome = str_replace_all(nome, "TRATADO DE EXTRADICAO CELEBRADO ", "TRATADO DE EXTRADICAO"))
sub_internacional <- sub_internacional %>%
  mutate(nome = str_replace_all(nome, "OS ESTADOS UNIDOS DO BRASIL E OS", "BRASIL E"))
sub_internacional <- sub_internacional %>%
  mutate(nome = str_replace_all(nome, "TRATADO DE EXTRADICAO COM OS EUA", "TRATADO DE EXTRADICAO BRASIL E EUA"))
sub_internacional <- sub_internacional %>%
  mutate(nome = str_replace_all(nome, "TRATADO DE EXTRADICAO COM A ARGENTINA", "TRATADO DE EXTRADICAO BRASIL E ARGENTINA"))
sub_internacional <- sub_internacional %>%
  mutate(nome = str_replace_all(nome, "TRATADO DE EXTRADICAO O BRASIL", "TRATADO DE EXTRADICAO BRASIL"))
sub_internacional <- sub_internacional %>%
  mutate(nome = str_replace_all(nome, "A PERU", "PERU"))
sub_internacional <- sub_internacional %>%
  mutate(nome = str_replace_all(nome, "ACORDO DE EXTRADICAO  OS ESTADOS PARTES DO MERCOSUL E A BOLIVIA E A CHILE", "ACORDO DE EXTRADICAO ESTADOS PARTES DO MERCOSUL"))
sub_internacional <- sub_internacional %>%
  mutate(nome = str_replace_all(nome, "ACORDO DE ACORDO DE EXTRADICAO  OS ", "ACORDO DE EXTRADICAO "))
sub_internacional <- sub_internacional %>%
  mutate(nome = str_replace_all(nome, "ACORDO DE EXTRADICAO  OS ESTADOS PARTES DO MERCOSUL", "ACORDO DE EXTRADICAO ESTADOS PARTES DO MERCOSUL"))
sub_internacional <- sub_internacional %>%
  mutate(nome = str_replace_all(nome, "ACORDO GERAL SOBRE TARIFAS ADUANEIRAS E COMERCIO GATT", "ACORDO GERAL SOBRE TARIFAS ADUANEIRAS E COMERCIO"))
sub_internacional <- sub_internacional %>%
  mutate(nome = str_replace_all(nome, "ACORDO GERAL SOBRE TARIFAS E COMERCIO", "ACORDO GERAL SOBRE TARIFAS ADUANEIRAS E COMERCIO"))
sub_internacional <- sub_internacional %>%
  mutate(nome = str_replace_all(nome, "ACORDO GERAL SOBRE TARIFAS E COMERCIO GATT", "ACORDO GERAL SOBRE TARIFAS ADUANEIRAS E COMERCIO"))
sub_internacional <- sub_internacional %>%
  mutate(nome = str_replace_all(nome, "ADOTADO PELO CONSELHO GERAL DA ORGANIZACAO MUNDIAL DO COMERCIO", ""))
sub_internacional <- sub_internacional %>%
  mutate(nome = ifelse(str_detect(nome, "CONVENCAO AMERICANA DE DIREITOS HUM"), "CONVENCAO AMERICANA DE DIREITOS HUMANOS", nome))
sub_internacional <- sub_internacional %>%
  mutate(nome = ifelse(str_detect(nome, "CONVENCAO AMERICANA SOBRE DIREITOS HUM"), "CONVENCAO AMERICANA DE DIREITOS HUMANOS", nome))
sub_internacional <- sub_internacional %>%
  mutate(nome = str_replace_all(nome, "CONVENCAO AMERICANA SOBRE OS DIREITOS HUMANOS", "CONVENCAO AMERICANA DE DIREITOS HUMANOS"))
sub_internacional <- sub_internacional %>%
  mutate(nome = str_replace_all(nome, "CONVENCAO CONCERNENTE A INSPECAO DO TRABALHO NA INDUSTRIA E NO COMERCIO ADOTADA PELA CONFERENCIA EM SUA TRIGESIMA SESSAO GENEBRA DE 19 DE JUNHO DE 1947", "CONVENCAO CONCERNENTE A INSPECAO DO TRABALHO NA INDUSTRIA E NO COMERCIO DA OIT"))
sub_internacional <- sub_internacional %>%
  mutate(nome = str_replace_all(nome, "CONVENCAO CONTRA A TORTURA E OUTROS TRATAMENTOS OU PENAS CRUEISDESUMANAS OU DEGRADANTES", "CONVENCAO CONTRA A TORTURA E OUTROS TRATAMENTOS OU PENAS CRUEIS DESUMANOS OU DEGRADANTES"))
sub_internacional <- sub_internacional %>%
  mutate(nome = str_replace_all(nome, "CONVENCAO CONVENCAO", "CONVENCAO"))
sub_internacional <- sub_internacional %>%
  mutate(nome = str_replace_all(nome, "PROTOCOLO CONTRA A FABRICACAO E O TRAFICO ILICITO DE ARMAS DE FOGO SUAS PECAS COMPONENTES E MUNICOES COMPLEMENTANDO A CONVENCAO DAS NACOES UNIDAS CONTRA O CRIME ORGANIZADO TRANSNACIONAL ADOTADO EM NOVA YORK EM 31 DE MAIO DE 2001", "PROTOCOLO CONTRA A FABRICACAO E O TRAFICO ILICITO DE ARMAS DE FOGO SUAS PECAS COMPONENTES E MUNICOES"))
sub_internacional <- sub_internacional %>%
  mutate(nome = str_replace_all(nome, "CONVENCAO DAS NACOES UNIDAS CONTRA O CRIME", "CONVENCAO DAS NACOES UNIDAS CONTRA O CRIME ORGANIZADO TRANSNACIONAL"))
sub_internacional <- sub_internacional %>%
  mutate(nome = ifelse(str_detect(nome, "CONVENCAO DAS NACOES UNIDAS CONTRA O CRIMEORGANIZADO"), "CONVENCAO DAS NACOES UNIDAS CONTRA O CRIME ORGANIZADO TRANSNACIONAL", nome))
sub_internacional <- sub_internacional %>%
  mutate(nome = ifelse(str_detect(nome, "CONVENCAO DAS NACOES UNIDAS CONTRA O CRIME ORGANIZADO"), "CONVENCAO DAS NACOES UNIDAS CONTRA O CRIME ORGANIZADO TRANSNACIONAL", nome))
sub_internacional <- sub_internacional %>%
  mutate(nome = ifelse(str_detect(nome, "PALERMO"), "CONVENCAO DAS NACOES UNIDAS CONTRA O CRIME ORGANIZADO TRANSNACIONAL", nome))
sub_internacional <- sub_internacional %>%
  mutate(nome = str_replace_all(nome, "ASSINADA EM GENEBRA OIT", ""))
sub_internacional <- sub_internacional %>%
  mutate(nome = str_replace_all(nome, "CONCLUIDA EM GENEBRA EM 19 DE JUNHO DE 1981", ""))
sub_internacional <- sub_internacional %>%
  mutate(nome = ifelse(str_detect(nome, "CONVENCAO UNICA SOBRE ENTORPECENTES"), "CONVENCAO UNICA SOBRE ENTORPECENTES", nome))
sub_internacional <- sub_internacional %>%
  mutate(nome = ifelse(str_detect(nome, "TRAFICO ILICITO DE ENTORPECENTES E SUBSTANCIAS PSICOTROPICAS"), "CONVENCAO CONTRA O TRAFICO ILICITO DE ENTORPECENTES E SUBSTANCIAS PSICOTROPICAS", nome))
sub_internacional <- sub_internacional %>%
  mutate(nome = str_replace_all(nome, "ADOTADA PELA ASSEMBLEIA GERAL DAS NACOES UNIDAS EM 31 DE OUTUBRO DE 2003 E ASSINADA PELO BRASIL EM 9 DE DEZEMBRO DE 2003", ""))
sub_internacional <- sub_internacional %>%
  mutate(nome = str_replace_all(nome, "FIRMADA EM 1978", ""))
sub_internacional <- sub_internacional %>%
  mutate(nome = str_replace_all(nome, "CONCLUIDA EM GENEBRA EM 22 DE JUNHO DE 1981", ""))
sub_internacional <- sub_internacional %>%
  mutate(nome = str_replace_all(nome, "CONCLUIDA EM MONTEGO BAY JAMAICA EM 10 DE DEZEMBRO DE 1982", ""))
sub_internacional <- sub_internacional %>%
  mutate(nome = str_replace_all(nome, "DE 9 DE SETEMBRO DE 1886 REVISTA EM PARIS A 24 DE JULHO DE 1971", ""))
sub_internacional <- sub_internacional %>%
  mutate(nome = str_replace_all(nome, "CONCLUIDA EM PARIS EM 17 DE DEZEMBRO DE 1997", ""))
sub_internacional <- sub_internacional %>%
  mutate(nome = str_replace_all(nome, "DE 29 DE MARCO DE 1996", ""))
sub_internacional <- sub_internacional %>%
  mutate(nome = str_replace_all(nome, "CONVENCAO DA ONU CONTRA A CORRUPCAO CONVENCAO DE MERIDA", "CONVENCAO DAS NACOES UNIDAS CONTRA A CORRUPCAO"))
sub_internacional <- sub_internacional %>%
  mutate(nome = str_replace_all(nome, "3
CONVENCAO DAS NACOES UNIDAS CONTRA A CORRUPCAO ", "CONVENCAO DAS NACOES UNIDAS CONTRA A CORRUPCAO"))
sub_internacional <- sub_internacional %>%
  mutate(nome = str_replace_all(nome, "ADOTADA NAQUELA CIDADE EM 22 DE MAIO DE 2001 ", ""))
sub_internacional <- sub_internacional %>%
  mutate(nome = str_replace_all(nome, "ASSINADA EM HAVANA CODIGO BUSTAMENTE", ""))
sub_internacional <- sub_internacional %>%
  mutate(nome = str_replace_all(nome, "FIRMADA EM CIDADE DA PRAIA CABO VERDE EM 23 DE NOVEMBRO DE 2005", ""))
sub_internacional <- sub_internacional %>%
  mutate(nome = str_replace_all(nome, "ADOTADA NAQUELA CIDADE EM 22 DE MAIO DE 2001", ""))
sub_internacional <- sub_internacional %>%
  mutate(nome = str_replace_all(nome, "FIRMADA PELBRASIL EM KUMAMOTO EM 10 DE OUTUBRO DE 2013", ""))
sub_internacional <- sub_internacional %>%
  mutate(nome = str_replace_all(nome, "ASSINADA EM VIENA EM 20 DE SETEMBRO DE 1994", ""))
sub_internacional <- sub_internacional %>%
  mutate(nome = str_replace_all(nome, "CONVENCAO DE VARSOVIA CONVENCAO PARA UNIFICACAO DE CERTAS REGRAS RELATIVAS AO TRANSPORTE AEREO INTERNACIONAL", "CONVENCAO DE VARSOVIA"))
sub_internacional <- sub_internacional %>%
  mutate(nome = ifelse(str_detect(nome, "CONVENCAO DE VARSOVIA"), "CONVENCAO DE VARSOVIA", nome))
sub_internacional <- sub_internacional %>%
  mutate(nome = str_replace_all(nome, "CONCLUIDA EM 23 DE MAIO DE 1969 COM RESERVA AOS ARTIGOS 25 E 66", ""))
sub_internacional <- sub_internacional %>%
  mutate(nome = str_replace_all(nome, "CONCLUIDA EM 23 DE MAIO DE 1969", ""))
sub_internacional <- sub_internacional %>%
  mutate(nome = str_replace_all(nome, "DOBRE", "SOBRE"))
sub_internacional <- sub_internacional %>%
  mutate(nome = str_replace_all(nome, "CONVENCAO DE VIENA SOBRE OS TRATADOS", "CONVENCAO DE VIENA SOBRE O DIREITO DOS TRATADOS"))
sub_internacional <- sub_internacional %>%
  mutate(nome = ifelse(str_detect(nome, "CONVENCAO EUROPEIA DOS DIREITOS DO HOMEM"), "CONVENCAO EUROPEIA DOS DIREITOS DO HOMEM", nome))
sub_internacional <- sub_internacional %>%
  mutate(nome = str_replace_all(nome, "CONVENCAO EUROPEIA PARA SALVAGUARDA DOS DIREITOS DO HOMEM E DAS LIBERDADES FUNDAMENTAIS", "CONVENCAO EUROPEIA DOS DIREITOS HUMANOS"))
sub_internacional <- sub_internacional %>%
  mutate(nome = str_replace_all(nome, "CONCLUIDA EM WASHINGTON EM 14 DE NOVEMBRO DE 1997", ""))
sub_internacional <- sub_internacional %>%
  mutate(nome = str_replace_all(nome, "FIRMADO PELBRASIL NA GUATEMALA EM 5 DE JUNHO DE 2013", ""))
sub_internacional <- sub_internacional %>%
  mutate(nome = str_replace_all(nome, "ASSINADA EM BARBADOS EM 3 DE JUNHO DE 2002", ""))
sub_internacional <- sub_internacional %>%
  mutate(nome = str_replace_all(nome, "ASSINADA EM BARBADOS", ""))
sub_internacional <- sub_internacional %>%
  mutate(nome = str_replace_all(nome, "CONCLUIDA EM BELEM DO PARA EM 9 DE JUNHO DE 1994", ""))
sub_internacional <- sub_internacional %>%
  mutate(nome = str_replace_all(nome, "FIRMADA PELBRASIL EM BELEM EM 10 DE JUNHO DE 1994", ""))
sub_internacional <- sub_internacional %>%
  mutate(nome = str_replace_all(nome, "FIRMADO NO RIO DE JANEIRO A 28 DE DEZEMBRO DE 1933 E O RESPECTIVO PROTOCOLO ADICIONAL FIRMADO NO RIO DE JANEIRO A 18 DE SETEMBRO DE 1935", ""))
sub_internacional <- sub_internacional %>%
  mutate(nome = str_replace_all(nome, "FIRMADO NO RIO DE JANEIRO D F A 23 DE JULHO DE 1932", ""))
sub_internacional <- sub_internacional %>%
  mutate(nome = str_replace_all(nome, "ASSINADO NO RIO DE JANEIRO EM 6 DE MAIO DE 1953", ""))
sub_internacional <- sub_internacional %>%
  mutate(nome = str_replace_all(nome, "EPORTUGAL", "E PORTUGAL"))
sub_internacional <- sub_internacional %>%
  mutate(nome = str_replace_all(nome, "EESPANHA", "E ESPANHA"))
sub_internacional <- sub_internacional %>%
  mutate(nome = str_replace_all(nome, "E O MEXICO", "E MEXICO"))
sub_internacional <- sub_internacional %>%
  mutate(nome = str_replace_all(nome, "E A SUICA", "E SUICA"))
sub_internacional <- sub_internacional %>%
  mutate(nome = str_replace_all(nome, "E A BELGICA", "E BELGICA"))
sub_internacional <- sub_internacional %>%
  mutate(nome = str_replace_all(nome, "O BRASIL", "BRASIL"))
sub_internacional <- sub_internacional %>%
  mutate(nome = str_replace_all(nome, "EXTRADICABRASIL", "EXTRADICAO BRASIL"))
sub_internacional <- sub_internacional %>%
  mutate(nome = str_replace_all(nome, "TRATADO DE EXTRADICAO OS ESTADOS PARTES DO MERCOSUL E AS REPUBLICAS DA BOLIVIA E DO CHILE", "TRATADO DE EXTRADICAO ESTADOS PARTES DO MERCOSUL BOLIVIA E CHILE"))
sub_internacional <- sub_internacional %>%
  mutate(nome = str_replace_all(nome, "TRATADO DE EXTRADICAO OS PAISES PARTE DO MERCOSUL DA BOLIVIA E DO CHILE", "TRATADO DE EXTRADICAO ESTADOS PARTES DO MERCOSUL BOLIVIA E CHILE"))
sub_internacional <- sub_internacional %>%
  mutate(nome = str_replace_all(nome, " REPUBLICA POTUGUESA", "PORTUGAL"))
sub_internacional <- sub_internacional %>%
  mutate(nome = str_replace_all(nome, " REPUBLICA PORTUGUESA", "PORTUGAL"))
sub_internacional <- sub_internacional %>%
  mutate(nome = str_replace_all(nome, "REPUBLICA FRANCESA CELEBRADO EM PARIS EM 28 DE MAIO DE 1996", "FRANCA"))
sub_internacional <- sub_internacional %>%
  mutate(nome = str_replace_all(nome, "CELEBRADO EM LIMA EM 25 DE AGOSTO DE 2003", ""))
sub_internacional <- sub_internacional %>%
  mutate(nome = str_replace_all(nome, "CELEBRADO EM LIMA", ""))
sub_internacional <- sub_internacional %>%
  mutate(nome = str_replace_all(nome, "CONVENCAO SOBRE DIREITOS DAS PESSOAS COM DEFICIENCIAS ASSINADO EM 30 DE MARCO DE 2007", "CONVENCAO SOBRE OS DIREITOS DAS PESSOAS COM DEFICIENCIA"))
sub_internacional <- sub_internacional %>%
  mutate(nome = str_replace_all(nome, "CONVENCAO INTERNACIONAL SOBRE OS DIREITOS DAS PESSOAS COM DEFICIENCIA E SEU PROTOCOLO FACULTATIVO ASSINADOS EM NOVA YORK EM 30 DE MARCO DE 2007", "CONVENCAO SOBRE OS DIREITOS DAS PESSOAS COM DEFICIENCIA"))
sub_internacional <- sub_internacional %>%
  mutate(nome = str_replace_all(nome, "CONVENCAO INTERNACIONAL SOBRE OS DIREITOS DAS PESSOAS COM DEFICIENCIA E SEU PROTOCOLO FACULTATIVO", "CONVENCAO SOBRE OS DIREITOS DAS PESSOAS COM DEFICIENCIA"))
sub_internacional <- sub_internacional %>%
  mutate(nome = str_replace_all(nome, "CONVENCAO INTERNACIONAL SOBRE OS DIREITOS DAS PESSOAS COM DEFICIENCIA ASSINADA EM NOVA YORK EM 30 DE MARCO DE 2007", "CONVENCAO SOBRE OS DIREITOS DAS PESSOAS COM DEFICIENCIA"))
sub_internacional <- sub_internacional %>%
  mutate(nome = str_replace_all(nome, "CONVENCAO INTERNACIONAL DAS NACOES UNIDAS SOBRE OS DIREITOS DAS PESSOAS COM DEFICIENCIA", "CONVENCAO SOBRE OS DIREITOS DAS PESSOAS COM DEFICIENCIA"))
sub_internacional <- sub_internacional %>%
  mutate(nome = str_replace_all(nome, "ACORDBRASIL", "ACORDO BRASIL"))
sub_internacional <- sub_internacional %>%
  mutate(nome = str_replace_all(nome, "AMERICA ASSINADA PELBRASIL A 27 DE DEZEMBRO DE 1940", "AMERICAS"))
sub_internacional <- sub_internacional %>%
  mutate(nome = str_replace_all(nome, "CELEBRADA EM MONTREAL EM 28 DE MAIO DE 1999", ""))
sub_internacional <- sub_internacional %>%
  mutate(nome = str_replace_all(nome, "CONCLUIDA EM WASHINGTON EM 2 DE FEVEREIRO DE 1971", ""))
sub_internacional <- sub_internacional %>%
  mutate(nome = str_replace_all(nome, " EDAS", " E DAS"))
sub_internacional <- sub_internacional %>%
  mutate(nome = str_replace_all(nome, "ASSINADA EM NOVA YORK EM 9 DE MAIO DE 1992", ""))
sub_internacional <- sub_internacional %>%
  mutate(nome = str_replace_all(nome, "ASSINADA EM GENEBRA", ""))
sub_internacional <- sub_internacional %>%
  mutate(nome = str_replace_all(nome, "CONCLUIDA EM GENEBRA EM 28 DE JULHO DE 1951", ""))
sub_internacional <- sub_internacional %>%
  mutate(nome = str_replace_all(nome, "ADOTADA PELA ASSEMBLEIA GERAL DAS NACOES UNIDAS ONU", ""))
sub_internacional <- sub_internacional %>%
  mutate(nome = str_replace_all(nome, "CONVENCAO QUADRO DAS NACOES UNIDAS SOBRE A MUDANCA DO CLIMA", "CONVENCAO QUADRO DAS NACOES UNIDAS SOBRE MUDANCA DO CLIMA"))
sub_internacional <- sub_internacional %>%
  mutate(nome = str_replace_all(nome, "CELEBRADO EM BRASILIA EM 17 DE NOVEMBRO DE 2003", ""))
sub_internacional <- sub_internacional %>%
  mutate(nome = str_replace_all(nome, "E ESTADOS UNIDOS DAAMERICA", "E EUA"))
sub_internacional <- sub_internacional %>%
  mutate(nome = str_replace_all(nome, "MINICANA", "REPUBLICA DOMINICANA"))
sub_internacional <- sub_internacional %>%
  mutate(nome = ifelse(str_detect(nome, "CONVENCAO SOBRE A ELIMINACAO DE TODAS AS FORMAS DE DISCRIMINACAO CONTRA A MULHER"), "CONVENCAO SOBRE A ELIMINACAO DE TODAS AS FORMAS DE DISCRIMINACAO CONTRA A MULHER", nome))
sub_internacional <- sub_internacional %>%
  mutate(nome = ifelse(str_detect(nome, "CONVENCAO RELATIVA A APLICACAO DOS PRINCIPIOS DO DIREITO DE ORGANIZACAO E DE NEGOCIACAO COLETIVA"), "CONVENCAO RELATIVA A APLICACAO DOS PRINCIPIOS DO DIREITO DE ORGANIZACAO E DE NEGOCIACAO COLETIVA DA OIT", nome))
sub_internacional <- sub_internacional %>%
  mutate(nome = str_replace_all(nome, "DEGUERRA", "DE GUERRA"))
sub_internacional <- sub_internacional %>%
  mutate(nome = str_replace_all(nome, "DA OIT", "OIT"))
sub_internacional <- sub_internacional %>%
  mutate(nome = str_replace_all(nome, "EMENDADA PELO PROTOCOLO DE 1º DE JUNHO DE 2010 FIRMADA PELBRASIL EM CANNES EM 3 DE NOVEMBRO DE 2011", ""))
sub_internacional <- sub_internacional %>%
  mutate(nome = str_replace_all(nome, "CONCLUIDA EM CHICAGO A 7 DE DEZEMBRO DE 1944 E FIRMADO PELBRASIL EM WASHINGTON A 29 DE MAIO DE 1945", ""))
sub_internacional <- sub_internacional %>%
  mutate(nome = str_replace_all(nome, "ASSINADA DURANTE A CONFERENCIA DAS NACOES UNIDAS SOBRE MEIO AMBIENTE E DESENVOLVIMENTO", ""))
sub_internacional <- sub_internacional %>%
  mutate(nome = str_replace_all(nome, "ASSINADA NO RIO DE JANEIRO EM 05 DE JUNHO DE 1992", ""))
sub_internacional <- sub_internacional %>%
  mutate(nome = str_replace_all(nome, "ASSINADA EM BRASILIA", ""))
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



