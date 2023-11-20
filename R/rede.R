#Instação de pacotes
install.packages("pacman")
pacman::p_load(
  tidyverse, igraph, remotes, abjutils, tm, stopwords, kableExtra)
if (!"stf" %in% installed.packages()){
  remotes::install_github("jjesusfilho/stf")
}
library(stf)
#
# Download dos acórdãos do STF com o termo "internacional": 2947 observações iniciais (2325 casos distintos) reduzida para 1101 observações (552 casos) após exclusão dos casos sem citação a legislação estrangeira com indicação LEG-INT na coluna documental_legislacao_citada_texto
#Período: 06/1950 a 07/2023
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
  mutate(nome = str_replace_all(nome, "CONVENCAO DAS NACOES UNIDAS CONTRA A CORRUPCAO ", "CONVENCAO DAS NACOES UNIDAS CONTRA A CORRUPCAO"))
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
sub_internacional <- sub_internacional %>%
  mutate(nome = str_replace_all(nome, "CONCLUIDA EM GENEBRA A 29 DE ABRIL DE 1958", ""))
sub_internacional <- sub_internacional %>%
  mutate(nome = str_replace_all(nome, "CONVENCAO PARA A REDUCAO DOS CASOS SOBRE O ESTATUTO DOS APATRIDIA", "CONVENCAO PARA A REDUCAO DOS CASOS DE APATRIDIA"))
sub_internacional <- sub_internacional %>%
  mutate(nome = str_replace_all(nome, "FIRMADA PELOS PLENIPOTENCIARIOS DBRASIL NA SEGUNDA CONFERENCIA DA PAZ EM 1907 NA HAYA", "NA SEGUNDA CONFERENCIA DE PAZ DE 1907"))
sub_internacional <- sub_internacional %>%
  mutate(nome = str_replace_all(nome, "CONVENCAO OIT SOBRE POVOS INDIGENAS E TRIBAIS", "CONVENCAO NO 169 OIT SOBRE POVOS INDIGENAS E TRIBAIS"))
sub_internacional <- sub_internacional %>%
  mutate(nome = ifelse(str_detect(nome, "ASBESTO"), "CONVENCAO Nº 162 OIT SOBRE A UTILIZACAO DO ASBESTO COM SEGURANCA", nome))
sub_internacional <- sub_internacional %>%
  mutate(nome = ifelse(str_detect(nome, "CONVENCAO SOBRE O TERMINO DA RELACAO DE TRABALHO POR INICIATIVA DO EMPREGADOR"), "CONVENCAO SOBRE O TERMINO DA RELACAO DE TRABALHO POR INICIATIVA DO EMPREGADOR OIT", nome))
sub_internacional <- sub_internacional %>%
  mutate(nome = ifelse(str_detect(nome, "CONVENCAO SOBRE O TRABALHO FORCADO OU OBRIGATORIO"), "CONVENCAO SOBRE O TRABALHO FORCADO OU OBRIGATORIO OIT", nome))
sub_internacional <- sub_internacional %>%
  mutate(nome = ifelse(str_detect(nome, "CONVENCAO SOBRE OS DIREITOS DA CRIANCA"), "CONVENCAO SOBRE OS DIREITOS DAS CRIANCAS", nome))
sub_internacional <- sub_internacional %>%
  mutate(nome = str_replace_all(nome, "CONCLUIDA NA CIDADE DE HAIA EM 25 DE OUTUBRO DE 1980", ""))
sub_internacional <- sub_internacional %>%
  mutate(nome = str_replace_all(nome, "CONVENCAO SOBRE OS POVOS INDIGENAS E TRIBAIS OIT", "CONVENCAO SOBRE POVOS INDIGENAS E TRIBAIS OIT"))
sub_internacional <- sub_internacional %>%
  mutate(nome = str_replace_all(nome, "ADOTADA A 21 DE NOVEMBRO DE 1947 PELA ASSEMBLEIA GERAL DAS NACOES UNIDAS", ""))
sub_internacional <- sub_internacional %>%
  mutate(nome = str_replace_all(nome, "ADOTADA EM LONDRES A 13 DE FEVEREIRO DE 1946 POR OCASIAO DA ASSEMBLEIA GERAL DAS NACOES UNIDAS", ""))
sub_internacional <- sub_internacional %>%
  mutate(nome = str_replace_all(nome, "ADOTADA EM 10 DE SETEMBRO DE 1998 NA CIDADE DE ROTERDA", ""))
sub_internacional <- sub_internacional %>%
  mutate(nome = str_replace_all(nome, "CELEBRADA EM VIENA", ""))
sub_internacional <- sub_internacional %>%
  mutate(nome = str_replace_all(nome, "CONVENCAO SOBRE SUBSTANCIAS PSICOTROPICAS CONVENCAO DE VIENA", "CONVENCAO SOBRE SUBSTANCIAS PSICOTROPICAS"))
sub_internacional <- sub_internacional %>%
  mutate(nome = str_replace_all(nome, "CONHECIDA COMO CONVENCAO DE RAMSAR DE 02 DE FEVEREIRO DE 1971", ""))
sub_internacional <- sub_internacional %>%
  mutate(nome = str_replace_all(nome, "ESTUPEFACIENTES", "ENTORPECENTES"))
sub_internacional <- sub_internacional %>%
  mutate(nome = str_replace_all(nome, "CONCLUIDA EM GENEBRA A 6 DE SETEMBRO DE 1952", ""))
sub_internacional <- sub_internacional %>%
  mutate(nome = str_replace_all(nome, "CONVENCAO UNICA SOBRE ENTORPECENTES", "CONVENCAO UNICA DE NOVA IORQUE SOBRE ENTORPECENTES"))
sub_internacional <- sub_internacional %>%
  mutate(nome = str_replace_all(nome, "CONCLUIDA EM LONDRES A 29 DE DEZEMBRO DE 1972", ""))
sub_internacional <- sub_internacional %>%
  mutate(nome = str_replace_all(nome, "PACTO INTERNACIONAL DOS DIREITOS CIVIS E POLITICOS 1966", "PACTO INTERNACIONAL DOS DIREITOS CIVIS E POLITICOS"))
sub_internacional <- sub_internacional %>%
  mutate(nome = str_replace_all(nome, "EPOLITICOS", "E POLITICOS"))
sub_internacional <- sub_internacional %>%
  mutate(nome = str_replace_all(nome, "POLITICOS ONU", "POLITICOS"))
sub_internacional <- sub_internacional %>%
  mutate(nome = str_replace_all(nome, "ACORDADO NA 58ª ASSEMBLEIA GERAL DA ORGANIZACAO MUNDIAL DE SAUDE EM 23 DE MAIO DE 2005", ""))
sub_internacional <- sub_internacional %>%
  mutate(nome = str_replace_all(nome, "A CONVENCAO QUADRO DAS NACOES UNIDAS SOBRE MUDANCA DO CLIMA ABERTO A ASSINATURAS NA CIDADE DE QUIOTO JAPAO EM 11 DE DEZEMBRO DE 1997 POR OCASIAO DA TERCEIRA CONFERENCIA DAS PARTES DA CONVENCAO QUADRO DAS NACOES UNIDAS SOBRE MUDANCA DO CLIMA", ""))
sub_internacional <- sub_internacional %>%
  mutate(nome = str_replace_all(nome, "TRATADBRASIL", "TRATADO BRASIL"))
sub_internacional <- sub_internacional %>%
  mutate(nome = str_replace_all(nome, "APORTUGAL ", "PORTUGAL"))
sub_internacional <- sub_internacional %>%
  mutate(nome = str_replace_all(nome, "PERTENCENTES EM CONDOMINIO AOS DOIS PAISES DESDE E INCLUSIVE O SALTO GRANDE DE SETE QUEDAS OU SALTO DE GUAIRA ATE A FOZ DO RIO IGUACU", ""))
sub_internacional <- sub_internacional %>%
  mutate(nome = str_replace_all(nome, "CELEBRADO EM PORTO SEGURO EM 22 DE ABRIL DE 2", ""))
sub_internacional <- sub_internacional %>%
  mutate(nome = str_replace_all(nome, "O GOVERNO DO ", ""))
sub_internacional <- sub_internacional %>%
  mutate(nome = str_replace_all(nome, " CELEBRADO EM BRASILIA EM 27 DE JANEIRO DE 1995", ""))
sub_internacional <- sub_internacional %>%
  mutate(nome = str_replace_all(nome, "NAVEGACABRASIL", "NAVEGACAO BRASIL"))
sub_internacional <- sub_internacional %>%
  mutate(nome = str_replace_all(nome, "E O URUGUAI ", "E URUGUAI "))
sub_internacional <- sub_internacional %>%
  mutate(nome = str_replace_all(nome, "RIO DE JANEIRO 25 DE AGOSTO DE 1933", ""))
sub_internacional <- sub_internacional %>%
  mutate(nome = str_replace_all(nome, "ASUICA", "SUICA"))
sub_internacional <- sub_internacional %>%
  mutate(nome = str_replace_all(nome, "A 6 DE MAIO DE 1953", ""))
sub_internacional <- sub_internacional %>%
  mutate(nome = str_replace_all(nome, "PROTOCOLO DE MONTREAL 1", "PROTOCOLO DE MONTREAL "))
sub_internacional <- sub_internacional %>%
  mutate(nome = str_replace_all(nome, "PROTOCOLO DE MONTREAL 2", "PROTOCOLO DE MONTREAL "))
sub_internacional <- sub_internacional %>%
  mutate(nome = str_replace_all(nome, "PROTOCOLO ADICIONAL ASSINADO EM MONTREAL EM 25 DE SETEMBRO DE 1975 QUE MODIFICA A CONVENCAO PARA A UNIFICACAO DE CERTAS REGRAS RELATIVAS AO TRANSPORTE AEREO INTERNACIONAL CONCLUIDA EM VARSOVIA EM 12 DE OUTUBRO DE 1929 E EMENDADA PELO PROTOCOLO CELEBRADO NA HAIA EM 28 DE SETEMBRO DE 1955", "PROTOCOLO DE MONTREAL "))
sub_internacional <- sub_internacional %>%
  mutate(nome = str_replace_all(nome, " DE ", " "))
sub_internacional <- sub_internacional %>%
  mutate(nome = str_replace_all(nome, " DOS ", " "))
sub_internacional <- sub_internacional %>%
  mutate(nome = str_replace_all(nome, "E O URUGUAI", "E URUGUAI"))
sub_internacional <- sub_internacional %>%
  mutate(nome = str_replace_all(nome, "E A PERU", "E PERU"))
sub_internacional <- sub_internacional %>%
  mutate(nome = str_replace_all(nome, "E A REPUBLICA ITALIANA 17 OUTUBRO 1989", "E ITALIA"))
sub_internacional <- sub_internacional %>%
  mutate(nome = str_replace_all(nome, "FIRMADO EM BRASILIA EM 12 NOVEMBRO 2004", ""))
sub_internacional <- sub_internacional %>%
  mutate(nome = str_replace_all(nome, " E A REPUBLICA POPULAR DA CHINA", " E CHINA"))
sub_internacional <- sub_internacional %>%
  mutate(nome = str_replace_all(nome, "DA BOLIVIA E DA CHILE", "BOLIVIA E CHILE"))
sub_internacional <- sub_internacional %>%
  mutate(nome = str_replace_all(nome, "^3 ", ""))
sub_internacional <- sub_internacional %>%
  mutate(nome = str_replace_all(nome, "^A ", ""))
sub_internacional <- sub_internacional %>%
  mutate(nome = str_replace_all(nome, "ONU ONU", "ONU"))
sub_internacional <- sub_internacional %>%
  mutate(nome = str_replace_all(nome, "FIRMADA EM CIDADE DA PRAIA CABO VERDE EM 23 NOVEMBRO 2005", ""))
sub_internacional <- sub_internacional %>%
  mutate(nome = str_replace_all(nome, "ACORDO EXTRADICAO ESTADOS PARTES DO MERCOSUL E A BOLIVIA E A CHILE", "ACORDO EXTRADICAO ESTADOS PARTES DO MERCOSUL BOLIVIA E CHILE"))
sub_internacional <- sub_internacional %>%
  mutate(nome = str_replace_all(nome, "TRATADO EXTRADICAO OS ESTADOS PARTES DO MERCOSUL E AS REPUBLICAS DA BOLIVIA E DO CHILE", "TRATADO EXTRADICAO ESTADOS PARTES DO MERCOSUL BOLIVIA E CHILE"))
sub_internacional <- sub_internacional %>%
  mutate(nome = str_replace_all(nome, "E A REPUBLICA ITALIANA", "E ITALIA"))
sub_internacional <- sub_internacional %>%
  mutate(nome = str_replace_all(nome, "TRATADO EXTRADICAO OS PAISES PARTE DO MERCOSUL BOLIVIA E CHILE", "TRATADO EXTRADICAO ESTADOS PARTES DO MERCOSUL BOLIVIA E CHILE"))
sub_internacional <- sub_internacional %>%
  mutate(nome = str_replace_all(nome, "PACTO INTERNACIONAL SOBRE DIREITOS CIVIS E POLITICOS ONU", "PACTO INTERNACIONAL SOBRE DIREITOS CIVIS E POLITICOS"))
sub_internacional <- sub_internacional %>%
  mutate(nome = str_replace_all(nome, "PACTO INTERNACIONAL SOBRE OS DIREITOS CIVIS E POLITICOS", "PACTO INTERNACIONAL SOBRE DIREITOS CIVIS E POLITICOS"))
sub_internacional <- sub_internacional %>%
  mutate(nome = str_replace_all(nome, "DA ORGANIZACAO INTERNACIONAL DO TRABALHO OIT", "OIT"))
sub_internacional <- sub_internacional %>%
  mutate(nome = str_replace_all(nome, "ADOTADA PELA CONFERENCIA EM SUA TRIGESIMA SESSAO GENEBRA 19 JUNHO 1947", ""))
sub_internacional <- sub_internacional %>%
  mutate(nome = str_replace_all(nome, "DA ORGANIZACAO INTERNACIONAL DO TRABALHO OIT", ""))
sub_internacional <- sub_internacional %>%
  mutate(nome = str_replace_all(nome, "CONVENCAO CONCERNENTE A INSPECAO DO TRABALHO NA INDUSTRIA E NO COMERCIO OIT", "CONVENCAO CONCERNENTE A INSPECAO DO TRABALHO NA INDUSTRIA E NO COMERCIO"))
sub_internacional <- sub_internacional %>%
  mutate(nome = str_replace_all(nome, "CONVENCAO RELATIVA A APLICACAO PRINCIPIOS DO DIREITO ORGANIZACAO E NEGOCIACAO COLETIVA OIT", "CONVENCAO N° 98 RELATIVA A APLICACAO PRINCIPIOS DO DIREITO ORGANIZACAO E NEGOCIACAO COLETIVA"))
sub_internacional <- sub_internacional %>%
  mutate(nome = str_replace_all(nome, "CONVENCAO SOBRE A ABOLICAO DO TRABALHO FORCADO OIT", "CONVENCAO RELATIVA A ABOLICAO DO TRABALHO FORCADO"))
sub_internacional <- sub_internacional %>%
  mutate(nome = str_replace_all(nome, "CONVENCAO A RECOMENDACAO Nº 146 SOBRE IDADE MINIMA ADMISSAO AO EMPREGO OIT", "CONVENCAO SOBRE A IDADE MINIMA OIT"))
sub_internacional <- sub_internacional %>%
  mutate(nome = str_replace_all(nome, "CONVENCAO OIT SOBRE A PREVENCAO E O CONTROLE RISCOS PROFISSIONAIS CAUSADOS PELAS SUBSTANCIAS OU AGENTES CANCERIGENOS", "CONVENCAO SOBRE A PREVENCAO E O CONTROLE RISCOS PROFISSIONAIS CAUSADOS PELAS SUBSTANCIAS OU AGENTES CANCERIGENOS OIT"))
sub_internacional <- sub_internacional %>%
  mutate(nome = str_replace_all(nome, "DA ORGANIZACAO INTERNACIONAL DO TRABALHO", ""))
sub_internacional <- sub_internacional %>%
  mutate(nome = str_replace_all(nome, "TRATADO EXTRADICAO OS PAISES PARTE DO MERCOSUL DA BOLIVIA E DA CHILE", "TRATADO EXTRADICAO MERCOSUL BOLIVIA E CHILE"))
sub_internacional <- sub_internacional %>%
  mutate(nome = str_replace_all(nome, "TRATADO EXTRADICAO OS ESTADOS PARTES DO MERCOSUL E AS REPUBLICAS DA BOLIVIA E DO CHILE", "TRATADO EXTRADICAO MERCOSUL BOLIVIA E CHILE"))
sub_internacional <- sub_internacional %>%
  mutate(nome = str_replace_all(nome, "APORTUGAL", "PORTUGAL"))
sub_internacional <- sub_internacional %>%
  mutate(nome = str_replace_all(nome, "A CONFEDERACAO SUICA CELEBRADO EM BERNA EM 12 MAIO 2004", "SUICA"))
sub_internacional <- sub_internacional %>%
  mutate(nome = str_replace_all(nome, "A CONFEDERACAO SUICA", "SUICA"))
sub_internacional <- sub_internacional %>%
  mutate(nome = str_replace_all(nome, "TRATADO COOPERACAO JUDICIAL BRASIL E SUICA", "TRATADO COOPERACAO JURIDICA EM MATERIA PENAL BRASIL E SUICA"))
sub_internacional <- sub_internacional %>%
  mutate(nome = str_replace_all(nome, "CONVENCAO SOBRE SUBSTANCIAS PSICOTROPICAS CONVENCAO VIENA", "CONVENCAO SOBRE SUBSTANCIAS PSICOTROPICAS"))
sub_internacional <- sub_internacional %>%
  mutate(nome = str_replace_all(nome, "CONVENCAO SOBRE SEGURANCA E SAUDE TRABALHADORES E O MEIO AMBIENTE TRABALHO OIT", "CONVENCAO SOBRE SEGURANCA E SAUDE TRABALHADORES E O MEIO AMBIENTE TRABALHO"))
sub_internacional <- sub_internacional %>%
  mutate(nome = str_replace_all(nome, "CONVENCAO SOBRE OS POVOS INDIGENAS E TRIBAIS OIT", "CONVENCAO SOBRE POVOS INDIGENAS E TRIBAIS OIT"))
sub_internacional <- sub_internacional %>%
  mutate(nome = str_replace_all(nome, "CONVENCAO SOBRE O INCENTIVO A NEGOCIACAO COLETIVA OIT", "CONVENCAO SOBRE O INCENTIVO A NEGOCIACAO COLETIVA"))
sub_internacional <- sub_internacional %>%
  mutate(nome = str_replace_all(nome, "ASSINADA DURANTE A CONFERENCIA DAS NACOES UNIDAS SOBRE MEIO AMBIENTE E DESENVOLVIMENTO", ""))
sub_internacional <- sub_internacional %>%
  mutate(nome = str_replace_all(nome, "CONVENCAO SOBRE POVOS INDIGENAS E TRIBAIS OIT", "CONVENCAO SOBRE POVOS INDIGENAS E TRIBAIS"))
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

# Ao final, há menção há cerca de 230 atos internacionais (Falta concluir a limpeza)
#
tipos_unicos <- as.data.frame(unique(sub_internacional$nome))
print(base_sem_acento$documental_legislacao_citada_texto[1191])

# Adicionar a coluna competência
sub_internacional <- sub_internacional %>%
  mutate(competencia = ifelse(grepl("\\s{2,}", titulo), "recursal", ifelse(grepl("^\\S+ \\S+$", titulo), "originaria", NA)))
sub_internacional <- sub_internacional %>%
  mutate(competencia = replace(competencia, is.na(competencia), "recursal"))

# Criar a coluna "classe" no dataframe sub_internacional
sub_internacional <- sub_internacional %>%
  mutate(classe = str_extract(titulo, "[A-Za-z]+(?=\\s*\\d)"))

# Criar a coluna contraparte
sub_internacional <- sub_internacional %>%
  mutate(contraparte = ifelse(grepl("FRANCA", nome), "FRANCA", NA))
sub_internacional <- sub_internacional %>%
  mutate(contraparte = ifelse(grepl("SUICA", nome), "SUICA", contraparte))
sub_internacional <- sub_internacional %>%
  mutate(contraparte = ifelse(grepl("EUA", nome), "EUA", contraparte))
sub_internacional <- sub_internacional %>%
  mutate(contraparte = ifelse(grepl("MERCOSUL", nome), "MERCOSUL", contraparte))
sub_internacional <- sub_internacional %>%
  mutate(contraparte = ifelse(is.na(contraparte) & grepl("ARGENTINA", nome), "ARGENTINA", contraparte))
sub_internacional <- sub_internacional %>%
  mutate(contraparte = ifelse(is.na(contraparte) & grepl("ITALIA", nome), "ITALIA", contraparte))
sub_internacional <- sub_internacional %>%
  mutate(contraparte = ifelse(is.na(contraparte) & grepl("URUGUAI", nome), "URUGUAI", contraparte))
sub_internacional <- sub_internacional %>%
  mutate(contraparte = ifelse(is.na(contraparte) & grepl("PERU", nome), "PERU", contraparte))
sub_internacional <- sub_internacional %>%
  mutate(contraparte = ifelse(is.na(contraparte) & grepl("GATT", nome), "OMC", contraparte))
sub_internacional <- sub_internacional %>%
  mutate(contraparte = ifelse(is.na(contraparte) & grepl("PACTO SAO JOSE DA COSTA", nome), "OEA", contraparte))
sub_internacional <- sub_internacional %>%
  mutate(contraparte = ifelse(is.na(contraparte) & grepl("CONVENCAO AMERICANA", nome), "OEA", contraparte))
sub_internacional <- sub_internacional %>%
  mutate(contraparte = ifelse(is.na(contraparte) & grepl("NACOES UNIDAS", nome), "ONU", contraparte))
sub_internacional <- sub_internacional %>%
  mutate(contraparte = ifelse(is.na(contraparte) & grepl("PORTUGAL", nome), "PORTUGAL", contraparte))
sub_internacional <- sub_internacional %>%
  mutate(contraparte = ifelse(is.na(contraparte) & grepl("ESPANHA", nome), "ESPANHA", contraparte))
sub_internacional <- sub_internacional %>%
  mutate(contraparte = ifelse(is.na(contraparte) & grepl("PARAGUAI", nome), "PARAGUAI", contraparte))
sub_internacional <- sub_internacional %>%
  mutate(contraparte = ifelse(is.na(contraparte) & grepl("CHINA", nome), "CHINA", contraparte))
sub_internacional <- sub_internacional %>%
  mutate(contraparte = ifelse(is.na(contraparte) & grepl("GRA BRETANHA", nome), "REINO UNIDO", contraparte))
sub_internacional <- sub_internacional %>%
  mutate(contraparte = ifelse(is.na(contraparte) & grepl("COLOMBIA", nome), "COLOMBIA", contraparte))
sub_internacional <- sub_internacional %>%
  mutate(contraparte = ifelse(is.na(contraparte) & grepl("OIT", nome), "OIT", contraparte))
sub_internacional <- sub_internacional %>%
  mutate(contraparte = ifelse(is.na(contraparte) & grepl("OMC", nome), "OMC", contraparte))
sub_internacional <- sub_internacional %>%
  mutate(contraparte = ifelse(is.na(contraparte) & grepl("HOLANDA", nome), "HOLANDA", contraparte))
sub_internacional <- sub_internacional %>%
  mutate(contraparte = ifelse(is.na(contraparte) & grepl("ONU", nome), "ONU", contraparte))
sub_internacional <- sub_internacional %>%
  mutate(contraparte = ifelse(is.na(contraparte) & grepl("CONVENCAO INTERAMERICANA", nome), "OEA", contraparte))
sub_internacional <- sub_internacional %>%
  mutate(contraparte = ifelse(is.na(contraparte) & grepl("SANTA SE", nome), "VATICANO", contraparte))
sub_internacional <- sub_internacional %>%
  mutate(contraparte = ifelse(is.na(contraparte) & grepl("ITAIPU", nome), "PARAGUAI", contraparte))
sub_internacional <- sub_internacional %>%
  mutate(contraparte = ifelse(is.na(contraparte) & grepl("PACTO INTERNACIONAL SOBRE DIREITOS ECONOMICO", nome), "ONU", contraparte))
sub_internacional <- sub_internacional %>%
  mutate(contraparte = ifelse(is.na(contraparte) & grepl("PACTO INTERNACIONAL SOBRE DIREITOS CIVIS E POLI", nome), "OEA", contraparte))
sub_internacional <- sub_internacional %>%
  mutate(contraparte = ifelse(is.na(contraparte) & grepl("ACORDO GERAL SOBRE TARIFAS ADUANEIRAS E COMERCIO", nome), "OMC", contraparte))
sub_internacional <- sub_internacional %>%
  mutate(contraparte = ifelse(is.na(contraparte) & grepl("CONVENCAO SOBRE SEGURANCA E SAUDE TRABALHA", nome), "OIT", contraparte))
sub_internacional <- sub_internacional %>%
  mutate(contraparte = ifelse(is.na(contraparte) & grepl("CONVENCAO SOBRE A ELIMINACAO TODAS AS FORMAS", nome), "ONU", contraparte))
sub_internacional <- sub_internacional %>%
  mutate(contraparte = ifelse(is.na(contraparte) & grepl("REGULAMENTO SANITARIO INTERNACIONAL", nome), "OMS", contraparte))
sub_internacional <- sub_internacional %>%
  mutate(contraparte = ifelse(is.na(contraparte) & grepl("CONVENCAO EUROPEIA SOBRE EXTRADICAO", nome), "UE", contraparte))
sub_internacional <- sub_internacional %>%
  mutate(contraparte = ifelse(is.na(contraparte) & grepl("CONVENCAO VARSOVIA", nome), "ICAO", contraparte))
sub_internacional <- sub_internacional %>%
  mutate(contraparte = ifelse(is.na(contraparte) & grepl("MEXICO", nome), "MEXICO", contraparte))
sub_internacional <- sub_internacional %>%
  mutate(contraparte = ifelse(is.na(contraparte) & grepl("TRATADO ASSUNCAO", nome), "MERCOSUL", contraparte))
sub_internacional <- sub_internacional %>%
  mutate(contraparte = ifelse(is.na(contraparte) & grepl("REFUGIADOS", nome), "ACNUR", contraparte))
sub_internacional <- sub_internacional %>%
  mutate(contraparte = ifelse(is.na(contraparte) & grepl("PACTO INTERNACIONAL DIREITOS CIVIS E POLITICOS", nome), "ONU", contraparte))
sub_internacional <- sub_internacional %>%
  mutate(contraparte = ifelse(is.na(contraparte) & grepl("CONVENCAO EXTRADICAO OS ESTADOS MEMBROS DA", nome), "CPLP", contraparte))
sub_internacional <- sub_internacional %>%
  mutate(contraparte = ifelse(is.na(contraparte) & grepl("CONVENCAO SOBRE OS DIREITOS DAS PESSOAS COM", nome), "ONU", contraparte))
sub_internacional <- sub_internacional %>%
  mutate(contraparte = ifelse(is.na(contraparte) & grepl("CONVENCAO SOBRE OS DIREITOS DAS CRIANCAS", nome), "UNICEF", contraparte))
sub_internacional <- sub_internacional %>%
  mutate(contraparte = ifelse(is.na(contraparte) & grepl("RACIAL", nome), "ONU", contraparte))
sub_internacional <- sub_internacional %>%
  mutate(contraparte = ifelse(is.na(contraparte) & grepl("ACORDO SOBRE SUBSIDIOS E MEDIDAS COMPENSATORIAS", nome), "OMC", contraparte))
sub_internacional <- sub_internacional %>%
  mutate(contraparte = ifelse(is.na(contraparte) & grepl("BASILEIA", nome), "PNUMA", contraparte))
sub_internacional <- sub_internacional %>%
  mutate(contraparte = ifelse(is.na(contraparte) & grepl("CONVENCAO SOBRE O CONTROLE MOVIMENTOS", nome), "PNUMA", contraparte))
sub_internacional <- sub_internacional %>%
  mutate(contraparte = ifelse(is.na(contraparte) & grepl("CONVENCAO SOBRE PREVENCAO DA POLUICAO MARINHA", nome), "ONU", contraparte))
sub_internacional <- sub_internacional %>%
  mutate(contraparte = ifelse(is.na(contraparte) & grepl("ACORDO SOBRE BARREIRAS TECNICAS AO COMERCIO", nome), "OMC", contraparte))
sub_internacional <- sub_internacional %>%
  mutate(contraparte = ifelse(is.na(contraparte) & grepl("CONVENCAO SOBRE PROCEDIMENTO CONSENTIMENTO", nome), "PNUMA", contraparte))
sub_internacional <- sub_internacional %>%
  mutate(contraparte = ifelse(is.na(contraparte) & grepl("CONVENCAO SOBRE A IMPRESCRITIBILIDADE CRIMES", nome), "ONU", contraparte))
sub_internacional <- sub_internacional %>%
  mutate(contraparte = ifelse(is.na(contraparte) & grepl("CONVENCAO SOBRE DIVERSIDADE BIOLOGICA", nome), "ONU", contraparte))
sub_internacional <- sub_internacional %>%
  mutate(contraparte = ifelse(is.na(contraparte) & grepl("CONVENCAO VIENA SOBRE RELACOES DIPLOMATICAS", nome), "ONU", contraparte))
sub_internacional <- sub_internacional %>%
  mutate(contraparte = ifelse(is.na(contraparte) & grepl("CONVENCAO VIENA SOBRE O DIREITO TRATADOS", nome), "ONU", contraparte))
sub_internacional <- sub_internacional %>%
  mutate(contraparte = ifelse(is.na(contraparte) & grepl("SEGURANCA NUCLEAR", nome), "AIEA", contraparte))
sub_internacional <- sub_internacional %>%
  mutate(contraparte = ifelse(is.na(contraparte) & grepl("ARMAS NUCLEARES", nome), "ONU", contraparte))
sub_internacional <- sub_internacional %>%
  mutate(contraparte = ifelse(is.na(contraparte) & grepl("PROTOCOLO SAN SALVADOR", nome), "OEA", contraparte))
sub_internacional <- sub_internacional %>%
  mutate(contraparte = ifelse(is.na(contraparte) & grepl("ACORDO SOBRE A APLICACAO MEDIDAS SANITARIAS E", nome), "OMC", contraparte))
sub_internacional <- sub_internacional %>%
  mutate(contraparte = ifelse(is.na(contraparte) & grepl("PAISES BAIXOS", nome), "PAISES BAIXOS", contraparte))
sub_internacional <- sub_internacional %>%
  mutate(contraparte = ifelse(is.na(contraparte) & grepl("CONVENCAO EUROPEIA DIREITOS DO HOMEM", nome), "CONSELHO EUROPA", contraparte))
sub_internacional <- sub_internacional %>%
  mutate(contraparte = ifelse(is.na(contraparte) & grepl("DOMINICANA", nome), "REP DOMINICANA", contraparte))
sub_internacional <- sub_internacional %>%
  mutate(contraparte = ifelse(is.na(contraparte) & grepl("CONVENCAO VIENA CONTRA O TRAFICO ILICITO", nome), "ONU", contraparte))
sub_internacional <- sub_internacional %>%
  mutate(contraparte = ifelse(is.na(contraparte) & grepl("CONVENCAO CONTRA O TRAFICO ILICITO", nome), "ONU", contraparte))
sub_internacional <- sub_internacional %>%
  mutate(contraparte = ifelse(is.na(contraparte) & grepl("CONVENCAO SOBRE O MAR TERRITORIAL A ZONA", nome), "ONU", contraparte))
sub_internacional <- sub_internacional %>%
  mutate(contraparte = ifelse(is.na(contraparte) & grepl("PROTOCOLO CONTRA A FABRICACAO E O TRAFICO ", nome), "UNODC", contraparte))
sub_internacional <- sub_internacional %>%
  mutate(contraparte = ifelse(is.na(contraparte) & grepl("PROTOCOLO MONTREAL", nome), "PNUD", contraparte))
sub_internacional <- sub_internacional %>%
  mutate(contraparte = ifelse(is.na(contraparte) & grepl("CONVENCAO MONTREAL", nome), "ICAO", contraparte))
sub_internacional <- sub_internacional %>%
  mutate(contraparte = ifelse(is.na(contraparte) & grepl("CONVENCAO EUROPEIA DIREITOS HUMANOS", nome), "CONSELHO EUROPA", contraparte))
sub_internacional <- sub_internacional %>%
  mutate(contraparte = ifelse(is.na(contraparte) & grepl("ACORDO SOBRE ASPECTOS DIREITOS PROPRIEDADE", nome), "OMC", contraparte))
sub_internacional <- sub_internacional %>%
  mutate(contraparte = ifelse(is.na(contraparte) & grepl("TRIBUNAL", nome), "TPI", contraparte))
sub_internacional <- sub_internacional %>%
  mutate(contraparte = ifelse(is.na(contraparte) & grepl("AEREO", nome), "ICAO", contraparte))
sub_internacional <- sub_internacional %>%
  mutate(contraparte = ifelse(is.na(contraparte) & grepl("PACTO SOBRE DIREITOS ECONOMICOS SOCIAIS E", nome), "OEA", contraparte))
sub_internacional <- sub_internacional %>%
  mutate(contraparte = ifelse(is.na(contraparte) & grepl("BELGICA", nome), "BELGICA", contraparte))
sub_internacional <- sub_internacional %>%
  mutate(contraparte = ifelse(is.na(contraparte) & grepl("CONVENCAO SOBRE O ESTATUTO APATRIDAS", nome), "ONU", contraparte))
sub_internacional <- sub_internacional %>%
  mutate(contraparte = ifelse(is.na(contraparte) & grepl("APATRIDIA", nome), "ONU", contraparte))
sub_internacional <- sub_internacional %>%
  mutate(contraparte = ifelse(is.na(contraparte) & grepl("CONVENCAO VIENA SOBRE RELACOES CONSULARES", nome), "ONU", contraparte))
sub_internacional <- sub_internacional %>%
  mutate(contraparte = ifelse(is.na(contraparte) & grepl("CONVENCAO UNICA NOVA IORQUE SOBRE ENTORPECENTES", nome), "ONU", contraparte))
sub_internacional <- sub_internacional %>%
  mutate(contraparte = ifelse(is.na(contraparte) & grepl("CONVENCAO EUROPEIA SOBRE IMUNIDADE ESTADOS", nome), "CONSELHO EUROPA", contraparte))
sub_internacional$contraparte <- gsub("CONSELHO DA EUROPA", "CONSELHO EUROPA", sub_internacional$contraparte)
sub_internacional <- sub_internacional %>%
  mutate(contraparte = ifelse(is.na(contraparte) & grepl("CONVENCAO SOBRE O INCENTIVO A NEGOCIACAO", nome), "OIT", contraparte))
sub_internacional <- sub_internacional %>%
  mutate(contraparte = ifelse(is.na(contraparte) & grepl("CONVENCAO HAIA SOBRE OS ASPECTOS CIVIS DO", nome), "ONU", contraparte))
sub_internacional <- sub_internacional %>%
  mutate(contraparte = ifelse(is.na(contraparte) & grepl("BUSTAMENTE", nome), "DIVERSOS", contraparte))
sub_internacional <- sub_internacional %>%
  mutate(contraparte = ifelse(is.na(contraparte) & grepl("CONVENCAO CONTRA A TORTURA E OUTROS TRATA", nome), "ONU", contraparte))
sub_internacional <- sub_internacional %>%
  mutate(contraparte = ifelse(is.na(contraparte) & grepl("NEGOCIACAO COLETIVA", nome), "OIT", contraparte))
sub_internacional <- sub_internacional %>%
  mutate(contraparte = ifelse(is.na(contraparte) & grepl("CONVENCAO SOBRE POVOS INDIGENAS E TRIBAIS", nome), "OIT", contraparte))
sub_internacional <- sub_internacional %>%
  mutate(contraparte = ifelse(is.na(contraparte) & grepl("CIDH", nome), "OEA", contraparte))
sub_internacional <- sub_internacional %>%
  mutate(contraparte = ifelse(is.na(contraparte) & grepl("ESTADOS AMERICANOS", nome), "OEA", contraparte))
sub_internacional <- sub_internacional %>%
  mutate(contraparte = ifelse(is.na(contraparte) & grepl("CANADA", nome), "CANADA", contraparte))
sub_internacional <- sub_internacional %>%
  mutate(contraparte = ifelse(is.na(contraparte) & grepl("TRABALHO", nome), "OIT", contraparte))
sub_internacional <- sub_internacional %>%
  mutate(contraparte = ifelse(is.na(contraparte) & grepl("CONVENCAO SOBRE SUBSTANCIAS PSICOTROPICAS", nome), "ONU", contraparte))
sub_internacional <- sub_internacional %>%
  mutate(contraparte = ifelse(is.na(contraparte) & grepl("ROMENIA", nome), "ROMENIA", contraparte))
sub_internacional <- sub_internacional %>%
  mutate(contraparte = ifelse(is.na(contraparte) & grepl("ESTADOS UNIDOS", nome), "EUA", contraparte))
sub_internacional <- sub_internacional %>%
  mutate(contraparte = ifelse(is.na(contraparte) & grepl("CONVENCAO INTERNACIONAL SOBRE A PROTECAO E ", nome), "UNESCO", contraparte))
sub_internacional <- sub_internacional %>%
  mutate(contraparte = ifelse(is.na(contraparte) & grepl("CONVENCAO PARA PREVENIR E PUNIR OS ATOS TERRORISMO", nome), "OEA", contraparte))
sub_internacional <- sub_internacional %>%
  mutate(contraparte = ifelse(is.na(contraparte) & grepl("CONVENCAO PARA A PREVENCAO E A REPRESSAO DO CRIME", nome), "OEA", contraparte))
sub_internacional <- sub_internacional %>%
  mutate(contraparte = ifelse(is.na(contraparte) & grepl("CONVENCAO UNIVERSAL SOBRE DIREITO AUTOR", nome), "ONU", contraparte))
sub_internacional <- sub_internacional %>%
  mutate(contraparte = ifelse(is.na(contraparte) & grepl("CONVENCAO SOBRE IGUALDADE DIREITOS E DEVERES", nome), "PORTUGAL", contraparte))
sub_internacional <- sub_internacional %>%
  mutate(contraparte = ifelse(is.na(contraparte) & grepl("CONVENCAO SOBRE TRANSITO VIARIO", nome), "ONU", contraparte))
sub_internacional <- sub_internacional %>%
  mutate(contraparte = ifelse(is.na(contraparte) & grepl("PACTO INTERNACIONAL DIREITOS ECONOMICOS SOCIAIS", nome), "ONU", contraparte))
sub_internacional <- sub_internacional %>%
  mutate(contraparte = ifelse(is.na(contraparte) & grepl("CONVENCAO MINAMATA SOBRE MERCURIO", nome), "ONU", contraparte))
sub_internacional <- sub_internacional %>%
  mutate(contraparte = ifelse(is.na(contraparte) & grepl("CONVENCAO PARIS PARA A PROTECAO DO MEIO MARINHO", nome), "OIT", contraparte))
sub_internacional <- sub_internacional %>%
  mutate(contraparte = ifelse(is.na(contraparte) & grepl("CONVENCAO BAMAKO RELATIVA A INTERDICAO DA IMPORTACAO", nome), "PNUMA", contraparte))
sub_internacional <- sub_internacional %>%
  mutate(contraparte = ifelse(is.na(contraparte) & grepl("CONVENCAO SOBRE ZONAS UMIDAS IMPORTANCIA", nome), "COP", contraparte))
sub_internacional <- sub_internacional %>%
  mutate(contraparte = ifelse(is.na(contraparte) & grepl("CONVENCAO SOBRE ASSISTENCIA MUTUA ADMINISTRATIVA", nome), "OCDE", contraparte))
sub_internacional <- sub_internacional %>%
  mutate(contraparte = ifelse(is.na(contraparte) & grepl("PROTOCOLO QUIOTO", nome), "ONU", contraparte))
sub_internacional <- sub_internacional %>%
  mutate(contraparte = ifelse(is.na(contraparte) & grepl("PROTOCOLO REVISAO GENEBRA", nome), "ONU", contraparte))
sub_internacional <- sub_internacional %>%
  mutate(contraparte = ifelse(is.na(contraparte) & grepl("SUECIA", nome), "SUECIA", contraparte))
sub_internacional <- sub_internacional %>%
  mutate(contraparte = ifelse(is.na(contraparte) & grepl("CONVENCAO ESTOCOLMO SOBRE POLUENTES", nome), "PNUMA", contraparte))
sub_internacional <- sub_internacional %>%
  mutate(contraparte = ifelse(is.na(contraparte) & grepl("VENEZUELA", nome), "VENEZUELA", contraparte))
sub_internacional <- sub_internacional %>%
  mutate(contraparte = ifelse(is.na(contraparte) & grepl("CONVENCAO CHICAGO SOBRE AVIACAO CIVIL", nome), "ICAO", contraparte))
sub_internacional <- sub_internacional %>%
  mutate(contraparte = ifelse(is.na(contraparte) & grepl("CONVENCAO PARA A PROTECAO DA FLORA DA FAUNA", nome), "OEA", contraparte))
sub_internacional <- sub_internacional %>%
  mutate(contraparte = ifelse(is.na(contraparte) & grepl("ATO FIRMADOS EM NOVA YORK A 22 JULHO 1946", nome), "OMS", contraparte))
sub_internacional <- sub_internacional %>%
  mutate(contraparte = ifelse(is.na(contraparte) & grepl("RESOLUCAO DA COMISSAO INTERAMERICANA DIREITOS", nome), "OEA", contraparte))
sub_internacional <- sub_internacional %>%
  mutate(contraparte = ifelse(is.na(contraparte) & grepl("CONVENCAO BERNA PARA A PROTECAO DAS OBRAS", nome), "ONU", contraparte))
sub_internacional <- sub_internacional %>%
  mutate(contraparte = ifelse(is.na(contraparte) & grepl("CONVENCAO RELATIVA A LEI UNIFORME SOBRE LETR", nome), "LIGA NACOES", contraparte))
sub_internacional <- sub_internacional %>%
  mutate(contraparte = ifelse(is.na(contraparte) & grepl("CONVENCAO GENEBRA", nome), "OIT", contraparte))
sub_internacional <- sub_internacional %>%
  mutate(contraparte = ifelse(is.na(contraparte) & grepl("CONVENCAO RELATIVA AO ROMPIMENTO DAS", nome), "DIVERSOS", contraparte))
sub_internacional <- sub_internacional %>%
  mutate(contraparte = ifelse(is.na(contraparte) & grepl("CONVENCAO RELATIVA A PROTECAO CIVIS EM TEMPO", nome), "ONU", contraparte))
sub_internacional <- sub_internacional %>%
  mutate(contraparte = ifelse(is.na(contraparte) & grepl("BIOMEDICINA", nome), "CONSELHO EUROPA", contraparte))
sub_internacional <- sub_internacional %>%
  mutate(contraparte = ifelse(is.na(contraparte) & grepl("CONVENCAO SOBRE AVIACAO CIVIL INTERNACIONAL", nome), "ICAO", contraparte))
sub_internacional <- sub_internacional %>%
  mutate(contraparte = ifelse(is.na(contraparte) & grepl("CONVENCAO SOBRE A PROTECAO DA MATERNIDADE", nome), "OIT", contraparte))
sub_internacional <- sub_internacional %>%
  mutate(contraparte = ifelse(is.na(contraparte) & grepl("PROTOCOLO HAIA", nome), "ICAO", contraparte))
sub_internacional <- sub_internacional %>%
  mutate(contraparte = ifelse(is.na(contraparte) & grepl("CONVENCAO PROTECAO DA FLORA FAUNA E DAS", nome), "OEA", contraparte))
sub_internacional <- sub_internacional %>%
  mutate(contraparte = ifelse(is.na(contraparte) & grepl("CONVENCAO WASHINGTON SOBRE O COMERCIO", nome), "IUCN", contraparte))
sub_internacional <- sub_internacional %>%
  mutate(contraparte = ifelse(is.na(contraparte) & grepl("CONVENCAO INTERNACIONAL CONTRA O GENOCIDIO", nome), "OEA", contraparte))
sub_internacional <- sub_internacional %>%
  mutate(contraparte = ifelse(is.na(contraparte) & grepl("CONVENCAO SOBRE O COMBATE DA CORRUPCAO FUNCIONARIOS", nome), "OCDE", contraparte))
sub_internacional <- sub_internacional %>%
  mutate(contraparte = ifelse(is.na(contraparte) & grepl("CONVENCAO PARA PROTECAO DIREITOS DO HOMEM", nome), "ONU", contraparte))
sub_internacional <- sub_internacional %>%
  mutate(contraparte = ifelse(is.na(contraparte) & grepl("CONVENCAO SOBRE O COMERCIO DAS ESPECIES DA", nome), "PNUMA", contraparte))
sub_internacional <- sub_internacional %>%
  mutate(contraparte = ifelse(is.na(contraparte) & grepl("CONVENCAO RELATIVA A LUTA CONTRA A DISCRIMINACAO", nome), "OEA", contraparte))
sub_internacional <- sub_internacional %>%
  mutate(contraparte = ifelse(is.na(contraparte) & grepl("ACORDO SUL AMERICANO ENTORPECENTES E", nome), "AMER SUL", contraparte))
sub_internacional <- sub_internacional %>%
  mutate(contraparte = ifelse(is.na(contraparte) & grepl("CONVENCAO SOBRE A PROTECAO E A PROMOCAO DA", nome), "UNESCO", contraparte))
sub_internacional <- sub_internacional %>%
  mutate(contraparte = ifelse(is.na(contraparte) & grepl("CONVENCAO INTERNACIONAL SOBRE DIREITOS ECONOMICOS", nome), "ONU", contraparte))
sub_internacional <- sub_internacional %>%
  mutate(contraparte = ifelse(is.na(contraparte) & grepl("CONVENCAO INTERNACIONAL PARA PROTECAO AOS ARTISTAS", nome), "ONU", contraparte))
sub_internacional <- sub_internacional %>%
  mutate(contraparte = ifelse(is.na(contraparte) & grepl("CONVENCAO INTERNACIONAL SOBRE NORMAS TREINAMENTO", nome), "ONU", contraparte))
sub_internacional <- sub_internacional %>%
  mutate(contraparte = ifelse(is.na(contraparte) & grepl("TRATADO QUE ESTABELECE UMA ZONA LIVRE COMERCIO", nome), "ALALC", contraparte))
sub_internacional <- sub_internacional %>%
  mutate(contraparte = ifelse(is.na(contraparte) & grepl("CONVENCAO RELATIVA AS LEIS E USOS DA GUERRA TERRESTRE", nome), "DIVERSOS", contraparte))
sub_internacional <- sub_internacional %>%
  mutate(contraparte = ifelse(is.na(contraparte) & grepl("CONVENCAO SOBRE OS ASPECTOS CIVIS DO SEQUESTRO", nome), "ONU", contraparte))
sub_internacional <- sub_internacional %>%
  mutate(contraparte = ifelse(is.na(contraparte) & grepl("CONVENIO EUROPEU PARA A PROTECAO DIREITOS", nome), "CONSELHO EUROPA", contraparte))
sub_internacional <- sub_internacional %>%
  mutate(contraparte = ifelse(is.na(contraparte) & grepl("CONVENCAO RELATIVA AOS DIREITOS DAS CRIANCAS", nome), "ONU", contraparte))
sub_internacional <- sub_internacional %>%
  mutate(contraparte = ifelse(is.na(contraparte) & grepl("CONVENCAO SOBRE AS POPULACOES INDIGENAS E TRIB", nome), "OIT", contraparte))
sub_internacional <- sub_internacional %>%
  mutate(contraparte = ifelse(is.na(contraparte) & grepl("RESOLUCAO DO COMITE MINISTROS DO CONSELHO DA EUROPA", nome), "CONSELHO EUROPA", contraparte))
#
# Criando tema
sub_internacional <- sub_internacional %>%
  mutate(tema = NA)
sub_internacional <- sub_internacional %>%
  mutate(tema = ifelse(contraparte == "OIT", "TRABALHO", tema))
sub_internacional <- sub_internacional %>%
  mutate(tema = ifelse(contraparte == "OMC", "COMERCIO", tema))
sub_internacional <- sub_internacional %>%
  mutate(tema = ifelse(contraparte == "PNUMA", "MEIO AMBIENTE", tema))
sub_internacional <- sub_internacional %>%
  mutate(tema = ifelse(contraparte == "OMS", "SAUDE", tema))
sub_internacional <- sub_internacional %>%
  mutate(tema = ifelse(contraparte == "ICAO", "TRANSPORTE AEREO", tema))
sub_internacional <- sub_internacional %>%
  mutate(tema = ifelse(grepl("EXTRADICAO", nome), "EXTRADICAO", tema))
sub_internacional <- sub_internacional %>%
  mutate(tema = ifelse(grepl("COSTA RICA", nome), "DIREITOS HUMANOS", tema))
sub_internacional <- sub_internacional %>%
  mutate(tema = ifelse(grepl("TORTURA", nome), "DIREITOS HUMANOS", tema))
sub_internacional <- sub_internacional %>%
  mutate(tema = ifelse(grepl("DIREITOS HUMANOS", nome), "DIREITOS HUMANOS", tema))
sub_internacional <- sub_internacional %>%
  mutate(tema = ifelse(is.na(tema) & grepl("GUERRA", nome), "GUERRA", tema))
sub_internacional <- sub_internacional %>%
  mutate(tema = ifelse(is.na(tema) & grepl("COMERCIO", nome), "COMERCIO", tema))
sub_internacional <- sub_internacional %>%
  mutate(tema = ifelse(is.na(tema) & grepl("CRIME", nome), "CRIME", tema))
sub_internacional <- sub_internacional %>%
  mutate(tema = ifelse(is.na(tema) & grepl("PENAL", nome), "CRIME", tema))
sub_internacional <- sub_internacional %>%
  mutate(tema = ifelse(is.na(tema) & grepl("TRABALHO", nome), "TRABALHO", tema))
sub_internacional <- sub_internacional %>%
  mutate(tema = ifelse(is.na(tema) & grepl("TRAFICO", nome), "TRAFICO", tema))
sub_internacional <- sub_internacional %>%
  mutate(tema = ifelse(is.na(tema) & grepl("CONSULARES", nome), "DIP", tema))
sub_internacional <- sub_internacional %>%
  mutate(tema = ifelse(is.na(tema) & grepl("TRATADOS", nome), "DIP", tema))
sub_internacional <- sub_internacional %>%
  mutate(tema = ifelse(is.na(tema) & grepl("DIPLOMATICAS", nome), "DIP", tema))
sub_internacional <- sub_internacional %>%
  mutate(tema = ifelse(is.na(tema) & grepl("ARMAS", nome), "GUERRA", tema))
sub_internacional <- sub_internacional %>%
  mutate(tema = ifelse(is.na(tema) & grepl("IMUNIDADES", nome), "DIP", tema))
sub_internacional <- sub_internacional %>%
  mutate(tema = ifelse(is.na(tema) & grepl("IMUNIDADE", nome), "DIP", tema))
sub_internacional <- sub_internacional %>%
  mutate(tema = ifelse(is.na(tema) & grepl("ENTORPECENTES", nome), "DROGA", tema))
sub_internacional <- sub_internacional %>%
  mutate(tema = ifelse(is.na(tema) & grepl("PSICOTROPICAS", nome), "DROGA", tema))
sub_internacional <- sub_internacional %>%
  mutate(tema = ifelse(is.na(tema) & grepl("DIREITOS", nome), "DIREITOS HUMANOS", tema))
sub_internacional <- sub_internacional %>%
  mutate(tema = ifelse(is.na(tema) & grepl("BUSTAMENTE", nome), "COMERCIO", tema))
sub_internacional <- sub_internacional %>%
  mutate(tema = ifelse(is.na(tema) & grepl("AUTOR", nome), "COMERCIO", tema))
sub_internacional <- sub_internacional %>%
  mutate(tema = ifelse(is.na(tema) & grepl("DIREITO DO MAR", nome), "COMERCIO", tema))
sub_internacional <- sub_internacional %>%
  mutate(tema = ifelse(is.na(tema) & grepl("NUCLEAR", nome), "ENERGIA", tema))
sub_internacional <- sub_internacional %>%
  mutate(tema = ifelse(is.na(tema) & grepl("AIEA", nome), "ENERGIA", tema))
sub_internacional <- sub_internacional %>%
  mutate(tema = ifelse(is.na(tema) & grepl("CORRUPCAO", nome), "CRIME", tema))
sub_internacional <- sub_internacional %>%
  mutate(tema = ifelse(is.na(tema) & grepl("DISCRIMINACAO", nome), "DIREITOS HUMANOS", tema))
sub_internacional <- sub_internacional %>%
  mutate(tema = ifelse(is.na(tema) & grepl("REFUGIADOS", nome), "REFUGIADOS", tema))
sub_internacional <- sub_internacional %>%
  mutate(tema = ifelse(is.na(tema) & grepl("CLIMA", nome), "MEIO AMBIENTE", tema))
sub_internacional <- sub_internacional %>%
  mutate(tema = ifelse(is.na(tema) & grepl("POLUICAO", nome), "MEIO AMBIENTE", tema))
sub_internacional <- sub_internacional %>%
  mutate(tema = ifelse(is.na(tema) & grepl("PENAS", nome), "EXTRADICAO", tema))
sub_internacional <- sub_internacional %>%
  mutate(tema = ifelse(is.na(tema) & grepl("TRIBUTO", nome), "TRIBUTACAO", tema))
sub_internacional <- sub_internacional %>%
  mutate(tema = ifelse(is.na(tema) & grepl("TRIBUTARIA", nome), "TRIBUTACAO", tema))
sub_internacional <- sub_internacional %>%
  mutate(tema = ifelse(is.na(tema) & grepl("TRIBUTACAO", nome), "TRIBUTACAO", tema))
sub_internacional <- sub_internacional %>%
  mutate(tema = ifelse(is.na(tema) & grepl("TERRORISMO", nome), "TERRORISMO", tema))
sub_internacional <- sub_internacional %>%
  mutate(tema = ifelse(is.na(tema) & grepl("BIOLOGICA", nome), "MEIO AMBIENTE", tema))
sub_internacional <- sub_internacional %>%
  mutate(tema = ifelse(is.na(tema) & grepl("PROTOCOLO MONTREAL", nome), "MEIO AMBIENTE", tema))
sub_internacional <- sub_internacional %>%
  mutate(tema = ifelse(is.na(tema) & grepl("QUIOTO", nome), "MEIO AMBIENTE", tema))
sub_internacional <- sub_internacional %>%
  mutate(tema = ifelse(is.na(tema) & grepl("FLORA", nome), "MEIO AMBIENTE", tema))
sub_internacional <- sub_internacional %>%
  mutate(tema = ifelse(is.na(tema) & grepl("HABITAT", nome), "MEIO AMBIENTE", tema))
sub_internacional <- sub_internacional %>%
  mutate(tema = ifelse(is.na(tema) & grepl("UNIFORME", nome), "COMERCIO", tema))
sub_internacional <- sub_internacional %>%
  mutate(tema = ifelse(is.na(tema) & grepl("ARTISTAS", nome), "COMERCIO", tema))
sub_internacional <- sub_internacional %>%
  mutate(tema = ifelse(is.na(tema) & grepl("BERNA", nome), "COMERCIO", tema))
sub_internacional <- sub_internacional %>%
  mutate(tema = ifelse(is.na(tema) & grepl("ZONA", nome), "COMERCIO", tema))
sub_internacional <- sub_internacional %>%
  mutate(tema = ifelse(is.na(tema) & grepl("MULHER", nome), "DIREITOS HUMANOS", tema))
sub_internacional <- sub_internacional %>%
  mutate(tema = ifelse(is.na(tema) & grepl("NACOES UNIDAS", nome), "DIREITOS HUMANOS", tema))
sub_internacional <- sub_internacional %>%
  mutate(tema = ifelse(is.na(tema) & grepl("DESAPARECIMENTO", nome), "DIREITOS HUMANOS", tema))
sub_internacional <- sub_internacional %>%
  mutate(tema = ifelse(is.na(tema) & grepl("CRIANCAS", nome), "DIREITOS HUMANOS", tema))
sub_internacional <- sub_internacional %>%
  mutate(tema = ifelse(is.na(tema) & grepl("PROTECAO", nome), "DIREITOS HUMANOS", tema))
sub_internacional <- sub_internacional %>%
  mutate(tema = ifelse(is.na(tema) & grepl("HOSTILIDADES", nome), "GUERRA", tema))
sub_internacional <- sub_internacional %>%
  mutate(tema = ifelse(is.na(tema) & grepl("SANTA SE", nome), "DIP", tema))
sub_internacional <- sub_internacional %>%
  mutate(tema = ifelse(is.na(tema) & grepl("MERCURIO", nome), "MEIO AMBIENTE", tema))
sub_internacional <- sub_internacional %>%
  mutate(tema = ifelse(is.na(tema) & grepl("GENOCIDIO", nome), "DIREITOS HUMANOS", tema))
sub_internacional <- sub_internacional %>%
  mutate(tema = ifelse(is.na(tema) & grepl("ASSEMBLEIA", nome), "DIREITOS HUMANOS", tema))
sub_internacional <- sub_internacional %>%
  mutate(tema = ifelse(is.na(tema) & grepl("ASSUNCAO", nome), "COMERCIO", tema))
sub_internacional <- sub_internacional %>%
  mutate(tema = ifelse(is.na(tema) & grepl("SANITARIA", nome), "SAUDE", tema))
sub_internacional <- sub_internacional %>%
  mutate(tema = ifelse(is.na(tema) & grepl("GENEBRA", nome), "GUERRA", tema))
sub_internacional <- sub_internacional %>%
  mutate(tema = ifelse(is.na(tema) & grepl("ITAIPU", nome), "DIP", tema))
sub_internacional <- sub_internacional %>%
  mutate(tema = ifelse(is.na(tema) & grepl("BILATERAL", nome), "DIP", tema))
sub_internacional <- sub_internacional %>%
  mutate(tema = ifelse(is.na(tema) & grepl("APATRIDA", nome), "NACIONALIDADE", tema))
sub_internacional <- sub_internacional %>%
  mutate(tema = ifelse(is.na(tema) & grepl("APATRIDAS", nome), "NACIONALIDADE", tema))
sub_internacional <- sub_internacional %>%
  mutate(tema = ifelse(is.na(tema) & grepl("APATRIDIA", nome), "NACIONALIDADE", tema))
sub_internacional <- sub_internacional %>%
  mutate(tema = ifelse(is.na(tema) & grepl("SALVADOR", nome), "DIREITOS HUMANOS", tema))
sub_internacional <- sub_internacional %>%
  mutate(tema = ifelse(is.na(tema) & grepl("ONU", nome), "DIREITOS HUMANOS", tema))
sub_internacional <- sub_internacional %>%
  mutate(tema = ifelse(is.na(tema) & grepl("QUARTO", nome), "COMERCIO", tema))
sub_internacional <- sub_internacional %>%
  mutate(tema = ifelse(is.na(tema) & grepl("INDIGENAS", nome), "TRABALHO", tema))
sub_internacional <- sub_internacional %>%
  mutate(tema = ifelse(is.na(tema), "DIP", tema))
sub_internacional$tema <- gsub("DROGAS", "DROGA", sub_internacional$tema)
#observacoes_na <- filter(sub_internacional, is.na(tema))

## Tabela do dataset
nomes_colunas <- colnames(sub_internacional)
tabela <- kable(sub_internacional) %>%
  add_header_above(nomes_colunas)
print(tabela)
rm(nomes_colunas)
rm(tabela)

# 552 processos únicos
sub_internacional %>% 
  distinct(id) %>% 
  n_distinct()

base_original %>% 
  distinct(id) %>% 
  n_distinct()

# Calcular a tabela de frequência da coluna "tema"
tabela_frequencia <- sub_internacional %>%
  count(tema) %>%
  arrange(desc(n))
tabela_frequencia %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", full_width = FALSE)
rm(tabela_frequencia)

# Calcular a tabela de frequência da coluna "contraparte"
tabela_frequencia <- sub_internacional %>%
  count(contraparte) %>%
  arrange(desc(n))
parte1 <- tabela_frequencia[1:ceiling(nrow(tabela_frequencia)/2), ]
parte2 <- tabela_frequencia[(ceiling(nrow(tabela_frequencia)/2) + 1):nrow(tabela_frequencia), ]
tabela_com_duas_colunas <- kbl(list(parte1, parte2), align = "c") %>%
  kable_styling(bootstrap_options = "striped", full_width = FALSE)
print(tabela_com_duas_colunas)
rm(tabela_frequencia)
rm(parte1)
rm(parte2)

# Calcular a tabela de frequência da coluna "tipo"
tabela_frequencia <- sub_internacional %>%
  count(tipo) %>%
  arrange(desc(n))
tabela_frequencia %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", full_width = FALSE)
rm(tabela_frequencia)

# Calcular a tabela de frequência da coluna "classe"
tabela_frequencia <- sub_internacional %>%
  count(classe) %>%
  arrange(desc(n))
parte1 <- tabela_frequencia[1:ceiling(nrow(tabela_frequencia)/2), ]
parte2 <- tabela_frequencia[(ceiling(nrow(tabela_frequencia)/2) + 1):nrow(tabela_frequencia), ]
tabela_com_duas_colunas <- kbl(list(parte1, parte2), align = "c") %>%
  kable_styling(bootstrap_options = "striped", full_width = FALSE)
print(tabela_com_duas_colunas)
rm(tabela_frequencia)
rm(parte1)
rm(parte2)

# Criar o gráfico de barras da competência do STF
sub_internacional_unico <- sub_internacional %>%
  distinct(id, .keep_all = TRUE)
grafico_barras <- ggplot(sub_internacional_unico, aes(x = competencia, fill = competencia)) +
  geom_bar() +
  labs(title = "Gráfico de Barras por Competência (Um Caso por ID)",
       x = "Competência") +
  theme_minimal()
print(grafico_barras)
rm(sub_internacional_unico)
rm(grafico_barras)

# 
sub_internacional$ano <- as.numeric(sub_internacional$ano)
sub_internacional$julgamento_data <- as.Date(sub_internacional$julgamento_data)
sub_internacional$tipo <- as.factor(sub_internacional$tipo)
sub_internacional$nome <- as.factor(sub_internacional$nome)
sub_internacional$competencia <- as.factor(sub_internacional$competencia)
sub_internacional$classe <- as.factor(sub_internacional$classe)
sub_internacional$contraparte <- as.factor(sub_internacional$contraparte)
sub_internacional$tema <- as.factor(sub_internacional$tema)
sub_internacional$titulo <- as.factor(sub_internacional$titulo)
sub_internacional$id <- as.factor(sub_internacional$id)
summary(sub_internacional)
# Média de citação de legisção internacional de 1,9 documentos por processo. 252 processos (46,5%) só citam 1 documento.
sub_internacional %>%
  count(id) %>%
  filter(n == 1) %>%
  summarise(total_casos_unicos = n())
sub_internacional %>% 
  count(nome) %>%
  group_by(nome, n) %>%
  arrange(n) %>% 
  arrange(desc(n))

#

#Scatterplot
dados_agrupados <- sub_internacional %>%
  group_by(ano = year(julgamento_data), tipo, classe) %>%
  summarise(numero_observacoes = n()) %>%
  drop_na() # Remover linhas com valores ausentes
simbolos <- c(16, 17, 15, 18, 0, 1, 2, 3, 4, 5, 6)
ggplot(dados_agrupados, aes(x = ano, y = numero_observacoes, color = classe, shape = tipo)) +
  geom_point() +
  labs(x = "Ano da Data do Julgamento", y = "Número de Observações", color = "Classe", shape = "Tipo") +
  ggtitle("Scatterplot do Dataset Sub Internacional") +
  scale_shape_manual(values = simbolos)
rm(dados_agrupados)
rm(simbolos)
#
#Gráfico de barra
dados_agrupados <- sub_internacional %>%
  group_by(tipo, classe) %>%
  summarise(numero_observacoes = n()) %>%
  drop_na() # Remover linhas com valores ausentes
ggplot(dados_agrupados, aes(x = classe, y = numero_observacoes, fill = tipo)) +
  geom_bar(stat = "identity") +
  labs(x = "Classe", y = "Número de Observações", fill = "Tipo") +
  ggtitle("Histograma Classe/Tipo") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
rm(dados_agrupados)
###
########
# Montagem da rede
rede_internacional <- sub_internacional %>%
  group_by(classe, tipo) %>%
  drop_na()

# Criar objeto igraph a partir do dataset
rede <- graph_from_data_frame(rede_internacional, directed = TRUE)
plot(rede, vertex.label = NA, edge.label = NA, edge.width = 0.5, vertex.color = blues9)

# Pendências
# finalizar nomes
# Montagem e analise da rede
# Teste usando igraph ou gephi
# Escrita fundamentada na literatura sobre o tema
####
#Conclusão da limpeza
sub_internacional <- sub_internacional %>%
  mutate(nome = str_remove(nome, "\\bO\\b"))
sub_internacional <- sub_internacional %>%
  mutate(nome = str_remove_all(nome, "\\bOS\\b"))
sub_internacional <- sub_internacional %>%
  mutate(nome = str_replace_all(nome, "\\bSOBRE\\b", ""))
sub_internacional <- sub_internacional %>%
  mutate(nome = str_replace_all(nome, "\\bPARA\\b", ""))
sub_internacional <- sub_internacional %>%
  mutate(nome = str_replace_all(nome, "\\bDAS\\b", ""))
sub_internacional <- sub_internacional %>%
  mutate(nome = str_replace_all(nome, "\\bCOM\\b", ""))
sub_internacional <- sub_internacional %>%
  mutate(nome = str_replace_all(nome, "\\bE\\b", ""))
sub_internacional <- sub_internacional %>%
  mutate(nome = str_replace_all(nome, "\\bA\\b", ""))
sub_internacional <- sub_internacional %>%
  mutate(nome = str_replace_all(nome, "\\bAO\\b", ""))
sub_internacional <- sub_internacional %>%
  mutate(nome = str_replace_all(nome, "\\bDA\\b", ""))
sub_internacional <- sub_internacional %>%
  mutate(nome = str_replace_all(nome, "\\bAS\\b", ""))
sub_internacional <- sub_internacional %>%
  mutate(nome = str_replace_all(nome, "\\bDO\\b", ""))
sub_internacional <- sub_internacional %>%
  mutate(nome = str_replace_all(nome, "\\bEM\\b", ""))
sub_internacional <- sub_internacional %>%
  mutate(nome = str_replace_all(nome, "\\s+", " "))
# Definir o conjunto de stopwords em português
stopwords_pt <- stopwords("pt")
# Aplicar a remoção de stopwords na coluna "nome"
sub_internacional <- sub_internacional %>%
  mutate(nome = removeWords(nome, stopwords_pt))
#
sub_internacional <- sub_internacional %>%
  mutate(nome = str_replace_all(nome, "PACTO SAO JOSE DA COSTA RICA CONVENCAO AMERICANA DIREITOS HUMANOS", "CONVENCAO AMERICANA DIREITOS HUMANOS"))
sub_internacional <- sub_internacional %>%
  mutate(nome = str_replace_all(nome, "ONU ONU", "ONU"))

#
save(sub_internacional, file = "Bases/02_sub_internacional.rdata")
