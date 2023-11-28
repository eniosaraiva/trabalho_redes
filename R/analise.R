set.seed(123)

analise_rede <- sub_internacional[, c("classe", "nome", "julgamento_data")]
write.csv(analise_rede, file = "Bases/rede.csv", row.names = FALSE)

nos_classe <- analise_rede %>%
  distinct(classe) %>%
  rename(label = classe) %>%
  mutate(type = "classe")
nos_nome <- analise_rede %>%
  distinct(nome) %>%
  rename(label = nome) %>%
  mutate(type = "ato")
nos <- bind_rows(nos_classe, nos_nome) %>%
  mutate(id = 1:n())
write.csv(nos, file = "Bases/nos.csv", row.names = FALSE)

arestas <- analise_rede %>%
  select(source = classe, target = nome)
arestas <- arestas %>%
  group_by(source, target) %>%
  summarize(weight = sum(n()))
write.csv(arestas, file = "Bases/arestas.csv", row.names = FALSE)

rede <- graph_from_data_frame(analise_rede, directed = TRUE, vertices = NULL)

plot(rede, vertex.label = NA, edge.label = NA,
     vertex.color = blues9, edge.arrow.size = 0.5, vertex.size = V(rede)$weight,
     vertex.label.color = "black", vertex.label.cex = 0.8, vertex.label.dist = 3,
     edge.curved = 0.5, layout = layout_with_kk)

###
rede <- graph_from_data_frame(arestas, directed = TRUE, vertices = NULL)
grau <- degree(rede, mode = "in")  # Grau de entrada
summary(grau)
centralidade <- centr_degree(rede, mode = "in")$vector  # Centralidade de entrada
summary(centralidade)
intermediacao <- betweenness(rede, directed = TRUE)  # Intermediação
summary(intermediacao)
proximidade <- closeness(rede, mode = "in")  # Proximidade
summary(proximidade)
pagerank <- page.rank(rede)$vector  # PageRank
summary(pagerank)

df_anterior <- sub_internacional %>%
  filter(julgamento_data < as.Date("1988-10-05"))

df_anterior <- df_anterior %>%
  group_by(classe, nome) %>%
  summarise(repeticoes = n()) %>%
  arrange(desc(repeticoes))

rede <- graph_from_data_frame(df_anterior, directed = TRUE, vertices = NULL)

plot(rede, vertex.color = blues9, edge.arrow.size = 0.5, vertex.size = V(rede)$weight,
     vertex.label.color = "black", vertex.label.cex = 0.8, vertex.label.dist = 3,
     edge.curved = 0.5, layout = layout_with_kk)

df_posterior <- sub_internacional %>%
  filter(julgamento_data > as.Date("1988-10-05"))

df_posterior <- df_posterior %>%
  group_by(classe, nome) %>%
  summarise(repeticoes = n()) %>%
  arrange(desc(repeticoes))

rede <- graph_from_data_frame(df_posterior, directed = TRUE, vertices = NULL)

plot(rede, vertex.label = NA, vertex.color = blues9, edge.arrow.size = 0.5, vertex.size = V(rede)$weight,
     vertex.label.color = "black", vertex.label.cex = 0.8, vertex.label.dist = 3,
     edge.curved = 0.5, layout = layout_with_kk)
