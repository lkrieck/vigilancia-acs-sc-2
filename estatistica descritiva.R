### Estatistica descritiva


## ---- biblioteca_stat --------
library(tidyverse)
library(readr)
library(gtsummary)



## ---- funcoes --------

#criando função moda

mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}



find_mode <- function(x) {
  u <- unique(x)
  tab <- tabulate(match(x, u))
  u[tab == max(tab)]
}


Mode <- function(x) {
  if(is.numeric(x)) {  x <- round(x,2)}
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}


### Nãor rodar, exemplo para fazer teste pe
## define custom test
fisher.test.simulate.p.values <- function(data, variable, by, ...) {
  result <- list()
  test_results <- stats::fisher.test(data[[variable]], data[[by]], simulate.p.value = TRUE)
  result$p <- test_results$p.value
  result$test <- test_results$method
  result
}





## ---- dados_stat--------


banco_total<- read_csv2("banco_limpeza.csv", locale = locale(encoding = "ISO8859-1")) 

banco_total <- banco_total %>% 
  mutate(GrupoOcupacaoMae = case_when(
    str_detect(CBOMae,"^6") ~ "Trabalhadora agropecuária, florestal e da pesca",
    str_detect(CBOMae,"^7|^8") ~ "Trabalhadora da produção de bens e serviços industriais",
    str_detect(CBOMae,"999992") ~ "Dona de casa",
    str_detect(CBOMae,"998999") ~ "Ignorado",
    str_detect(CBOMae,"999991") ~ "Estudante",
    str_detect(CBOMae,"^0") ~ "Membro das forças armadas, policiais, e bombeiros militares",
    str_detect(CBOMae,"^1") ~ "Membro do poder público e gerentes",
    str_detect(CBOMae,"^2") ~ "Profissional das ciências e das artes",
    str_detect(CBOMae,"^3") ~ "Técnica de nível médio",
    str_detect(CBOMae,"^4") ~ "Trabalhadora de serviços administrativos",
    str_detect(CBOMae,"^5") ~ "Trabalhadora de serviços diversos ou comércio",
    str_detect(CBOMae,"^9") ~ "Trabalhadora de serviços de reparação e manutenção")) %>% 
  mutate (GrupoOcupacaoMae = replace_na(GrupoOcupacaoMae, "Ignorado")) %>% 
  mutate(Apgar1Minuto = na_if(Apgar1Minuto, 99)) %>% 
  mutate(Apgar5Minuto = na_if(Apgar5Minuto, 99)) %>% 
  mutate(NumeroAbortos = na_if(NumeroAbortos, 99)) %>% 
  mutate(Sexo = as.factor(Sexo)) %>% 
  mutate(Raca = as.factor(Raca)) %>% 
  mutate(SemanasGestacao = as.factor(SemanasGestacao)) %>% 
  mutate(TipoParto = as.factor(TipoParto)) %>% 
  mutate(TipoGravidez = as.factor(TipoGravidez)) %>% 
  mutate(ConsultasPrenatal = as.factor(ConsultasPrenatal)) %>% 
  mutate(nvApresentacaoRN = as.factor(nvApresentacaoRN)) %>%
  mutate(LocalNascimento = as.factor(LocalNascimento)) %>%
  mutate(Apgar1Minuto = as.factor(Apgar1Minuto)) %>% 
  mutate(Apgar5Minuto = as.factor(Apgar5Minuto )) %>% 
  mutate(NumeroAbortosFaixa = case_when(
    NumeroAbortos >=10 ~ "Dez ou mais",
    NumeroAbortos >=7 ~ "Sete a nove",
    NumeroAbortos >=4 ~ "Quatro a seis",
    NumeroAbortos >=1 ~ "Um a três",
    NumeroAbortos ==0 ~ "Nenhum",
    is.na(NumeroAbortos) ~ "Ignorado")) %>% 
  mutate(NumeroAbortosFaixa = as.factor(NumeroAbortosFaixa))



banco_nv <- banco_total %>% 
  filter (CID10Anomalia %in% c("Ignorado","Não se aplica")) %>% 
  mutate (Grupo = "Sem anomalias")


banco_acs <- banco_total %>% 
  filter(str_detect(CID10Anomalia,"Q000|Q001|Q002|Q01|Q05|Q02|Q20|Q21|Q22|Q23|Q24|Q25|Q26|Q27|Q28|Q35|Q36|Q37|Q54|Q56|Q66|Q69|Q71|Q72|Q73|Q743|Q792|Q793|Q90")) %>% 
  mutate(Grupo = case_when(
    str_detect(CID10Anomalia,"Q000|Q001|Q002|Q01|Q05") ~ "Defeitos de tubo neural",
    str_detect(CID10Anomalia,"Q02") ~ "Microcefalia",
    str_detect(CID10Anomalia,"Q20|Q21|Q22|Q23|Q24|Q25|Q26|Q27|Q28") ~ "Cardiopatias congênitas",
    str_detect(CID10Anomalia,"Q35|Q36|Q37") ~ "Fendas orais",
    str_detect(CID10Anomalia,"Q54") ~ "Anomalias de órgãos genitais - Hipospádias",
    str_detect(CID10Anomalia,"Q56") ~ "Anomalias de órgãos genitais - Sexo indefinido",
    str_detect(CID10Anomalia,"Q66|Q69|Q71|Q72|Q73|Q743") ~ "Defeitos de membros",
    str_detect(CID10Anomalia,"Q792|Q793") ~ "Defeitos da parede abdominal",
    str_detect(CID10Anomalia,"Q90") ~ "Síndrome de Down"))



## ---- dados_rn --------

banco_nv1 <- banco_nv %>% 
  select(Sexo, Raca, PesoNascimento, Apgar1Minuto, Apgar5Minuto, nvNumeroSemanasGestacao,
         SemanasGestacao, TipoParto, nvApresentacaoRN, TipoGravidez, 
         ConsultasPrenatal, NumeroAbortosFaixa, LocalNascimento, Grupo)
  

banco_acs1 <- banco_acs %>% 
  select(NumeroIdentificacao, Sexo, Raca, PesoNascimento, Apgar1Minuto, Apgar5Minuto, nvNumeroSemanasGestacao,
         SemanasGestacao, TipoParto, nvApresentacaoRN, TipoGravidez, 
         ConsultasPrenatal, NumeroAbortosFaixa, LocalNascimento, Grupo) 
  

banco_acs2 <- banco_acs1 %>% 
  group_by(Grupo) %>% 
  distinct(NumeroIdentificacao, .keep_all = TRUE)


banco_acs2<- banco_acs2 %>% 
  select(Sexo, Raca, PesoNascimento, Apgar1Minuto, Apgar5Minuto, nvNumeroSemanasGestacao,
         SemanasGestacao, TipoParto, nvApresentacaoRN, TipoGravidez, 
         ConsultasPrenatal, NumeroAbortosFaixa, LocalNascimento, Grupo)


banco_rn <- rbind(banco_nv1, banco_acs2)


banco_rn <- banco_rn %>% 
  mutate(Sexo = recode(Sexo, "F" = "Feminino")) %>% 
  mutate(Sexo = recode(Sexo, "M" = "Masculino")) %>% 
  mutate(Sexo = recode(Sexo, "I" = "Ignorado")) %>% 
  mutate(Sexo=fct_relevel(Sexo, "Ignorado", after = 2)) %>% 
  mutate(Raca=fct_relevel(Raca, "Ignorado", after = 5)) %>% 
  mutate(SemanasGestacao=fct_relevel(SemanasGestacao, "Menos de 22 semanas", after = 0)) %>% 
  mutate(TipoParto=fct_relevel(TipoParto, "Ignorado", after = 2)) %>%
  mutate(TipoGravidez=fct_relevel(TipoGravidez, "Única", after = 0)) %>%
  mutate(TipoGravidez=fct_relevel(TipoGravidez, "Ignorado", after = 3)) %>% 
  mutate(ConsultasPrenatal=fct_relevel(ConsultasPrenatal, "7 ou mais", after = 2)) %>% 
  mutate(ConsultasPrenatal=fct_relevel(ConsultasPrenatal, "Ignorado", after = 4)) %>% 
  mutate(ConsultasPrenatal=fct_relevel(ConsultasPrenatal, "Nenhuma", after = 0)) %>% 
  mutate(nvApresentacaoRN=fct_relevel(nvApresentacaoRN, "Ignorado", after = 3)) %>% 
  mutate(LocalNascimento=fct_relevel(LocalNascimento, "Ignorado", after = 4)) %>% 
  mutate(NumeroAbortosFaixa=fct_relevel(NumeroAbortosFaixa, "Nenhum", after = 0)) %>%
  mutate(NumeroAbortosFaixa=fct_relevel(NumeroAbortosFaixa, "Um a três", after = 1)) %>%
  mutate(NumeroAbortosFaixa=fct_relevel(NumeroAbortosFaixa, "Sete a nove", after = 3)) %>%
  mutate(NumeroAbortosFaixa=fct_relevel(NumeroAbortosFaixa, "Ignorado", after = 5))


## ---- tabela_rn --------

banco_rn %>%  tbl_summary(by = "Grupo",
                          missing = "no",
                          type = all_continuous() ~ "continuous2",
                          statistic = list(all_continuous() ~ c("{N_nonmiss}",
                                                                "{mean} ({sd})",
                                                                "{min}",
                                                                "{max}",
                                                                "{median}",
                                                                "{var}"),
                                           all_categorical() ~ "{n}({p}%)"),
                          label = list(Sexo ~ "Sexo",
                                       Raca ~ "Raça",
                                       PesoNascimento ~ "Peso ao nascimento",
                                       Apgar1Minuto ~ "Apgar 1º Minuto",
                                       Apgar5Minuto ~ "Apgar 5º Minuto",
                                       nvNumeroSemanasGestacao ~ "Nº de Semanas de Gestação",
                                       SemanasGestacao ~ "Semanas de Gestação (faixa)",
                                       TipoParto ~ "Tipo de Parto",
                                       nvApresentacaoRN ~ "Apresentação RN",
                                       TipoGravidez ~ "Tipo de Gravidez",
                                       ConsultasPrenatal ~ "Número de Consultas Pré-natal",
                                       NumeroAbortosFaixa ~ "Número abortos(faixa)",
                                       LocalNascimento ~ "Local de Nascimento")) %>% 
  add_stat_label(label = all_continuous() ~ c("n", "Média (DP)", "Mínimo", "Máximo",
                                              "Mediana", "Variância")) %>%
  modify_table_body(~.x %>% dplyr::relocate(stat_9, .after = label)) %>%
  modify_header(
    label =        "**Característica**",  
    stat_9 =       "**Sem anomalias**, N = 937.083",
    stat_1 =       "**A**, N = 497",
    stat_2 =       "**B**, N = 109",
    stat_3 =       "**C**, N = 1.022",
    stat_4 =       "**D**, N = 336",
    stat_5 =       "**E**, N = 2.078",
    stat_6 =       "**F**, N = 439",
    stat_7 =       "**G**, N = 771",
    stat_8 =       "**H**, N = 74",
    stat_10 =      "**I**, N = 545") %>% 
  add_n() %>%
  modify_spanning_header(c(stat_1, 
                           stat_2,
                           stat_3,
                           stat_4,
                           stat_5,
                           stat_6,
                           stat_7,
                           stat_8,
                           stat_10) ~ "**Grupos de anomalias**") %>% 
  modify_caption("**Caracterização dos nascidos vivos segundo variáveis de gestação e parto para o período de 2011-2020**") %>%
  bold_labels() %>%
  modify_footnote(c(stat_1, 
                    stat_2,
                    stat_3,
                    stat_4,
                    stat_5,
                    stat_6,
                    stat_7,
                    stat_8,
                    stat_10) ~ "A = Anomalias de órgãos genitais - Hipospádias, 
                  B = Anomalias de órgãos genitais - Sexo indefinido,
                  C = Cardiopatias congênitas,
                  D = Defeitos da parede abdominal,
                  E = Defeitos de membros, 
                  F = Defeitos de tubo neural,
                  G = Fendas orais,
                  H = Microcefalia,
                  I = Síndrome de Down") %>% 
  modify_column_alignment(columns = everything(), align = "left") %>% 
  as_gt() %>%
  gt::tab_source_note(gt::md("*Fonte: Sinasc*"))



## ---- dados_pais --------


banco_nv2 <- banco_nv %>% 
  select(EstadoCivilMae, AnosEstudoMae, Escolaridade2010, 
         GrupoOcupacaoMae, RacaMae, IdadeMae, nvIdadePai, Grupo) %>%
  mutate(IdadeMae = na_if(IdadeMae, 99)) %>% 
  mutate(EstadoCivilMae = as.factor(EstadoCivilMae)) %>% 
  mutate(AnosEstudoMae = as.factor(AnosEstudoMae)) %>% 
  mutate(Escolaridade2010 = as.factor(Escolaridade2010)) %>%  
  mutate(RacaMae = as.factor(RacaMae)) %>% 
  mutate(GrupoOcupacaoMae= as.factor(GrupoOcupacaoMae))


banco_acs3 <- banco_acs %>% 
  select(NumeroIdentificacao, EstadoCivilMae, AnosEstudoMae, Escolaridade2010, 
         GrupoOcupacaoMae, RacaMae, IdadeMae, nvIdadePai, Grupo) %>% 
  mutate(IdadeMae = na_if(IdadeMae, 99)) %>% 
  mutate(EstadoCivilMae = as.factor(EstadoCivilMae)) %>% 
  mutate(AnosEstudoMae = as.factor(AnosEstudoMae)) %>% 
  mutate(Escolaridade2010 = as.factor(Escolaridade2010)) %>%  
  mutate(RacaMae = as.factor(RacaMae)) %>% 
  mutate(GrupoOcupacaoMae = as.factor(GrupoOcupacaoMae))


banco_acs4 <- banco_acs3 %>% 
  group_by(Grupo) %>% 
  distinct(NumeroIdentificacao, .keep_all = TRUE)


banco_acs4<- banco_acs4 %>% 
  select(EstadoCivilMae, AnosEstudoMae, Escolaridade2010, GrupoOcupacaoMae,
         RacaMae, IdadeMae, nvIdadePai, Grupo)


banco_pais <- rbind(banco_nv2, banco_acs4)


banco_pais <- banco_pais %>% 
  mutate(EstadoCivilMae=fct_relevel(EstadoCivilMae, "Ignorado", after = 5)) %>% 
  mutate(AnosEstudoMae=fct_relevel(AnosEstudoMae, "Nenhum", after = 0)) %>%
  mutate(AnosEstudoMae=fct_relevel(AnosEstudoMae, "12 ou mais", after = 4)) %>%
  mutate(Escolaridade2010=fct_relevel(Escolaridade2010, "Sem escolaridade", after = 0)) %>%
  mutate(Escolaridade2010=fct_relevel(Escolaridade2010, "Ignorado", after = 6)) %>% 
  mutate(RacaMae=fct_relevel(RacaMae, "Ignorado", after = 5)) %>% 
  mutate(GrupoOcupacaoMae=fct_relevel(GrupoOcupacaoMae, "Ignorado", after = 10))




## ---- tabela_pais --------

banco_pais %>%  tbl_summary(by = "Grupo",
                            missing = "no",
                            type = all_continuous() ~ "continuous2",
                            statistic = list(all_continuous() ~ c("{N_nonmiss}",
                                                                  "{mean} ({sd})",
                                                                  "{min}",
                                                                  "{max}",
                                                                  "{median}",
                                                                  "{var}"),
                                             all_categorical() ~ "{n}({p}%)"),
                            label = list(EstadoCivilMae ~ "Estado Civil da Mãe",
                                         AnosEstudoMae ~ "Anos de Estudo da Mãe",
                                         Escolaridade2010 ~ "Escolaridade da Mãe",
                                         GrupoOcupacaoMae ~ "Grupo da Ocupação da Mãe",
                                         RacaMae ~ "Raça da Mãe",
                                         IdadeMae ~ "Idade da Mãe",
                                         nvIdadePai ~ "Idade do Pai")) %>% 
  add_stat_label(label = all_continuous() ~ c("n", "Média (DP)", "Mínimo", "Máximo",
                                              "Mediana", "Variância")) %>%
  modify_table_body(~.x %>% dplyr::relocate(stat_9, .after = label)) %>%
  modify_header(
    label =        "**Característica**",  
    stat_9 =       "**Sem anomalias**, N = 937.083",
    stat_1 =       "**A**, N = 497",
    stat_2 =       "**B**, N = 109",
    stat_3 =       "**C**, N = 1.022",
    stat_4 =       "**D**, N = 336",
    stat_5 =       "**E**, N = 2.078",
    stat_6 =       "**F**, N = 439",
    stat_7 =       "**G**, N = 771",
    stat_8 =       "**H**, N = 74",
    stat_10 =      "**I**, N = 545") %>% 
  add_n() %>%
  modify_spanning_header(c(stat_1, 
                           stat_2,
                           stat_3,
                           stat_4,
                           stat_5,
                           stat_6,
                           stat_7,
                           stat_8,
                           stat_10) ~ "**Grupos de anomalias**") %>% 
  modify_caption("**Caracterização dos nascidos vivos segundo variáveis dos pais para o período de 2011-2020**") %>%
  bold_labels() %>%
  modify_footnote(c(stat_1, 
                    stat_2,
                    stat_3,
                    stat_4,
                    stat_5,
                    stat_6,
                    stat_7,
                    stat_8,
                    stat_10) ~ "A = Anomalias de órgãos genitais - Hipospádias, 
                  B = Anomalias de órgãos genitais - Sexo indefinido,
                  C = Cardiopatias congênitas,
                  D = Defeitos da parede abdominal,
                  E = Defeitos de membros, 
                  F = Defeitos de tubo neural,
                  G = Fendas orais,
                  H = Microcefalia,
                  I = Síndrome de Down") %>% 
  modify_column_alignment(columns = everything(), align = "left") %>% 
  as_gt() %>%
  gt::tab_source_note(gt::md("*Fonte: Sinasc*"))




## ---- tabela_comparacao_rn_hipospadia --------


comp_rn_hipospadia <- banco_rn %>% 
  filter(Grupo %in% c("Sem anomalias", "Anomalias de órgãos genitais - Hipospádias"))




comp_rn_hipospadia %>%  tbl_summary(by = "Grupo",
                                    missing = "no",
                                    type = all_continuous() ~ "continuous2",
                                    statistic = list(all_continuous() ~ c("{N_nonmiss}",
                                                                          "{mean} ({sd})",
                                                                          "{min}",
                                                                          "{max}",
                                                                          "{median}",
                                                                          "{var}"),
                                                     all_categorical() ~ "{n}({p}%)"),
                                    label = list(Sexo ~ "Sexo",
                                                 Raca ~ "Raça",
                                                 PesoNascimento ~ "Peso ao nascimento",
                                                 Apgar1Minuto ~ "Apgar 1º Minuto",
                                                 Apgar5Minuto ~ "Apgar 5º Minuto",
                                                 nvNumeroSemanasGestacao ~ "Nº de Semanas de Gestação",
                                                 SemanasGestacao ~ "Semanas de Gestação (faixa)",
                                                 TipoParto ~ "Tipo de Parto",
                                                 nvApresentacaoRN ~ "Apresentação RN",
                                                 TipoGravidez ~ "Tipo de Gravidez",
                                                 ConsultasPrenatal ~ "Número de Consultas Pré-natal",
                                                 NumeroAbortosFaixa ~ "Número abortos(faixa)",
                                                 LocalNascimento ~ "Local de Nascimento")) %>% 
  add_stat_label(label = all_continuous() ~ c("n", "Média (DP)", "Mínimo", "Máximo",
                                              "Mediana", "Variância")) %>%
  add_n() %>%
  modify_caption("**Comparação entre nascidos vivos sem anomalias e com anomalias de órgãos genitais - hipospádias para o período de 2011-2020**") %>%
  bold_labels() %>%
  modify_header(label = "**Característica**")  %>% 
  add_p(
    test = list(all_categorical() ~ "fisher.test.simulate.p.values",
                all_continuous() ~ "kruskal.test")) %>% 
  modify_column_alignment(columns = everything(), align = "left") %>% 
  as_gt() %>%
  gt::tab_source_note(gt::md("*Fonte: Sinasc*"))




## ---- tabela_comparacao_rn_sexind --------

comp_rn_sexind <- banco_rn %>% 
  filter(Grupo %in% c("Sem anomalias", "Anomalias de órgãos genitais - Sexo indefinido"))




comp_rn_sexind %>%  tbl_summary(by = "Grupo",
                                missing = "no",
                                type = all_continuous() ~ "continuous2",
                                statistic = list(all_continuous() ~ c("{N_nonmiss}",
                                                                      "{mean} ({sd})",
                                                                      "{min}",
                                                                      "{max}",
                                                                      "{median}",
                                                                      "{var}"),
                                                 all_categorical() ~ "{n}({p}%)"),
                                label = list(Sexo ~ "Sexo",
                                             Raca ~ "Raça",
                                             PesoNascimento ~ "Peso ao nascimento",
                                             Apgar1Minuto ~ "Apgar 1º Minuto",
                                             Apgar5Minuto ~ "Apgar 5º Minuto",
                                             nvNumeroSemanasGestacao ~ "Nº de Semanas de Gestação",
                                             SemanasGestacao ~ "Semanas de Gestação (faixa)",
                                             TipoParto ~ "Tipo de Parto",
                                             nvApresentacaoRN ~ "Apresentação RN",
                                             TipoGravidez ~ "Tipo de Gravidez",
                                             ConsultasPrenatal ~ "Número de Consultas Pré-natal",
                                             NumeroAbortosFaixa ~ "Número abortos(faixa)",
                                             LocalNascimento ~ "Local de Nascimento")) %>% 
  add_stat_label(label = all_continuous() ~ c("n", "Média (DP)", "Mínimo", "Máximo",
                                              "Mediana", "Variância")) %>%
  add_n() %>%
  modify_caption("**Comparação entre nascidos vivos sem anomalias e com anomalias de órgãos genitais - sexo indefinido para o período de 2011-2020**") %>%
  bold_labels() %>%
  modify_header(label = "**Característica**")  %>% 
  add_p(
    test = list(all_categorical() ~ "fisher.test.simulate.p.values",
                all_continuous() ~ "kruskal.test")) %>% 
  modify_column_alignment(columns = everything(), align = "left") %>% 
  as_gt() %>%
  gt::tab_source_note(gt::md("*Fonte: Sinasc*"))


## ---- tabela_comparacao_rn_cardio --------

comp_rn_cardio <- banco_rn %>% 
  filter(Grupo %in% c("Sem anomalias", "Cardiopatias congênitas"))




comp_rn_cardio %>%  tbl_summary(by = "Grupo",
                                missing = "no",
                                type = all_continuous() ~ "continuous2",
                                statistic = list(all_continuous() ~ c("{N_nonmiss}",
                                                                      "{mean} ({sd})",
                                                                      "{min}",
                                                                      "{max}",
                                                                      "{median}",
                                                                      "{var}"),
                                                 all_categorical() ~ "{n}({p}%)"),
                                label = list(Sexo ~ "Sexo",
                                             Raca ~ "Raça",
                                             PesoNascimento ~ "Peso ao nascimento",
                                             Apgar1Minuto ~ "Apgar 1º Minuto",
                                             Apgar5Minuto ~ "Apgar 5º Minuto",
                                             nvNumeroSemanasGestacao ~ "Nº de Semanas de Gestação",
                                             SemanasGestacao ~ "Semanas de Gestação (faixa)",
                                             TipoParto ~ "Tipo de Parto",
                                             nvApresentacaoRN ~ "Apresentação RN",
                                             TipoGravidez ~ "Tipo de Gravidez",
                                             ConsultasPrenatal ~ "Número de Consultas Pré-natal",
                                             NumeroAbortosFaixa ~ "Número abortos(faixa)",
                                             LocalNascimento ~ "Local de Nascimento")) %>% 
  add_stat_label(label = all_continuous() ~ c("n", "Média (DP)", "Mínimo", "Máximo",
                                              "Mediana", "Variância")) %>%
  add_n() %>%
  modify_caption("**Comparação entre nascidos vivos sem anomalias e com cardiopatias congênitas para o período de 2011-2020**") %>%
  bold_labels() %>%
  modify_header(label = "**Característica**")  %>% 
  add_p(
    test = list(all_categorical() ~ "fisher.test.simulate.p.values",
                all_continuous() ~ "kruskal.test")) %>% 
  modify_column_alignment(columns = everything(), align = "left") %>% 
  as_gt() %>%
  gt::tab_source_note(gt::md("*Fonte: Sinasc*"))




## ---- tabela_comparacao_rn_abdominal --------

comp_rn_abdominal <- banco_rn %>% 
  filter(Grupo %in% c("Sem anomalias", "Defeitos da parede abdominal"))




comp_rn_abdominal %>%  tbl_summary(by = "Grupo",
                                   missing = "no",
                                   type = all_continuous() ~ "continuous2",
                                   statistic = list(all_continuous() ~ c("{N_nonmiss}",
                                                                         "{mean} ({sd})",
                                                                         "{min}",
                                                                         "{max}",
                                                                         "{median}",
                                                                         "{var}"),
                                                    all_categorical() ~ "{n}({p}%)"),
                                   label = list(Sexo ~ "Sexo",
                                                Raca ~ "Raça",
                                                PesoNascimento ~ "Peso ao nascimento",
                                                Apgar1Minuto ~ "Apgar 1º Minuto",
                                                Apgar5Minuto ~ "Apgar 5º Minuto",
                                                nvNumeroSemanasGestacao ~ "Nº de Semanas de Gestação",
                                                SemanasGestacao ~ "Semanas de Gestação (faixa)",
                                                TipoParto ~ "Tipo de Parto",
                                                nvApresentacaoRN ~ "Apresentação RN",
                                                TipoGravidez ~ "Tipo de Gravidez",
                                                ConsultasPrenatal ~ "Número de Consultas Pré-natal",
                                                NumeroAbortosFaixa ~ "Número abortos(faixa)",
                                                LocalNascimento ~ "Local de Nascimento")) %>% 
  add_stat_label(label = all_continuous() ~ c("n", "Média (DP)", "Mínimo", "Máximo",
                                              "Mediana", "Variância")) %>%
  add_n() %>%
  modify_caption("**Comparação entre nascidos vivos sem anomalias e com defeitos da parede abdominal para o período de 2011-2020**") %>%
  bold_labels() %>%
  modify_header(label = "**Característica**")  %>% 
  add_p(
    test = list(all_categorical() ~ "fisher.test.simulate.p.values",
                all_continuous() ~ "kruskal.test")) %>% 
  modify_column_alignment(columns = everything(), align = "left") %>% 
  as_gt() %>%
  gt::tab_source_note(gt::md("*Fonte: Sinasc*"))




## ---- tabela_comparacao_rn_membros --------

comp_rn_membros <- banco_rn %>% 
  filter(Grupo %in% c("Sem anomalias", "Defeitos de membros"))




comp_rn_membros %>%  tbl_summary(by = "Grupo",
                                 missing = "no",
                                 type = all_continuous() ~ "continuous2",
                                 statistic = list(all_continuous() ~ c("{N_nonmiss}",
                                                                       "{mean} ({sd})",
                                                                       "{min}",
                                                                       "{max}",
                                                                       "{median}",
                                                                       "{var}"),
                                                  all_categorical() ~ "{n}({p}%)"),
                                 label = list(Sexo ~ "Sexo",
                                              Raca ~ "Raça",
                                              PesoNascimento ~ "Peso ao nascimento",
                                              Apgar1Minuto ~ "Apgar 1º Minuto",
                                              Apgar5Minuto ~ "Apgar 5º Minuto",
                                              nvNumeroSemanasGestacao ~ "Nº de Semanas de Gestação",
                                              SemanasGestacao ~ "Semanas de Gestação (faixa)",
                                              TipoParto ~ "Tipo de Parto",
                                              nvApresentacaoRN ~ "Apresentação RN",
                                              TipoGravidez ~ "Tipo de Gravidez",
                                              ConsultasPrenatal ~ "Número de Consultas Pré-natal",
                                              NumeroAbortosFaixa ~ "Número abortos(faixa)",
                                              LocalNascimento ~ "Local de Nascimento")) %>% 
  add_stat_label(label = all_continuous() ~ c("n", "Média (DP)", "Mínimo", "Máximo",
                                              "Mediana", "Variância")) %>%
  add_n() %>%
  modify_caption("**Comparação entre nascidos vivos sem anomalias e com defeitos de membros para o período de 2011-2020**") %>%
  bold_labels() %>%
  modify_header(label = "**Característica**")  %>% 
  add_p(
    test = list(all_categorical() ~ "fisher.test.simulate.p.values",
                all_continuous() ~ "kruskal.test")) %>% 
  modify_column_alignment(columns = everything(), align = "left") %>% 
  as_gt() %>%
  gt::tab_source_note(gt::md("*Fonte: Sinasc*"))



## ---- tabela_comparacao_rn_neural --------

comp_rn_neural <- banco_rn %>% 
  filter(Grupo %in% c("Sem anomalias", "Defeitos de tubo neural"))




comp_rn_neural %>%  tbl_summary(by = "Grupo",
                                missing = "no",
                                type = all_continuous() ~ "continuous2",
                                statistic = list(all_continuous() ~ c("{N_nonmiss}",
                                                                      "{mean} ({sd})",
                                                                      "{min}",
                                                                      "{max}",
                                                                      "{median}",
                                                                      "{var}"),
                                                 all_categorical() ~ "{n}({p}%)"),
                                label = list(Sexo ~ "Sexo",
                                             Raca ~ "Raça",
                                             PesoNascimento ~ "Peso ao nascimento",
                                             Apgar1Minuto ~ "Apgar 1º Minuto",
                                             Apgar5Minuto ~ "Apgar 5º Minuto",
                                             nvNumeroSemanasGestacao ~ "Nº de Semanas de Gestação",
                                             SemanasGestacao ~ "Semanas de Gestação (faixa)",
                                             TipoParto ~ "Tipo de Parto",
                                             nvApresentacaoRN ~ "Apresentação RN",
                                             TipoGravidez ~ "Tipo de Gravidez",
                                             ConsultasPrenatal ~ "Número de Consultas Pré-natal",
                                             NumeroAbortosFaixa ~ "Número abortos(faixa)",
                                             LocalNascimento ~ "Local de Nascimento")) %>% 
  add_stat_label(label = all_continuous() ~ c("n", "Média (DP)", "Mínimo", "Máximo",
                                              "Mediana", "Variância")) %>%
  add_n() %>%
  modify_caption("**Comparação entre nascidos vivos sem anomalias e com defeitos de tubo neural para o período de 2011-2020**") %>%
  bold_labels() %>%
  modify_header(label = "**Característica**")  %>% 
  add_p(
    test = list(all_categorical() ~ "fisher.test.simulate.p.values",
                all_continuous() ~ "kruskal.test")) %>% 
  modify_column_alignment(columns = everything(), align = "left") %>% 
  as_gt() %>%
  gt::tab_source_note(gt::md("*Fonte: Sinasc*"))



## ---- tabela_comparacao_rn_fendas --------

comp_rn_fendas <- banco_rn %>% 
  filter(Grupo %in% c("Sem anomalias", "Fendas orais"))




comp_rn_fendas %>%  tbl_summary(by = "Grupo",
                                missing = "no",
                                type = all_continuous() ~ "continuous2",
                                statistic = list(all_continuous() ~ c("{N_nonmiss}",
                                                                      "{mean} ({sd})",
                                                                      "{min}",
                                                                      "{max}",
                                                                      "{median}",
                                                                      "{var}"),
                                                 all_categorical() ~ "{n}({p}%)"),
                                label = list(Sexo ~ "Sexo",
                                             Raca ~ "Raça",
                                             PesoNascimento ~ "Peso ao nascimento",
                                             Apgar1Minuto ~ "Apgar 1º Minuto",
                                             Apgar5Minuto ~ "Apgar 5º Minuto",
                                             nvNumeroSemanasGestacao ~ "Nº de Semanas de Gestação",
                                             SemanasGestacao ~ "Semanas de Gestação (faixa)",
                                             TipoParto ~ "Tipo de Parto",
                                             nvApresentacaoRN ~ "Apresentação RN",
                                             TipoGravidez ~ "Tipo de Gravidez",
                                             ConsultasPrenatal ~ "Número de Consultas Pré-natal",
                                             NumeroAbortosFaixa ~ "Número abortos(faixa)",
                                             LocalNascimento ~ "Local de Nascimento")) %>% 
  add_stat_label(label = all_continuous() ~ c("n", "Média (DP)", "Mínimo", "Máximo",
                                              "Mediana", "Variância")) %>%
  add_n() %>%
  modify_caption("**Comparação entre nascidos vivos sem anomalias e com fendas orais para o período de 2011-2020**") %>%
  bold_labels() %>%
  modify_header(label = "**Característica**")  %>% 
  add_p(
    test = list(all_categorical() ~ "fisher.test.simulate.p.values",
                all_continuous() ~ "kruskal.test")) %>% 
  modify_column_alignment(columns = everything(), align = "left") %>% 
  as_gt() %>%
  gt::tab_source_note(gt::md("*Fonte: Sinasc*"))



## ---- tabela_comparacao_rn_microcefalia --------

comp_rn_microcefalia <- banco_rn %>% 
  filter(Grupo %in% c("Sem anomalias", "Microcefalia"))




comp_rn_microcefalia %>%  tbl_summary(by = "Grupo",
                                      missing = "no",
                                      type = all_continuous() ~ "continuous2",
                                      statistic = list(all_continuous() ~ c("{N_nonmiss}",
                                                                            "{mean} ({sd})",
                                                                            "{min}",
                                                                            "{max}",
                                                                            "{median}",
                                                                            "{var}"),
                                                       all_categorical() ~ "{n}({p}%)"),
                                      label = list(Sexo ~ "Sexo",
                                                   Raca ~ "Raça",
                                                   PesoNascimento ~ "Peso ao nascimento",
                                                   Apgar1Minuto ~ "Apgar 1º Minuto",
                                                   Apgar5Minuto ~ "Apgar 5º Minuto",
                                                   nvNumeroSemanasGestacao ~ "Nº de Semanas de Gestação",
                                                   SemanasGestacao ~ "Semanas de Gestação (faixa)",
                                                   TipoParto ~ "Tipo de Parto",
                                                   nvApresentacaoRN ~ "Apresentação RN",
                                                   TipoGravidez ~ "Tipo de Gravidez",
                                                   ConsultasPrenatal ~ "Número de Consultas Pré-natal",
                                                   NumeroAbortosFaixa ~ "Número abortos(faixa)",
                                                   LocalNascimento ~ "Local de Nascimento")) %>% 
  add_stat_label(label = all_continuous() ~ c("n", "Média (DP)", "Mínimo", "Máximo",
                                              "Mediana", "Variância")) %>%
  add_n() %>%
  modify_caption("**Comparação entre nascidos vivos sem anomalias e com microcefalia para o período de 2011-2020**") %>%
  bold_labels() %>%
  modify_header(label = "**Característica**")  %>% 
  add_p(
    test = list(all_categorical() ~ "fisher.test.simulate.p.values",
                all_continuous() ~ "kruskal.test")) %>% 
  modify_column_alignment(columns = everything(), align = "left") %>% 
  as_gt() %>%
  gt::tab_source_note(gt::md("*Fonte: Sinasc*"))





## ---- tabela_comparacao_rn_down --------

comp_rn_down <- banco_rn %>% 
  filter(Grupo %in% c("Sem anomalias", "Síndrome de Down"))




comp_rn_down  %>%  tbl_summary(by = "Grupo",
                               missing = "no",
                               type = all_continuous() ~ "continuous2",
                               statistic = list(all_continuous() ~ c("{N_nonmiss}",
                                                                     "{mean} ({sd})",
                                                                     "{min}",
                                                                     "{max}",
                                                                     "{median}",
                                                                     "{var}"),
                                                all_categorical() ~ "{n}({p}%)"),
                               label = list(Sexo ~ "Sexo",
                                            Raca ~ "Raça",
                                            PesoNascimento ~ "Peso ao nascimento",
                                            Apgar1Minuto ~ "Apgar 1º Minuto",
                                            Apgar5Minuto ~ "Apgar 5º Minuto",
                                            nvNumeroSemanasGestacao ~ "Nº de Semanas de Gestação",
                                            SemanasGestacao ~ "Semanas de Gestação (faixa)",
                                            TipoParto ~ "Tipo de Parto",
                                            nvApresentacaoRN ~ "Apresentação RN",
                                            TipoGravidez ~ "Tipo de Gravidez",
                                            ConsultasPrenatal ~ "Número de Consultas Pré-natal",
                                            NumeroAbortosFaixa ~ "Número abortos(faixa)",
                                            LocalNascimento ~ "Local de Nascimento")) %>% 
  add_stat_label(label = all_continuous() ~ c("n", "Média (DP)", "Mínimo", "Máximo",
                                              "Mediana", "Variância")) %>%
  modify_caption("**Comparação entre nascidos vivos sem anomalias e com Síndrome de Down para o período de 2011-2020**") %>%
  bold_labels() %>%
  modify_header(label = "**Característica**")  %>% 
  modify_table_body(~.x %>% dplyr::relocate(stat_2, .after = label)) %>%
  add_n() %>%
  add_p(
    test = list(all_categorical() ~ "fisher.test.simulate.p.values",
                all_continuous() ~ "kruskal.test")) %>% 
  modify_column_alignment(columns = everything(), align = "left") %>% 
  as_gt() %>%
  gt::tab_source_note(gt::md("*Fonte: Sinasc*"))




## ---- tabela_comparacao_pais_hipospadia --------

comp_pais_hipospadia <- banco_pais %>% 
  filter(Grupo %in% c("Sem anomalias", "Anomalias de órgãos genitais - Hipospádias"))



comp_pais_hipospadia %>%  tbl_summary(by = "Grupo",
                                      missing = "no",
                                      type = all_continuous() ~ "continuous2",
                                      statistic = list(all_continuous() ~ c("{N_nonmiss}",
                                                                            "{mean} ({sd})",
                                                                            "{min}",
                                                                            "{max}",
                                                                            "{median}",
                                                                            "{var}"),
                                                       all_categorical() ~ "{n}({p}%)"),
                                      label = list(EstadoCivilMae ~ "Estado Civil da Mãe",
                                                   AnosEstudoMae ~ "Anos de Estudo da Mãe",
                                                   Escolaridade2010 ~ "Escolaridade da Mãe",
                                                   GrupoOcupacaoMae ~ "Grupo da Ocupação da Mãe",
                                                   RacaMae ~ "Raça da Mãe",
                                                   IdadeMae ~ "Idade da Mãe",
                                                   nvIdadePai ~ "Idade do Pai")) %>% 
  add_stat_label(label = all_continuous() ~ c("n", "Média (DP)", "Mínimo", "Máximo",
                                              "Mediana", "Variância")) %>%
  modify_caption("**Comparação entre nascidos vivos sem anomalias e com anomalias de órgãos genitais - hipospádias, segundo variáveis dos pais para operíodo de 2011-2020**") %>%
  bold_labels() %>%
  modify_header(label = "**Característica**")  %>% 
  add_n() %>%
  add_p(
    test = list(all_categorical() ~ "fisher.test.simulate.p.values",
                all_continuous() ~ "kruskal.test")) %>% 
  modify_column_alignment(columns = everything(), align = "left") %>% 
  as_gt() %>%
  gt::tab_source_note(gt::md("*Fonte: Sinasc*"))



## ---- tabela_comparacao_pais_sexind --------

comp_pais_sexind <- banco_pais %>% 
  filter(Grupo %in% c("Sem anomalias", "Anomalias de órgãos genitais - Sexo indefinido"))



comp_pais_sexind %>%  tbl_summary(by = "Grupo",
                                  missing = "no",
                                  type = all_continuous() ~ "continuous2",
                                  statistic = list(all_continuous() ~ c("{N_nonmiss}",
                                                                        "{mean} ({sd})",
                                                                        "{min}",
                                                                        "{max}",
                                                                        "{median}",
                                                                        "{var}"),
                                                   all_categorical() ~ "{n}({p}%)"),
                                  label = list(EstadoCivilMae ~ "Estado Civil da Mãe",
                                               AnosEstudoMae ~ "Anos de Estudo da Mãe",
                                               Escolaridade2010 ~ "Escolaridade da Mãe",
                                               GrupoOcupacaoMae ~ "Grupo da Ocupação da Mãe",
                                               RacaMae ~ "Raça da Mãe",
                                               IdadeMae ~ "Idade da Mãe",
                                               nvIdadePai ~ "Idade do Pai")) %>% 
  add_stat_label(label = all_continuous() ~ c("n", "Média (DP)", "Mínimo", "Máximo",
                                              "Mediana", "Variância")) %>%
  modify_caption("**Comparação entre nascidos vivos sem anomalias e com anomalias de órgãos genitais - sexo indefinido, segundo variáveis dos pais para o período de 2011-2020**") %>%
  bold_labels() %>%
  modify_header(label = "**Característica**")  %>% 
  add_n() %>%
  add_p(
    test = list(all_categorical() ~ "fisher.test.simulate.p.values",
                all_continuous() ~ "kruskal.test")) %>% 
  modify_column_alignment(columns = everything(), align = "left") %>% 
  as_gt() %>%
  gt::tab_source_note(gt::md("*Fonte: Sinasc*"))


## ---- tabela_comparacao_pais_cardio --------

comp_pais_cardio <- banco_pais %>% 
  filter(Grupo %in% c("Sem anomalias", "Cardiopatias congênitas"))



comp_pais_cardio  %>%  tbl_summary(by = "Grupo",
                                   missing = "no",
                                   type = all_continuous() ~ "continuous2",
                                   statistic = list(all_continuous() ~ c("{N_nonmiss}",
                                                                         "{mean} ({sd})",
                                                                         "{min}",
                                                                         "{max}",
                                                                         "{median}",
                                                                         "{var}"),
                                                    all_categorical() ~ "{n}({p}%)"),
                                   label = list(EstadoCivilMae ~ "Estado Civil da Mãe",
                                                AnosEstudoMae ~ "Anos de Estudo da Mãe",
                                                Escolaridade2010 ~ "Escolaridade da Mãe",
                                                GrupoOcupacaoMae ~ "Grupo da Ocupação da Mãe",
                                                RacaMae ~ "Raça da Mãe",
                                                IdadeMae ~ "Idade da Mãe",
                                                nvIdadePai ~ "Idade do Pai")) %>% 
  add_stat_label(label = all_continuous() ~ c("n", "Média (DP)", "Mínimo", "Máximo",
                                              "Mediana", "Variância")) %>%
  modify_caption("**Comparação entre nascidos vivos sem anomalias e com cardiopatias congênitas, segundo variáveis dos pais para o período de 2011-2020**") %>%
  bold_labels() %>%
  modify_header(label = "**Característica**")  %>% 
  add_n() %>%
  add_p(
    test = list(all_categorical() ~ "fisher.test.simulate.p.values",
                all_continuous() ~ "kruskal.test")) %>% 
  modify_column_alignment(columns = everything(), align = "left") %>% 
  as_gt() %>%
  gt::tab_source_note(gt::md("*Fonte: Sinasc*"))



## ---- tabela_comparacao_pais_abdominal --------

comp_pais_abdominal <- banco_pais %>% 
  filter(Grupo %in% c("Sem anomalias", "Defeitos da parede abdominal"))



comp_pais_abdominal %>%  tbl_summary(by = "Grupo",
                                     missing = "no",
                                     type = all_continuous() ~ "continuous2",
                                     statistic = list(all_continuous() ~ c("{N_nonmiss}",
                                                                           "{mean} ({sd})",
                                                                           "{min}",
                                                                           "{max}",
                                                                           "{median}",
                                                                           "{var}"),
                                                      all_categorical() ~ "{n}({p}%)"),
                                     label = list(EstadoCivilMae ~ "Estado Civil da Mãe",
                                                  AnosEstudoMae ~ "Anos de Estudo da Mãe",
                                                  Escolaridade2010 ~ "Escolaridade da Mãe",
                                                  GrupoOcupacaoMae ~ "Grupo da Ocupação da Mãe",
                                                  RacaMae ~ "Raça da Mãe",
                                                  IdadeMae ~ "Idade da Mãe",
                                                  nvIdadePai ~ "Idade do Pai")) %>% 
  add_stat_label(label = all_continuous() ~ c("n", "Média (DP)", "Mínimo", "Máximo",
                                              "Mediana", "Variância")) %>%
  modify_caption("**Comparação entre nascidos vivos sem anomalias e com defeitos da parede abdominal, segundo variáveis dos pais para o período de 2011-2020**") %>%
  bold_labels() %>%
  modify_header(label = "**Característica**")  %>% 
  add_n() %>%
  add_p(
    test = list(all_categorical() ~ "fisher.test.simulate.p.values",
                all_continuous() ~ "kruskal.test")) %>% 
  modify_column_alignment(columns = everything(), align = "left") %>% 
  as_gt() %>%
  gt::tab_source_note(gt::md("*Fonte: Sinasc*"))



## ---- tabela_comparacao_pais_membros --------

comp_pais_membros <- banco_pais %>% 
  filter(Grupo %in% c("Sem anomalias", "Defeitos de membros"))



comp_pais_membros %>%  tbl_summary(by = "Grupo",
                                   missing = "no",
                                   type = all_continuous() ~ "continuous2",
                                   statistic = list(all_continuous() ~ c("{N_nonmiss}",
                                                                         "{mean} ({sd})",
                                                                         "{min}",
                                                                         "{max}",
                                                                         "{median}",
                                                                         "{var}"),
                                                    all_categorical() ~ "{n}({p}%)"),
                                   label = list(EstadoCivilMae ~ "Estado Civil da Mãe",
                                                AnosEstudoMae ~ "Anos de Estudo da Mãe",
                                                Escolaridade2010 ~ "Escolaridade da Mãe",
                                                GrupoOcupacaoMae ~ "Grupo da Ocupação da Mãe",
                                                RacaMae ~ "Raça da Mãe",
                                                IdadeMae ~ "Idade da Mãe",
                                                nvIdadePai ~ "Idade do Pai")) %>% 
  add_stat_label(label = all_continuous() ~ c("n", "Média (DP)", "Mínimo", "Máximo",
                                              "Mediana", "Variância")) %>%
  modify_caption("**Comparação entre nascidos vivos sem anomalias e com defeitos de membros, segundo variáveis dos pais para o período de 2011-2020**") %>%
  bold_labels() %>%
  modify_header(label = "**Característica**")  %>% 
  add_n() %>%
  add_p(
    test = list(all_categorical() ~ "fisher.test.simulate.p.values",
                all_continuous() ~ "kruskal.test")) %>% 
  modify_column_alignment(columns = everything(), align = "left") %>% 
  as_gt() %>%
  gt::tab_source_note(gt::md("*Fonte: Sinasc*"))


## ---- tabela_comparacao_pais_neural --------

comp_pais_neural <- banco_pais %>% 
  filter(Grupo %in% c("Sem anomalias", "Defeitos de tubo neural"))



comp_pais_neural %>%  tbl_summary(by = "Grupo",
                                  missing = "no",
                                  type = all_continuous() ~ "continuous2",
                                  statistic = list(all_continuous() ~ c("{N_nonmiss}",
                                                                        "{mean} ({sd})",
                                                                        "{min}",
                                                                        "{max}",
                                                                        "{median}",
                                                                        "{var}"),
                                                   all_categorical() ~ "{n}({p}%)"),
                                  label = list(EstadoCivilMae ~ "Estado Civil da Mãe",
                                               AnosEstudoMae ~ "Anos de Estudo da Mãe",
                                               Escolaridade2010 ~ "Escolaridade da Mãe",
                                               GrupoOcupacaoMae ~ "Grupo da Ocupação da Mãe",
                                               RacaMae ~ "Raça da Mãe",
                                               IdadeMae ~ "Idade da Mãe",
                                               nvIdadePai ~ "Idade do Pai")) %>% 
  add_stat_label(label = all_continuous() ~ c("n", "Média (DP)", "Mínimo", "Máximo",
                                              "Mediana", "Variância")) %>%
  modify_caption("**Comparação entre nascidos vivos sem anomalias e com defeitos de tubo neural, segundo variáveis dos pais para o período de 2011-2020**") %>%
  bold_labels() %>%
  modify_header(label = "**Característica**")  %>% 
  add_n() %>%
  add_p(
    test = list(all_categorical() ~ "fisher.test.simulate.p.values",
                all_continuous() ~ "kruskal.test")) %>% 
  modify_column_alignment(columns = everything(), align = "left") %>% 
  as_gt() %>%
  gt::tab_source_note(gt::md("*Fonte: Sinasc*"))


## ---- tabela_comparacao_pais_fendas --------

comp_pais_fendas <- banco_pais %>% 
  filter(Grupo %in% c("Sem anomalias", "Fendas orais"))



comp_pais_fendas %>%  tbl_summary(by = "Grupo",
                                  missing = "no",
                                  type = all_continuous() ~ "continuous2",
                                  statistic = list(all_continuous() ~ c("{N_nonmiss}",
                                                                        "{mean} ({sd})",
                                                                        "{min}",
                                                                        "{max}",
                                                                        "{median}",
                                                                        "{var}"),
                                                   all_categorical() ~ "{n}({p}%)"),
                                  label = list(EstadoCivilMae ~ "Estado Civil da Mãe",
                                               AnosEstudoMae ~ "Anos de Estudo da Mãe",
                                               Escolaridade2010 ~ "Escolaridade da Mãe",
                                               GrupoOcupacaoMae ~ "Grupo da Ocupação da Mãe",
                                               RacaMae ~ "Raça da Mãe",
                                               IdadeMae ~ "Idade da Mãe",
                                               nvIdadePai ~ "Idade do Pai")) %>% 
  add_stat_label(label = all_continuous() ~ c("n", "Média (DP)", "Mínimo", "Máximo",
                                              "Mediana", "Variância")) %>%
  modify_caption("**Comparação entre nascidos vivos sem anomalias e com fendas orais, segundo variáveis dos pais para o período de 2011-2020**") %>%
  bold_labels() %>%
  modify_header(label = "**Característica**")  %>% 
  add_n() %>%
  add_p(
    test = list(all_categorical() ~ "fisher.test.simulate.p.values",
                all_continuous() ~ "kruskal.test")) %>% 
  modify_column_alignment(columns = everything(), align = "left") %>% 
  as_gt() %>%
  gt::tab_source_note(gt::md("*Fonte: Sinasc*"))


## ---- tabela_comparacao_pais_microcefalia --------

comp_pais_microcefalia <- banco_pais %>% 
  filter(Grupo %in% c("Sem anomalias", "Microcefalia"))



comp_pais_microcefalia %>%  tbl_summary(by = "Grupo",
                                        missing = "no",
                                        type = all_continuous() ~ "continuous2",
                                        statistic = list(all_continuous() ~ c("{N_nonmiss}",
                                                                              "{mean} ({sd})",
                                                                              "{min}",
                                                                              "{max}",
                                                                              "{median}",
                                                                              "{var}"),
                                                         all_categorical() ~ "{n}({p}%)"),
                                        label = list(EstadoCivilMae ~ "Estado Civil da Mãe",
                                                     AnosEstudoMae ~ "Anos de Estudo da Mãe",
                                                     Escolaridade2010 ~ "Escolaridade da Mãe",
                                                     GrupoOcupacaoMae ~ "Grupo da Ocupação da Mãe",
                                                     RacaMae ~ "Raça da Mãe",
                                                     IdadeMae ~ "Idade da Mãe",
                                                     nvIdadePai ~ "Idade do Pai")) %>% 
  add_stat_label(label = all_continuous() ~ c("n", "Média (DP)", "Mínimo", "Máximo",
                                              "Mediana", "Variância")) %>%
  modify_caption("**Comparação entre nascidos vivos sem anomalias e com microcefalia, segundo variáveis dos pais para o período de 2011-2020**") %>%
  bold_labels() %>%
  modify_header(label = "**Característica**")  %>% 
  add_n() %>%
  add_p(
    test = list(all_categorical() ~ "fisher.test.simulate.p.values",
                all_continuous() ~ "kruskal.test")) %>% 
  modify_column_alignment(columns = everything(), align = "left") %>% 
  as_gt() %>%
  gt::tab_source_note(gt::md("*Fonte: Sinasc*"))



## ---- tabela_comparacao_pais_down --------

comp_pais_down <- banco_pais %>% 
  filter(Grupo %in% c("Sem anomalias", "Síndrome de Down"))



comp_pais_down %>%  tbl_summary(by = "Grupo",
                                missing = "no",
                                type = all_continuous() ~ "continuous2",
                                statistic = list(all_continuous() ~ c("{N_nonmiss}",
                                                                      "{mean} ({sd})",
                                                                      "{min}",
                                                                      "{max}",
                                                                      "{median}",
                                                                      "{var}"),
                                                 all_categorical() ~ "{n}({p}%)"),
                                label = list(EstadoCivilMae ~ "Estado Civil da Mãe",
                                             AnosEstudoMae ~ "Anos de Estudo da Mãe",
                                             Escolaridade2010 ~ "Escolaridade da Mãe",
                                             GrupoOcupacaoMae ~ "Grupo da Ocupação da Mãe",
                                             RacaMae ~ "Raça da Mãe",
                                             IdadeMae ~ "Idade da Mãe",
                                             nvIdadePai ~ "Idade do Pai")) %>% 
  add_stat_label(label = all_continuous() ~ c("n", "Média (DP)", "Mínimo", "Máximo",
                                              "Mediana", "Variância")) %>%
  modify_caption("**Comparação entre nascidos vivos sem anomalias e com síndrome de Down, segundo variáveis dos pais para o período de 2011-2020**") %>%
  bold_labels() %>%
  modify_table_body(~.x %>% dplyr::relocate(stat_2, .after = label)) %>%
  modify_header(label = "**Característica**")  %>% 
  add_n() %>%
  add_p(
    test = list(all_categorical() ~ "fisher.test.simulate.p.values",
                all_continuous() ~ "kruskal.test")) %>% 
  modify_column_alignment(columns = everything(), align = "left") %>% 
  as_gt() %>%
  gt::tab_source_note(gt::md("*Fonte: Sinasc*"))



#histogramas


## ---- hist_peso --------

ggplot(banco_rn, aes(x = PesoNascimento)) +
  geom_histogram(aes(y=..density..)) +
  geom_density(color = "red") +
  facet_wrap(~Grupo, ncol = 3, scales = "free_y")


## ---- hist_semana --------

ggplot(banco_rn, aes(x = nvNumeroSemanasGestacao)) +
  geom_histogram(aes(y=..density..), bins = 20, binwidth = 1 ) +
  geom_density(color = "red") +
  scale_x_continuous(breaks = seq(from = 18, to = 46, by = 2)) +
  facet_wrap(~Grupo, ncol = 3, scales = "free_y") 

## ---- hist_mae --------
ggplot(banco_pais, aes(x = IdadeMae)) +
  geom_histogram(aes(y=..density..)) +
  geom_density(color = "red") +
  scale_x_continuous(breaks = seq(from = 10, to = 65, by = 5)) +
  facet_wrap(~Grupo, ncol = 3, scales = "free_y")

## ---- hist_pai --------
ggplot(banco_pais, aes(x = nvIdadePai)) +
  geom_histogram(aes(y=..density..)) +
  geom_density(color = "red") +
  scale_x_continuous(breaks = seq(from = 9, to = 99, by = 10)) +
  facet_wrap(~Grupo, ncol = 3, scales = "free_y")


