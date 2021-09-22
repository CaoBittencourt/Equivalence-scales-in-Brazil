# 1. PACOTES
pkg <- c('sandwich', 'lmtest', 'stargazer', 'np', 'Matching', 'systemfit', 'ivreg', #Regressões e estatística
         'rgdal', 'geobr', 'gghighlight', 'ggridges', 'ggthemes', 'viridis', 'patchwork', 'naniar', 'scales', #Visualização
         'rio', 'plyr', 'glue', 'tidyverse') #Leitura e Manipulação de Dados
# Obs: testar depois o pacote "micEcon" (tem aidsest() etc)
lapply(pkg, function(x)
  if(!require(x, character.only = T))
  {install.packages(x); require(x)})

# lapply(pkg, function(x)
#   {citation(package = x)})

# Função de Agregação de POF
source('C:/Users/Sony/Documents/GitHub/TCC/POF_AGG.R')

# Estimação de Escalas de Engel e Rothbarth
source('C:/Users/Sony/Documents/GitHub/TCC/Escalas_Engel_Rothbarth.R')

select <- dplyr::select
mutate <- dplyr::mutate
summarise <- dplyr::summarise

# 2. DADOS
setwd('C:/Users/Sony/Documents/TCC_DADOS')

# POF 2002-2003, 2008-2009 via Stata (DataZoom)
basepadrao.pof2002 <- import('./POF2002/pof2002_dom_standard.dta')
basepadrao.pof2008 <- import('./POF2008/pof2008_dom_standard.dta')

# Registros 1 a 4 das POFs 2002-2003, 2008-2009, 2017-2018 via Stata (DataZoom)
# Obs: registro 3 e 4 não interessam => Não utilizar

lapply(1:4, function(x){
  
  glue('./POF2002/pof2002_tr{x}.dta') %>%
    import(.)
  
}) -> tr1_4.pof2002

lapply(1:4, function(x){
  
  glue('./POF2008/pof2008_tr{x}.dta') %>%
    import(.)
  
}) -> tr1_4.pof2008

lapply(1:4, function(x){
  
  glue('./POF2017/pof2017_tr{x}.dta') %>%
    import(.)
  
}) -> tr1_4.pof2017

# Nomes em comum/diferentes entre as bases de dados
# lapply(1:4, function(x){
# 
# intersect(names(tr1_4.pof2002[[x]]),
#           names(tr1_4.pof2008[[x]]))
# 
# })
# 
# lapply(1:4, function(x){
# 
# setdiff(names(tr1_4.pof2002[[x]]),
#         names(tr1_4.pof2008[[x]]))
# 
# })

# # Identificadores das categorias (despesas e receitas)
# Não é necessário agregar as receitas (as variáveis renda e renda_total já são agregações mensais)
id_receitas.agg <- list(receitas = c('vare', 'vre', 'vvp'))

# Categorias gerais
list(alimentacao = 'da',
     moradia = 'dd02',
     vestuario = 'dd03',
     transporte = 'dd04',
     higiene = 'dd05',
     saude = c('dd06', 'dd6'),
     educacao = 'dd07',
     lazer = 'dd08',
     fumo = 'dd09',
     servicos.pessoais = 'dd10',
     despesas.diversas = 'dd11',
     impostos_previdencia_doacoes = 'dd12',
     imoveis_investimentos = 'dd13',
     emprestimos = 'dd14') %>% 
  lapply(function(x){
    
    c(paste0('va',x),
      paste0('nm',x),
      paste0('cr',x))
    
  }) -> id_despesas.agg

# Identidade das despesas para agregação (categorias específicas utilizadas em alguns métodos de estimação) 
list(takeout.food = c('da21', 'da22', 'da23', 'da24',
                      'da25', 'da28', 'da29'),
     lanche.escolar = 'da27',
     vestuario.infantil = 'dd033',
     vestuario.homem_mulher = c('dd031', 'dd032'),
     bebidas.alcoolicas = 'da26',
     jogos_apostas = 'dd111') %>% 
  lapply(function(x){
    
    c(paste0('va',x),
      paste0('nm',x),
      paste0('cr',x))
    
  }) -> id_despesas.especificas


# Agregação das POFs
# Nomes com base na POF 2008-2009 => Mudar nomes das variáveis nas outras POFs antes de aplicar o algoritmo
# POF 2002-2003
# setdiff(names(basepadrao.pof2002),
#         names(basepadrao.pof2008))

# Consumo
# basepadrao.pof2002 %>% 
#   rename(cod_uf = uf,
#          num_seq = seq,
#          num_dv = dv,
#          cod_domc = domcl,
#          num_ext_renda = estrato,
#          fator_expansao1 = fator_set,
#          fator_expansao2 = fator,
#          qtd_morador_domc = n_morador,
#          renda_total = renda) -> basepadrao.pof2002_renomeada
# 
# # Registro de indivíduos
# tr1_4.pof2002[[2]] %>% 
#   rename(cod_uf = uf,
#          num_seq = seq,
#          num_dv = dv,
#          cod_domc = domcl,
#          num_ext_renda = estrato,
#          fator_expansao1 = fator_set,
#          fator_expansao2 = fator,
#          renda_total = renda,
#          cod_rel_pess_refe_uc = rel_chefe,
#          cod_sexo = sexo,
#          idade_anos = idade) -> individuos_renomeados
# 
# pof.agg(individuos = individuos_renomeados,
#         consumo = basepadrao.pof2002_renomeada,
#         lista.id_receitas.agg = id_receitas.agg,
#         lista.id_despesas.agg = id_despesas.agg,
#         lista.id_despesas.especificas = id_despesas.especificas,
#         faixas.etarias = c(0, 4, 9, 14, max(individuos_renomeados$idade_anos)),
#         sexo = F) -> pof.2002_vaz_semsexo

# referencia.sem_sexo.vaz <- '`(14,110] anos`'
# referencia.sem_sexo.vaz <- '`Homens: (14,110] anos`'

# pof.agg(individuos = individuos_renomeados,
#         consumo = basepadrao.pof2002_renomeada,
#         lista.id_receitas.agg = id_receitas.agg,
#         lista.id_despesas.agg = id_despesas.agg,
#         lista.id_despesas.especificas = id_despesas.especificas,
#         faixas.etarias = c(0, 4, 9, 14, max(individuos_renomeados$idade_anos)),
#         sexo = T) %>% 
#   filter(!(qtd_conjuge == 0 & qtd_filhos > 0)) %>%
#   filter(qtd_morador_domc <= 5, qtd_filhos <= 3) %>%
#   lm.engel.rothbarth(welfare.indicator = 'share_despesas.mensais.alimentacao',
#                      expenditure = 'renda_total_per.capita') %>% 
#   lm.heteroskedasticity(.) %>%
#   equivalence.scales.dudel(pessoa.referencia = referencia.sem_sexo.vaz,
#                            expenditure = 'renda_total_per.capita',
#                            na.h = 2, nc.h = 1,
#                            na.r = 2, nc.r = 0) %>% 
#   select(term, equivalence.scale, std.error,
#          member.cost, p.value) %>%
#   mutate(p.value = p.value <= 0.05)


# pof.2002_vaz_semsexo %>% 
#   select(contains('anos'),
#          share_despesas.mensais.alimentacao,
#          despesas.mensais.totais_per.capita,
#          renda_total_per.capita,
#          desc_urbano,
#          UF_sigla) -> lala
# 
# pof.2002_vaz_semsexo %>% 
#   iv2sls.engel.rothbarth(welfare.indicator = 'share_despesas.mensais.alimentacao',
#                          expenditure = 'despesas.mensais.totais_per.capita',
#                          iv.expenditure = 'renda_total_per.capita',
#                          control = c('UF_sigla', 'desc_urbano')) %>% 
#   lm.heteroskedasticity(.) %>%
#   equivalence.scales.dudel(pessoa.referencia = referencia.sem_sexo.vaz,
#                            expenditure = 'despesas.mensais.totais_per.capita',
#                            na.h = 2, nc.h = 1,
#                            na.r = 2, nc.r = 0) %>% 
#   select(term, equivalence.scale, std.error,
#          member.cost, p.value) %>%
#   mutate(p.value = p.value <= 0.05) 
# 
# pof.agg(individuos = tr1_4.pof2008[[2]],
#         consumo = basepadrao.pof2008,
#         lista.id_receitas.agg = id_receitas.agg,
#         lista.id_despesas.agg = id_despesas.agg,
#         lista.id_despesas.especificas = id_despesas.especificas,
#         faixas.etarias = c(0, 4, 9, 14, max(tr1_4.pof2008[[2]]$idade_anos)),
#         sexo = F) %>% 
#   filter(!(qtd_conjuge == 0 & qtd_filhos > 0)) %>%
#   filter(qtd_morador_domc <= 5, qtd_filhos <= 3) %>%
#   lm.engel.rothbarth(welfare.indicator = 'share_despesas.mensais.alimentacao') %>%
#   # iv2sls.engel.rothbarth(welfare.indicator = 'share_https://upload.wikimedia.org/wikipedia/commons/c/c8/William-Adolphe_Bouguereau_%281825-1905%29_-_The_Madonna_of_the_Roses_%281903%29.jpgdespesas.mensais.alimentacao',
#   #                        expenditure = 'despesas.mensais.totais_per.capita',
#   #                        iv.expenditure = 'renda_total_per.capita',
#   #                        control = c('UF_sigla', 'desc_urbano')) %>% 
#   lm.heteroskedasticity(.) %>%
#   equivalence.scales.dudel(pessoa.referencia = '`(14,104] anos`',
#                            expenditure = 'despesas.mensais.totais_per.capita',
#                            na.h = 2, nc.h = 1,
#                            na.r = 2, nc.r = 0) %>% 
#   select(term, equivalence.scale, std.error,
#          member.cost, p.value) %>%
#   mutate(p.value = p.value <= 0.05) 


# REGISTROS
