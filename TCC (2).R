# 1. PACOTES
pkg <- c('sandwich', 'lmtest', 'stargazer', 'rgdal',
         'geobr', 'gghighlight', 'ggridges', 'ggthemes', 'viridis', 'patchwork', 'naniar', 'scales',
         'np', 'Matching', 'systemfit', 'ivreg',
         'rio', 'plyr', 'glue', 'tidyverse')

lapply(pkg, function(x)
  if(!require(x, character.only = T))
  {install.packages(x); require(x)})

select <- dplyr::select
mutate <- dplyr::mutate
summarise <- dplyr::summarise

# 2. DADOS

setwd('C:/Users/Sony/Documents/GitHub/TCC')

# POF 2002-2003, 2008-2009 via Stata (DataZoom)
basepadrao.pof2002 <- import('./Dados/POF2002/pof2002_dom_standard.dta')
basepadrao.pof2008 <- import('./Dados/POF2008/pof2008_dom_standard.dta')

# Registros 1 a 4 das POFs 2002-2003, 2008-2009, 2017-2018 via Stata (DataZoom)
lapply(1:4, function(x){
  
  glue('./Dados/POF2002/pof2002_tr{x}.dta') %>%
    import(.)
  
}) -> tr1_4.pof2002

lapply(1:4, function(x){
  
  glue('./Dados/POF2008/pof2008_tr{x}.dta') %>%
    import(.)
  
}) -> tr1_4.pof2008

lapply(1:4, function(x){
  
  glue('./Dados/POF2017/pof2017_tr{x}.dta') %>%
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



# usar id_dom


# Não é necessário agregar as receitas (as variáveis renda e renda_total já são agregações mensais)
# id_receitas.agg <- list(receitas = c('vare', 'vre', 'vvp'))

lapply(id_despesas.agg, function(id){
  
  basepadrao.pof2002 %>%
    replace(is.na(.), 0) %>%
    mutate(rowSums(across(.cols = contains(id))))
  
}) %>% as_tibble(.) %>% View()

lapply(id_despesas.agg[1], function(id){
  
  basepadrao.pof2002 %>%
    mutate(!!sym(names(id_despesas.agg[id])) := 
             rowSums(across(.cols = contains(id_despesas.agg[[id]])),
                     na.rm = T)) %>% 
    select(-contains(id_despesas.agg[[id]]))
  
})

base <- basepadrao.pof2002

lapply(names(id_despesas.especificas), function(id){
  
  base %>%
    mutate(!!sym(glue('despesas.mensais.{names(id_despesas.especificas[id])}')) :=
             rowSums(across(.cols = contains(id_despesas.especificas[[id]])),
                     na.rm = T)/12) %>%
    select(-contains(id_despesas.especificas[[id]])) ->> base
  
}) %>% 
  invisible(.)

lapply(names(id_despesas.agg), function(id){
  
  base %>%
    mutate(!!sym(glue('despesas.mensais.{names(id_despesas.agg[id])}')) :=
             rowSums(across(.cols = contains(id_despesas.agg[[id]])),
                     na.rm = T)/12) %>%
    select(-contains(id_despesas.agg[[id]])) ->> base
  
}) %>% 
  invisible(.)

base

id_despesas.agg %>% names()
id_despesas.agg[1]
id_despesas.agg[1] %>% names()
id_despesas.agg[[1]]

lapply(id_despesas.agg, function(x)
  
  x
  
)



id <- 'emprestimos'

c(emprestimos = '1',
  lala = '2') %>%
  lapply(function(x)
    
    c(paste0('va',x),
      paste0('nm',x),
      paste0('cr',x))
    
  ) %>% names()

# mapply(list.cat = id_despesas.agg['emprestimos'], 
#        id = names(id_despesas.agg['emprestimos']),
#        FUN = function(list.cat, id){
#          
#          basepadrao.pof2002 %>%
#            mutate(!!sym(id) :=
#                     rowSums(across(.cols = contains(list.cat)),
#                             na.rm = T)) %>%
#            select(-contains(list.cat))
#          
#        })

# Identidade das despesas para agregação (categorias gerais)
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
  sapply(function(x){
    
    c(paste0('va',x),
      paste0('nm',x),
      paste0('cr',x))
    
  }) -> names(id_despesas.agg) 

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


pof.agg <- function(individuos, consumo, 
                    lista.id_receitas,
                    lista.id_despesas.agg,
                    lista.id_despesas.especificas,
                    faixas.etarias, sexo = T){
  
  individuos %>% 
    transmute(cod_rel_pess_refe_uc = case_when(cod_rel_pess_refe_uc == 1 ~ 'num_pessoa.ref',
                                               cod_rel_pess_refe_uc == 2 ~ 'num_conjuge',
                                               cod_rel_pess_refe_uc == 3 ~ 'num_filhos',
                                               cod_rel_pess_refe_uc == 4 ~ 'num_outros.parentes',
                                               cod_rel_pess_refe_uc == 5 ~ 'num_agregados',
                                               cod_rel_pess_refe_uc == 6 ~ 'num_pensionistas',
                                               cod_rel_pess_refe_uc == 7 ~ 'num_empregados.dom',
                                               cod_rel_pess_refe_uc == 8 ~ 'num_parentes.empregados'),
              cod_uf = cod_uf, num_seq = num_seq, 
              num_dv = num_dv, cod_domc = cod_domc,
              num_ext_renda = num_ext_renda, 
              fator_expansao1 = fator_expansao1, 
              fator_expansao2 = fator_expansao2) %>% 
    pivot_wider(names_from = cod_rel_pess_refe_uc,
                values_from = 1,
                values_fn = length,
                values_fill = 0) -> individuos.agg_parentesco
  
  if(sexo == T)
    individuos %>% 
    transmute(cod_sexo = case_when(cod_sexo == 1 ~ 'Homens',
                                   cod_sexo == 2 ~ 'Mulheres'),
              idade = cut(idade_anos,
                          breaks = faixas.etarias,
                          include.lowest = T,
                          right = T,
                          na.rm = T),
              cod_uf = cod_uf, num_seq = num_seq, 
              num_dv = num_dv, cod_domc = cod_domc,
              num_ext_renda = num_ext_renda, 
              fator_expansao1 = fator_expansao1, 
              fator_expansao2 = fator_expansao2) %>% 
    pivot_wider(names_from = c(cod_sexo, idade),
                names_glue = '{cod_sexo}: {idade} anos',
                names_sort = T,
                values_from = 1,
                values_fn = length,
                values_fill = 0) -> individuos.agg_sexo.idade
  
  else
    individuos %>% 
    transmute(idade = cut(idade_anos,
                          breaks = faixas.etarias,
                          include.lowest = T,
                          right = T,
                          na.rm = T),
              cod_uf = cod_uf, num_seq = num_seq, 
              num_dv = num_dv, cod_domc = cod_domc,
              num_ext_renda = num_ext_renda, 
              fator_expansao1 = fator_expansao1, 
              fator_expansao2 = fator_expansao2) %>% 
    pivot_wider(names_from = idade,
                names_glue = '{idade} anos',
                names_sort = T,
                values_from = 1,
                values_fn = length,
                values_fill = 0) -> individuos.agg_sexo.idade
  
  # GERAL
  merge(individuos.agg_parentesco,
        individuos.agg_sexo.idade) -> individuos.agg
  
  # CATEGORIAS ORÇAMENTÁRIAS
  id_despesas <- list(alimentacao = 'da',
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
                      empresimos = 'dd14')
  
  id_take.out <- c('da21', 'da22', 'da23', 'da24',
                   'da25', 'da28', 'da29')
  
  id_lanche.escolar <- 'da27'
  
  id_vestuario_infantil <- 'dd033'
  
  id_vestuario_homem.mulher <- c('dd031', 'dd032')
  
  id_bebidas_alcoolicas <- 'da26'
  
  id_jogos_apostas <- 'dd111'
  
  id_receitas <- 're'
  
  
  # CONSUMO AGREGADO
  lapply(lista.id_receitas,function(id){
    
    consumo %>%
      mutate(across(.cols = contains(id),
                    .fns = rowSums(na.rm = T),
                    .names = '{names(id)}'))}) -> consumo
  lapply(lista.id_despesas.agg, function(id){
    
    consumo %>%
      mutate(across(.cols = contains(id),
                    .fns = rowSums(na.rm = T),
                    .names = '{names(id)}'))}) -> consumo
  lapply(lista.id_despesas.especificas, function(id){
    
    consumo %>%
      mutate(across(.cols = contains(id),
                    .fns = rowSums(na.rm = T),
                    .names = '{names(id)}'))}) -> consumo
  
  consumo %>% 
    select(names(lista.id_receitas),
           names(lista.id_despesas.agg),
           names(lista.id_despesas.especificas))
  
  
  
  consumo %>%
    transmute(despesas.alimentacao = rowSums(select(.,contains(id_despesas$alimentacao)), na.rm = T),
              despesas.alimentacao.fora = rowSums(select(.,contains(id_take.out)), na.rm = T),
              despesas.alimentacao.lanche.escolar = rowSums(select(.,contains(id_lanche.escolar)), na.rm = T),
              
              despesas.moradia = rowSums(select(.,contains(id_despesas$moradia)), na.rm = T),
              despesas.vestuario = rowSums(select(.,contains(id_despesas$vestuario)), na.rm = T),
              despesas.transporte = rowSums(select(.,contains(id_despesas$transporte)), na.rm = T),
              despesas.higiene = rowSums(select(.,contains(id_despesas$higiene)), na.rm = T),
              despesas.saude = rowSums(select(.,contains(id_despesas$saude)), na.rm = T),
              
              despesas.educacao = rowSums(select(.,contains(id_despesas$educacao)), na.rm = T),
              
              despesas.lazer = rowSums(select(.,contains(id_despesas$lazer)), na.rm = T),
              despesas.fumo = rowSums(select(.,contains(id_despesas$fumo)), na.rm = T),
              despesas.servicos.pessoais = rowSums(select(.,contains(id_despesas$servicos.pessoais)), na.rm = T),
              despesas.diversas = rowSums(select(.,contains(id_despesas$despesas.diversas)), na.rm = T),
              despesas.impostos_previdencia_doacoes = rowSums(select(.,contains(id_despesas$impostos_previdencia_doacoes)), na.rm = T),
              despesas.imoveis_investimentos = rowSums(select(.,contains(id_despesas$imoveis_investimentos)), na.rm = T),
              despesas.empresimos = rowSums(select(.,contains(id_despesas$empresimos)), na.rm = T),
              despesas.var_ativo_passivo = rowSums(select(.,contains(id_despesas$var_ativo_passivo)), na.rm = T),
              despesas.totais = rowSums(select(.,contains(as.character(unlist(id_despesas)))), na.rm = T),
              
              despesas.totais.nao.monetaria = rowSums(select(.,contains('nm')), na.rm = T),
              
              
              # Para o método de Rothbarth
              despesas.vestuario_infantil = rowSums(select(.,contains(id_vestuario_infantil)), na.rm = T),
              
              despesas.vestuario_homem.mulher = rowSums(select(.,contains(id_vestuario_homem.mulher)), na.rm = T),
              despesas.vestuario_adulto = ifelse((despesas.vestuario - despesas.vestuario_infantil < 0),
                                                 0, (despesas.vestuario - despesas.vestuario_infantil)),
              despesas.bebidas_alcoolicas = rowSums(select(.,contains(id_bebidas_alcoolicas)), na.rm = T),
              despesas.jogos_apostas = rowSums(select(.,contains(id_jogos_apostas)), na.rm = T),
              
              receitas.totais = rowSums(select(.,contains(id_receitas)), na.rm = T),
              
              cod_uf = cod_uf,
              num_seq = num_seq,
              num_dv = num_dv,
              cod_domc = cod_domc,
              id_dom = id_dom,
              urbano = ifelse(urbano == 0, 'Rural', 'Urbano'),
              urbano = factor(urbano),
              
              # Código regional utilizado pelo IBGE = Primeiro número do código estadual
              code_region = substr(cod_uf, start = 1, stop = 1),
              code_region = factor(code_region),
              
              # Estratificação Social (renda per capita)
              classe.social = cut(receitas.totais,
                                  quantile(receitas.totais,
                                           probs = seq(0,1,0.2),
                                           na.rm = T),
                                  labels = c('Primeiro Quinto (Renda per capita)',
                                             'Segundo Quinto (Renda per capita)',
                                             'Terceiro Quinto (Renda per capita)',
                                             'Quarto Quinto (Renda per capita)',
                                             'Quinto Quinto (Renda per capita)'),
                                  include.lowest = T),
              
              qtd_morador_domc = qtd_morador_domc,
              renda_bruta_monetaria = renda_bruta_monetaria,
              renda_bruta_nao_monetaria = renda_bruta_nao_monetaria,
              renda_total = renda_total) %>% 
    mutate(across(.cols = contains('despesas.'), ~ . / 12, 
                  .names = '{.col}.mensal')) %>% 
    mutate(across(.cols = contains('receitas.'), ~ . / 12, 
                  .names = '{.col}.mensal')) %>% 
    mutate(across(.cols = contains('.mensal'), ~ . / qtd_morador_domc, 
                  .names = '{.col}.per_capita')) -> consumo.agg
  
  
  # CONSUMO AGREGADO POR DOMICÍLIO
  merge(individuos.agg, consumo.agg)
  
}


