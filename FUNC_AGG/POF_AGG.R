# 1. PACOTES --------------------------------------------------------------
pkg <- c('plyr', 'glue', 'tidyverse')

lapply(pkg, function(x)
  if(!require(x, character.only = T))
  {install.packages(x); require(x)})

# Citação dos pacotes
# lapply(pkg, function(x)
#   {citation(package = x)})

select <- dplyr::select
mutate <- dplyr::mutate
recode <- dplyr::recode
summarise <- dplyr::summarise


# 2. LISTAS DE DESPESAS (NOMES DAS VARIÁVEIS) ----------------------------------------
# Identificadores das categorias (despesas e receitas) são os mesmos em todas as POFs
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


# 3. FUNÇÃO DE AGREGAÇÃO DE DESPESAS ------------------------------------
pof.agg_fun <- function(
  consumo, 
  lista.id_receitas.agg = id_receitas.agg,
  lista.id_despesas.agg = id_despesas.agg,
  lista.id_despesas.especificas = id_despesas.especificas,
  lista.var.recode,
  qtd_moradores,
  renda,
  unid_fed
){
  
  # CONSUMO AGREGADO
  # Agregação das despesas específicas
  lapply(names(lista.id_despesas.especificas), function(id){
    
    consumo %>%
      mutate(!!sym(glue('despesas.mensais.{names(lista.id_despesas.especificas[id])}')) :=
               rowSums(across(.cols = contains(lista.id_despesas.especificas[[id]])),
                       na.rm = T)/12) ->> consumo
    
  }) %>% invisible(.)
  
  # Agregação das despesas gerais
  lapply(names(lista.id_despesas.agg), function(id){
    
    consumo %>%
      mutate(!!sym(glue('despesas.mensais.{names(lista.id_despesas.agg[id])}')) :=
               rowSums(across(.cols = contains(lista.id_despesas.agg[[id]])),
                       na.rm = T)/12) ->> consumo
    
  }) %>% invisible(.)
  
  # Despesas totais
  consumo %>%
    mutate(despesas.mensais.totais =
             rowSums(
               across(
                 .cols = matches(unlist(lista.id_despesas.agg) %>%
                                   paste(collapse = '|'))
               ), na.rm = T)/12) -> consumo
  
  # Participação orçamentária
  consumo %>% 
    mutate(
      across(
        .cols = contains('despesas.mensais'),
        ~ . / despesas.mensais.totais,
        .names = "share_{.col}"
      )
    ) -> consumo
  
  # Variáveis per capita
  # Obs: starts_with para não calcular per capita do share_
  consumo %>% 
    mutate(
      across(
        .cols = c(starts_with('despesas.mensais'), # Despesas per capita
                  !!sym(renda)), # Receita per capita
        ~ . / !!sym(qtd_moradores),
        .names = "{.col}_per.capita"
      )
    ) -> consumo
  
  # Ajustes finais  
  consumo %>%
    mutate(
      # Estratificação social conforme o IBGE (quintis de renda per capita)
      classe_social = cut(.data[[renda]],
                          quantile(.data[[renda]],
                                   probs = seq(0,1,0.2),
                                   na.rm = T),
                          labels = c('Primeiro Quinto (Renda per capita)',
                                     'Segundo Quinto (Renda per capita)',
                                     'Terceiro Quinto (Renda per capita)',
                                     'Quarto Quinto (Renda per capita)',
                                     'Quinto Quinto (Renda per capita)'),
                          include.lowest = T),
      
      UF_sigla = dplyr::recode(.data[[unid_fed]],
                               `11`	= 'RO', `12`	= 'AC', `13`	= 'AM', `14`	= 'RR', `15`	= 'PA', `16`	= 'AP', `17`	= 'TO', 
                               `21` = 'MA', `22` = 'PI', `23` = 'CE', `24` = 'RN', `25` = 'PB', `26` = 'PE', `27` = 'AL', `28` = 'SE', `29` = 'BA',
                               `31` = 'MG', `32` = 'ES', `33` = 'RJ', `35` = 'SP',
                               `41` = 'PR', `42` = 'SC', `43` = 'RS', 
                               `50` = 'MS', `51` = 'MT', `52` = 'GO', `53` = 'DF'),
      
      code_region = substr(.data[[unid_fed]], start = 1, stop = 1), # Código da região (utilizado para plotagem)
      
      regiao = dplyr::recode(code_region,
                             `1` = 'Norte',
                             `2` = 'Nordeste',
                             `3` = 'Sudeste',
                             `4` = 'Sul',
                             `5` = 'Centro-Oeste')
      
      
    ) -> consumo
  
  # Remoção das variáveis desagregadas
  consumo %>% 
    select(-matches(unlist(lista.id_despesas.agg) %>% 
                      paste(collapse = '|')),
           -matches(unlist(lista.id_despesas.especificas) %>% 
                      paste(collapse = '|')),
           -matches(unlist(lista.id_receitas.agg) %>% 
                      paste(collapse = '|'))) %>% return(.)
}


