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

# CUIDADO! 
# AS VARIÁVEIS SÃO SEPARADAS EM TRÊS TIPOS: TOTAL(VA), A PRAZO (CR) E NÃO MONETÁRIA (NM)
# AS VARIÁVEIS À VISTA NÃO SÃO CONTABILIZADAS, MAS PODERIAM SER OBTIDAS PELA DIFERENÇA VA - (CR + NM)
# EXISTEM ERROS CONTÁBEIS MUITO PEQUENOS (EM CENTAVOS), DE MODO QUE NEM SEMPRE VA = CR + NM
# UM TESTE INGÊNUO DO TIPO VA > CR + NM SERIA VÁLIDO APENAS 50% DAS VEZES (APROXIMADAMENTE)
# MAS, TENDO EM VISTA QUE TRATA-SE DE UM ERRO CONTÁBIL PEQUENO, O TESTE APROPRIADO SERIA
# round(VA,0) > round(CR + NM,0), QUE É VÁLIDO 100% DAS VEZES
# PORTANTO, DE FATO VA = CR + NM E PODE-SE UTILIZAR VA COMO MEDIDA AGREGADA


# Identificadores das categorias (despesas e receitas) são os mesmos em todas as POFs
# Não é necessário agregar as receitas (as variáveis renda e renda_total já são agregações mensais)
id_receitas.agg <- list(receitas = c('vare', 'vre', 'vvp'))

# Categorias gerais
list(alimentacao = 'vada',
     moradia = 'vadd02',
     vestuario = 'vadd03',
     transporte = 'vadd04',
     higiene = 'vadd05',
     saude = c('vadd06', 'vadd6'),
     educacao = 'vadd07',
     lazer = 'vadd08',
     fumo = 'vadd09',
     servicos.pessoais = 'vadd10',
     despesas.diversas = 'vadd11',
     impostos_previdencia_doacoes = 'vadd12',
     imoveis_investimentos = 'vadd13',
     emprestimos = 'vadd14') -> id_despesas.agg #%>% 
# lapply(function(x){
#   
#   c(paste0('va',x),
#     paste0('nm',x),
#     paste0('cr',x))
#   
# }) -> id_despesas.agg

# Identidade das despesas para agregação (categorias específicas utilizadas em alguns métodos de estimação) 
list(
  takein.food = c('vada01', 'vada02', 'vada03', 'vada04',
                  'vada05', 'vada06', 'vada07', 'vada08',
                  'vada09', 'vada10', 'vada11', 'vada12',
                  'vada13', 'vada14', 'vada15', 'vada16'),
  takeout.food = c('vada21', 'vada22', 'vada23', 'vada24',
                   'vada25', 'vada28', 'vada29'),
  alimentacao_menos.bebidas.alcoolicas = c('vada01', 'vada02', 'vada03', 'vada04',
                                           'vada05', 'vada06', 'vada07', 'vada08',
                                           'vada09', 'vada10', 'vada11', 'vada12',
                                           'vada13', 'vada14', 'vada15', 'vada16',
                                           'vada21', 'vada22', 'vada23', 'vada24',
                                           'vada25', 'vada27', 'vada28', 'vada29'),
  bebidas.alcoolicas = 'vada26',
  vestuario.infantil = 'vadd033',
  vestuario.homem_mulher = c('vadd031', 'vadd032'),
  joias = 'vadd035',
  transporte.proprio_proxy = c('vadd042', 'vadd043',
                               'vadd044', 'vadd045'),
  transporte.proprio_proxy_apenas.combustivel = c('vadd042', 'vadd043'),
  viagens = 'vadd046',
  perfume = 'vadd051',
  ensino.superior = 'vadd072',
  artigos.escolares = 'vadd075',
  lazer_adulto = c('vadd082', 'vadd083', 'vadd084', 'vadd085'),
  brinquedos_jogos = 'vadd081',
  manicure_pedicure = 'vadd102',
  jogos_apostas = 'vadd111',
  imoveis.aquisicao = 'vadd131',
  imoveis.reforma = 'vadd132',
  imoveis.prestacao = 'vadd142'
) -> id_despesas.especificas #%>% 
# lapply(function(x){
#   
#   c(paste0('va',x),
#     paste0('nm',x),
#     paste0('cr',x))
#   
# }) -> id_despesas.especificas

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


