# 1. PACOTES --------------------------------------------------------------
pkg <- c(
  'tidyverse' #Leitura e manipulação de dados
) 

lapply(pkg, function(x)
  if(!require(x, character.only = T))
  {install.packages(x); require(x)})

# lapply(pkg, function(x)
#   {citation(package = x)})

# 2. FUNÇÃO DE AJUSTE DE POF  ---------------------------------------------
# Ajuste de renda conforme escala de equivalência
pof.ajuste.escalas.ac_fun <- function(
  df.pof
  ,df.escalas.ac
){
  
  # Renda por adulto-equivalente
  # Diferencial entre a renda per-capita e renda por adulto-equivalente
  # Renda total equivalente ()
  
  # Despesas por adulto-equivalente
  # Diferencial entre a despesa per-capita e despesa por adulto-equivalente
  # Despesa total equivalente ()
  
  # CUIDADO: não deve-se misturar despesas equivalentes com renda equivalente!
  # Apenas deve-se ajustar ou renda, ou despesa! Do contrário o ajuste é feito 2x.
  
  
  # 1. TIPO DA FAMÍLIA
  df.pof %>%
    select(
      contains(c('] anos', '[')), #C
      contains(c('] anos', '(')) #A
    ) %>% names(.) -> old.names
  
  c('C', 'A') -> new.names
  
  names(df.pof)[match(old.names,
                      names(df.pof))] <- new.names
  
  df.pof %>% 
    mutate(
      A.str = strrep('A', A),
      C.str = strrep('C', C),
      family.type = paste0(A.str, C.str)
    ) %>% 
    select(!c(A.str, C.str))-> df.pof
  
  df.escalas.ac %>%
    select(equivalence.scale
           ,family.type
           ,family.ref
           ,child.cost) %>%
    full_join(df.pof) %>% 
    filter(
      # Configurações familiares para as quais não se calculou escalas de equivalência => NA
      !is.na(equivalence.scale)
      # Ajustes não podem ser realizados com base na família de referência
      & str_count(family.type) != str_count(first(family.ref))
      ) -> df.pof
  
  # 2. RENDA AJUSTADA
  df.pof %>%
    mutate(
      across(
        .cols = starts_with('renda')
        ,.fns = function(x){x*str_length(family.type)/(str_length(family.ref)*equivalence.scale)}
        ,.names = '{.col}_ajustada'
      )
    ) -> df.pof

  # 3. DESPESAS AJUSTADAS
  df.pof %>%
    mutate(
      across(
        .cols = starts_with('despesas.mensais')
        ,.fns = function(x){x*str_length(family.type)/(str_length(family.ref)*equivalence.scale)}
        ,.names = '{.col}_consumo.efetivo'
      )
    ) -> df.pof

  # 4. CUSTO DE VIDA AJUSTADO
  df.pof %>%
    group_by(classe_social, family.type) %>%
    mutate(
      across(
        .cols = c(starts_with('despesas.mensais'), -contains('consumo.efetivo'))
        ,.fns = function(x){median(x)*(str_length(family.ref)*equivalence.scale)/str_length(family.type)}
        ,.names = '{.col}_life.cost'
      )
    ) -> df.pof
  
  return(df.pof)
  
}
