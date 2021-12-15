# 1. PACOTES --------------------------------------------------------------
pkg <- c(
  'rio' #Importação e exportação de dados
  , 'purrr', 'plyr', 'glue', 'tidyverse' #Leitura e manipulação de dados
) 

lapply(pkg, function(x)
  if(!require(x, character.only = T))
  {install.packages(x); require(x)})

# lapply(pkg, function(x)
#   {citation(package = x)})

# Confecção própria
# source('C:/Users/Sony/Documents/GitHub/TCC/CALC/DADOS.R', encoding = 'UTF-8')


# 2. DADOS INICIAIS --------------------
pof2002 <- lista.pof2002_ss$pof_ac_2002_ss
pof2008 <- lista.pof2008_ss$pof_ac_2008_ss


# 3. ESTATÍSTICAS DESCRITIVAS (DESPESAS) ------------------------------------------------
# POF 2002 (sem sexo)
pof2002 %>%
  filter(
    `(14,110] anos` > 0
  ) %>%
  summarise(
    across(
      .cols = c(
        starts_with('despesas')
        & -contains('capita')
        & -contains('proxy')
        & -contains('_adulto')
        & -contains('infantil')
        & -contains('menos')
        & matches(
          paste0(
            paste0(names(id_despesas.agg),collapse = '|')
            , paste0(names(id_despesas.especificas),collapse = '|')
            , paste0('|totais')
          )
        )
      )
      ,.fns = list(
        'min' = min
        , 'mean' = mean
        , 'max' = max
        , 'sd' = sd)
    )
  ) %>% pivot_longer(cols = everything()) %>% 
  mutate(value = round(value, 2)) %>% View()


# POF 2008 (sem sexo)
pof2008 %>%
  filter(
    `(14,104] anos` > 0
  ) %>%
  summarise(
    across(
      .cols = c(
        starts_with('despesas')
        & -contains('capita')
        & -contains('proxy')
        & -contains('_adulto')
        & -contains('infantil')
        & -contains('menos')
        & matches(
          paste0(
            paste0(names(id_despesas.agg),collapse = '|')
            , paste0(names(id_despesas.especificas),collapse = '|')
            , paste0('|totais')
          )
        )
      )
      ,.fns = list(
        'min' = min
        , 'mean' = mean
        , 'max' = max
        , 'sd' = sd)
    )
  ) %>% pivot_longer(cols = everything()) %>% 
  mutate(value = round(value, 2)) %>% View()


# 3. ESTATÍSTICAS DESCRITIVAS (PARTICIPAÇÃO ORÇAMENTÁRIA) ------------------------------------------------
# POF 2002 (sem sexo)
pof2002 %>%
  filter(
    `(14,110] anos` > 0
  ) %>%
  summarise(
    across(
      .cols = c(
        starts_with('share')
        & -contains('capita')
        & -contains('proxy')
        & -contains('_adulto')
        & -contains('infantil')
        & -contains('menos')
        & matches(
          paste0(
            paste0(names(id_despesas.agg),collapse = '|')
            , paste0(names(id_despesas.especificas),collapse = '|')
            , paste0('|totais')
          )
        )
      )
      ,.fns = list(
        'min' = min
        , 'mean' = mean
        , 'max' = max
        , 'sd' = sd)
    )
  ) %>% pivot_longer(cols = everything()) %>% 
  mutate(value = round(value, 2)) %>% View()


# POF 2008 (sem sexo)
pof2008 %>%
  filter(
    `(14,104] anos` > 0
  ) %>%
  summarise(
    across(
      .cols = c(
        starts_with('share')
        & -contains('capita')
        & -contains('proxy')
        & -contains('_adulto')
        & -contains('infantil')
        & -contains('menos')
        & matches(
          paste0(
            paste0(names(id_despesas.agg),collapse = '|')
            , paste0(names(id_despesas.especificas),collapse = '|')
            , paste0('|totais')
          )
        )
      )
      ,.fns = list(
        'min' = min
        , 'mean' = mean
        , 'max' = max
        , 'sd' = sd)
    )
  ) %>% pivot_longer(cols = everything()) %>% 
  mutate(value = round(value, 2)) %>% View()


# 5. ESTATÍSTICAS DESCRITIVAS (DOMICÍLIOS) ------------------------------------------------
# POF 2002 (sem sexo)
pof2002 %>% 
  filter(
    `(14,110] anos` > 0
  ) %>%
  summarise(
    across(
      .cols = c(contains('morador')
                ,contains('] anos')
                ,contains('renda') & contains('per.capita')
      )
      ,.fns = list(
        'min' = min
        , 'mean' = mean
        , 'max' = max
        , 'sd' = sd)
    )
  ) %>% pivot_longer(cols = everything())

# POF 2008 (sem sexo)
pof2008 %>%
  filter(
    `(14,104] anos` > 0
  ) %>%
  summarise(
    across(
      .cols = c(contains('morador')
                ,contains('] anos')
                ,contains('renda') & contains('per.capita')
      )
      ,.fns = list(
        'min' = min
        , 'mean' = mean
        , 'max' = max
        , 'sd' = sd)
    )
  ) %>% pivot_longer(cols = everything())

