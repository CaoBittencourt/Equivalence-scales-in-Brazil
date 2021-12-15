# 1. PACOTES --------------------------------------------------------------
pkg <- c(
  'rio' #Importação e exportação de dados
  # , 'ggfittext' #Visualização de dados
  , 'purrr', 'plyr', 'glue', 'tidyverse' #Leitura e manipulação de dados
) 

lapply(pkg, function(x)
  if(!require(x, character.only = T))
  {install.packages(x); require(x)})

# lapply(pkg, function(x)
#   {citation(package = x)})

source('C:/Users/Sony/Documents/GitHub/TCC/CALC/CALC_ENGEL.R', encoding = 'UTF-8')
source('C:/Users/Sony/Documents/GitHub/TCC/CALC/CALC_ROTHBARTH.R', encoding = 'UTF-8')
source('C:/Users/Sony/Documents/GitHub/TCC/FUNC_CALC/ESCALAS_AJUSTE_POF.R', encoding = 'UTF-8')

setwd('C:/Users/Sony/Documents/GitHub/TCC/PLOTS')

# 2. ESCALAS PARA O AJUSTE ------------------------------------------------
# POF 2002 (sem sexo)
list(
  'Engel' = pof2002_ss.engel_scales
  , 'Rothbarth' = pof2002_ss.rothbarth_scales
) -> list.pof2002_ss_scales

# POF 2008 (sem sexo)
list(
  'Engel' = pof2008_ss.engel_scales
  , 'Rothbarth' = pof2008_ss.rothbarth_scales
) -> list.pof2008_ss_scales

# 3. AJUSTE DAS POFS ------------------------------------------------------
# POF 2002 (sem sexo)
Map(
  function(escalas, pof){
    
    pof.ajuste.escalas.ac_fun(
      df.pof = pof
      , df.escalas.ac = escalas
    )
    
  }
  , escalas = list.pof2002_ss_scales
  , pof = lista.pof2002_ss
  
) -> lista.pof2002_ss.ajustada

# POF 2008 (sem sexo)
Map(
  function(escalas, pof){
    
    pof.ajuste.escalas.ac_fun(
      df.pof = pof
      , df.escalas.ac = escalas
    )
    
  }
  , escalas = list.pof2008_ss_scales
  , pof = lista.pof2008_ss
) -> lista.pof2008_ss.ajustada


# 4. SUMÁRIO DOS AJUSTES POR CLASSE SOCIAL -----------------------------------
# POF 2002 (sem sexo)
lapply(
  lista.pof2002_ss.ajustada
  , function(pof.ajustada){
    
    pof.ajustada %>%
      group_by(classe_social, family.type) %>% 
      summarise(
        across(
          .cols = c(
            starts_with('renda')
            , starts_with('despesas.mensais')
            , -contains('monetar')
            # , contains('consumo.efetivo')
            # , contains('median.life.cost')
          )
          ,.fns = median
        )
      )
  }
) -> lista.pof2002_ss.ajustada.summarise

# POF 2008 (sem sexo)
lapply(
  lista.pof2008_ss.ajustada
  , function(pof.ajustada){
    
    pof.ajustada %>%
      group_by(classe_social, family.type) %>% 
      summarise(
        across(
          .cols = c(
            starts_with('renda')
            , starts_with('despesas.mensais')
            , -contains('monetar')
            # contains('ajustada')
            # , contains('consumo.efetivo')
            # , contains('median.life.cost')
          )
          ,.fns = median
        )
      )
  }
) -> lista.pof2008_ss.ajustada.summarise

# 5. SUMÁRIO DO SUMÁRIO DOS AJUSTES POR CLASSE SOCIAL (PARA PLOTAGEM) -----------------------------------
# POF 2002 (sem sexo)
lapply(
  lista.pof2002_ss.ajustada.summarise
  , function(df){
    df %>%
      select(
        classe_social
        , family.type
        , contains('renda')
        , contains('totais')
      ) %>% 
      pivot_longer(
        cols = -c(classe_social, family.type)
      ) %>% 
      mutate(
        
        name = str_to_title(name)
        , name = str_replace_all(name, '_',' ')
        , name = str_replace_all(name, '\\.',' ')
        , name = str_replace_all(name, 'ajustada','EQ')
        # Custo de vida equivalente (tomando por base a mediana do custo de vida)
        , name = str_replace_all(name, 'life cost','CTV EQ')
        # Consumo equivalente
        , name = str_replace_all(name, 'consumo efetivo','CSM EQ')
        , name = str_replace_all(name, 'per capita','PC')
        , name = str_remove_all(name, 'total')
        # Despesas totais medianas por quintil
        , name = str_remove_all(name, 'mensais ')
        , name = str_replace_all(name, 'Despesas totais','DTM')
        , name = str_replace_all(name, 'DTM CTV','CTV')
        , name = str_replace_all(name, 'DTM PC CTV','CTV PC')
        
        , classe_social = str_to_lower(classe_social)
        , classe_social = str_replace_all(classe_social, 'per capita','PC')
        , classe_social = str_replace_all(classe_social, 'renda','Renda')
        , classe_social = str_replace_all(classe_social, ' quinto','Q')
        , classe_social = str_replace_all(classe_social, 'primeiro','1')
        , classe_social = str_replace_all(classe_social, 'segundo','2')
        , classe_social = str_replace_all(classe_social, 'terceiro','3')
        , classe_social = str_replace_all(classe_social, 'quarto','4')
        , classe_social = str_replace_all(classe_social, 'quinto','5')
        , classe_social = str_replace_all(classe_social, 'per capita','PC')
        , classe_social = str_remove_all(classe_social, '\\(')
        , classe_social = str_remove_all(classe_social, '\\)')
        , classe_social = factor(classe_social)
        
      ) -> pof.ajustada
  }) -> lista.pof2002_ss.ajustada.summarise3

# POF 2008 (sem sexo)
lapply(
  lista.pof2008_ss.ajustada.summarise
  , function(df){
    df %>%
      select(
        classe_social
        , family.type
        , contains('renda')
        , contains('totais')
      ) %>% 
      pivot_longer(
        cols = -c(classe_social, family.type)
      ) %>% 
      mutate(
        
        name = str_to_title(name)
        , name = str_remove_all(name, '_total')
        , name = str_replace_all(name, '_',' ')
        , name = str_replace_all(name, '\\.',' ')
        , name = str_replace_all(name, 'ajustada','EQ')
        # Custo de vida equivalente (tomando por base a mediana do custo de vida)
        , name = str_replace_all(name, 'life cost','CTV EQ')
        # Consumo equivalente
        , name = str_replace_all(name, 'consumo efetivo','CSM EQ')
        , name = str_replace_all(name, 'per capita','PC')
        # Despesas totais medianas por quintil
        , name = str_remove_all(name, 'mensais ')
        , name = str_replace_all(name, 'Despesas totais','DTM')
        , name = str_replace_all(name, 'DTM CTV','CTV')
        , name = str_replace_all(name, 'DTM PC CTV','CTV PC')
        
        , classe_social = str_to_lower(classe_social)
        , classe_social = str_replace_all(classe_social, 'per capita','PC')
        , classe_social = str_replace_all(classe_social, 'renda','Renda')
        , classe_social = str_replace_all(classe_social, ' quinto','Q')
        , classe_social = str_replace_all(classe_social, 'primeiro','1')
        , classe_social = str_replace_all(classe_social, 'segundo','2')
        , classe_social = str_replace_all(classe_social, 'terceiro','3')
        , classe_social = str_replace_all(classe_social, 'quarto','4')
        , classe_social = str_replace_all(classe_social, 'quinto','5')
        , classe_social = str_replace_all(classe_social, 'per capita','PC')
        , classe_social = str_remove_all(classe_social, '\\(')
        , classe_social = str_remove_all(classe_social, '\\)')
        , classe_social = factor(classe_social)
        
      ) -> pof.ajustada
  }) -> lista.pof2008_ss.ajustada.summarise3


# # 6. SUMÁRIO DO SUMÁRIO DOS AJUSTES POR CLASSE SOCIAL (PARA APRESENTAR) -----------------------------------
# # POF 2002 (sem sexo)
# lapply(
#   lista.pof2002_ss.ajustada.summarise
#   , function(df){
#     df %>%
#       select(
#         classe_social
#         , family.type
#         , contains('renda')
#         , contains('totais')
#       ) %>% 
#       pivot_longer(
#         cols = -c(classe_social, family.type)
#       ) %>%
#       pivot_wider(
#         names_from = family.type
#         ,values_from = value
#       ) -> pof.ajustada
#   }) -> lista.pof2002_ss.ajustada.summarise2
# 
# # POF 2008 (sem sexo)
# lapply(
#   lista.pof2008_ss.ajustada.summarise
#   , function(df){
#     df %>%
#       select(
#         classe_social
#         , family.type
#         , contains('renda')
#         , contains('totais')
#       ) %>% 
#       pivot_longer(
#         cols = -c(classe_social, family.type)
#       ) %>%
#       pivot_wider(
#         names_from = family.type
#         ,values_from = value
#       ) -> pof.ajustada
#   }) -> lista.pof2008_ss.ajustada.summarise2

# 7. VISUALIZAÇÃO (RENDA) -------------------------------------------------------------------------
theme_set(ggthemes::theme_hc(base_size = 25)) 

# ENGEL POF 2002
lista.pof2002_ss.ajustada.summarise3$Engel %>%
  filter(str_detect(name,'Renda')) %>%
  mutate(
    name = fct_reorder(name, value, max)
    , value = round(value, 2)
  ) %>% 
  ggplot(
    aes(
      x = name
      , y = value
      , fill = classe_social
    )) + 
  geom_col(position = 'dodge') + 
  facet_wrap(facets = vars(family.type)) +
  # scale_x_discrete(
  #   labels = function(x){
  #     x %>% 
  #       str_replace(
  #         pattern = ' '
  #         , replacement = '\n'
  #       )
  #   }
  # ) + 
  scale_y_continuous(labels = scales::label_dollar(
    prefix = 'R$'
    , big.mark = '.'
    , decimal.mark = ','
  )) +
  viridis::scale_fill_viridis(
    discrete = T
    , option = 'D'
  ) + 
  theme(
    legend.title = element_blank()
    , axis.title.x = element_blank()
    , axis.title.y = element_blank()
  )

ggsave(
  filename = 'pof2002_renda_engel.png'
  , height = 8.25*3 #, height = 7.66*3
  , width = 15*3
  , units = 'cm'
)

# ENGEL POF 2008
lista.pof2008_ss.ajustada.summarise3$Engel %>%
  filter(str_detect(name,'Renda')) %>%
  mutate(
    name = fct_reorder(name, value, max)
    , value = round(value, 2)
  ) %>% 
  ggplot(
    aes(
      x = name
      , y = value
      , fill = classe_social
    )) + 
  geom_col(position = 'dodge') + 
  facet_wrap(facets = vars(family.type)) +
  # scale_x_discrete(
  #   labels = function(x){
  #     x %>% 
  #       str_replace(
  #         pattern = ' '
  #         , replacement = '\n'
  #       )
  #   }
  # ) + 
  scale_y_continuous(labels = scales::label_dollar(
    prefix = 'R$'
    , big.mark = '.'
    , decimal.mark = ','
  )) +
  viridis::scale_fill_viridis(
    discrete = T
    , option = 'D'
  ) + 
  theme(
    legend.title = element_blank()
    , axis.title.x = element_blank()
    , axis.title.y = element_blank()
  )

ggsave(
  filename = 'pof2008_renda_engel.png'
  , height = 8.25*3 #, height = 7.66*3
  , width = 15*3
  , units = 'cm'
)

# ROTHBARTH POF 2002
lista.pof2002_ss.ajustada.summarise3$Rothbarth %>%
  filter(str_detect(name,'Renda')) %>%
  mutate(
    name = fct_reorder(name, value, max)
    , value = round(value, 2)
  ) %>% 
  ggplot(
    aes(
      x = name
      , y = value
      , fill = classe_social
    )) + 
  geom_col(position = 'dodge') + 
  facet_wrap(facets = vars(family.type), nrow = 1) +
  scale_x_discrete(
    labels = function(x){
      x %>%
        str_replace(
          pattern = ' '
          , replacement = '\n'
        )
    }
  ) +
  scale_y_continuous(labels = scales::label_dollar(
    prefix = 'R$'
    , big.mark = '.'
    , decimal.mark = ','
  )) +
  viridis::scale_fill_viridis(
    discrete = T
    , option = 'D'
  ) + 
  theme(
    legend.title = element_blank()
    , axis.title.x = element_blank()
    , axis.title.y = element_blank()
  )

ggsave(
  filename = 'pof2002_renda_rothbarth.png'
  , height = 8.25*3 #, height = 7.66*3
  , width = 15*3
  , units = 'cm'
)

# ROTHBARTH POF 2008
lista.pof2008_ss.ajustada.summarise3$Rothbarth %>%
  filter(str_detect(name,'Renda')) %>%
  mutate(
    name = fct_reorder(name, value, max)
    , value = round(value, 2)
  ) %>% 
  ggplot(
    aes(
      x = name
      , y = value
      , fill = classe_social
    )) + 
  geom_col(position = 'dodge') + 
  facet_wrap(facets = vars(family.type), nrow = 1) +
  scale_x_discrete(
    labels = function(x){
      x %>%
        str_replace(
          pattern = ' '
          , replacement = '\n'
        )
    }
  ) +
  scale_y_continuous(labels = scales::label_dollar(
    prefix = 'R$'
    , big.mark = '.'
    , decimal.mark = ','
  )) +
  viridis::scale_fill_viridis(
    discrete = T
    , option = 'D'
  ) + 
  theme(
    legend.title = element_blank()
    , axis.title.x = element_blank()
    , axis.title.y = element_blank()
  )

ggsave(
  filename = 'pof2008_renda_rothbarth.png'
  , height = 8.25*3 #, height = 7.66*3
  , width = 15*3
  , units = 'cm'
)



# 8. VISUALIZAÇÃO (CONSUMO EFETIVO) -------------------------------------------------------------------------
theme_set(ggthemes::theme_hc(base_size = 25))

# ENGEL POF 2002
lista.pof2002_ss.ajustada.summarise3$Engel %>%
  filter(str_detect(name,'DTM'), !str_detect(name,'CTV')) %>%
  mutate(
    name = fct_reorder(name, value, max)
    , value = round(value, 2)
  ) %>% 
  ggplot(
    aes(
      x = name
      , y = value
      , fill = classe_social
    )) + 
  geom_col(position = 'dodge') + 
  facet_wrap(facets = vars(family.type)) +
  # scale_x_discrete(
  #   labels = function(x){
  #     x %>% 
  #       str_replace(
  #         pattern = '^(\\S+) (\\S+) '
  #         , replacement = '\\1 \\2\n'
  #       )
  #   }
  # ) + 
  scale_y_continuous(labels = scales::label_dollar(
    prefix = 'R$'
    , big.mark = '.'
    , decimal.mark = ','
  )) +
  viridis::scale_fill_viridis(
    discrete = T
    , option = 'D'
  ) + 
  theme(
    legend.title = element_blank()
    , axis.title.x = element_blank()
    , axis.title.y = element_blank()
  )

ggsave(
  filename = 'pof2002_consumo_engel.png'
  , height = 8.25*3 #, height = 7.66*3
  , width = 15*3
  , units = 'cm'
)

# ENGEL POF 2008
lista.pof2008_ss.ajustada.summarise3$Engel %>%
  filter(str_detect(name,'DTM'), !str_detect(name,'CTV')) %>%
  mutate(
    name = fct_reorder(name, value, max)
    , value = round(value, 2)
  ) %>% 
  ggplot(
    aes(
      x = name
      , y = value
      , fill = classe_social
    )) + 
  geom_col(position = 'dodge') + 
  facet_wrap(facets = vars(family.type)) +
  # scale_x_discrete(
  #   labels = function(x){
  #     x %>% 
  #       str_replace(
  #         pattern = '^(\\S+) (\\S+) '
  #         , replacement = '\\1 \\2\n'
  #       )
  #   }
  # ) + 
  scale_y_continuous(labels = scales::label_dollar(
    prefix = 'R$'
    , big.mark = '.'
    , decimal.mark = ','
  )) +
  viridis::scale_fill_viridis(
    discrete = T
    , option = 'D'
  ) + 
  theme(
    legend.title = element_blank()
    , axis.title.x = element_blank()
    , axis.title.y = element_blank()
  )

ggsave(
  filename = 'pof2008_consumo_engel.png'
  , height = 8.25*3 #, height = 7.66*3
  , width = 15*3
  , units = 'cm'
)

# ROTHBARTH POF 2002
lista.pof2002_ss.ajustada.summarise3$Rothbarth %>%
  filter(str_detect(name,'DTM'), !str_detect(name,'CTV')) %>%
  mutate(
    name = fct_reorder(name, value, max)
    , value = round(value, 2)
  ) %>% 
  ggplot(
    aes(
      x = name
      , y = value
      , fill = classe_social
    )) + 
  geom_col(position = 'dodge') + 
  facet_wrap(facets = vars(family.type), nrow = 1) +
  scale_x_discrete(
    labels = function(x){
      x %>%
        str_replace(
          pattern = '^(\\S+) (\\S+) '
          , replacement = '\\1 \\2\n'
        )
    }
  ) +
  scale_y_continuous(labels = scales::label_dollar(
    prefix = 'R$'
    , big.mark = '.'
    , decimal.mark = ','
  )) +
  viridis::scale_fill_viridis(
    discrete = T
    , option = 'D'
  ) + 
  ggthemes::theme_hc(base_size = 24) + 
  theme(
    legend.title = element_blank()
    , axis.title.x = element_blank()
    , axis.title.y = element_blank()
  )

ggsave(
  filename = 'pof2002_consumo_rothbarth.png'
  , height = 8.25*3 #, height = 7.66*3
  , width = 15*3
  , units = 'cm'
)


# ROTHBARTH POF 2008
lista.pof2008_ss.ajustada.summarise3$Rothbarth %>%
  filter(str_detect(name,'DTM'), !str_detect(name,'CTV')) %>%
  mutate(
    name = fct_reorder(name, value, max)
    , value = round(value, 2)
  ) %>% 
  ggplot(
    aes(
      x = name
      , y = value
      , fill = classe_social
    )) + 
  geom_col(position = 'dodge') + 
  facet_wrap(facets = vars(family.type), nrow = 1) +
  scale_x_discrete(
    labels = function(x){
      x %>%
        str_replace(
          pattern = '^(\\S+) (\\S+) '
          , replacement = '\\1 \\2\n'
        )
    }
  ) +
  scale_y_continuous(labels = scales::label_dollar(
    prefix = 'R$'
    , big.mark = '.'
    , decimal.mark = ','
  )) +
  viridis::scale_fill_viridis(
    discrete = T
    , option = 'D'
  ) + 
  ggthemes::theme_hc(base_size = 24) + 
  theme(
    legend.title = element_blank()
    , axis.title.x = element_blank()
    , axis.title.y = element_blank()
  )

ggsave(
  filename = 'pof2008_consumo_rothbarth.png'
  , height = 8.25*3 #, height = 7.66*3
  , width = 15*3
  , units = 'cm'
)


# 9. VISUALIZAÇÃO (CUSTO DE VIDA) -------------------------------------------------------------------------
theme_set(ggthemes::theme_hc(base_size = 25))

# ENGEL POF 2002
lista.pof2002_ss.ajustada.summarise3$Engel %>%
  filter(str_detect(name,'DTM|CTV'), !str_detect(name,'CSM')) %>%
  mutate(
    name = fct_reorder(name, value, max, .desc = T)
    , value = round(value, 2)
  ) %>% 
  ggplot(
    aes(
      x = name
      , y = value
      , fill = classe_social
    )) + 
  geom_col(position = 'dodge') + 
  facet_wrap(facets = vars(family.type)) +
  # scale_x_discrete(
  #   labels = function(x){
  #     x %>% 
  #       str_replace(
  #         pattern = '^(\\S+) (\\S+) '
  #         , replacement = '\\1 \\2\n'
  #       )
  #   }
  # ) + 
  scale_y_continuous(labels = scales::label_dollar(
    prefix = 'R$'
    , big.mark = '.'
    , decimal.mark = ','
  )) +
  viridis::scale_fill_viridis(
    discrete = T
    , option = 'D'
  ) + 
  theme(
    legend.title = element_blank()
    , axis.title.x = element_blank()
    , axis.title.y = element_blank()
  )

ggsave(
  filename = 'pof2002_custodevida_engel.png'
  , height = 8.25*3
  , width = 15*3
  , units = 'cm'
)

# ENGEL POF 2008
lista.pof2008_ss.ajustada.summarise3$Engel %>%
  filter(str_detect(name,'DTM|CTV'), !str_detect(name,'CSM')) %>%
  mutate(
    name = fct_reorder(name, value, max, .desc = T)
    , value = round(value, 2)
  ) %>% 
  ggplot(
    aes(
      x = name
      , y = value
      , fill = classe_social
    )) + 
  geom_col(position = 'dodge') + 
  facet_wrap(facets = vars(family.type)) +
  # scale_x_discrete(
  #   labels = function(x){
  #     x %>% 
  #       str_replace(
  #         pattern = '^(\\S+) (\\S+) '
  #         , replacement = '\\1 \\2\n'
  #       )
  #   }
  # ) + 
  scale_y_continuous(labels = scales::label_dollar(
    prefix = 'R$'
    , big.mark = '.'
    , decimal.mark = ','
  )) +
  viridis::scale_fill_viridis(
    discrete = T
    , option = 'D'
  ) + 
  theme(
    legend.title = element_blank()
    , axis.title.x = element_blank()
    , axis.title.y = element_blank()
  )

ggsave(
  filename = 'pof2008_custodevida_engel.png'
  , height = 8.25*3 #, height = 7.66*3
  , width = 15*3
  , units = 'cm'
)

# ROTHBARTH POF 2002
lista.pof2002_ss.ajustada.summarise3$Rothbarth %>%
  filter(str_detect(name,'DTM|CTV'), !str_detect(name,'CSM')) %>%
  mutate(
    name = fct_reorder(name, value, max, .desc = T)
    , value = round(value, 2)
  ) %>% 
  ggplot(
    aes(
      x = name
      , y = value
      , fill = classe_social
    )) + 
  geom_col(position = 'dodge') + 
  facet_wrap(facets = vars(family.type), nrow = 1) +
  scale_x_discrete(
    labels = function(x){
      x %>%
        str_replace(
          pattern = '^(\\S+) (\\S+) '
          , replacement = '\\1 \\2\n'
        )
    }
  ) +
  scale_y_continuous(labels = scales::label_dollar(
    prefix = 'R$'
    , big.mark = '.'
    , decimal.mark = ','
  )) +
  viridis::scale_fill_viridis(
    discrete = T
    , option = 'D'
  ) + 
  ggthemes::theme_hc(base_size = 24) + 
  theme(
    legend.title = element_blank()
    , axis.title.x = element_blank()
    , axis.title.y = element_blank()
  )

ggsave(
  filename = 'pof2002_custodevida_rothbarth.png'
  , height = 8.25*3 #, height = 7.66*3
  , width = 15*3
  , units = 'cm'
)


# ROTHBARTH POF 2008
lista.pof2008_ss.ajustada.summarise3$Rothbarth %>%
  filter(str_detect(name,'DTM|CTV'), !str_detect(name,'CSM')) %>%
  mutate(
    name = fct_reorder(name, value, max, .desc = T)
    , value = round(value, 2)
  ) %>% 
  ggplot(
    aes(
      x = name
      , y = value
      , fill = classe_social
    )) + 
  geom_col(position = 'dodge') + 
  facet_wrap(facets = vars(family.type), nrow = 1) +
  scale_x_discrete(
    labels = function(x){
      x %>%
        str_replace(
          pattern = '^(\\S+) (\\S+) '
          , replacement = '\\1 \\2\n'
        )
    }
  ) +
  scale_y_continuous(labels = scales::label_dollar(
    prefix = 'R$'
    , big.mark = '.'
    , decimal.mark = ','
  )) +
  viridis::scale_fill_viridis(
    discrete = T
    , option = 'D'
  ) + 
  ggthemes::theme_hc(base_size = 24) + 
  theme(
    legend.title = element_blank()
    , axis.title.x = element_blank()
    , axis.title.y = element_blank()
  )

ggsave(
  filename = 'pof2008_custodevida_rothbarth.png'
  , height = 8.25*3 #, height = 7.66*3
  , width = 15*3
  , units = 'cm'
)





# 10. CÁLCULOS ADICIONAIS --------------------------------------------------------------------
# lista.pof2002_ss.ajustada.summarise3$Engel %>%
#   pivot_wider(
#     id_cols = c(family.type, classe_social)
#     , names_from = name
#     , values_from = value
#   ) %>% 
#   group_by(family.type) %>% 
#   mutate(
#     Renda = 1 - (Renda/max(Renda))
#   )
# 
# lista.pof2002_ss.ajustada.summarise3$Engel %>%
#   pivot_wider(
#     id_cols = c(family.type, classe_social)
#     , names_from = name
#     , values_from = value
#   ) %>% 
#   filter(classe_social != '5Q Renda PC') %>% 
#   group_by(family.type) %>% 
#   summarise(
#     Renda = sum(Renda)
#   )
# 
# lista.pof2002_ss.ajustada.summarise3$Engel %>%
#   pivot_wider(
#     id_cols = c(family.type, classe_social)
#     , names_from = name
#     , values_from = value
#   ) %>% 
#   group_by(family.type) %>% 
#   transmute(
#     Renda.prc = Renda/`Renda EQ`
#     , ganho.prc = 1 - Renda.prc
#   )
# 
# lista.pof2008_ss.ajustada.summarise3$Engel %>%
#   filter(
#     str_detect(name,'DTM|CTV|Renda')
#     , !str_detect(name,'CSM')
#     , classe_social == '3Q Renda PC'
#     ) %>%
#   pivot_wider(
#     id_cols = c(family.type, classe_social)
#     , names_from = name
#     , values_from = value
#   ) %>% View()
# 
# lista.pof2008_ss.ajustada.summarise3$Rothbarth %>%
#   filter(
#     str_detect(name,'DTM|CTV|Renda')
#     , !str_detect(name,'CSM')
#     , classe_social == '4Q Renda PC'
#     ) %>%
#   pivot_wider(
#     id_cols = c(family.type, classe_social)
#     , names_from = name
#     , values_from = value
#   ) %>% View()
