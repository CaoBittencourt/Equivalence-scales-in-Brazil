# 1. PACOTES --------------------------------------------------------------
pkg <- c(
  'readr'
  , 'ggthemes'
  , 'gghighlight'
  , 'tidyverse'
)

lapply(pkg, function(x)
  if(!require(x, character.only = T))
  {install.packages(x); require(x)})

# lapply(pkg, function(x)
#   {citation(package = x)})

# 2. DADOS -------------------------------------------------------------------
# Escalas nacionais e internacionais normalizadas para AA <=> (2,0)
readr::read_csv(
  url('https://docs.google.com/spreadsheets/d/e/2PACX-1vRsrF8OaQGKav6PMpUKtnxMYJheKdti3WDkCql92BpmaXepFUUsYqmIuKuMO5h6gumDo8EyDDJWNs4o/pub?gid=0&single=true&output=csv')
) -> scales_norm

# Custos por criança das escalas nacionais e internacionais
readr::read_csv(
  url('https://docs.google.com/spreadsheets/d/e/2PACX-1vRsrF8OaQGKav6PMpUKtnxMYJheKdti3WDkCql92BpmaXepFUUsYqmIuKuMO5h6gumDo8EyDDJWNs4o/pub?gid=1481008835&single=true&output=csv')
) -> child_cost

# Diretório de trabalho
setwd('C:/Users/Sony/Documents/GitHub/TCC/PLOTS')

# 3. MANIPULAÇÃO DE DADOS PARA PLOTAGEM -----------------------------------
scales_norm %>% 
  mutate(
    scale.rank = rank(
      AACCC
      , ties.method = 'max'
      , na.last = F
      )
  ) -> scales_norm

scales_norm %>% 
  pivot_longer(
    cols = contains('AA')
    , names_to = 'family.type'
    , values_to = 'scale'
  ) %>% select(-A) -> scales_norm.long

child_cost %>%
  mutate(
    child.cost.rank = rank(
      AACCC
      , ties.method = 'max'
      , na.last = F
    )
  ) -> child_cost

child_cost %>%
  pivot_longer(
    cols = contains('AA')
    , names_to = 'family.type'
    , values_to = 'child.cost'
  ) -> child_cost.long


# 4. VISUALIZAÇÃO (ESCALAS) -----------------------------------------------
theme_set(ggthemes::theme_hc(base_size = 24))

scales_norm.long %>% 
  mutate(
    Escala = fct_reorder(
      Escala, scale.rank, max
    )
    , highlight = str_detect(Escala, 'Confec')
  ) %>% 
  ggplot(
    aes(
      x = Escala
      , y = scale
      , fill = Região
    )) +
  geom_col(size = 1.25, color = 'white') +
  facet_wrap(
    facets = vars(family.type)
    , nrow = 1
  ) +
  gghighlight(
    highlight
    , calculate_per_facet = T
    , unhighlighted_params = list(
      fill = NULL, color = NULL, alpha = 0.25)
  ) +
  # ggthemes::scale_fill_wsj() +
  ggthemes::scale_fill_gdocs() +
  coord_flip() +
  theme(
    legend.title = element_blank()
    , legend.key.size = unit(1.5,'line')
    , axis.title.x = element_blank()
    , axis.title.y = element_blank()
  )

ggsave(
  filename = 'scales_comp.png'
  , height = 19*3 #, height = 7.66*3
  # , height = 8.5*3 #, height = 7.66*3
  , width = 15*3
  , units = 'cm'
)


# 5. VISUALIZAÇÃO (CHILD COST) -----------------------------------------------
child_cost.long %>% 
  mutate(
    Escala = fct_reorder(
      Escala, child.cost.rank, max
    )
    , highlight = str_detect(Escala, 'Confec')
  ) %>% 
  ggplot(
    aes(
      x = Escala
      , y = child.cost
      , fill = Região
    )) +
  geom_col(size = 1.25, color = 'white') +
  facet_wrap(
    facets = vars(family.type)
    , nrow = 1
  ) +
  gghighlight(
    highlight
    , calculate_per_facet = T
    , unhighlighted_params = list(
      fill = NULL, color = NULL, alpha = 0.25)
  ) +
  # ggthemes::scale_fill_wsj() +
  ggthemes::scale_fill_gdocs() +
  coord_flip() +
  theme(
    legend.title = element_blank()
    , legend.key.size = unit(1.5,'line')
    , axis.title.x = element_blank()
    , axis.title.y = element_blank()
  )

ggsave(
  filename = 'child.cost_comp.png'
  , height = 19*3 #, height = 7.66*3
  # , height = 8.5*3 #, height = 7.66*3
  , width = 15*3
  , units = 'cm'
)



# 6. CÁLCULOS ADICIONAIS --------------------------------------------------
# child_cost %>% 
#   group_by(family.type) %>%
#   summarise(across(
#     .cols = child.cost
#     ,.fns = list(
#       'mean' = function(x){mean(x, na.rm = T)}
#       , 'sd' = function(x){sd(x, na.rm = T)}
#       ) 
#   )) %>% View()
# 
scales_norm %>% 
  group_by(Método) %>% 
  tally(.) %>% 
  arrange(desc(n))

scales_norm %>% 
  mutate(Top10 = scale.rank >= max(scale.rank) - 10) %>%
  filter(
    Método == 'Engel'
    , !is.na(AACCC)
    ) %>% 
  select(Top10)
  


