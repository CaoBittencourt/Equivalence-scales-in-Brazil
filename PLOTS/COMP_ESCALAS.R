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


# 3. MANIPULAÇÃO DE DADOS PARA PLOTAGEM -----------------------------------
scales_norm %>% 
  pivot_longer(
    cols = contains('AA')
    , names_to = 'family.type'
    , values_to = 'scale'
  ) %>% select(-A) %>%
  replace(is.na(.), 0) -> scales_norm

child_cost %>%
  pivot_longer(
    cols = contains('AA')
    , names_to = 'family.type'
    , values_to = 'child.cost'
  )  %>% replace(is.na(.), 0) -> child_cost


# 4. VISUALIZAÇÃO (ESCALAS) -----------------------------------------------
theme_set(ggthemes::theme_hc(base_size = 25))

scales_norm %>% 
  mutate(
    Escala = fct_reorder(
      Escala, scale, max
    )
    , highlight = str_detect(Escala, 'confec')
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
    , legend.key.size = unit(2,'line')
    , axis.title.x = element_blank()
    , axis.title.y = element_blank()
  )

ggsave(
  filename = 'dsdsds.png'
  , height = 19*3 #, height = 7.66*3
  # , height = 8.5*3 #, height = 7.66*3
  , width = 15*3
  , units = 'cm'
)



# 3. VISUALIZAÇÃO (CHILD COST) -----------------------------------------------
child_cost %>% 
  mutate(
    Escala = fct_reorder(
      Escala, child.cost, min
    )
    , highlight = str_detect(Escala, 'confec')
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
    , legend.key.size = unit(2,'line')
    , axis.title.x = element_blank()
    , axis.title.y = element_blank()
  )



