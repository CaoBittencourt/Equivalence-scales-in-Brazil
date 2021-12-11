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
    , names_to = 'family_type'
    , values_to = 'scale'
  ) %>% select(-A) %>%
  replace(is.na(.), 0) -> scales_norm

# child_cost %>% 
#   pivot_longer(
#     cols = contains('AA')
#     , names_to = 'family_type'
#     , values_to = 'scale'
#   ) -> scales_norm


# 4. VISUALIZAÇÃO (ESCALAS) -----------------------------------------------
# theme_set(ggthemes::theme_hc(base_size = 25)) 
theme_set(ggthemes::theme_hc(base_size = 15)) 

scales_norm %>% 
  mutate(
    Escala = fct_reorder(
      Escala, scale, max, .desc = T
    )
    , highlight = str_detect(Escala, 'confec')
  ) %>% 
  ggplot(
    aes(
      x = family_type
      , y = scale
      , fill = Região
    )) + 
  geom_col() + 
  # geom_col() + 
  facet_wrap(facets = vars(Escala)) + 
  gghighlight(
    highlight
    , calculate_per_facet = T
    , unhighlighted_params = list(fill = NULL, alpha = 0.5)
    ) +
  ggthemes::scale_fill_wsj() +
  coord_flip() + 
  theme(
    legend.title = element_blank()
    , axis.title.x = element_blank()
    , axis.title.y = element_blank()
  )




# 3. VISUALIZAÇÃO (CHILD COST) -----------------------------------------------

























