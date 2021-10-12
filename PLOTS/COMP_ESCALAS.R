# 1. PACOTES --------------------------------------------------------------
c(
  'gghighlight', 'ggridges', 'ggthemes', 'viridis', 'patchwork', 'scales', #Visualização de dados
  'plyr', 'tidyverse' #Manipulação de dados
) -> pkg

lapply(pkg, function(x)
  if(!require(x, character.only = T))
  {install.packages(x); require(x)})

# Citação dos pacotes
# lapply(pkg, function(x)
#   {citation(package = x)})

# 2. DADOS (E AJUSTES RÁPIDOS) ----------------------------------------------------------------
# Links
list(
  'eq_scales.inter' = 'https://docs.google.com/spreadsheets/d/e/2PACX-1vRsrF8OaQGKav6PMpUKtnxMYJheKdti3WDkCql92BpmaXepFUUsYqmIuKuMO5h6gumDo8EyDDJWNs4o/pub?gid=0&single=true&output=csv'
  # , 'eq_scales.brasil' = 'https://docs.google.com/spreadsheets/d/e/2PACX-1vRsrF8OaQGKav6PMpUKtnxMYJheKdti3WDkCql92BpmaXepFUUsYqmIuKuMO5h6gumDo8EyDDJWNs4o/pub?gid=1272641510&single=true&output=csv'
) -> url.eq_scales

list(
  'child_cost.inter' = 'https://docs.google.com/spreadsheets/d/e/2PACX-1vRsrF8OaQGKav6PMpUKtnxMYJheKdti3WDkCql92BpmaXepFUUsYqmIuKuMO5h6gumDo8EyDDJWNs4o/pub?gid=2118253380&single=true&output=csv'
  # , 'child_cost.brasil' =  
) -> url.child_cost

# Data frames
lapply(
  url.eq_scales, 
  function(link){
    read.csv(
      url(link), 
      encoding = 'UTF-8',
      na.strings = 'NA',
      header = T, 
      sep = ','
    ) %>% as_tibble(.) %>% 
      mutate(
        across(.cols = 4:ncol(.), ~ str_replace(.x,',','.')),
        across(.cols = 4:ncol(.), ~ as.numeric(.x))
      )
  }
) -> list.eq_scales

lapply(
  url.child_cost, 
  function(link){
    read.csv(
      url(link), 
      encoding = 'UTF-8',
      na.strings = 'NA',
      header = T, 
      sep = ','
    ) %>% as_tibble(.) %>% 
      mutate(
        across(.cols = 4:ncol(.), ~ str_replace(.x,',','.')),
        across(.cols = 4:ncol(.), ~ as.numeric(.x))
      )
  }
) -> list.child_cost


# 3. TEMAS ----------------------------------------------------------------
theme_set(theme_solarized_2()) 

# 4. PLOT ESCALAS DE EQUIVALÊNCIA -----------------------------------------
# Internacionais

# Nacionais

# Comparação de ambas





# 5. PLOT CUSTO DE CRIANÇA ------------------------------------------------
