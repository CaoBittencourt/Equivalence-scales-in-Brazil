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

# source('C:/Users/Sony/Documents/GitHub/TCC/CALC/CALC_ENGEL.R', encoding = 'UTF-8')
# source('C:/Users/Sony/Documents/GitHub/TCC/CALC/CALC_ROTHBARTH.R', encoding = 'UTF-8')



# 2. DADOS INICIAIS VS AMOSTRA PARA CÁLCULO DE ESCALAS --------------------



# 3. ESTATÍSTICAS DESCRITIVAS (DESPESAS) ------------------------------------------------
# POF 2002 (sem sexo)
lapply(
  lista.pof2002_ss
  , function(pof){
    
    pof %>%
      select(
        starts_with('despesas')
        & -contains('capita')
        & -contains('proxy')
        & -contains('homem_mulher')
        & -contains('infantil')
        & -contains('menos')
        & matches(paste0(names(id_despesas.agg),collapse = '|')) 
      ) %>%
      summary(.) 
    
  })

# POF 2008 (sem sexo)
lapply(
  lista.pof2008_ss
  , function(pof){
    
    pof %>%
      select(
        starts_with('despesas')
        & -contains('capita')
        & -contains('proxy')
        & -contains('homem_mulher')
        & -contains('infantil')
        & -contains('menos')
        & matches(paste0(names(id_despesas.agg),collapse = '|')) 
      ) %>%
      summary(.) 
    
  })


# 4. ESTATÍSTICAS DESCRITIVAS (PARTICIPAÇÃO ORÇAMENTÁRIA) ------------------------------------------------
# POF 2002 (sem sexo)
lapply(
  lista.pof2002_ss
  , function(pof){
    
    pof %>%
      select(
        starts_with('share')
        & -contains('capita')
        & -contains('proxy')
        & -contains('homem_mulher')
        & -contains('infantil')
        & -contains('menos')
        & matches(paste0(names(id_despesas.agg),collapse = '|')) 
      ) %>%
      summary(.) 
    
  })

# POF 2008 (sem sexo)
lapply(
  lista.pof2008_ss
  , function(pof){
    
    pof %>%
      select(
        starts_with('share')
        & -contains('capita')
        & -contains('proxy')
        & -contains('homem_mulher')
        & -contains('infantil')
        & -contains('menos')
        & matches(paste0(names(id_despesas.agg),collapse = '|')) 
      ) %>%
      summary(.) 
    
  })


# 5. ESTATÍSTICAS DESCRITIVAS (DOMICÍLIOS) ------------------------------------------------
# POF 2002 (sem sexo)
lapply(
  lista.pof2002_ss.rothbarth.sample
  , function(pof){
    
    pof %>%
      select(
        contains('morador')
        ,contains('] anos')
        ,contains('renda') & contains('per.capita')
      ) %>%
      summary(.) 
    
  })


lapply(
  lista.pof2002_ss.rothbarth.sample
  , function(pof){
    
    Map(
      function(var){
        
        pof %>%
          select(
            contains(var)
          ) %>% sd(.) 
        
      }
      , var = c('morador','] anos','renda')
    )
    
  })

lista.pof2002_ss$pof_ac_2002_ss %>% select(contains('renda')) %>% pull(.) %>% sd(.)


lapply(
  lista.pof2002_ss
  , function(pof){
    
    Map(
      function(var){
        
        pof %>%
          group_by(
            !!sym(var)
          ) %>%
          tally(.) %>%
          mutate(
            prc = n/sum(n)
          )
        
      }
      , var = c('regiao','urbano','classe_social')
    )
    
  })


# POF 2008 (sem sexo)
lapply(
  lista.pof2008_ss
  , function(pof){
    
    pof %>%
      select(
        contains('morador')
        ,contains('] anos')
      ) %>%
      summary(.) 
    
    Map(
      function(var){
        
        pof %>%
          group_by(
            !!sym(var)
          ) %>%
          tally(.) %>%
          mutate(
            prc = n/sum(n)
          )
        
      }
      , var = c('regiao','urbano','classe_social')
    )
  })


