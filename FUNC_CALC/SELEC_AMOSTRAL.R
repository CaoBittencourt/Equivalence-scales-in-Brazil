# 1. PACOTES --------------------------------------------------------------
# Pacotes
pkg <- c('plyr', 'glue', 'tidyverse') #Leitura e Manipulação de Dados

lapply(pkg, function(x)
  if(!require(x, character.only = T))
  {install.packages(x); require(x)})

# Citação dos pacotes
# lapply(pkg, function(x)
#   {citation(package = x)})

# 2. FUNÇÃO DE SELEÇÃO AMOSTRAL -------------------------------------------
sample.selection <- function(
  df, 
  # regioes = levels(df$regiao),
  # UF = levels(df$UF_sigla),
  # rural_urbano = levels(df$urbano),
  
  qtd_morador = 'qtd_morador_domc',
  
  incluir_solteiros_sem.filhos = T,
  incluir_solteiros_com.filhos = F,
  
  max_moradores = 7,
  max_conjuge = 1,
  max_filhos = 4,
  max_outros.parentes = 3,
  max_agregados = 3, 
  max_pensionistas = 0,
  max_empregados = 0, 
  max_parentes.empregados = 0
){
  
  df %>%
    filter(
      # regiao %in% regioes,
      # UF_sigla %in% UF,
      # urbano %in% rural_urbano,
      
      !!sym(qtd_morador) <= max_moradores,
      
      qtd_conjuge <= max_conjuge,
      qtd_filhos <= max_filhos,
      qtd_outros.parentes <= max_outros.parentes,
      qtd_agregados <= max_agregados,
      qtd_pensionistas <= max_pensionistas,
      qtd_empregados <= max_empregados,
      qtd_parentes.empregados <= max_parentes.empregados) -> sample
  
  
  if(!incluir_solteiros_sem.filhos){
    
    df %>% 
      filter(
        !(qtd_conjuge == 0 & qtd_filhos == 0)
      ) -> sample
    
  } 
  
  if(!incluir_solteiros_com.filhos){ 
    
    df %>% 
      filter(
        !(qtd_conjuge == 0 & qtd_filhos > 0)
      ) -> sample
    
  }
  
  if(!incluir_solteiros_sem.filhos & 
     !incluir_solteiros_com.filhos){ 
    
    df %>% 
      filter(
        !(qtd_conjuge == 0)
      ) -> sample
    
  }
  
  return(sample)
  
}
