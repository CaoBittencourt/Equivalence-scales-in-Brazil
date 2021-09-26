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
# 
# sample.selection <- function(df, regioes = levels(df$regiao),
#                              UF = levels(df$UF.sigla),
#                              rural.urbano = levels(df$urbano),
#                              max.moradores = 7, max.filhos = 4,
#                              max.agregados = 0, max.pensionistas = 0,
#                              max.empregados.dom = 0, max.fam.empregados.dom = 0,
#                              incluir.solteiros = F){
#   if(incluir.solteiros == T){
#     df %>% 
#       filter(regiao %in% regioes,
#              UF.sigla %in% UF,
#              urbano %in% rural.urbano,
#              qtd_morador_domc <= max.moradores,
#              num_filhos <= max.filhos,
#              num_agregados <= max.agregados,
#              num_pensionistas <= max.pensionistas,
#              num_empregados.dom <= max.empregados.dom,
#              num_parentes.empregados <= max.fam.empregados.dom) %>%
#       mutate(across(c(regiao, UF.sigla, UF.nome, urbano),
#                     .fns = factor))
#   }
#   else
#     df %>% 
#     filter(regiao %in% regioes,
#            UF.sigla %in% UF,
#            urbano %in% rural.urbano,
#            num_conjuge == 1,
#            qtd_morador_domc <= max.moradores,
#            num_filhos <= max.filhos,
#            num_agregados <= max.agregados,
#            num_pensionistas <= max.pensionistas,
#            num_empregados.dom <= max.empregados.dom,
#            num_parentes.empregados <= max.fam.empregados.dom) %>%
#     mutate(across(c(regiao, UF.sigla, UF.nome, urbano),
#                   .fns = factor))
# }
# 
# 
