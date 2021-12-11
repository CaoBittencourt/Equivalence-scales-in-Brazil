# 1. PACOTES --------------------------------------------------------------
# Pacotes
c(
  'openxlsx', 'rio', 'plyr', 'glue', 'broom', 'tidyverse', #Leitura e manipulação de dados
  'huxtable', 'stargazer', 'gtsummary' #Tabelas de regressão bonitas
) -> pkg

lapply(pkg, function(x)
  if(!require(x, character.only = T))
  {install.packages(x); require(x)})

# Citação dos pacotes
# lapply(pkg, function(x)
#   {citation(package = x)})

# Confecção própria
source('C:/Users/Sony/Documents/GitHub/TCC/CALC/DADOS.R', encoding = 'UTF-8')
source('C:/Users/Sony/Documents/GitHub/TCC/FUNC_CALC/SELEC_AMOSTRAL.R', encoding = 'UTF-8')
source('C:/Users/Sony/Documents/GitHub/TCC/FUNC_CALC/ENGEL_ROTHBARTH.R', encoding = 'UTF-8')

# 2. PESSOAS DE REFERÊNCIA ------------------------------------------------
# Pessoas de referência
ref_ac_2002_ss <- '`(14,110] anos`' 
# ref_normais_2002_ss <- '`(18,65] anos`'
# ref_vaz_2002_ss <- '`(14,110] anos`'
# ref_deaton_2002_ss <- '`(14,54] anos`'
# ref_lancaster_2002_ss <- '`(17,110] anos`'

# ref_ac_2002_cs <- '`Homens_(14,110]`'
# ref_normais_2002_cs <- '`Homens_(18,65]`'
# ref_vaz_2002_cs <- '`Homens_(14,110]`'
# ref_deaton_2002_cs <- '`Homens_(14,54]`'

ref_ac_2008_ss <- '`(14,104] anos`' 
# ref_normais_2008_ss <- '`(18,65] anos`'
# ref_vaz_2008_ss <- '`(14,104] anos`'
# ref_deaton_2008_ss <- '`(14,54] anos`'
# ref_lancaster_2008_ss <- '`(17,104] anos`'

# ref_ac_2008_cs <- '`Homens_(14,104]`'
# ref_normais_2008_cs <- '`Homens_(18,65]`'
# ref_vaz_2008_cs <- '`Homens_(14,104]`'
# ref_deaton_2008_cs <- '`Homens_(14,54]`'

lista.ref_2002_ss <- list(ref_ac_2002_ss
                          # , ref_vaz_2002_ss
                          # , ref_deaton_2002_ss
                          # , ref_lancaster_2002_ss
)
# lista.ref_2002_cs <- list(ref_ac_2002_cs, ref_vaz_2002_cs
# , ref_deaton_2002_cs
# )

lista.ref_2008_ss <- list(ref_ac_2008_ss
                          # , ref_vaz_2008_ss
                          # , ref_deaton_2008_ss
                          # , ref_lancaster_2008_ss
)
# lista.ref_2008_cs <- list(ref_ac_2008_cs, ref_vaz_2008_cs
#                           # , ref_deaton_2008_cs
# )

# 3. VARIÁVEIS DE CONTROLE ------------------------------------------------
lista.pof2002_ss$pof_ac_2002_ss %>% 
  select(
    contains('control'),
    -contains('Não'),
    UF_sigla,
    urbano
  ) %>%
  names(.) %>% 
  {glue('`{.}`')} -> control_pof2002

# POF2008
lista.pof2008_ss$pof_ac_2008_ss %>% 
  select(
    contains('control'),
    -contains('Não'),
    UF_sigla,
    urbano
  ) %>%
  names(.) %>% 
  {glue('`{.}`')} -> control_pof2008

# 4. OUTROS ARGUMENTOS DO MODELO ------------------------------------------------
# POF2002
rothbarth.welfare.indicator_pof2002 <- 'despesas.mensais.bens_adultos' #Para o método de Rothbarth, o bem-estar é inferido do nível de consumo de bens adultos
expenditure_pof2002 <- 'despesas.mensais.totais_per.capita' #Dispêndio per capita
iv.expenditure_pof2002 <- 'renda_per.capita' #Variável instrumental do dispêndio = renda
weights.var_pof2002 <- 'fator' #Peso amostral

# POF2008
rothbarth.welfare.indicator_pof2008 <- 'despesas.mensais.bens_adultos' #Para o método de Rothbarth, o bem-estar é inferido do nível de consumo de bens adultos
expenditure_pof2008 <- 'despesas.mensais.totais_per.capita' #Dispêndio per capita
iv.expenditure_pof2008 <- 'renda_total_per.capita' #Variável instrumental do dispêndio = renda
weights.var_pof2008 <- 'fator_expansao1' #Peso amostral

# Quantidade de crianças para cálculo de escalas de equivalência
n.child.range <- seq(0,3)

# Adultos na família de referência
n.adult.ref.rothbarth <- 2

# 5. BENS ADULTOS (ROTHBARTH) ------------------------------------------------------------
# SELEÇÃO DE BENS ADULTOS
bens_adultos <- c(
  
  # # 1) Bens adultos utilizados por Castro (2006) e Vaz & Vaz (2007)
  'despesas.mensais.vestuario.homem_mulher' # Bom
  ,'despesas.mensais.bebidas.alcoolicas' # Ruim
  ,'despesas.mensais.fumo' # Perfeito (ambas parecidas com a escala OCDE modificada)
  ,'despesas.mensais.jogos_apostas' # Bom (velha escala OCDE 2002, OCDE 2008. um pouco menor)
  # 
  # #  2) Outros candidatos a bens de adulto
  # ,'despesas.mensais.lazer_adulto' # Muito bom
  # ,'despesas.mensais.takeout.food' # Muito bom
  # ,'despesas.mensais.joias' # Muito Bom (velha escala OCDE 2002, OCDE 2008)
  # ,'despesas.mensais.perfume' # Muito bom (velha escala OCDE 2002, OCDE 2008)
  # ,'despesas.mensais.ensino.superior'# Ok (entre Engel e velha escala OCDE)

)


# POF2002
# Sem sexo
lapply( 
  lista.pof2002_ss,
  function(pof){ 
    pof %>%
      mutate(
        despesas.mensais.bens_adultos = rowSums(
          across(
            .cols = bens_adultos),
          na.rm = T)
      ) -> pof.temp
    
    # assign(x = glue('{pof.name}.rothbarth.sample'),
    #        value = pof.temp,
    #        envir = .GlobalEnv)
  }
) -> lista.pof2002_ss.rothbarth

# # Com sexo
# lapply( 
#   lista.pof2002_cs,
#   function(pof){ 
#     pof %>%
#       mutate(
#         despesas.mensais.bens_adultos = rowSums(
#           across(
#             .cols = bens_adultos),
#           na.rm = T)
#       ) -> pof.temp
#     
#     # assign(x = glue('{pof.name}.rothbarth.sample'),
#     #        value = pof.temp,
#     #        envir = .GlobalEnv)
#   }
# ) -> lista.pof2002_cs.rothbarth

# POF2008
# Sem sexo
lapply( 
  lista.pof2008_ss,
  function(pof){ 
    pof %>%
      mutate(
        despesas.mensais.bens_adultos = rowSums(
          across(
            .cols = bens_adultos),
          na.rm = T)
      ) -> pof.temp
    
    # assign(x = glue('{pof.name}.rothbarth.sample'),
    #        value = pof.temp,
    #        envir = .GlobalEnv)
  }
) -> lista.pof2008_ss.rothbarth

# # Com sexo
# lapply( 
#   lista.pof2008_cs,
#   function(pof){ 
#     pof %>%
#       mutate(
#         despesas.mensais.bens_adultos = rowSums(
#           across(
#             .cols = bens_adultos),
#           na.rm = T)
#       ) -> pof.temp
#     
#     # assign(x = glue('{pof.name}.rothbarth.sample'),
#     #        value = pof.temp,
#     #        envir = .GlobalEnv)
#   }
# ) -> lista.pof2008_cs.rothbarth


# 6. SELEÇÃO AMOSTRAL ------------------------------------------------------------
# POF2002
# Sem sexo
lapply( 
  lista.pof2002_ss.rothbarth,
  function(pof){ 
    pof %>%
      filter(
        round(!!sym(rothbarth.welfare.indicator_pof2002),2) > 0,
        round(!!sym(expenditure_pof2002),2) > 0,
        round(!!sym(iv.expenditure_pof2002),2) > 0
      ) %>% 
      sample.selection(
        incluir_solteiros_sem.filhos = T,
        incluir_solteiros_com.filhos = T,
        var.chefe_idade = 'control.chefe_idade',
        max_idade_chefe = 69, 
        min_idade_chefe = 18, 
        qtd_morador = qtd_moradores_pof2002,
        max_moradores = 5, 
        max_filhos = 3, 
        max_outros.parentes = 3,
        max_agregados = 0,
        max_pensionistas = 0,
        max_empregados = 0,
        max_parentes.empregados = 0
      ) -> pof.temp
    
    # assign(x = glue('{pof.name}.rothbarth.sample'),
    #        value = pof.temp,
    #        envir = .GlobalEnv)
  }
) -> lista.pof2002_ss.rothbarth.sample

# # Com sexo
# lapply( 
#   lista.pof2002_cs.rothbarth,
#   function(pof){ 
#     pof %>%
#       filter(
#         round(!!sym(rothbarth.welfare.indicator_pof2002),2) > 0,
#         round(!!sym(expenditure_pof2002),2) > 0,
#         round(!!sym(iv.expenditure_pof2002),2) > 0
#       ) %>%
#       sample.selection( 
#         incluir_solteiros_sem.filhos = T,
#         incluir_solteiros_com.filhos = T,
#         var.chefe_idade = 'control.chefe_idade',
#         max_idade_chefe = 69, 
#         min_idade_chefe = 18, 
#         qtd_morador = qtd_moradores_pof2002,
#         max_moradores = 5, 
#         max_filhos = 3, 
#         max_outros.parentes = 3,
#         max_agregados = 0,
#         max_pensionistas = 0,
#         max_empregados = 0,
#         max_parentes.empregados = 0
#       ) -> pof.temp
#     
#     # assign(x = glue('{pof.name}.rothbarth.sample'),
#     #        value = pof.temp,
#     #        envir = .GlobalEnv)
#   }
# ) -> lista.pof2002_cs.rothbarth.sample

# POF2008
# Sem sexo
lapply( 
  lista.pof2008_ss.rothbarth,
  function(pof){ 
    pof %>%
      filter(
        round(!!sym(rothbarth.welfare.indicator_pof2002),2) > 0,
        round(!!sym(expenditure_pof2008),2) > 0,
        round(!!sym(iv.expenditure_pof2008),2) > 0
      ) %>% 
      sample.selection( 
        incluir_solteiros_sem.filhos = T,
        incluir_solteiros_com.filhos = T,
        var.chefe_idade = 'control.chefe_idade_anos',
        max_idade_chefe = 69, 
        min_idade_chefe = 18, 
        qtd_morador = qtd_moradores_pof2008,
        max_moradores = 5, 
        max_filhos = 3, 
        max_outros.parentes = 3,
        max_agregados = 0,
        max_pensionistas = 0,
        max_empregados = 0,
        max_parentes.empregados = 0
      ) -> pof.temp
    
    # assign(x = glue('{pof.name}.rothbarth.sample'),
    #        value = pof.temp,
    #        envir = .GlobalEnv)
  }
) -> lista.pof2008_ss.rothbarth.sample

# # Com sexo
# lapply( 
#   lista.pof2008_cs.rothbarth,
#   function(pof){ 
#     pof %>%
#       filter(
        # round(!!sym(rothbarth.welfare.indicator_pof2008),2) > 0,
        # round(!!sym(expenditure_pof2008),2) > 0,
        # round(!!sym(iv.expenditure_pof2008),2) > 0
#       ) %>%
#       sample.selection( 
#         incluir_solteiros_sem.filhos = T,
#         incluir_solteiros_com.filhos = T,
#         var.chefe_idade = 'control.chefe_idade_anos',
#         max_idade_chefe = 69, 
#         min_idade_chefe = 18, 
#         qtd_morador = qtd_moradores_pof2008,
#         max_moradores = 5, 
#         max_filhos = 3, 
#         max_outros.parentes = 3,
#         max_agregados = 0,
#         max_pensionistas = 0,
#         max_empregados = 0,
#         max_parentes.empregados = 0
#       ) -> pof.temp
#     
#     # assign(x = glue('{pof.name}.rothbarth.sample'),
#     #        value = pof.temp,
#     #        envir = .GlobalEnv)
#   }
# ) -> lista.pof2008_cs.rothbarth.sample

# 7. ESCALAS DE EQUIVALÊNCIA: POF 2002, todas as faixas etárias, sem sexo ------------------------------------------------------------
# POF2002, todas as faixas etárias, sem sexo
# Modelos 2SLS (para output de tabelas)
Map(
  pof = lista.pof2002_ss.rothbarth.sample,
  pessoa_ref = lista.ref_2002_ss,
  function(pof, pessoa_ref){
    pof %>%
      iv.engel.rothbarth(
        welfare.indicator = rothbarth.welfare.indicator_pof2002,
        expenditure = expenditure_pof2002,
        iv.expenditure = iv.expenditure_pof2002,
        control = control_pof2002,
        weights = T,
        weights.var = weights.var_pof2002,
        show.diagnostics = T
      ) %>% return(.)
  }
) -> lista.pof2002_ss.rothbarth.sample.ivreg

# Erros robustos (para output de tabelas)
# lapply(
#   lista.pof2002_ss.rothbarth.sample.ivreg,
#   function(model){
#     model %>% 
#       robust_std.errors(.type = 'HC3')
#   }
# ) -> lista.pof2002_ss.rothbarth.sample.str_errors.robust

# Modelos robustos (para cálculos)
lapply(
  lista.pof2002_ss.rothbarth.sample.ivreg,
  function(model){
    model %>%
      fix.heteroskedasticity(.type = 'HC3')
  }
) -> lista.pof2002_ss.rothbarth.sample.ivreg.robust

# Escalas de equivalência
Map(
  iv.rothbarth.model = lista.pof2002_ss.rothbarth.sample.ivreg.robust,
  sample = names(lista.pof2002_ss.rothbarth.sample.ivreg.robust),
  pessoa_ref = lista.ref_2002_ss,
  function(
    iv.rothbarth.model, 
    sample,
    pessoa_ref,
    n.child = n.child.range
  ){
    lapply(
      n.child, 
      function(n){
        equivalence.scales.engel.rothbarth(
          model = iv.rothbarth.model,
          pessoa.referencia = pessoa_ref,
          na.h = 2, nc.h = n,
          na.r = n.adult.ref.rothbarth, nc.r = 0
        ) %>%
          mutate(
            data = sample
          )
      }
    ) %>% bind_rows(.)
  }) %>% bind_rows(.) %>% 
  mutate(
    child.cost = equivalence.scale - first(equivalence.scale)
    , child.cost = child.cost*(n.adult.ref.rothbarth/str_count(family.type,'C'))
    , acresc.scale = equivalence.scale - lag(equivalence.scale)
    , family.ref = strrep('A',n.adult.ref.rothbarth) 
  ) -> pof2002_ss.rothbarth_scales

# 8. ESCALAS DE EQUIVALÊNCIA: POF 2008, todas as faixas etárias, sem sexo ------------------------------------------------------------
# POF2008, todas as faixas etárias, sem sexo
# Modelos 2SLS (para output de tabelas)
Map(
  pof = lista.pof2008_ss.rothbarth.sample,
  pessoa_ref = lista.ref_2008_ss,
  function(pof, pessoa_ref){
    pof %>%
      iv.engel.rothbarth(
        welfare.indicator = rothbarth.welfare.indicator_pof2008,
        expenditure = expenditure_pof2008,
        iv.expenditure = iv.expenditure_pof2008,
        control = control_pof2008,
        weights = T,
        weights.var = weights.var_pof2008,
        show.diagnostics = T
      ) %>% return(.)
  }
) -> lista.pof2008_ss.rothbarth.sample.ivreg

# Erros robustos (para output de tabelas)
# lapply(
#   lista.pof2008_ss.rothbarth.sample.ivreg,
#   function(model){
#     model %>% 
#       robust_std.errors(.type = 'HC3')
#   }
# ) -> lista.pof2008_ss.rothbarth.sample.str_errors.robust

# Modelos robustos (para cálculos)
lapply(
  lista.pof2008_ss.rothbarth.sample.ivreg,
  function(model){
    model %>% 
      fix.heteroskedasticity(.type = 'HC3')
  }
) -> lista.pof2008_ss.rothbarth.sample.ivreg.robust

# Escalas de equivalência
Map(
  iv.rothbarth.model = lista.pof2008_ss.rothbarth.sample.ivreg.robust,
  sample = names(lista.pof2008_ss.rothbarth.sample.ivreg.robust),
  pessoa_ref = lista.ref_2008_ss,
  function(
    iv.rothbarth.model, 
    sample,
    pessoa_ref,
    n.child = n.child.range
  ){
    lapply(
      n.child, 
      function(n){
        equivalence.scales.engel.rothbarth(
          model = iv.rothbarth.model,
          pessoa.referencia = pessoa_ref,
          na.h = 2, nc.h = n,
          na.r = n.adult.ref.rothbarth, nc.r = 0
        ) %>%
          mutate(
            data = sample
          )
      }
    ) %>% bind_rows(.) 
  }) %>% bind_rows(.)  %>% 
  mutate(
    child.cost = equivalence.scale - first(equivalence.scale)
    , child.cost = child.cost*(n.adult.ref.rothbarth/str_count(family.type,'C'))
    , acresc.scale = equivalence.scale - lag(equivalence.scale)
    , family.ref = strrep('A',n.adult.ref.rothbarth) 
  ) -> pof2008_ss.rothbarth_scales

# # 9. ESCALAS DE EQUIVALÊNCIA: POF 2002, todas as faixas etárias, com sexo ------------------------------------------------------------
# # POF2002, todas as faixas etárias, sem sexo
# # Modelos 2SLS (para output de tabelas)
# Map(
#   pof = lista.pof2002_cs.rothbarth.sample,
#   pessoa_ref = lista.ref_2002_cs,
#   function(pof, pessoa_ref){
#     pof %>%
#       # iv.engel.rothbarth.econ_scale(
#       iv.engel.rothbarth(
#         welfare.indicator = rothbarth.welfare.indicator_pof2002,
#         expenditure = expenditure_pof2002,
#         iv.expenditure = iv.expenditure_pof2002,
#         # qtd_morador = qtd_moradores_pof2002,
#         
#         control = control_pof2002,
#         weights = T,
#         weights.var = weights.var_pof2002,
#         show.diagnostics = T
#       ) %>% return(.)
#   }
# ) -> lista.pof2002_cs.rothbarth.sample.ivreg
# 
# # Erros robustos (para output de tabelas)
# # lapply(
# #   lista.pof2002_cs.rothbarth.sample.ivreg,
# #   function(model){
# #     model %>% 
# #       robust_std.errors(.type = 'HC3')
# #   }
# # ) -> lista.pof2002_cs.rothbarth.sample.str_errors.robust
# 
# # Modelos robustos (para cálculos)
# lapply(
#   lista.pof2002_cs.rothbarth.sample.ivreg,
#   function(model){
#     model %>% 
#       fix.heteroskedasticity(.type = 'HC3')
#   }
# ) -> lista.pof2002_cs.rothbarth.sample.ivreg.robust
# 
# # Escalas de equivalência
# Map(
#   iv.rothbarth.model = lista.pof2002_cs.rothbarth.sample.ivreg.robust,
#   sample = names(lista.pof2002_cs.rothbarth.sample.ivreg.robust),
#   pessoa_ref = lista.ref_2002_cs,
#   function(
#     iv.rothbarth.model, 
#     sample,
#     pessoa_ref,
#     n.child = n.child.range
#   ){
#     lapply(
#       n.child, 
#       function(n){
#         equivalence.scales.engel.rothbarth(
#           model = iv.rothbarth.model,
#           pessoa.referencia = pessoa_ref,
#           na.h = 2, nc.h = n,
#           na.r = 2, nc.r = 0
#         ) %>% 
#           mutate(
#             family.type = glue('AA', strrep('C',n)),
#             data = sample
#           )
#       }
#     ) %>% bind_rows(.)
#   } 
# ) %>% bind_rows(.) %>% View(.)
# # -> pof2002_cs.rothbarth_scales
# # pof2002_cs.rothbarth_scales
# # 10. ESCALAS DE EQUIVALÊNCIA: POF 2008, todas as faixas etárias, com sexo ------------------------------------------------------------
# # POF2008, todas as faixas etárias, sem sexo
# # Modelos 2SLS (para output de tabelas)
# Map(
#   pof = lista.pof2008_cs.rothbarth.sample,
#   pessoa_ref = lista.ref_2008_cs,
#   function(pof, pessoa_ref){
#     pof %>%
#       # iv.engel.rothbarth.econ_scale(
#       iv.engel.rothbarth(
#         welfare.indicator = rothbarth.welfare.indicator_pof2008,
#         expenditure = expenditure_pof2008,
#         iv.expenditure = iv.expenditure_pof2008,
#         # qtd_morador = qtd_moradores_pof2008,
#         
#         control = control_pof2008,
#         weights = T,
#         weights.var = weights.var_pof2008,
#         show.diagnostics = T
#       ) %>% return(.)
#   }
# ) -> lista.pof2008_cs.rothbarth.sample.ivreg
# 
# # Erros robustos (para output de tabelas)
# # lapply(
# #   lista.pof2008_cs.rothbarth.sample.ivreg,
# #   function(model){
# #     model %>% 
# #       robust_std.errors(.type = 'HC3')
# #   }
# # ) -> lista.pof2008_cs.rothbarth.sample.str_errors.robust
# 
# # Modelos robustos (para cálculos)
# lapply(
#   lista.pof2008_cs.rothbarth.sample.ivreg,
#   function(model){
#     model %>% 
#       fix.heteroskedasticity(.type = 'HC3')
#   }
# ) -> lista.pof2008_cs.rothbarth.sample.ivreg.robust
# 
# # Escalas de equivalência
# Map(
#   iv.rothbarth.model = lista.pof2008_cs.rothbarth.sample.ivreg.robust,
#   sample = names(lista.pof2008_cs.rothbarth.sample.ivreg.robust),
#   pessoa_ref = lista.ref_2008_cs,
#   function(
#     iv.rothbarth.model, 
#     sample,
#     pessoa_ref,
#     n.child = n.child.range
#   ){
#     lapply(
#       n.child, 
#       function(n){
#         equivalence.scales.engel.rothbarth(
#           model = iv.rothbarth.model,
#           pessoa.referencia = pessoa_ref,
#           na.h = 2, nc.h = n,
#           na.r = 2, nc.r = 0
#         ) %>% 
#           mutate(
#             family.type = glue('AA', strrep('C',n)),
#             data = sample
#           )
#       }
#     ) %>% bind_rows(.)
#   } 
# ) %>% bind_rows(.) %>% View(.)
# # -> pof2008_cs.rothbarth_scales
# # pof2008_cs.rothbarth_scales

# 11. ARQUIVOS EXCEL ------------------------------------------------------
# list(
#   'POF2002 (sem sexo)' = pof2002_ss.rothbarth_scales,
#   # 'POF2002 (com sexo)' = pof2002_cs.rothbarth_scales,
#   'POF2008 (sem sexo)' = pof2008_ss.rothbarth_scales
#   # , 'POF2008 (com sexo)' = pof2008_cs.rothbarth_scales
# ) %>% 
#   openxlsx::write.xlsx(file = 'Escalas_Equivalencia_Rothbarth.xlsx')