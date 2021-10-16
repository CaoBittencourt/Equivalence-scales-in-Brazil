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
ref_vaz_2002_ss <- '`(14,110] anos`'
ref_deaton_2002_ss <- '`(14,54] anos`'
# ref_lancaster_2002_ss <- '`(17,110] anos`'

ref_ac_2002_cs <- '`Homens_(14,110]`'
# ref_normais_2002_cs <- '`Homens_(18,65]`'
ref_vaz_2002_cs <- '`Homens_(14,110]`'
ref_deaton_2002_cs <- '`Homens_(14,54]`'

ref_ac_2008_ss <- '`(14,104] anos`' 
# ref_normais_2008_ss <- '`(18,65] anos`'
ref_vaz_2008_ss <- '`(14,104] anos`'
ref_deaton_2008_ss <- '`(14,54] anos`'
# ref_lancaster_2008_ss <- '`(17,104] anos`'

ref_ac_2008_cs <- '`Homens_(14,104]`'
# ref_normais_2008_cs <- '`Homens_(18,65]`'
ref_vaz_2008_cs <- '`Homens_(14,104]`'
ref_deaton_2008_cs <- '`Homens_(14,54]`'

lista.ref_2002_ss <- list(ref_ac_2002_ss, ref_vaz_2002_ss
                          # , ref_deaton_2002_ss
                          # , ref_lancaster_2002_ss
)
lista.ref_2002_cs <- list(ref_ac_2002_cs, ref_vaz_2002_cs
                          # , ref_deaton_2002_cs
)

lista.ref_2008_ss <- list(ref_ac_2008_ss, ref_vaz_2008_ss
                          # , ref_deaton_2008_ss
                          # , ref_lancaster_2008_ss
)
lista.ref_2008_cs <- list(ref_ac_2008_cs, ref_vaz_2008_cs
                          # , ref_deaton_2008_cs
)

# # 3. VARIÁVEIS DE CONTROLE ------------------------------------------------
# # POF2002
# lista.pof2002_ss$pof_ac_2002_ss %>% 
#   select(
#     `control.domc_cond_ocup`
#     ,`control.chefe_idade`
#     ,`control.chefe_orc_rend`
#     # ,`control.chefe_anos_est`
#     ,`control.chefe_nivel_instr `
#     ,`control.chefe_cor`
#     ,`control.qtd_Empregado`
#     ,`control.qtd_Tem cartão de crédito`
#     ,`control.qtd_Tem plano de saúde`
#     ,`control.qtd_Tem cheque especial`
#     ,`UF_sigla`
#     ,`urbano`
#     
#     # contains('control'),
#     # -contains('Não tem'),
#     # # -contains('chefe_idade'),
#     # -contains('Não empregado'),
#     # # -contains('cheque'),
#     
#     # [PROVAVELMENTE NÃO USAR, OU USAR COMO DUMMY APENAS]
#     # despesas.mensais.transporte.proprio_proxy, # Veículo próprio
#     # despesas.mensais.transporte.proprio_proxy_apenas.combustivel, # Veículo próprio (apenas combustível)
#     # [NÃO FAZ MUITA DIFERENÇA]
#     # share_despesas.mensais.imoveis.aquisicao, # Despesas atípicas/temporárias com imóveis
#     # share_despesas.mensais.imoveis.prestacao, # Despesas atípicas/temporárias com imóveis
#     # share_despesas.mensais.imoveis.reforma, # Despesas atípicas/temporárias com imóveis
#     # UF_sigla,
#     # urbano
#   ) %>%
#   names(.) %>% 
#   {glue('`{.}`')} -> control_pof2002
# 
# # POF2008
# lista.pof2008_ss$pof_ac_2008_ss %>% 
#   select(
#     contains('control'),
#     -contains('Não tem'),
#     # -contains('chefe_idade'),
#     -contains('Não empregado'),
#     # -contains('cheque'),
#     
#     # share_despesas.mensais.transporte.proprio_proxy, # Veículo próprio 
#     # share_despesas.mensais.transporte.proprio_proxy_apenas.combustivel, # Veículo próprio (apenas combustível)
#     # share_despesas.mensais.imoveis.aquisicao, # Despesas atípicas/temporárias com imóveis
#     # share_despesas.mensais.imoveis.prestacao, # Despesas atípicas/temporárias com imóveis
#     # share_despesas.mensais.imoveis.reforma, # Despesas atípicas/temporárias com imóveis
#     UF_sigla,
#     urbano
#   ) %>%
#   names(.) %>% 
#   {glue('`{.}`')} -> control_pof2008

# 3. ==> [teste] VARIÁVEIS DE CONTROLE ------------------------------------------------
lista.pof2002_ss$pof_ac_2002_ss %>% 
  select(
    # control.domc_cond_ocup
    # 
    # ,control.chefe_anos_est
    # ,control.chefe_sexo
    # ,control.chefe_orc_rend
    # ,control.chefe_cor
    # 
    # ,control.qtd_Empregado
    # ,'control.qtd_Tem cartão de crédito'
    # ,'control.qtd_Tem plano de saúde'
    # ,'control.qtd_Tem cheque especial'
    # # ,regiao
    # ,UF_sigla
    # ,urbano
    # # 
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
    
    # control.domc_cod_cond_ocup
    # ,control.chefe_anos_de_estudo
    # ,control.chefe_cod_sexo
    # ,control.chefe_cod_cor_raca
    # ,control.chefe_cod_sit_receita
    # 
    # ,control.qtd_Empregado
    # ,'control.qtd_Tem cartão de crédito'
    # ,'control.qtd_Tem plano de saúde'
    # ,'control.qtd_Tem cheque especial'
    # ,UF_sigla
    # ,urbano
    
  ) %>%
  names(.) %>% 
  {glue('`{.}`')} -> control_pof2008

# 4. OUTROS ARGUMENTOS DO MODELO ------------------------------------------------

# [Observação] Na POF bebidas alcoólicas são contabilizadas junto com alimentos
# [Observação] Na POF existe separação entre comida consumida no domicílio e fora do domicílio
# [Observação] Álcool e comida consumida fora do domicílio são considerados bens de adultos (e.g. Lancaster & Ray, 1998)
# [Solução] Utilizar apenas a comida consumida dentro do domicílio para estimar as escalas de Engel

# POF2002
engel.welfare.indicator_pof2002 <- 'share_despesas.mensais.takein.food' #Para o método de Engel, o bem-estar é inferido da participação orçamentária de comida
# engel.welfare.indicator_pof2002 <- 'share_despesas.mensais.alimentacao_menos.bebidas.alcoolicas' #Para o método de Engel, o bem-estar é inferido da participação orçamentária de comida
# engel.welfare.indicator_pof2002 <- 'share_despesas.mensais.alimentacao' #Para o método de Engel, o bem-estar é inferido da participação orçamentária de comida
expenditure_pof2002 <- 'despesas.mensais.totais_per.capita' #Dispêndio per capita
iv.expenditure_pof2002 <- 'renda_per.capita' #Variável instrumental do dispêndio = renda
weights.var_pof2002 <- 'fator' #Peso amostral

# POF2008
engel.welfare.indicator_pof2008 <- 'share_despesas.mensais.takein.food' #Para o método de Engel, o bem-estar é inferido da participação orçamentária de comida
# engel.welfare.indicator_pof2008 <- 'share_despesas.mensais.alimentacao_menos.bebidas.alcoolicas' #Para o método de Engel, o bem-estar é inferido da participação orçamentária de comida
# engel.welfare.indicator_pof2008 <- 'share_despesas.mensais.alimentacao' #Para o método de Engel, o bem-estar é inferido da participação orçamentária de comida
expenditure_pof2008 <- 'despesas.mensais.totais_per.capita' #Dispêndio per capita
iv.expenditure_pof2008 <- 'renda_total_per.capita' #Variável instrumental do dispêndio = renda
weights.var_pof2008 <- 'fator_expansao1' #Peso amostral

# Quantidade de crianças para cálculo de escalas de equivalência
n.child.range <- seq(0,3)

# 6. X SELEÇÃO AMOSTRAL (CASTRO, 2006) ------------------------------------------------------------
# POF2002
# Sem sexo
lapply( 
  lista.pof2002_ss,
  function(pof){ 
    pof %>%
      filter(
        !!sym(engel.welfare.indicator_pof2002) > 0,
        !!sym(expenditure_pof2002) > 0,
        !!sym(iv.expenditure_pof2002) > 0
      ) %>% 
      sample.selection( # Seleção amostral Castro (2006)
        incluir_solteiros_sem.filhos = F,
        incluir_solteiros_com.filhos = F,
        var.chefe_idade = 'control.chefe_idade',
        max_idade_chefe = 70,
        min_idade_chefe = 21,
        qtd_morador = qtd_moradores_pof2002,
        max_moradores = 5,
        max_filhos = 3,
        max_outros.parentes = 0,
        max_agregados = 0,
        max_pensionistas = 0,
        max_empregados = 0,
        max_parentes.empregados = 0
      ) -> pof.temp
    
    # assign(x = glue('{pof.name}.engle.sample'),
    #        value = pof.temp,
    #        envir = .GlobalEnv)
  }
) -> lista.pof2002_ss.engle.sample

# Com sexo
lapply( 
  lista.pof2002_cs,
  function(pof){ 
    pof %>%
      filter(
        !!sym(engel.welfare.indicator_pof2002) > 0,
        !!sym(expenditure_pof2002) > 0,
        !!sym(iv.expenditure_pof2002) > 0
      ) %>%
      sample.selection( # Seleção amostral Castro (2006)
        incluir_solteiros_sem.filhos = F,
        incluir_solteiros_com.filhos = F,
        var.chefe_idade = 'control.chefe_idade',
        max_idade_chefe = 70,
        min_idade_chefe = 21,
        qtd_morador = qtd_moradores_pof2002,
        max_moradores = 5,
        max_filhos = 3,
        max_outros.parentes = 0,
        max_agregados = 0,
        max_pensionistas = 0,
        max_empregados = 0,
        max_parentes.empregados = 0
      ) -> pof.temp
    
    # assign(x = glue('{pof.name}.engle.sample'),
    #        value = pof.temp,
    #        envir = .GlobalEnv)
  }
) -> lista.pof2002_cs.engle.sample

# POF2008
# Sem sexo
lapply( 
  lista.pof2008_ss,
  function(pof){ 
    pof %>%
      filter(
        !!sym(engel.welfare.indicator_pof2008) > 0,
        !!sym(expenditure_pof2008) > 0,
        !!sym(iv.expenditure_pof2008) > 0
      ) %>% 
      sample.selection( # Seleção amostral Castro (2006)
        incluir_solteiros_sem.filhos = F,
        incluir_solteiros_com.filhos = F,
        var.chefe_idade = 'control.chefe_idade_anos',
        max_idade_chefe = 70,
        min_idade_chefe = 21,
        qtd_morador = qtd_moradores_pof2008,
        max_moradores = 5,
        max_filhos = 3,
        max_outros.parentes = 0,
        max_agregados = 0,
        max_pensionistas = 0,
        max_empregados = 0,
        max_parentes.empregados = 0
      ) -> pof.temp
    
    # assign(x = glue('{pof.name}.engle.sample'),
    #        value = pof.temp,
    #        envir = .GlobalEnv)
  }
) -> lista.pof2008_ss.engle.sample

# Com sexo
lapply( 
  lista.pof2008_cs,
  function(pof){ 
    pof %>%
      filter(
        !!sym(engel.welfare.indicator_pof2008) > 0,
        !!sym(expenditure_pof2008) > 0,
        !!sym(iv.expenditure_pof2008) > 0
      ) %>%
      sample.selection( # Seleção amostral Castro (2006)
        incluir_solteiros_sem.filhos = F,
        incluir_solteiros_com.filhos = F,
        var.chefe_idade = 'control.chefe_idade_anos',
        max_idade_chefe = 70,
        min_idade_chefe = 21,
        qtd_morador = qtd_moradores_pof2008,
        max_moradores = 5,
        max_filhos = 3,
        max_outros.parentes = 0,
        max_agregados = 0,
        max_pensionistas = 0,
        max_empregados = 0,
        max_parentes.empregados = 0
      ) -> pof.temp
    
    # assign(x = glue('{pof.name}.engle.sample'),
    #        value = pof.temp,
    #        envir = .GlobalEnv)
  }
) -> lista.pof2008_cs.engle.sample

# 7. (TESTE) SELEÇÃO AMOSTRAL (TESTE) ------------------------------------------------------------
# POF2002
# Sem sexo
lapply(
  lista.pof2002_ss,
  function(pof){
    pof %>% 
      filter(
        round(!!sym(engel.welfare.indicator_pof2002),2) > 0,
        round(!!sym(expenditure_pof2002),2) > 0,
        round(!!sym(iv.expenditure_pof2002),2) > 0,
      ) %>%
      sample.selection( # Seleção amostral VAZ & VAZ (2007)
        incluir_solteiros_sem.filhos = F,
        incluir_solteiros_com.filhos = F,
        var.chefe_idade = 'control.chefe_idade',
        max_idade_chefe = 65,
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
  }
) -> lista.pof2002_ss.engle.sample

# POF2008
# Sem sexo
lapply( 
  lista.pof2008_ss,
  function(pof){ 
    pof %>%
      filter(
        round(!!sym(engel.welfare.indicator_pof2008),2) > 0,
        round(!!sym(expenditure_pof2008),2) > 0,
        round(!!sym(iv.expenditure_pof2008),2) > 0
      ) %>% 
      sample.selection( # Seleção amostral VAZ & VAZ (2007)
        incluir_solteiros_sem.filhos = F,
        incluir_solteiros_com.filhos = F,
        var.chefe_idade = 'control.chefe_idade_anos',
        max_idade_chefe = 65,
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
    
    # assign(x = glue('{pof.name}.engle.sample'),
    #        value = pof.temp,
    #        envir = .GlobalEnv)
  }
) -> lista.pof2008_ss.engle.sample

# 9. ESCALAS DE EQUIVALÊNCIA: POF 2002, todas as faixas etárias, sem sexo ------------------------------------------------------------
# POF2002, todas as faixas etárias, sem sexo
# Modelos 2SLS (para output de tabelas)
Map(
  pof = lista.pof2002_ss.engle.sample,
  pessoa_ref = lista.ref_2002_ss,
  function(pof, pessoa_ref){
    pof %>%
      # iv.engel.rothbarth.econ_scale(
      iv.engel.rothbarth(
        welfare.indicator = engel.welfare.indicator_pof2002,
        expenditure = expenditure_pof2002,
        iv.expenditure = iv.expenditure_pof2002,
        # expenditure = 'despesas.mensais.totais_per.capita',
        # iv.expenditure = 'renda_per.capita',
        
        # qtd_morador = qtd_moradores_pof2002,
        
        control = control_pof2002,
        weights = T,
        weights.var = weights.var_pof2002,
        show.diagnostics = T
      ) %>% return(.)
  }
) -> lista.pof2002_ss.engle.sample.ivreg

# Erros robustos (para output de tabelas)
# lapply(
#   lista.pof2002_ss.engle.sample.ivreg,
#   function(model){
#     model %>% 
#       robust_std.errors(.type = 'HC3')
#   }
# ) -> lista.pof2002_ss.engle.sample.str_errors.robust

# Modelos robustos (para cálculos)
lapply(
  lista.pof2002_ss.engle.sample.ivreg,
  function(model){
    model %>% 
      fix.heteroskedasticity(.type = 'HC3')
  }
) -> lista.pof2002_ss.engle.sample.ivreg.robust

# Escalas de equivalência
Map(
  iv.engel.model = lista.pof2002_ss.engle.sample.ivreg.robust,
  sample = names(lista.pof2002_ss.engle.sample.ivreg.robust),
  pessoa_ref = lista.ref_2002_ss,
  function(
    iv.engel.model, 
    sample,
    pessoa_ref,
    n.child = n.child.range
  ){
    lapply(
      n.child, 
      function(n){
        # equivalence.scales.engel.rothbarth.econ_scale(
        equivalence.scales.engel.rothbarth2(
          model = iv.engel.model,
          pessoa.referencia = pessoa_ref,
          na.h = 2, nc.h = n,
          na.r = 2, nc.r = 0
        ) %>% 
          mutate(
            family.comparison = glue('AA vs AA', strrep('C',n)),
            data = sample
          )
      }
    ) %>% bind_rows(.)
  } 
) %>% bind_rows(.) %>% View(.)
# -> pof2002_ss.engle_scales
# pof2002_ss.engle_scales 
# 11. ESCALAS DE EQUIVALÊNCIA: POF 2008, todas as faixas etárias, sem sexo ------------------------------------------------------------
# POF2008, todas as faixas etárias, sem sexo
# Modelos 2SLS (para output de tabelas)
Map(
  pof = lista.pof2008_ss.engle.sample,
  pessoa_ref = lista.ref_2008_ss,
  function(pof, pessoa_ref){
    pof %>%
      # iv.engel.rothbarth.econ_scale(
      iv.engel.rothbarth(
        welfare.indicator = engel.welfare.indicator_pof2008,
        expenditure = expenditure_pof2008,
        iv.expenditure = iv.expenditure_pof2008,
        # qtd_morador = qtd_moradores_pof2008,
        
        control = control_pof2008,
        weights = T,
        weights.var = weights.var_pof2008,
        show.diagnostics = T
      ) %>% return(.)
  }
) -> lista.pof2008_ss.engle.sample.ivreg

# Erros robustos (para output de tabelas)
# lapply(
#   lista.pof2008_ss.engle.sample.ivreg,
#   function(model){
#     model %>% 
#       robust_std.errors(.type = 'HC3')
#   }
# ) -> lista.pof2008_ss.engle.sample.str_errors.robust

# Modelos robustos (para cálculos)
lapply(
  lista.pof2008_ss.engle.sample.ivreg,
  function(model){
    model %>% 
      fix.heteroskedasticity(.type = 'HC3')
  }
) -> lista.pof2008_ss.engle.sample.ivreg.robust

# Escalas de equivalência
Map(
  iv.engel.model = lista.pof2008_ss.engle.sample.ivreg.robust,
  sample = names(lista.pof2008_ss.engle.sample.ivreg.robust),
  pessoa_ref = lista.ref_2008_ss,
  function(
    iv.engel.model, 
    sample,
    pessoa_ref,
    n.child = n.child.range
  ){
    lapply(
      n.child, 
      function(n){
        # equivalence.scales.engel.rothbarth(
        equivalence.scales.engel.rothbarth2(
          model = iv.engel.model,
          pessoa.referencia = pessoa_ref,
          na.h = 2, nc.h = n,
          na.r = 2, nc.r = 0
        ) %>% 
          mutate(
            family.comparison = glue('AA vs AA', strrep('C',n)),
            data = sample
          )
      }
    ) %>% bind_rows(.)
  } 
) %>% bind_rows(.) %>% View(.)
# -> pof2008_ss.engle_scales

# # 10. ESCALAS DE EQUIVALÊNCIA: POF 2002, todas as faixas etárias, com sexo ------------------------------------------------------------
# # POF2002, todas as faixas etárias, sem sexo
# # Modelos 2SLS (para output de tabelas)
# Map(
#   pof = lista.pof2002_cs.engle.sample,
#   pessoa_ref = lista.ref_2002_cs,
#   function(pof, pessoa_ref){
#     pof %>%
#       iv.engel.rothbarth.econ_scale(
#         # iv.engel.rothbarth(
#         welfare.indicator = engel.welfare.indicator_pof2002,
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
# ) -> lista.pof2002_cs.engle.sample.ivreg
# 
# # Erros robustos (para output de tabelas)
# lapply(
#   lista.pof2002_cs.engle.sample.ivreg,
#   function(model){
#     model %>% 
#       robust_std.errors(.type = 'HC3')
#   }
# ) -> lista.pof2002_cs.engle.sample.str_errors.robust
# 
# # Modelos robustos (para cálculos)
# lapply(
#   lista.pof2002_cs.engle.sample.ivreg,
#   function(model){
#     model %>% 
#       fix.heteroskedasticity(.type = 'HC3')
#   }
# ) -> lista.pof2002_cs.engle.sample.ivreg.robust
# 
# # Escalas de equivalência
# Map(
#   iv.engel.model = lista.pof2002_cs.engle.sample.ivreg.robust,
#   sample = names(lista.pof2002_cs.engle.sample.ivreg.robust),
#   pessoa_ref = lista.ref_2002_cs,
#   function(
#     iv.engel.model, 
#     sample,
#     pessoa_ref,
#     n.child = n.child.range
#   ){
#     lapply(
#       n.child, 
#       function(n){
#         equivalence.scales.engel.rothbarth(
#           model = iv.engel.model,
#           pessoa.referencia = pessoa_ref,
#           na.h = 2, nc.h = n,
#           na.r = 2, nc.r = 0
#         ) %>% 
#           mutate(
#             family.comparison = glue('AA vs AA', strrep('C',n)),
#             data = sample
#           )
#       }
#     ) %>% bind_rows(.)
#   } 
# ) %>% bind_rows(.) -> pof2002_cs.engle_scales

# # 12. ESCALAS DE EQUIVALÊNCIA: POF 2008, todas as faixas etárias, com sexo ------------------------------------------------------------
# # POF2008, todas as faixas etárias, sem sexo
# # Modelos 2SLS (para output de tabelas)
# Map(
#   pof = lista.pof2008_cs.engle.sample,
#   pessoa_ref = lista.ref_2008_cs,
#   function(pof, pessoa_ref){
#     pof %>%
#       iv.engel.rothbarth.econ_scale(
#         # iv.engel.rothbarth(
#         welfare.indicator = engel.welfare.indicator_pof2008,
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
# ) -> lista.pof2008_cs.engle.sample.ivreg
# 
# # Erros robustos (para output de tabelas)
# lapply(
#   lista.pof2008_cs.engle.sample.ivreg,
#   function(model){
#     model %>% 
#       robust_std.errors(.type = 'HC3')
#   }
# ) -> lista.pof2008_cs.engle.sample.str_errors.robust
# 
# # Modelos robustos (para cálculos)
# lapply(
#   lista.pof2008_cs.engle.sample.ivreg,
#   function(model){
#     model %>% 
#       fix.heteroskedasticity(.type = 'HC3')
#   }
# ) -> lista.pof2008_cs.engle.sample.ivreg.robust
# 
# # Escalas de equivalência
# Map(
#   iv.engel.model = lista.pof2008_cs.engle.sample.ivreg.robust,
#   sample = names(lista.pof2008_cs.engle.sample.ivreg.robust),
#   pessoa_ref = lista.ref_2008_cs,
#   function(
#     iv.engel.model, 
#     sample,
#     pessoa_ref,
#     n.child = n.child.range
#   ){
#     lapply(
#       n.child, 
#       function(n){
#         equivalence.scales.engel.rothbarth(
#           model = iv.engel.model,
#           pessoa.referencia = pessoa_ref,
#           na.h = 2, nc.h = n,
#           na.r = 2, nc.r = 0
#         ) %>% 
#           mutate(
#             family.comparison = glue('AA vs AA', strrep('C',n)),
#             data = sample
#           )
#       }
#     ) %>% bind_rows(.)
#   } 
# ) %>% bind_rows(.) -> pof2008_cs.engle_scales

# 13. ARQUIVOS EXCEL ------------------------------------------------------
list(
  'POF2002 (sem sexo)' = pof2002_ss.engle_scales,
  'POF2002 (com sexo)' = pof2002_cs.engle_scales,
  'POF2008 (sem sexo)' = pof2008_ss.engle_scales,
  'POF2008 (com sexo)' = pof2008_cs.engle_scales
) %>% 
  openxlsx::write.xlsx(file = 'Escalas_Equivalencia_Engel.xlsx')

# 14. TESTE TABELAS REGRESSÃO BONITAS ------------------------------------------------------------
# stargazer(
#   teste.lista,
#   type = 'html',
#   out = 'teste.htm',
#   title = 'Regressões de Engel (POF 2002): todas as faixas etárias, sem separação por sexo',
#   dep.var.labels.include = F,
#   dep.var.caption = 'Variável dependente: participação orçamentária de comida',
#   # covariate.labels = 
#   align = T,
#   # column.labels = names(teste.lista)
# ) 
# 
# stargazer(
#   lista.pof2002_cs.engle.sample.ivreg,
#   type = 'html',
#   out = 'teste.htm',
#   title = 'Regressões de Engel (POF 2002): todas as faixas etárias, sem separação por sexo',
#   dep.var.labels.include = F,
#   dep.var.caption = 'Variável dependente: participação orçamentária de comida',
#   se = lista.pof2002_cs.engle.sample.str_errors.robust,
#   # covariate.labels = 
#   align = T
#   # column.labels = names(teste.lista)
# ) 

# stargazer(
#   type = 'html', 
#   out = 'lala.htm',
#   
#   lista.pof2002_cs.engle.sample.ivreg$pof_ac_2002_cs
#   # ,
#   # se = lista.pof2002_cs.engle.sample.str_errors.robust
# )


Map(
  list.models = list(
    lista.pof2002_ss.engle.sample.ivreg,
    # lista.pof2002_cs.engle.sample.ivreg,
    lista.pof2008_ss.engle.sample.ivreg
    # lista.pof2008_cs.engle.sample.ivreg
  ),
  list.str_errors = list(
    lista.pof2002_ss.engle.sample.str_errors.robust,
    # lista.pof2002_cs.engle.sample.str_errors.robust,
    lista.pof2008_ss.engle.sample.str_errors.robust
    # lista.pof2008_cs.engle.sample.str_errors.robust
  ),
  list.names = list(
    'Engle_2SLS_POF2002_SemSexo',
    # 'Engle_2SLS_POF2002_ComSexo',
    'Engle_2SLS_POF2008_SemSexo'
    # 'Engle_2SLS_POF2008_ComSexo'
  ),
  function(
    list.models,
    list.str_errors,
    list.names
  ){
    
    stargazer(
      list.models,
      se = list.str_errors,
      type = 'html',
      # add.lines = list(
      #   
      # ),
      out = glue('{list.names}.htm')
    )
  }
)

# c(rownames(models.summary$diagnostics)[1], 
#   round(models.summary$diagnostics[1, 'p-value'], 2), 
#   round(models.summary$diagnostics[1, 'p-value'], 2)),


# stargazer(
#   lista.pof2002_ss.engle.sample.ivreg,
#   se = lista.pof2002_ss.engle.sample.str_errors.robust,
#   type = 'html',
#   out = 'Engle_2SLS_POF2002_SemSexo.htm',
#   title = 'Escalas de Engel - POF 2002-2003 (sem separação por sexo)'
#   )
# 
# stargazer(
#   lista.pof2002_cs.engle.sample.ivreg,
#   se = lista.pof2002_cs.engle.sample.str_errors.robust,
#   type = 'html',
#   out = 'Engle_2SLS_POF2002_ComSexo.htm',
#   title = 'Escalas de Engel - POF 2002-2003 (com separação por sexo)'
#   )
# 
# stargazer(
#   lista.pof2008_ss.engle.sample.ivreg,
#   se = lista.pof2008_ss.engle.sample.str_errors.robust,
#   type = 'html',
#   out = 'Engle_2SLS_POF2008_SemSexo.htm',
#   title = 'Escalas de Engel - POF 2008-2009 (sem separação por sexo)'
#   )
# 
# stargazer(
#   lista.pof2008_cs.engle.sample.ivreg,
#   se = lista.pof2008_cs.engle.sample.str_errors.robust,
#   type = 'html',
#   out = 'Engle_2SLS_POF2008_ComSexo.htm',
#   title = 'Escalas de Engel - POF 2008-2009 (com separação por sexo)'
#   )
# 
# 
# 
# huxreg(lista.pof2002_ss.engle.sample.ivreg)
# 
# # stargazer(lista.pof2002_cs$pof_ac_2002_cs %>% 
# #             select(
# #               `Homens_[0,14]`,
# #               `Homens_(14,110]`,
# #               `Mulheres_[0,14]`,
# #               `Mulheres_(14,110]`
# #             ),
# #           
# #           type = 'html',
# #           out = 'teste2.htm'
# # )
# 
# lapply(
#   teste.lista, 
#   broom::tidy
# ) -> teste.lista.tidy
# 
# lapply(
#   teste.lista.tidy, 
#   function(model){
#     model %>%
#       df.recode(list.recode = list.recode.models2002) 
#   }
# ) -> teste.lista.tidy
# 
# huxreg(
#   teste.lista.tidy, 
#   error_pos = 'below',
#   stars = c(
#     '***' = 0.001,
#     '**' = 0.01,
#     '*' = 0.05,
#     '.' = 0.1
#   ),
#   statistics = NULL,
#   note = 'Nota: {stars}'
# )
# 
# lalala %>% 
#   bind_rows(.) %>% 
#   df.recode(list.recode = list.recode.models2002) %>%
#   export(file = 'lalala.xlsx')
# 
# View(.)
# 
# lalala$pof_deaton_2002_ss %>% View(.)
# 
# stargazer(
#   type = 'html',
#   out = 'teste3.htm',
#   lista.pof2002_ss.engle.sample.ivreg,
#   se = lista.pof2002_ss.engle.sample.str_errors.robust
# )
# 
# stargazer(
#   type = 'html',
#   out = 'teste2.htm',
#   lista.pof2002_ss.engle.sample.ivreg.robust
# )
# 
# lapply(
#   lalala, 
#   function(model){
#     model %>%
#       df.recode(list.recode = list.recode.models2002) 
#   }
# ) -> lalala.tidy
# 
# lapply(
#   lalala.tidy,
#   huxtable::quick_html
# )
# 
#   # pof2002_ss.engle_scales %>%
#   pof2002_cs.engle_scales %>%
#   # pof2008_ss.engle_scales %>%
#   # pof2008_cs.engle_scales %>%
#   ggplot(aes(x = term,
#              y = member.cost)) +
#   geom_bar(stat = 'identity',
#            aes(fill = `p.value<=0.05`)) +
#   facet_grid(
#     cols = vars(family.comparison),
#     rows = vars(data)
#   ) +
#   coord_flip() +
#   ggthemes::theme_economist()
# 
# huxreg(
#   teste.lista.tidy, 
#   error_pos = 'below',
#   stars = c(
#     '***' = 0.001,
#     '**' = 0.01,
#     '*' = 0.05,
#     '.' = 0.1
#   ),
#   statistics = NULL,
#   note = 'Nota: {stars}'
# )
# 
# list(
#   'term' = list(
#     '(Intercept)' = 'Intercepto',
#     '`[0,4] anos`' = '0-4 anos',
#     '`(4,9] anos`' = '5-9 anos',
#     '`(9,14] anos`' = '10-14 anos',
#     '`(14,54] anos`' = '15-54 anos',
#     '`(54,110] anos`' = 'Acima de 54 anos',
#     '`[0,14] anos`' = '0-14 anos',
#     '`(14,110] anos`' = 'Acima de 15 anos',
#     'despesas.mensais.totais_per.capita' = 'Despesas totais per capita',                
#     'n_morador' = 'Número de moradores no domicílio',
#     'control.domc_cond_ocupImóvel próprio (já pago)' = 'Imóvel próprio (pago)',
#     'control.domc_cond_ocupImóvel próprio (em pagamento)' = 'Imóvel próprio (em pagamento)',
#     'control.domc_cond_ocupImóvel alugado' = 'Imóvel alugado',
#     'control.domc_cond_ocupImóvel cedido (por empregador)' = 'Imóvel cedido (por empregador)',
#     'control.domc_cond_ocupImóvel cedido (outra forma)' = 'Imóvel cedido (de outra forma)',
#     'control.domc_cond_ocupOutras condições de ocupação'  = 'Outras condições de moradia',
#     'control.chefe_anos_est' = 'Anos de estudo do chefe da família',
#     'control.qtd_Empregado' = 'Pessoas empregadas',
#     '`control.qtd_Tem cartão de crédito`' = 'Pessoas com cartão de crédito',
#     '`control.qtd_Tem plano de saúde`' = 'Pessoas com plano de saúde',
#     'share_despesas.mensais.imoveis.aquisicao' = 'Despesas com aquisição de imóveis (%)',
#     'share_despesas.mensais.imoveis.prestacao' = 'Despesas com prestações de imóveis (%)',
#     'share_despesas.mensais.imoveis.reforma' = 'Despesas com reforma de imóveis (%)',
#     'UF_siglaAL' = 'UF: AL',
#     'UF_siglaAM' = 'UF: AM',                  
#     'UF_siglaAP' = 'UF: AP',
#     'UF_siglaBA' = 'UF: BA',                                
#     'UF_siglaCE' = 'UF: CE',                    
#     'UF_siglaDF' = 'UF: DF',                                
#     'UF_siglaES' = 'UF: ES',                                
#     'UF_siglaGO' = 'UF: GO',                                
#     'UF_siglaMA' = 'UF: MA',                                
#     'UF_siglaMG' = 'UF: MG',                                
#     'UF_siglaMS' = 'UF: MS',                                
#     'UF_siglaMT' = 'UF: MT',                                
#     'UF_siglaPA' = 'UF: PA',                                
#     'UF_siglaPB' = 'UF: PB',                                
#     'UF_siglaPE' = 'UF: PE',                                
#     'UF_siglaPI' = 'UF: PI',                                
#     'UF_siglaPR' = 'UF: PR',                                
#     'UF_siglaRJ' = 'UF: RJ',                                
#     'UF_siglaRN' = 'UF: RN',                                
#     'UF_siglaRO' = 'UF: RO',                                
#     'UF_siglaRR' = 'UF: RR',                                
#     'UF_siglaRS' = 'UF: RS',                                
#     'UF_siglaSC' = 'UF: SC',                                
#     'UF_siglaSE' = 'UF: SE',                                
#     'UF_siglaSP' = 'UF: SP',                                
#     'UF_siglaTO' = 'UF: TO',                                
#     'urbano' = 'Região: Urbana'
#   )
# ) -> list.recode.models2002
# 
# 
# 
# #     stargazer(models.var[[country]]$varresult[1],
# #               models.var[[country]]$varresult[2],
# #               type = 'html',
# #               title = paste0('VAR Model (',
# #                              lags,
# #                              ') - ',
# #                              country.name),
# #               column.labels = c(names(models.var[[country]]$varresult[1]),
# #                                 names(models.var[[country]]$varresult[2])),
# #               dep.var.labels = '',
# #               model.numbers = F,
# #               align = T) -> model.stargazer
# 
# 
# # cat(file = 'Engel_pof_2002_ss.docx',
# # append = T)
# lista.pof2002_cs$pof_ac_2002_cs %>%
#   modelr::bootstrap(100)
