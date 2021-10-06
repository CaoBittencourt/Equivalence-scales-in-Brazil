# 1. PACOTES --------------------------------------------------------------
# Pacotes
c(
  'rio', 'plyr', 'glue', 'broom', 'tidyverse', #Leitura e manipulação de dados
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

ref_ac_2002_cs <- '`Homens_(14,110]`'
# ref_normais_2002_cs <- '`Homens_(18,65]`'
ref_vaz_2002_cs <- '`Homens_(14,110]`'
ref_deaton_2002_cs <- '`Homens_(14,54]`'

ref_ac_2008_ss <- '`(14,104] anos`' 
# ref_normais_2008_ss <- '`(18,65] anos`'
ref_vaz_2008_ss <- '`(14,104] anos`'
ref_deaton_2008_ss <- '`(14,54] anos`'

ref_ac_2008_cs <- '`Homens_(14,104]`'
# ref_normais_2008_cs <- '`Homens_(18,65]`'
ref_vaz_2008_cs <- '`Homens_(14,104]`'
ref_deaton_2008_cs <- '`Homens_(14,54]`'

lista.ref_2002_ss <- list(ref_ac_2002_ss, ref_vaz_2002_ss, ref_deaton_2002_ss)
lista.ref_2002_cs <- list(ref_ac_2002_cs, ref_vaz_2002_cs, ref_deaton_2002_cs)

lista.ref_2008_ss <- list(ref_ac_2008_ss, ref_vaz_2008_ss, ref_deaton_2008_ss)
lista.ref_2008_cs <- list(ref_ac_2008_cs, ref_vaz_2008_cs, ref_deaton_2008_cs)

# 3. VARIÁVEIS DE CONTROLE ------------------------------------------------
# POF2002
lista.pof2002_ss$pof_ac_2002_ss %>% 
  select(
    contains('control'),
    -contains('Não tem'),
    -contains('_cor.'),
    -contains('chefe_idade'),
    -contains('Não empregado'),
    # control.chefe_idade,
    share_despesas.mensais.imoveis.aquisicao,
    share_despesas.mensais.imoveis.prestacao,
    share_despesas.mensais.imoveis.reforma,
    UF_sigla, 
    urbano
  ) %>%
  names(.) %>% 
  {glue('`{.}`')} -> control_pof2002

# POF2008
lista.pof2008_ss$pof_ac_2008_ss %>% 
  select(
    contains('control'),
    -contains('Não tem'),
    -contains('_cor.'),
    -contains('chefe_idade'),
    -contains('Não empregado'),
    # control.chefe_idade_anos,
    share_despesas.mensais.imoveis.aquisicao,
    share_despesas.mensais.imoveis.prestacao,
    share_despesas.mensais.imoveis.reforma,
    UF_sigla, 
    urbano
  ) %>%
  names(.) %>% 
  {glue('`{.}`')} -> control_pof2008

# 4. OUTROS ARGUMENTOS DA REGRESSÃO ------------------------------------------------
# POF2002
engel.welfare.indicator_pof2002 <- 'share_despesas.mensais.alimentacao' #Para o método de Engel, o bem-estar é inferido da participação orçamentária de comida
expenditure_pof2002 <- 'despesas.mensais.totais_per.capita' #Dispêndio per capita
iv.expenditure_pof2002 <- 'renda_per.capita' #Variável instrumental do dispêndio = renda
weights.var_pof2002 <- 'fator' #Peso amostral

# POF2008
engel.welfare.indicator_pof2008 <- 'share_despesas.mensais.alimentacao' #Para o método de Engel, o bem-estar é inferido da participação orçamentária de comida
expenditure_pof2008 <- 'despesas.mensais.totais_per.capita' #Dispêndio per capita
iv.expenditure_pof2008 <- 'renda_total_per.capita' #Variável instrumental do dispêndio = renda
weights.var_pof2008 <- 'fator_expansao1' #Peso amostral

# Quantidade de crianças para cálculo de escalas de equivalência
n.child.range <- seq(1,4)

# 5. SELEÇÃO AMOSTRAL (APENAS Xi > 0) ------------------------------------------------------------
# POF2002
lapply( 
  lista.pof2002_ss,
  function(pof){ 
    pof %>%
      filter(
        !!sym(engel.welfare.indicator_pof2002) > 0,
        !!sym(expenditure_pof2002) > 0,
        !!sym(iv.expenditure_pof2002) > 0
      ) -> pof.temp
    
    # assign(x = glue('{pof.name}.engle.sample'),
    #        value = pof.temp,
    #        envir = .GlobalEnv)
  }
) -> lista.pof2002_ss.engle.sample

lapply( 
  lista.pof2002_cs,
  function(pof){ 
    pof %>%
      filter(
        !!sym(engel.welfare.indicator_pof2002) > 0,
        !!sym(expenditure_pof2002) > 0,
        !!sym(iv.expenditure_pof2002) > 0
      ) -> pof.temp
    
    # assign(x = glue('{pof.name}.engle.sample'),
    #        value = pof.temp,
    #        envir = .GlobalEnv)
  }
) -> lista.pof2002_cs.engle.sample

# POF2008
lapply( 
  lista.pof2008_ss,
  function(pof){ 
    pof %>%
      filter(
        !!sym(engel.welfare.indicator_pof2008) > 0,
        !!sym(expenditure_pof2008) > 0,
        !!sym(iv.expenditure_pof2008) > 0
      ) -> pof.temp
    
    # assign(x = glue('{pof.name}.engle.sample'),
    #        value = pof.temp,
    #        envir = .GlobalEnv)
  }
) -> lista.pof2008_ss.engle.sample

lapply( 
  lista.pof2008_cs,
  function(pof){ 
    pof %>%
      filter(
        !!sym(engel.welfare.indicator_pof2008) > 0,
        !!sym(expenditure_pof2008) > 0,
        !!sym(iv.expenditure_pof2008) > 0
      ) -> pof.temp
    
    # assign(x = glue('{pof.name}.engle.sample'),
    #        value = pof.temp,
    #        envir = .GlobalEnv)
  }
) -> lista.pof2008_cs.engle.sample

# 6. SELEÇÃO AMOSTRAL (CASTRO, 2006) ------------------------------------------------------------
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

# 7. SELEÇÃO AMOSTRAL (VAZ & VAZ, 2007) ------------------------------------------------------------
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
      sample.selection( # Seleção amostral VAZ & VAZ (2007)
        incluir_solteiros_sem.filhos = F,
        incluir_solteiros_com.filhos = F,
        var.chefe_idade = 'control.chefe_idade',
        max_idade_chefe = 69,
        min_idade_chefe = 18,
        qtd_morador = qtd_moradores_pof2002,
        max_moradores = 8,
        max_filhos = 6,
        max_outros.parentes = 6,
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
      sample.selection( # Seleção amostral VAZ & VAZ (2007)
        incluir_solteiros_sem.filhos = F,
        incluir_solteiros_com.filhos = F,
        var.chefe_idade = 'control.chefe_idade',
        max_idade_chefe = 69,
        min_idade_chefe = 18,
        qtd_morador = qtd_moradores_pof2002,
        max_moradores = 8,
        max_filhos = 6,
        max_outros.parentes = 6,
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
      sample.selection( # Seleção amostral VAZ & VAZ (2007)
        incluir_solteiros_sem.filhos = F,
        incluir_solteiros_com.filhos = F,
        var.chefe_idade = 'control.chefe_idade_anos',
        max_idade_chefe = 69,
        min_idade_chefe = 18,
        qtd_morador = qtd_moradores_pof2008,
        max_moradores = 8,
        max_filhos = 6,
        max_outros.parentes = 6,
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
      sample.selection( # Seleção amostral VAZ & VAZ (2007)
        incluir_solteiros_sem.filhos = F,
        incluir_solteiros_com.filhos = F,
        var.chefe_idade = 'control.chefe_idade_anos',
        max_idade_chefe = 69,
        min_idade_chefe = 18,
        qtd_morador = qtd_moradores_pof2008,
        max_moradores = 8,
        max_filhos = 6,
        max_outros.parentes = 6,
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

# 8. SELEÇÃO AMOSTRAL (DUDEL, 2020, FAZER DEPOIS) ------------------------------------------------------------
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
      sample.selection( # Seleção amostral VAZ & VAZ (2007)
        incluir_solteiros_sem.filhos = F,
        incluir_solteiros_com.filhos = F,
        var.chefe_idade = 'control.chefe_idade',
        max_idade_chefe = 69,
        min_idade_chefe = 18,
        qtd_morador = qtd_moradores_pof2002,
        max_moradores = 8,
        max_filhos = 6,
        max_outros.parentes = 6,
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
      sample.selection( # Seleção amostral VAZ & VAZ (2007)
        incluir_solteiros_sem.filhos = F,
        incluir_solteiros_com.filhos = F,
        var.chefe_idade = 'control.chefe_idade',
        max_idade_chefe = 69,
        min_idade_chefe = 18,
        qtd_morador = qtd_moradores_pof2002,
        max_moradores = 8,
        max_filhos = 6,
        max_outros.parentes = 6,
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
      sample.selection( # Seleção amostral VAZ & VAZ (2007)
        incluir_solteiros_sem.filhos = F,
        incluir_solteiros_com.filhos = F,
        var.chefe_idade = 'control.chefe_idade_anos',
        max_idade_chefe = 69,
        min_idade_chefe = 18,
        qtd_morador = qtd_moradores_pof2008,
        max_moradores = 8,
        max_filhos = 6,
        max_outros.parentes = 6,
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
      sample.selection( # Seleção amostral VAZ & VAZ (2007)
        incluir_solteiros_sem.filhos = F,
        incluir_solteiros_com.filhos = F,
        var.chefe_idade = 'control.chefe_idade_anos',
        max_idade_chefe = 69,
        min_idade_chefe = 18,
        qtd_morador = qtd_moradores_pof2008,
        max_moradores = 8,
        max_filhos = 6,
        max_outros.parentes = 6,
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

# 9. ESCALAS DE EQUIVALÊNCIA: POF 2002, todas as faixas etárias, sem sexo ------------------------------------------------------------
# POF2002, todas as faixas etárias, sem sexo
Map(
  pof = lista.pof2002_ss.engle.sample,
  pessoa_ref = lista.ref_2002_ss,
  function(pof, pessoa_ref){
    pof %>%
      iv.engel.rothbarth.econ_scale(
        # iv.engel.rothbarth(
        welfare.indicator = engel.welfare.indicator_pof2002,
        expenditure = expenditure_pof2002,
        iv.expenditure = iv.expenditure_pof2002,
        qtd_morador = qtd_moradores_pof2002,
        
        control = control_pof2002,
        weights = T,
        weights.var = weights.var_pof2002,
        show.diagnostics = F
      ) %>%
      fix.heteroskedasticity(.) -> iv.engel.model
    
    # equivalence.scales.engel.rothbarth.econ_scale(pessoa.referencia = ref_ac_2002_ss) %>% View(.)
    list(
      'AA vs AAC' =
        equivalence.scales.engel.rothbarth(
          model = iv.engel.model,
          pessoa.referencia = pessoa_ref,
          na.h = 2, nc.h = 1,
          na.r = 2, nc.r = 0
        ),
      'AA vs AACC' =
        equivalence.scales.engel.rothbarth(
          model = iv.engel.model,
          pessoa.referencia = pessoa_ref,
          na.h = 2, nc.h = 2,
          na.r = 2, nc.r = 0
        ),
      'AA vs AACCC' =
        equivalence.scales.engel.rothbarth(
          model = iv.engel.model,
          pessoa.referencia = pessoa_ref,
          na.h = 2, nc.h = 3,
          na.r = 2, nc.r = 0
        )
    ) %>% return(.)
  }
)

# TESTE ------------------------------------------------------------
# POF2002, todas as faixas etárias, sem sexo
Map(
  pof = lista.pof2002_ss.engle.sample,
  pessoa_ref = lista.ref_2002_ss,
  function(pof, pessoa_ref){
    pof %>%
      iv.engel.rothbarth.econ_scale(
        # iv.engel.rothbarth(
        welfare.indicator = engel.welfare.indicator_pof2002,
        expenditure = expenditure_pof2002,
        iv.expenditure = iv.expenditure_pof2002,
        qtd_morador = qtd_moradores_pof2002,
        
        control = control_pof2002,
        weights = T,
        weights.var = weights.var_pof2002,
        show.diagnostics = F
      ) %>% return(.)
  }
) -> lista.pof2002_ss.engle.sample.ivreg

lapply(
  lista.pof2002_ss.engle.sample.ivreg,
  function(model){
    model %>% 
      robust_std.errors(.type = 'HC1')
  }
) -> lista.pof2002_ss.engle.sample.str_errors.robust

lapply(
  lista.pof2002_ss.engle.sample.ivreg,
  function(model){
    model %>% 
      fix.heteroskedasticity(.type = 'HC1')
  }
) -> lista.pof2002_ss.engle.sample.ivreg.robust

# Map(
#   iv.engel.model = lista.pof2002_ss.engle.sample.ivreg.robust,
#   pessoa_ref = lista.ref_2002_ss,
#   function(iv.engel.model, pessoa_ref){
#     list(
#       'AA vs AAC' =
#         equivalence.scales.engel.rothbarth(
#           model = iv.engel.model,
#           pessoa.referencia = pessoa_ref,
#           na.h = 2, nc.h = 1,
#           na.r = 2, nc.r = 0
#         ),
#       'AA vs AACC' =
#         equivalence.scales.engel.rothbarth(
#           model = iv.engel.model,
#           pessoa.referencia = pessoa_ref,
#           na.h = 2, nc.h = 2,
#           na.r = 2, nc.r = 0
#         ),
#       'AA vs AACCC' =
#         equivalence.scales.engel.rothbarth(
#           model = iv.engel.model,
#           pessoa.referencia = pessoa_ref,
#           na.h = 2, nc.h = 3,
#           na.r = 2, nc.r = 0
#         )
#     ) %>% return(.)
#   }
# )

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
        equivalence.scales.engel.rothbarth(
          model = iv.engel.model,
          pessoa.referencia = pessoa_ref,
          na.h = 2, nc.h = n,
          na.r = 2, nc.r = 0
        ) %>% 
          mutate(
            # family.comparison = 'AA vs AAC',
            family.comparison = glue('AA vs AA', strrep('C',n)),
            data = sample
          ) %>% 
          arrange(
            family.comparison
          )
      }
    ) %>% bind_rows(.)
  } 
) %>% bind_rows(.)

lalala %>% 
  bind_rows(.) %>% 
  df.recode(list.recode = list.recode.models2002) %>%
  export(file = 'lalala.xlsx')

  View(.)

lalala$pof_deaton_2002_ss %>% View(.)

Map(
  iv.engel.model = lista.pof2002_ss.engle.sample.ivreg.robust,
  sample = names(lista.pof2002_ss.engle.sample.ivreg.robust),
  pessoa_ref = lista.ref_2002_ss,
  function(
    iv.engel.model, 
    sample,
    pessoa_ref
  ){
    equivalence.scales.engel.rothbarth.econ_scale(
      model = iv.engel.model,
      pessoa.referencia = pessoa_ref,
      na.h = 2, nc.h = 1,
      na.r = 2, nc.r = 0
    ) %>% 
      mutate(
        family.comparison = 'AA vs AAC',
        data = sample
      ) -> temp
    
    equivalence.scales.engel.rothbarth.ec(
      model = iv.engel.model,
      pessoa.referencia = pessoa_ref,
      na.h = 2, nc.h = 1,
      na.r = 2, nc.r = 0
    ) %>% 
      mutate(
        family.comparison = 'AA vs AAC',
        data = sample
      ) -> temp
    
    #   'AA vs AACC' =
    #     equivalence.scales.engel.rothbarth(
    #       model = iv.engel.model,
    #       pessoa.referencia = pessoa_ref,
    #       na.h = 2, nc.h = 2,
    #       na.r = 2, nc.r = 0
    #     ),
    #   'AA vs AACCC' =
    #     equivalence.scales.engel.rothbarth(
    #       model = iv.engel.model,
    #       pessoa.referencia = pessoa_ref,
    #       na.h = 2, nc.h = 3,
    #       na.r = 2, nc.r = 0
    #     )
    # ) %>% return(.)
    return(temp)}
) %>% bind_rows(.) %>% View(.)

stargazer(
  type = 'html',
  out = 'teste3.htm',
  lista.pof2002_ss.engle.sample.ivreg,
  se = lista.pof2002_ss.engle.sample.str_errors.robust
)

stargazer(
  type = 'html',
  out = 'teste2.htm',
  lista.pof2002_ss.engle.sample.ivreg.robust
)

lapply(
  lalala, 
  function(model){
    model %>%
      df.recode(list.recode = list.recode.models2002) 
  }
) -> lalala.tidy

lapply(
  lalala.tidy,
  huxtable::quick_html
)

# lalala.tidy$pof_vaz_2002_ss %>%
#   bind_rows(lalala.tidy$pof_vaz_2002_ss) %>% 
#   bind_rows(lalala.tidy$pof_deaton_2002_ss) %>% 
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

huxreg(
  teste.lista.tidy, 
  error_pos = 'below',
  stars = c(
    '***' = 0.001,
    '**' = 0.01,
    '*' = 0.05,
    '.' = 0.1
  ),
  statistics = NULL,
  note = 'Nota: {stars}'
)


# 10. ESCALAS DE EQUIVALÊNCIA: POF 2002, todas as faixas etárias, com sexo ------------------------------------------------------------
# POF2002, todas as faixas etárias, sem sexo
Map(
  pof = lista.pof2002_cs.engle.sample,
  pessoa_ref = lista.ref_2002_cs,
  function(pof, pessoa_ref){
    pof %>%
      iv.engel.rothbarth.econ_scale(
        # iv.engel.rothbarth(
        welfare.indicator = engel.welfare.indicator_pof2002,
        expenditure = expenditure_pof2002,
        iv.expenditure = iv.expenditure_pof2002,
        qtd_morador = qtd_moradores_pof2002,
        
        control = control_pof2002,
        weights = T,
        weights.var = weights.var_pof2002,
        show.diagnostics = F
      ) %>%
      fix.heteroskedasticity(.) -> iv.engel.model
    
    # equivalence.scales.engel.rothbarth.econ_scale(pessoa.referencia = ref_ac_2002_cs) %>% View(.)
    list(
      'AA vs AAC' = 
        equivalence.scales.engel.rothbarth(
          model = iv.engel.model,
          pessoa.referencia = pessoa_ref,
          na.h = 2, nc.h = 1,
          na.r = 2, nc.r = 0
        ),
      'AA vs AACC' = 
        equivalence.scales.engel.rothbarth(
          model = iv.engel.model,
          pessoa.referencia = pessoa_ref,
          na.h = 2, nc.h = 2,
          na.r = 2, nc.r = 0
        ),
      'AA vs AACCC' = 
        equivalence.scales.engel.rothbarth(
          model = iv.engel.model,
          pessoa.referencia = pessoa_ref,
          na.h = 2, nc.h = 3,
          na.r = 2, nc.r = 0
        ) 
    ) %>% return(.)
  }
)

# 11. ESCALAS DE EQUIVALÊNCIA: POF 2008, todas as faixas etárias, sem sexo ------------------------------------------------------------
# POF2008, todas as faixas etárias, sem sexo
Map(
  pof = lista.pof2008_ss.engle.sample,
  pessoa_ref = lista.ref_2008_ss,
  function(pof, pessoa_ref){
    pof %>%
      iv.engel.rothbarth.econ_scale(
        # iv.engel.rothbarth(
        welfare.indicator = engel.welfare.indicator_pof2008,
        expenditure = expenditure_pof2008,
        iv.expenditure = iv.expenditure_pof2008,
        qtd_morador = qtd_moradores_pof2008,
        
        control = control_pof2008,
        weights = T,
        weights.var = weights.var_pof2008,
        show.diagnostics = F
      ) %>%
      fix.heteroskedasticity(.) -> iv.engel.model
    
    # equivalence.scales.engel.rothbarth.econ_scale(pessoa.referencia = ref_ac_2008_ss) %>% View(.)
    list(
      'AA vs AAC' = 
        equivalence.scales.engel.rothbarth(
          model = iv.engel.model,
          pessoa.referencia = pessoa_ref,
          na.h = 2, nc.h = 1,
          na.r = 2, nc.r = 0
        ),
      'AA vs AACC' = 
        equivalence.scales.engel.rothbarth(
          model = iv.engel.model,
          pessoa.referencia = pessoa_ref,
          na.h = 2, nc.h = 2,
          na.r = 2, nc.r = 0
        ),
      'AA vs AACCC' = 
        equivalence.scales.engel.rothbarth(
          model = iv.engel.model,
          pessoa.referencia = pessoa_ref,
          na.h = 2, nc.h = 3,
          na.r = 2, nc.r = 0
        ) 
    ) %>% return(.)
  }
)

# 12. ESCALAS DE EQUIVALÊNCIA: POF 2008, todas as faixas etárias, com sexo ------------------------------------------------------------
# POF2008, todas as faixas etárias, sem sexo
Map(
  pof = lista.pof2008_cs.engle.sample,
  pessoa_ref = lista.ref_2008_cs,
  function(pof, pessoa_ref){
    pof %>%
      iv.engel.rothbarth.econ_scale(
        # iv.engel.rothbarth(
        welfare.indicator = engel.welfare.indicator_pof2008,
        expenditure = expenditure_pof2008,
        iv.expenditure = iv.expenditure_pof2008,
        qtd_morador = qtd_moradores_pof2008,
        
        control = control_pof2008,
        weights = T,
        weights.var = weights.var_pof2008,
        show.diagnostics = F
      ) %>%
      fix.heteroskedasticity(.) -> iv.engel.model
    
    # equivalence.scales.engel.rothbarth.econ_scale(pessoa.referencia = ref_ac_2008_cs) %>% View(.)
    list(
      'AA vs AAC' = 
        equivalence.scales.engel.rothbarth(
          model = iv.engel.model,
          pessoa.referencia = pessoa_ref,
          na.h = 2, nc.h = 1,
          na.r = 2, nc.r = 0
        ),
      'AA vs AACC' = 
        equivalence.scales.engel.rothbarth(
          model = iv.engel.model,
          pessoa.referencia = pessoa_ref,
          na.h = 2, nc.h = 2,
          na.r = 2, nc.r = 0
        ),
      'AA vs AACCC' = 
        equivalence.scales.engel.rothbarth(
          model = iv.engel.model,
          pessoa.referencia = pessoa_ref,
          na.h = 2, nc.h = 3,
          na.r = 2, nc.r = 0
        ) 
    ) %>% return(.)
  }
)


# FAZER DEPOIS? TABELAS DE RESULTADOS BONITAS ----------------------------------------------------------------------
# names(models$pof_ac_2002_ss)
# huxreg(models[['pof_ac_2002_ss']], 
#        models[['pof_vaz_2002_ss']],
#        statistics = names(models$pof_ac_2002_ss)[]
#        )
# ?huxreg()
# stargazer(
#   models[['pof_ac_2002_ss']], 
#   models[['pof_ac_2002_cs']], 
#   type = 'html'
# ) #%>% 
#   #cat(file = 'lala.doc')
# 
# stargazer(models.var[[country]]$varresult[1],
#           models.var[[country]]$varresult[2],
#           type = 'html',
#           title = paste0('VAR Model (',
#                          lags,
#                          ') - ',
#                          country.name),
#           column.labels = c(names(models.var[[country]]$varresult[1]),
#                             names(models.var[[country]]$varresult[2])),
#           dep.var.labels = '',
#           model.numbers = F,
#           align = T) -> model.stargazer
# cat(model.stargazer,
#     file = paste0('ModelosVAR(',lags,').doc'),
#     append = T)


# TESTE TABELAS REGRESSÃO BONITAS ------------------------------------------------------------
# POF2002, todas as faixas etárias, sem sexo
# [PROBLEMA]
# fix.heteroskedasticity() usa o pacote sandwich ou lmtest (função coeftest)
# retorna um objeto "coeftest", não um modelo
# se tirar essa parte funciona corretamente o resto
# mas é necessário consertar a heteroscedasticidade (de outro modo?)


Map(
  pof = lista.pof2002_ss.engle.sample,
  pessoa_ref = lista.ref_2002_ss,
  function(pof, pessoa_ref){
    pof %>%
      iv.engel.rothbarth.econ_scale.teste(
        # iv.engel.rothbarth(
        welfare.indicator = engel.welfare.indicator_pof2002,
        expenditure = expenditure_pof2002,
        iv.expenditure = iv.expenditure_pof2002,
        qtd_morador = qtd_moradores_pof2002,
        
        control = control_pof2002,
        weights = T,
        weights.var = weights.var_pof2002,
        show.diagnostics = F
      ) #%>%
    #fix.heteroskedasticity(.) -> iv.engel.model
  }
) -> teste.lista3

teste.lista$pof_ac_2002_ss %>% class(.)
teste.lista2$pof_ac_2002_ss %>% class(.)
teste.lista3$pof_ac_2002_ss %>% class(.)

stargazer(
  teste.lista,
  type = 'html',
  out = 'teste.htm',
  title = 'Regressões de Engel (POF 2002): todas as faixas etárias, sem separação por sexo',
  dep.var.labels.include = F,
  dep.var.caption = 'Variável dependente: participação orçamentária de comida',
  # covariate.labels = 
  align = T,
  # column.labels = names(teste.lista)
) 

stargazer(
  teste.lista, 
  type = 'html', 
  out = 'lala.htm'
)

# stargazer(lista.pof2002_cs$pof_ac_2002_cs %>% 
#             select(
#               `Homens_[0,14]`,
#               `Homens_(14,110]`,
#               `Mulheres_[0,14]`,
#               `Mulheres_(14,110]`
#             ),
#           
#           type = 'html',
#           out = 'teste2.htm'
# )

lapply(
  teste.lista, 
  broom::tidy
) -> teste.lista.tidy

lapply(
  teste.lista.tidy, 
  function(model){
    model %>%
      df.recode(list.recode = list.recode.models2002) 
  }
) -> teste.lista.tidy

huxreg(
  teste.lista.tidy, 
  error_pos = 'below',
  stars = c(
    '***' = 0.001,
    '**' = 0.01,
    '*' = 0.05,
    '.' = 0.1
  ),
  statistics = NULL,
  note = 'Nota: {stars}'
)

list(
  'term' = list(
    '(Intercept)' = 'Intercepto',
    '`[0,4] anos`' = '0-4 anos',
    '`(4,9] anos`' = '5-9 anos',
    '`(9,14] anos`' = '10-14 anos',
    '`(14,54] anos`' = '15-54 anos',
    '`(54,110] anos`' = 'Acima de 54 anos',
    '`[0,14] anos`' = '0-14 anos',
    '`(14,110] anos`' = 'Acima de 15 anos',
    'despesas.mensais.totais_per.capita' = 'Despesas totais per capita',                
    'n_morador' = 'Número de moradores no domicílio',
    'control.domc_cond_ocupImóvel próprio (já pago)' = 'Imóvel próprio (pago)',
    'control.domc_cond_ocupImóvel próprio (em pagamento)' = 'Imóvel próprio (em pagamento)',
    'control.domc_cond_ocupImóvel alugado' = 'Imóvel alugado',
    'control.domc_cond_ocupImóvel cedido (por empregador)' = 'Imóvel cedido (por empregador)',
    'control.domc_cond_ocupImóvel cedido (outra forma)' = 'Imóvel cedido (de outra forma)',
    'control.domc_cond_ocupOutras condições de ocupação'  = 'Outras condições de moradia',
    'control.chefe_anos_est' = 'Anos de estudo do chefe da família',
    'control.qtd_Empregado' = 'Pessoas empregadas',
    '`control.qtd_Tem cartão de crédito`' = 'Pessoas com cartão de crédito',
    '`control.qtd_Tem plano de saúde`' = 'Pessoas com plano de saúde',
    'share_despesas.mensais.imoveis.aquisicao' = 'Despesas com aquisição de imóveis (%)',
    'share_despesas.mensais.imoveis.prestacao' = 'Despesas com prestações de imóveis (%)',
    'share_despesas.mensais.imoveis.reforma' = 'Despesas com reforma de imóveis (%)',
    'UF_siglaAL' = 'UF: AL',
    'UF_siglaAM' = 'UF: AM',                  
    'UF_siglaAP' = 'UF: AP',
    'UF_siglaBA' = 'UF: BA',                                
    'UF_siglaCE' = 'UF: CE',                    
    'UF_siglaDF' = 'UF: DF',                                
    'UF_siglaES' = 'UF: ES',                                
    'UF_siglaGO' = 'UF: GO',                                
    'UF_siglaMA' = 'UF: MA',                                
    'UF_siglaMG' = 'UF: MG',                                
    'UF_siglaMS' = 'UF: MS',                                
    'UF_siglaMT' = 'UF: MT',                                
    'UF_siglaPA' = 'UF: PA',                                
    'UF_siglaPB' = 'UF: PB',                                
    'UF_siglaPE' = 'UF: PE',                                
    'UF_siglaPI' = 'UF: PI',                                
    'UF_siglaPR' = 'UF: PR',                                
    'UF_siglaRJ' = 'UF: RJ',                                
    'UF_siglaRN' = 'UF: RN',                                
    'UF_siglaRO' = 'UF: RO',                                
    'UF_siglaRR' = 'UF: RR',                                
    'UF_siglaRS' = 'UF: RS',                                
    'UF_siglaSC' = 'UF: SC',                                
    'UF_siglaSE' = 'UF: SE',                                
    'UF_siglaSP' = 'UF: SP',                                
    'UF_siglaTO' = 'UF: TO',                                
    'urbano' = 'Região: Urbana'
  )
) -> list.recode.models2002



#     stargazer(models.var[[country]]$varresult[1],
#               models.var[[country]]$varresult[2],
#               type = 'html',
#               title = paste0('VAR Model (',
#                              lags,
#                              ') - ',
#                              country.name),
#               column.labels = c(names(models.var[[country]]$varresult[1]),
#                                 names(models.var[[country]]$varresult[2])),
#               dep.var.labels = '',
#               model.numbers = F,
#               align = T) -> model.stargazer


# cat(file = 'Engel_pof_2002_ss.docx',
# append = T)
lista.pof2002_cs$pof_ac_2002_cs %>%
  modelr::bootstrap(100)
