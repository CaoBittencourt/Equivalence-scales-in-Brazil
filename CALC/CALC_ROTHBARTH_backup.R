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
rothbarth.welfare.indicator_pof2002 <- 'share_despesas.mensais.bens_adultos' #Para o método de Engel, o bem-estar é inferido da participação orçamentária de comida
expenditure_pof2002 <- 'despesas.mensais.totais_per.capita' #Dispêndio per capita
iv.expenditure_pof2002 <- 'renda_per.capita' #Variável instrumental do dispêndio = renda
weights.var_pof2002 <- 'fator' #Peso amostral

# POF2008
rothbarth.welfare.indicator_pof2008 <- 'share_despesas.mensais.bens_adultos' #Para o método de Engel, o bem-estar é inferido da participação orçamentária de comida
expenditure_pof2008 <- 'despesas.mensais.totais_per.capita' #Dispêndio per capita
iv.expenditure_pof2008 <- 'renda_total_per.capita' #Variável instrumental do dispêndio = renda
weights.var_pof2008 <- 'fator_expansao1' #Peso amostral

# 5. BENS ADULTOS (ROTHBARTH) ------------------------------------------------------------
# SELEÇÃO DE BENS ADULTOS
bens_adultos <- c(
  
  # 'share_despesas.mensais.vestuario.infantil' #Children goods?
  # 'share_despesas.mensais.artigos.escolares' #Children goods?
  # 'share_despesas.mensais.brinquedos_jogos' #Children goods?
  # 'share_despesas.mensais.lanche.escolar' #Children goods?
  
  'share_despesas.mensais.manicure_pedicure'#, Escalas de equivalência boas (análogas à nova escala OCDE) => Usar
  # POF2002: N(despesas_manicure) > 0 = 7870, POF2008: N(despesas_manicure) > 0 = 12041
  
  # 'share_despesas.mensais.ensino.superior' #, #Escalas extremamente incoerentes => Não usar
  # POF2002: N(despesas_ensino.superior) > 0 = 2134, POF2008: N(despesas_ensino.superior) > 0 = 2500
  
  # 'share_despesas.mensais.bebidas.alcoolicas' #, #Escalas mais ou menos, dependendo da seleção amostral => Usar em conjunto ou separado (talvez)
  # POF2002: N(despesas_alcool) > 0 = 7556, POF2008: N(despesas_alcool) > 0 = 6158
  
  # 'share_despesas.mensais.fumo' #, #Escalas de equivalência ruins => Provavelmente não utilizar (talvez em conjunto)
  # POF2002: N(despesas_fumo) > 0 = 14806, POF2008: N(despesas_fumo) > 0 = 11206
  
  # 'share_despesas.mensais.jogos_apostas' #, #Escalas de equivalência ruins => Provavelmente não utilizar (talvez em conjunto)
  # POF2002: N(despesas_apostas) > 0 = 5849, POF2008: N(despesas_apostas) > 0 = 6262
  
  # 'share_despesas.mensais.vestuario.homem_mulher' # Escalas de equivalênciam muito ruins => Provavlemente não utilizar (nem em conjunto)
  # POF2002: N(despesas_roupas.adultos) > 0 = 36224, POF2008: N(despesas_roupas.adultos) > 0 = 41499
  
)


# POF2002
# Sem sexo
lapply( 
  lista.pof2002_ss,
  function(pof){ 
    pof %>%
      mutate(
        share_despesas.mensais.bens_adultos = rowSums(
          across(
            .cols = bens_adultos),
          na.rm = T)
      ) -> pof.temp
    
    # assign(x = glue('{pof.name}.rothbarth.sample'),
    #        value = pof.temp,
    #        envir = .GlobalEnv)
  }
) -> lista.pof2002_ss.rothbarth

# Com sexo
lapply( 
  lista.pof2002_cs,
  function(pof){ 
    pof %>%
      mutate(
        share_despesas.mensais.bens_adultos = rowSums(
          across(
            .cols = bens_adultos),
          na.rm = T)
      ) -> pof.temp
    
    # assign(x = glue('{pof.name}.rothbarth.sample'),
    #        value = pof.temp,
    #        envir = .GlobalEnv)
  }
) -> lista.pof2002_cs.rothbarth

# POF2008
# Sem sexo
lapply( 
  lista.pof2008_ss,
  function(pof){ 
    pof %>%
      mutate(
        share_despesas.mensais.bens_adultos = rowSums(
          across(
            .cols = bens_adultos),
          na.rm = T)
      ) -> pof.temp
    
    # assign(x = glue('{pof.name}.rothbarth.sample'),
    #        value = pof.temp,
    #        envir = .GlobalEnv)
  }
) -> lista.pof2008_ss.rothbarth

# Com sexo
lapply( 
  lista.pof2008_cs,
  function(pof){ 
    pof %>%
      mutate(
        share_despesas.mensais.bens_adultos = rowSums(
          across(
            .cols = bens_adultos),
          na.rm = T)
      ) -> pof.temp
    
    # assign(x = glue('{pof.name}.rothbarth.sample'),
    #        value = pof.temp,
    #        envir = .GlobalEnv)
  }
) -> lista.pof2008_cs.rothbarth

# 5. SELEÇÃO AMOSTRAL (APENAS Xi > 0) ------------------------------------------------------------
# POF2002
lapply( 
  lista.pof2002_ss.rothbarth,
  function(pof){ 
    pof %>%
      filter(
        !!sym(rothbarth.welfare.indicator_pof2002) > 0,
        !!sym(expenditure_pof2002) > 0,
        !!sym(iv.expenditure_pof2002) > 0
      ) -> pof.temp
    
    # assign(x = glue('{pof.name}.rothbarth.sample'),
    #        value = pof.temp,
    #        envir = .GlobalEnv)
  }
) -> lista.pof2002_ss.rothbarth.sample

lapply( 
  lista.pof2002_cs.rothbarth,
  function(pof){ 
    pof %>%
      filter(
        !!sym(rothbarth.welfare.indicator_pof2002) > 0,
        !!sym(expenditure_pof2002) > 0,
        !!sym(iv.expenditure_pof2002) > 0
      ) -> pof.temp
    
    # assign(x = glue('{pof.name}.rothbarth.sample'),
    #        value = pof.temp,
    #        envir = .GlobalEnv)
  }
) -> lista.pof2002_cs.rothbarth.sample

# POF2008
lapply( 
  lista.pof2008_ss.rothbarth,
  function(pof){ 
    pof %>%
      filter(
        !!sym(rothbarth.welfare.indicator_pof2008) > 0,
        !!sym(expenditure_pof2008) > 0,
        !!sym(iv.expenditure_pof2008) > 0
      ) -> pof.temp
    
    # assign(x = glue('{pof.name}.rothbarth.sample'),
    #        value = pof.temp,
    #        envir = .GlobalEnv)
  }
) -> lista.pof2008_ss.rothbarth.sample

lapply( 
  lista.pof2008_cs.rothbarth,
  function(pof){ 
    pof %>%
      filter(
        !!sym(rothbarth.welfare.indicator_pof2008) > 0,
        !!sym(expenditure_pof2008) > 0,
        !!sym(iv.expenditure_pof2008) > 0
      ) -> pof.temp
    
    # assign(x = glue('{pof.name}.rothbarth.sample'),
    #        value = pof.temp,
    #        envir = .GlobalEnv)
  }
) -> lista.pof2008_cs.rothbarth.sample

# 6. SELEÇÃO AMOSTRAL (CASTRO, 2006) ------------------------------------------------------------
# POF2002
# Sem sexo
lapply( 
  lista.pof2002_ss.rothbarth,
  function(pof){ 
    pof %>%
      filter(
        !!sym(rothbarth.welfare.indicator_pof2002) > 0,
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
    
    # assign(x = glue('{pof.name}.rothbarth.sample'),
    #        value = pof.temp,
    #        envir = .GlobalEnv)
  }
) -> lista.pof2002_ss.rothbarth.sample

# Com sexo
lapply( 
  lista.pof2002_cs.rothbarth,
  function(pof){ 
    pof %>%
      filter(
        !!sym(rothbarth.welfare.indicator_pof2002) > 0,
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
    
    # assign(x = glue('{pof.name}.rothbarth.sample'),
    #        value = pof.temp,
    #        envir = .GlobalEnv)
  }
) -> lista.pof2002_cs.rothbarth.sample

# POF2008
# Sem sexo
lapply( 
  lista.pof2008_ss.rothbarth,
  function(pof){ 
    pof %>%
      filter(
        !!sym(rothbarth.welfare.indicator_pof2008) > 0,
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
    
    # assign(x = glue('{pof.name}.rothbarth.sample'),
    #        value = pof.temp,
    #        envir = .GlobalEnv)
  }
) -> lista.pof2008_ss.rothbarth.sample

# Com sexo
lapply( 
  lista.pof2008_cs.rothbarth,
  function(pof){ 
    pof %>%
      filter(
        !!sym(rothbarth.welfare.indicator_pof2008) > 0,
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
    
    # assign(x = glue('{pof.name}.rothbarth.sample'),
    #        value = pof.temp,
    #        envir = .GlobalEnv)
  }
) -> lista.pof2008_cs.rothbarth.sample

# 7. SELEÇÃO AMOSTRAL (VAZ & VAZ, 2007) ------------------------------------------------------------
# POF2002
# Sem sexo
lapply( 
  lista.pof2002_ss.rothbarth,
  function(pof){ 
    pof %>%
      filter(
        !!sym(rothbarth.welfare.indicator_pof2002) > 0,
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
    
    # assign(x = glue('{pof.name}.rothbarth.sample'),
    #        value = pof.temp,
    #        envir = .GlobalEnv)
  }
) -> lista.pof2002_ss.rothbarth.sample

# Com sexo
lapply( 
  lista.pof2002_cs.rothbarth,
  function(pof){ 
    pof %>%
      filter(
        !!sym(rothbarth.welfare.indicator_pof2002) > 0,
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
    
    # assign(x = glue('{pof.name}.rothbarth.sample'),
    #        value = pof.temp,
    #        envir = .GlobalEnv)
  }
) -> lista.pof2002_cs.rothbarth.sample

# POF2008
# Sem sexo
lapply( 
  lista.pof2008_ss.rothbarth,
  function(pof){ 
    pof %>%
      filter(
        !!sym(rothbarth.welfare.indicator_pof2008) > 0,
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
    
    # assign(x = glue('{pof.name}.rothbarth.sample'),
    #        value = pof.temp,
    #        envir = .GlobalEnv)
  }
) -> lista.pof2008_ss.rothbarth.sample

# Com sexo
lapply( 
  lista.pof2008_cs.rothbarth,
  function(pof){ 
    pof %>%
      filter(
        !!sym(rothbarth.welfare.indicator_pof2008) > 0,
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
    
    # assign(x = glue('{pof.name}.rothbarth.sample'),
    #        value = pof.temp,
    #        envir = .GlobalEnv)
  }
) -> lista.pof2008_cs.rothbarth.sample

# 8. SELEÇÃO AMOSTRAL (DUDEL, 2020, FAZER DEPOIS) ------------------------------------------------------------
# POF2002
# Sem sexo
lapply( 
  lista.pof2002_ss.rothbarth,
  function(pof){ 
    pof %>%
      filter(
        !!sym(rothbarth.welfare.indicator_pof2002) > 0,
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
    
    # assign(x = glue('{pof.name}.rothbarth.sample'),
    #        value = pof.temp,
    #        envir = .GlobalEnv)
  }
) -> lista.pof2002_ss.rothbarth.sample

# Com sexo
lapply( 
  lista.pof2002_cs.rothbarth,
  function(pof){ 
    pof %>%
      filter(
        !!sym(rothbarth.welfare.indicator_pof2002) > 0,
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
    
    # assign(x = glue('{pof.name}.rothbarth.sample'),
    #        value = pof.temp,
    #        envir = .GlobalEnv)
  }
) -> lista.pof2002_cs.rothbarth.sample

# POF2008
# Sem sexo
lapply( 
  lista.pof2008_ss.rothbarth,
  function(pof){ 
    pof %>%
      filter(
        !!sym(rothbarth.welfare.indicator_pof2008) > 0,
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
    
    # assign(x = glue('{pof.name}.rothbarth.sample'),
    #        value = pof.temp,
    #        envir = .GlobalEnv)
  }
) -> lista.pof2008_ss.rothbarth.sample

# Com sexo
lapply( 
  lista.pof2008_cs.rothbarth,
  function(pof){ 
    pof %>%
      filter(
        !!sym(rothbarth.welfare.indicator_pof2008) > 0,
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
    
    # assign(x = glue('{pof.name}.rothbarth.sample'),
    #        value = pof.temp,
    #        envir = .GlobalEnv)
  }
) -> lista.pof2008_cs.rothbarth.sample

# 9. ESCALAS DE EQUIVALÊNCIA: POF 2002, todas as faixas etárias, sem sexo ------------------------------------------------------------
# POF2002, todas as faixas etárias, sem sexo
Map(
  pof = lista.pof2002_ss.rothbarth.sample,
  pessoa_ref = lista.ref_2002_ss,
  function(pof, pessoa_ref){
    pof %>%
      iv.engel.rothbarth.econ_scale(
        # iv.engel.rothbarth(
        welfare.indicator = rothbarth.welfare.indicator_pof2002,
        expenditure = expenditure_pof2002,
        iv.expenditure = iv.expenditure_pof2002,
        qtd_morador = qtd_moradores_pof2002,
        
        control = control_pof2002,
        weights = T,
        weights.var = weights.var_pof2002,
        show.diagnostics = T
      ) %>%
      fix.heteroskedasticity(.) -> iv.rothbarth.model
    
    # equivalence.scales.engel.rothbarth.econ_scale(pessoa.referencia = ref_ac_2002_ss) %>% View(.)
    list(
      'AA vs AAC' = 
        equivalence.scales.engel.rothbarth(
          model = iv.rothbarth.model,
          pessoa.referencia = pessoa_ref,
          na.h = 2, nc.h = 1,
          na.r = 2, nc.r = 0
        ),
      'AA vs AACC' = 
        equivalence.scales.engel.rothbarth(
          model = iv.rothbarth.model,
          pessoa.referencia = pessoa_ref,
          na.h = 2, nc.h = 2,
          na.r = 2, nc.r = 0
        ),
      'AA vs AACCC' = 
        equivalence.scales.engel.rothbarth(
          model = iv.rothbarth.model,
          pessoa.referencia = pessoa_ref,
          na.h = 2, nc.h = 3,
          na.r = 2, nc.r = 0
        ) 
    ) %>% return(.)
  }
)

# 10. ESCALAS DE EQUIVALÊNCIA: POF 2002, todas as faixas etárias, com sexo ------------------------------------------------------------
# POF2002, todas as faixas etárias, sem sexo
Map(
  pof = lista.pof2002_cs.rothbarth.sample,
  pessoa_ref = lista.ref_2002_cs,
  function(pof, pessoa_ref){
    pof %>%
      iv.engel.rothbarth.econ_scale(
        # iv.engel.rothbarth(
        welfare.indicator = rothbarth.welfare.indicator_pof2002,
        expenditure = expenditure_pof2002,
        iv.expenditure = iv.expenditure_pof2002,
        qtd_morador = qtd_moradores_pof2002,
        
        control = control_pof2002,
        weights = T,
        weights.var = weights.var_pof2002,
        show.diagnostics = F
      ) %>%
      fix.heteroskedasticity(.) -> iv.rothbarth.model
    
    # equivalence.scales.engel.rothbarth.econ_scale(pessoa.referencia = ref_ac_2002_cs) %>% View(.)
    list(
      'AA vs AAC' = 
        equivalence.scales.engel.rothbarth(
          model = iv.rothbarth.model,
          pessoa.referencia = pessoa_ref,
          na.h = 2, nc.h = 1,
          na.r = 2, nc.r = 0
        ),
      'AA vs AACC' = 
        equivalence.scales.engel.rothbarth(
          model = iv.rothbarth.model,
          pessoa.referencia = pessoa_ref,
          na.h = 2, nc.h = 2,
          na.r = 2, nc.r = 0
        ),
      'AA vs AACCC' = 
        equivalence.scales.engel.rothbarth(
          model = iv.rothbarth.model,
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
  pof = lista.pof2008_ss.rothbarth.sample,
  pessoa_ref = lista.ref_2008_ss,
  function(pof, pessoa_ref){
    pof %>%
      iv.engel.rothbarth.econ_scale(
        # iv.engel.rothbarth(
        welfare.indicator = rothbarth.welfare.indicator_pof2008,
        expenditure = expenditure_pof2008,
        iv.expenditure = iv.expenditure_pof2008,
        qtd_morador = qtd_moradores_pof2008,
        
        control = control_pof2008,
        weights = T,
        weights.var = weights.var_pof2008,
        show.diagnostics = F
      ) %>%
      fix.heteroskedasticity(.) -> iv.rothbarth.model
    
    # equivalence.scales.engel.rothbarth.econ_scale(pessoa.referencia = ref_ac_2008_ss) %>% View(.)
    list(
      'AA vs AAC' = 
        equivalence.scales.engel.rothbarth(
          model = iv.rothbarth.model,
          pessoa.referencia = pessoa_ref,
          na.h = 2, nc.h = 1,
          na.r = 2, nc.r = 0
        ),
      'AA vs AACC' = 
        equivalence.scales.engel.rothbarth(
          model = iv.rothbarth.model,
          pessoa.referencia = pessoa_ref,
          na.h = 2, nc.h = 2,
          na.r = 2, nc.r = 0
        ),
      'AA vs AACCC' = 
        equivalence.scales.engel.rothbarth(
          model = iv.rothbarth.model,
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
  pof = lista.pof2008_cs.rothbarth.sample,
  pessoa_ref = lista.ref_2008_cs,
  function(pof, pessoa_ref){
    pof %>%
      iv.engel.rothbarth.econ_scale(
        # iv.engel.rothbarth(
        welfare.indicator = rothbarth.welfare.indicator_pof2008,
        expenditure = expenditure_pof2008,
        iv.expenditure = iv.expenditure_pof2008,
        qtd_morador = qtd_moradores_pof2008,
        
        control = control_pof2008,
        weights = T,
        weights.var = weights.var_pof2008,
        show.diagnostics = F
      ) %>%
      fix.heteroskedasticity(.) -> iv.rothbarth.model
    
    # equivalence.scales.engel.rothbarth.econ_scale(pessoa.referencia = ref_ac_2008_cs) %>% View(.)
    list(
      'AA vs AAC' = 
        equivalence.scales.engel.rothbarth(
          model = iv.rothbarth.model,
          pessoa.referencia = pessoa_ref,
          na.h = 2, nc.h = 1,
          na.r = 2, nc.r = 0
        ),
      'AA vs AACC' = 
        equivalence.scales.engel.rothbarth(
          model = iv.rothbarth.model,
          pessoa.referencia = pessoa_ref,
          na.h = 2, nc.h = 2,
          na.r = 2, nc.r = 0
        ),
      'AA vs AACCC' = 
        equivalence.scales.engel.rothbarth(
          model = iv.rothbarth.model,
          pessoa.referencia = pessoa_ref,
          na.h = 2, nc.h = 3,
          na.r = 2, nc.r = 0
        ) 
    ) %>% return(.)
  }
)


# FAZER DEPOIS: TABELAS DE RESULTADOS BONITAS ----------------------------------------------------------------------
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
