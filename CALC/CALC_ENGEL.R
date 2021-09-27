# 1. PACOTES --------------------------------------------------------------
# Pacotes
# pkg <- c('rio', 'plyr', 'glue', 'tidyverse') #Leitura e Manipulação de Dados
# 
# lapply(pkg, function(x)
#   if(!require(x, character.only = T))
#   {install.packages(x); require(x)})

# Citação dos pacotes
# lapply(pkg, function(x)
#   {citation(package = x)})

# Funções próprias
source('C:/Users/Sony/Documents/GitHub/TCC/CALC/DADOS.R', encoding = 'UTF-8')
source('C:/Users/Sony/Documents/GitHub/TCC/FUNC_CALC/SELEC_AMOSTRAL.R', encoding = 'UTF-8')
source('C:/Users/Sony/Documents/GitHub/TCC/FUNC_CALC/ENGEL_ROTHBARTH.R', encoding = 'UTF-8')

# 2. PESSOAS DE REFERÊNCIA E PESOS AMOSTRAIS ------------------------------------------------
# Pessoas de referência
ref_ac_2002_ss <- '`(14,110] anos`' 
ref_normais_2002_ss <- '`(18,65] anos`'
ref_vaz_2002_ss <- '`(14,110] anos`'
ref_deaton_2002_ss <- '`(14,54] anos`'

ref_ac_2002_cs <- '`Homens_(14,110]`'
ref_normais_2002_cs <- '`Homens_(18,65]`'
ref_vaz_2002_cs <- '`Homens_(14,110]`'
ref_deaton_2002_cs <- '`Homens_(14,54]`'

ref_ac_2008_ss <- '`(14,104] anos`' 
ref_normais_2008_ss <- '`(18,65] anos`'
ref_vaz_2008_ss <- '`(14,104] anos`'
ref_deaton_2008_ss <- '`(14,54] anos`'

ref_ac_2008_cs <- '`Homens_(14,104]`'
ref_normais_2008_cs <- '`Homens_(18,65]`'
ref_vaz_2008_cs <- '`Homens_(14,104]`'
ref_deaton_2008_cs <- '`Homens_(14,54]`'

# Pesos amostrais
wgt2002 <- 'fator'
wgt2008 <- 'fator_expansao1'


# 3. ESCALAS DE EQUIVALÊNCIA: POF 2002, todas as faixas etárias, sem sexo ------------------------------------------------------------
# Adulto e Criança, POF2002, sem sexo
pof_ac_2002_ss %>%
  sample.selection( # Seleção amostral VAZ & VAZ (2007)
    incluir_solteiros_sem.filhos = F,
    incluir_solteiros_com.filhos = F,
    qtd_morador = 'n_morador',
    max_moradores = 8,
    max_agregados = 0,
    max_pensionistas = 0,
    max_empregados = 0,
    max_parentes.empregados = 0
  ) %>% 
  iv.engel.rothbarth(weights = T, weights.var = wgt2002,
                     iv.expenditure = 'renda_per.capita',
                     show.diagnostics = F) %>%
  fix.heteroskedasticity(.) %>%
  equivalence.scales.engel.rothbarth(pessoa.referencia = ref_ac_2002_ss)

# Normais, POF2002, sem sexo  
pof_normais_2002_ss %>%
  sample.selection( # Seleção amostral VAZ & VAZ (2007)
    incluir_solteiros_sem.filhos = F,
    incluir_solteiros_com.filhos = F,
    qtd_morador = 'n_morador',
    max_moradores = 8,
    max_agregados = 0,
    max_pensionistas = 0,
    max_empregados = 0,
    max_parentes.empregados = 0
  ) %>%
  iv.engel.rothbarth(weights = T, weights.var = wgt2002,
                     iv.expenditure = 'renda_per.capita',
                     show.diagnostics = F) %>%
  fix.heteroskedasticity(.) %>%
  equivalence.scales.engel.rothbarth(pessoa.referencia = ref_normais_2002_ss)

# Vaz & Vaz, POF2002, sem sexo
pof_vaz_2002_ss %>%
  sample.selection( # Seleção amostral VAZ & VAZ (2007)
    incluir_solteiros_sem.filhos = F,
    incluir_solteiros_com.filhos = F,
    qtd_morador = 'n_morador',
    max_moradores = 8,
    max_agregados = 0,
    max_pensionistas = 0,
    max_empregados = 0,
    max_parentes.empregados = 0
  ) %>%
  iv.engel.rothbarth(weights = T, weights.var = wgt2002,
                     iv.expenditure = 'renda_per.capita',
                     show.diagnostics = F) %>%
  fix.heteroskedasticity(.) %>%
  equivalence.scales.engel.rothbarth(pessoa.referencia = ref_vaz_2002_ss)

# Deaton, POF2002, sem sexo
pof_deaton_2002_ss %>%
  sample.selection( # Seleção amostral VAZ & VAZ (2007)
    incluir_solteiros_sem.filhos = F,
    incluir_solteiros_com.filhos = F,
    qtd_morador = 'n_morador',
    max_moradores = 8,
    max_agregados = 0,
    max_pensionistas = 0,
    max_empregados = 0,
    max_parentes.empregados = 0
  ) %>%
  iv.engel.rothbarth(weights = T, weights.var = wgt2002,
                     iv.expenditure = 'renda_per.capita',
                     show.diagnostics = F) %>%
  fix.heteroskedasticity(.) %>%
  equivalence.scales.engel.rothbarth(pessoa.referencia = ref_deaton_2002_ss)
# 4. ESCALAS DE EQUIVALÊNCIA: POF 2002, todas as faixas etárias, com sexo ------------------------------------------------------------
# Adulto e Criança, POF2002, com sexo
pof_ac_2002_cs %>%
  sample.selection( # Seleção amostral VAZ & VAZ (2007)
    incluir_solteiros_sem.filhos = F,
    incluir_solteiros_com.filhos = F,
    qtd_morador = 'n_morador',
    max_moradores = 8,
    max_agregados = 0,
    max_pensionistas = 0,
    max_empregados = 0,
    max_parentes.empregados = 0
  ) %>% 
  iv.engel.rothbarth(weights = T, weights.var = wgt2002,
                     iv.expenditure = 'renda_per.capita',
                     show.diagnostics = F) %>%
  fix.heteroskedasticity(.) %>%
  equivalence.scales.engel.rothbarth(pecsoa.referencia = ref_ac_2002_cs)

# Normais, POF2002, com sexo  
pof_normais_2002_cs %>%
  sample.selection( # Seleção amostral VAZ & VAZ (2007)
    incluir_solteiros_sem.filhos = F,
    incluir_solteiros_com.filhos = F,
    qtd_morador = 'n_morador',
    max_moradores = 8,
    max_agregados = 0,
    max_pensionistas = 0,
    max_empregados = 0,
    max_parentes.empregados = 0
  ) %>%
  iv.engel.rothbarth(weights = T, weights.var = wgt2002,
                     iv.expenditure = 'renda_per.capita',
                     show.diagnostics = F) %>%
  fix.heteroskedasticity(.) %>%
  equivalence.scales.engel.rothbarth(pecsoa.referencia = ref_normais_2002_cs)

# Vaz & Vaz, POF2002, com sexo
pof_vaz_2002_cs %>%
  sample.selection( # Seleção amostral VAZ & VAZ (2007)
    incluir_solteiros_sem.filhos = F,
    incluir_solteiros_com.filhos = F,
    qtd_morador = 'n_morador',
    max_moradores = 8,
    max_agregados = 0,
    max_pensionistas = 0,
    max_empregados = 0,
    max_parentes.empregados = 0
  ) %>%
  iv.engel.rothbarth(weights = T, weights.var = wgt2002,
                     iv.expenditure = 'renda_per.capita',
                     show.diagnostics = F) %>%
  fix.heteroskedasticity(.) %>%
  equivalence.scales.engel.rothbarth(pecsoa.referencia = ref_vaz_2002_cs)

# Deaton, POF2002, com sexo
pof_deaton_2002_cs %>%
  sample.selection( # Seleção amostral VAZ & VAZ (2007)
    incluir_solteiros_sem.filhos = F,
    incluir_solteiros_com.filhos = F,
    qtd_morador = 'n_morador',
    max_moradores = 8,
    max_agregados = 0,
    max_pensionistas = 0,
    max_empregados = 0,
    max_parentes.empregados = 0
  ) %>%
  iv.engel.rothbarth(weights = T, weights.var = wgt2002,
                     iv.expenditure = 'renda_per.capita',
                     show.diagnostics = F) %>%
  fix.heteroskedasticity(.) %>%
  equivalence.scales.engel.rothbarth(pecsoa.referencia = ref_deaton_2002_cs)

# 5. ESCALAS DE EQUIVALÊNCIA: POF 2008, todas as faixas etárias, sem sexo ------------------------------------------------------------
# Adulto e Criança, POF2008, sem sexo
pof_ac_2008_ss %>%
  sample.selection( # Seleção amostral VAZ & VAZ (2007)
    incluir_solteiros_sem.filhos = F,
    incluir_solteiros_com.filhos = F,
    max_moradores = 8,
    max_agregados = 0,
    max_pensionistas = 0,
    max_empregados = 0,
    max_parentes.empregados = 0
  ) %>% 
  iv.engel.rothbarth(weights = T, weights.var = wgt2008,
                     show.diagnostics = F) %>%
  fix.heteroskedasticity(.) %>%
  equivalence.scales.engel.rothbarth(pessoa.referencia = ref_ac_2008_ss)

# Normais, POF2008, sem sexo  
pof_normais_2008_ss %>%
  sample.selection( # Seleção amostral VAZ & VAZ (2007)
    incluir_solteiros_sem.filhos = F,
    incluir_solteiros_com.filhos = F,
    max_moradores = 8,
    max_agregados = 0,
    max_pensionistas = 0,
    max_empregados = 0,
    max_parentes.empregados = 0
  ) %>%
  iv.engel.rothbarth(weights = T, weights.var = wgt2008,
                     show.diagnostics = F) %>%
  fix.heteroskedasticity(.) %>%
  equivalence.scales.engel.rothbarth(pessoa.referencia = ref_normais_2008_ss)

# Vaz & Vaz, POF2008, sem sexo
pof_vaz_2008_ss %>%
  sample.selection( # Seleção amostral VAZ & VAZ (2007)
    incluir_solteiros_sem.filhos = F,
    incluir_solteiros_com.filhos = F,
    max_moradores = 8,
    max_agregados = 0,
    max_pensionistas = 0,
    max_empregados = 0,
    max_parentes.empregados = 0
  ) %>%
  iv.engel.rothbarth(weights = T, weights.var = wgt2008,
                     show.diagnostics = F) %>%
  fix.heteroskedasticity(.) %>%
  equivalence.scales.engel.rothbarth(pessoa.referencia = ref_vaz_2008_ss)

# Deaton, POF2008, sem sexo
pof_deaton_2008_ss %>%
  sample.selection( # Seleção amostral VAZ & VAZ (2007)
    incluir_solteiros_sem.filhos = F,
    incluir_solteiros_com.filhos = F,
    max_moradores = 8,
    max_agregados = 0,
    max_pensionistas = 0,
    max_empregados = 0,
    max_parentes.empregados = 0
  ) %>%
  iv.engel.rothbarth(weights = T, weights.var = wgt2008,
                     show.diagnostics = F) %>%
  fix.heteroskedasticity(.) %>%
  equivalence.scales.engel.rothbarth(pessoa.referencia = ref_deaton_2008_ss)
# 6. ESCALAS DE EQUIVALÊNCIA: POF 2008, todas as faixas etárias, com sexo ------------------------------------------------------------
# Adulto e Criança, POF2008, com sexo
pof_ac_2008_cs %>%
  sample.selection( # Seleção amostral VAZ & VAZ (2007)
    incluir_solteiros_sem.filhos = F,
    incluir_solteiros_com.filhos = F,
    max_moradores = 8,
    max_agregados = 0,
    max_pensionistas = 0,
    max_empregados = 0,
    max_parentes.empregados = 0
  ) %>% 
  iv.engel.rothbarth(weights = T, weights.var = wgt2008,
                     show.diagnostics = F) %>%
  fix.heteroskedasticity(.) %>%
  equivalence.scales.engel.rothbarth(pessoa.referencia = ref_ac_2008_cs)

# Normais, POF2008, com sexo  
pof_normais_2008_cs %>%
  sample.selection( # Seleção amostral VAZ & VAZ (2007)
    incluir_solteiros_sem.filhos = F,
    incluir_solteiros_com.filhos = F,
    max_moradores = 8,
    max_agregados = 0,
    max_pensionistas = 0,
    max_empregados = 0,
    max_parentes.empregados = 0
  ) %>%
  iv.engel.rothbarth(weights = T, weights.var = wgt2008,
                     show.diagnostics = F) %>%
  fix.heteroskedasticity(.) %>%
  equivalence.scales.engel.rothbarth(pessoa.referencia = ref_normais_2008_cs)

# Vaz & Vaz, POF2008, com sexo
pof_vaz_2008_cs %>%
  sample.selection( # Seleção amostral VAZ & VAZ (2007)
    incluir_solteiros_sem.filhos = F,
    incluir_solteiros_com.filhos = F,
    max_moradores = 8,
    max_agregados = 0,
    max_pensionistas = 0,
    max_empregados = 0,
    max_parentes.empregados = 0
  ) %>%
  iv.engel.rothbarth(weights = T, weights.var = wgt2008,
                     show.diagnostics = F) %>%
  fix.heteroskedasticity(.) %>%
  equivalence.scales.engel.rothbarth(pessoa.referencia = ref_vaz_2008_cs)

# Deaton, POF2008, com sexo
pof_deaton_2008_cs %>%
  sample.selection( # Seleção amostral VAZ & VAZ (2007)
    incluir_solteiros_sem.filhos = F,
    incluir_solteiros_com.filhos = F,
    max_moradores = 8,
    max_agregados = 0,
    max_pensionistas = 0,
    max_empregados = 0,
    max_parentes.empregados = 0
  ) %>%
  iv.engel.rothbarth(weights = T, weights.var = wgt2008,
                     show.diagnostics = F) %>%
  fix.heteroskedasticity(.) %>%
  equivalence.scales.engel.rothbarth(pessoa.referencia = ref_deaton_2008_cs)

