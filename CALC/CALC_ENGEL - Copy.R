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

# 2. PESSOAS DE REFERÊNCIA ------------------------------------------------
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

# 3. VARIÁVEIS DE CONTROLE ------------------------------------------------
# POF2002
pof_ac_2002_cs %>% 
  select(
    contains('control'),
    -contains('Não tem'),
    -contains('_cor.'),
    -contains('chefe_idade'),
    -contains('Não empregado'),
    UF_sigla, 
    urbano
  ) %>%
  names(.) %>% 
  {glue('`{.}`')} -> control_pof2002

# POF2008
pof_ac_2008_cs %>% 
  select(
    contains('control'),
    -contains('Não tem'),
    -contains('_cor.'),
    -contains('chefe_idade'),
    -contains('Não empregado'),
    UF_sigla, 
    urbano
  ) %>%
  names(.) %>% 
  {glue('`{.}`')} -> control_pof2008

# 4. OUTROS ARGUMENTOS DA REGRESSÃO ------------------------------------------------
# POF2002
rothbarth.welfare.indicator_pof2002 <- 'share_despesas.mensais.bens_adultos' #Para o método de Rothbarth, o bem-estar é inferido da participação orçamentária de "bens adultos"
expenditure_pof2002 <- 'despesas.mensais.totais_per.capita' #Dispêndio per capita
iv.expenditure_pof2002 <- 'renda_per.capita' #Variável instrumental do dispêndio = renda
weights.var_pof2002 <- 'fator' #Peso amostral

# POF2008
rothbarth.welfare.indicator_pof2008 <- 'share_despesas.mensais.bens_adultos' #Para o método de Rothbarth, o bem-estar é inferido da participação orçamentária de "bens adultos"
expenditure_pof2008 <- 'despesas.mensais.totais_per.capita' #Dispêndio per capita
iv.expenditure_pof2008 <- 'renda_total_per.capita' #Variável instrumental do dispêndio = renda
weights.var_pof2008 <- 'fator_expansao1' #Peso amostral

# 5. BENS ADULTOS (ROTHBARTH) ------------------------------------------------------------
bens_adultos <- c(
  
  # 'share_despesas.mensais.vestuario.infantil' #Children goods?
  'share_despesas.mensais.bebidas.alcoolicas'#,
  # 'share_despesas.mensais.fumo'#,
  # 'share_despesas.mensais.jogos_apostas',
  # 'share_despesas.mensais.vestuario.homem_mulher'
)

# POF2002
# Adulto e Criança, POF2002, sem sexo
pof_ac_2002_ss %>% 
  mutate(
    share_despesas.mensais.bens_adultos = rowSums(
      across(
        .cols = bens_adultos),
      na.rm = T)
  ) -> pof_ac_2002_ss.rothbarth

# Adulto e Criança, POF2002, com sexo
pof_ac_2002_cs %>% 
  mutate(
    share_despesas.mensais.bens_adultos = rowSums(
      across(
        .cols = bens_adultos),
      na.rm = T)
  ) -> pof_ac_2002_cs.rothbarth

# Faixas etárias normais, POF2002, sem sexo
pof_normais_2002_ss %>% 
  mutate(
    share_despesas.mensais.bens_adultos = rowSums(
      across(
        .cols = bens_adultos),
      na.rm = T)
  ) -> pof_normais_2002_ss.rothbarth

# Faixas etárias normais, POF2002, com sexo
pof_normais_2002_cs %>% 
  mutate(
    share_despesas.mensais.bens_adultos = rowSums(
      across(
        .cols = bens_adultos),
      na.rm = T)
  ) -> pof_normais_2002_cs.rothbarth

# VAZ & VAZ, POF2002, sem sexo
pof_vaz_2002_ss %>% 
  mutate(
    share_despesas.mensais.bens_adultos = rowSums(
      across(
        .cols = bens_adultos),
      na.rm = T)
  ) -> pof_vaz_2002_ss.rothbarth

# VAZ & VAZ, POF2002, com sexo
pof_vaz_2002_cs %>% 
  mutate(
    share_despesas.mensais.bens_adultos = rowSums(
      across(
        .cols = bens_adultos),
      na.rm = T)
  ) -> pof_vaz_2002_cs.rothbarth

# Deaton, POF2002, sem sexo
pof_deaton_2002_ss %>% 
  mutate(
    share_despesas.mensais.bens_adultos = rowSums(
      across(
        .cols = bens_adultos),
      na.rm = T)
  ) -> pof_deaton_2002_ss.rothbarth

# Deaton, POF2002, com sexo
pof_deaton_2002_cs %>% 
  mutate(
    share_despesas.mensais.bens_adultos = rowSums(
      across(
        .cols = bens_adultos),
      na.rm = T)
  ) -> pof_deaton_2002_cs.rothbarth

# POF2008
# Adulto e Criança, POF2008, sem sexo
pof_ac_2008_ss %>% 
  mutate(
    share_despesas.mensais.bens_adultos = rowSums(
      across(
        .cols = bens_adultos),
      na.rm = T)
  ) -> pof_ac_2008_ss.rothbarth

# Adulto e Criança, POF2008, com sexo
pof_ac_2008_cs %>% 
  mutate(
    share_despesas.mensais.bens_adultos = rowSums(
      across(
        .cols = bens_adultos),
      na.rm = T)
  ) -> pof_ac_2008_cs.rothbarth

# Faixas etárias normais, POF2008, sem sexo
pof_normais_2008_ss %>% 
  mutate(
    share_despesas.mensais.bens_adultos = rowSums(
      across(
        .cols = bens_adultos),
      na.rm = T)
  ) -> pof_normais_2008_ss.rothbarth

# Faixas etárias normais, POF2008, com sexo
pof_normais_2008_cs %>% 
  mutate(
    share_despesas.mensais.bens_adultos = rowSums(
      across(
        .cols = bens_adultos),
      na.rm = T)
  ) -> pof_normais_2008_cs.rothbarth

# VAZ & VAZ, POF2008, sem sexo
pof_vaz_2008_ss %>% 
  mutate(
    share_despesas.mensais.bens_adultos = rowSums(
      across(
        .cols = bens_adultos),
      na.rm = T)
  ) -> pof_vaz_2008_ss.rothbarth

# VAZ & VAZ, POF2008, com sexo
pof_vaz_2008_cs %>% 
  mutate(
    share_despesas.mensais.bens_adultos = rowSums(
      across(
        .cols = bens_adultos),
      na.rm = T)
  ) -> pof_vaz_2008_cs.rothbarth

# Deaton, POF2008, sem sexo
pof_deaton_2008_ss %>% 
  mutate(
    share_despesas.mensais.bens_adultos = rowSums(
      across(
        .cols = bens_adultos),
      na.rm = T)
  ) -> pof_deaton_2008_ss.rothbarth

# Deaton, POF2008, com sexo
pof_deaton_2008_cs %>% 
  mutate(
    share_despesas.mensais.bens_adultos = rowSums(
      across(
        .cols = bens_adultos),
      na.rm = T)
  ) -> pof_deaton_2008_cs.rothbarth

# 6. SELEÇÃO AMOSTRAL ------------------------------------------------------------
# POF2002
# Adulto e Criança, POF2002, sem sexo
pof_ac_2002_ss.rothbarth %>% 
  filter(
    !!sym(rothbarth.welfare.indicator_pof2002) > 0,
    !!sym(expenditure_pof2002) > 0,
    !!sym(iv.expenditure_pof2002) > 0
  ) -> pof_ac_2002_ss.rothbarth.sample #%>%
# sample.selection( # Seleção amostral VAZ & VAZ (2007)
#   incluir_solteiros_sem.filhos = F,
#   incluir_solteiros_com.filhos = F,
#   var.chefe_idade = 'control.chefe_idade',
#   max_idade_chefe = 69,
#   min_idade_chefe = 18,
#   qtd_morador = qtd_moradores_pof2002,
#   max_moradores = 8,
#   max_agregados = 0,
#   max_pensionistas = 0,
#   max_empregados = 0,
#   max_parentes.empregados = 0
# ) -> pof_ac_2002_ss.rothbarth.sample

# Adulto e Criança, POF2002, com sexo
pof_ac_2002_cs.rothbarth %>% 
  filter(
    !!sym(rothbarth.welfare.indicator_pof2002) > 0,
    !!sym(expenditure_pof2002) > 0,
    !!sym(iv.expenditure_pof2002) > 0
  ) -> pof_ac_2002_cs.rothbarth.sample #%>%
# sample.selection( # Seleção amostral VAZ & VAZ (2007)
#   incluir_solteiros_sem.filhos = F,
#   incluir_solteiros_com.filhos = F,
#   var.chefe_idade = 'control.chefe_idade',
#   max_idade_chefe = 69,
#   min_idade_chefe = 18,
#   qtd_morador = qtd_moradores_pof2002,
#   max_moradores = 8,
#   max_agregados = 0,
#   max_pensionistas = 0,
#   max_empregados = 0,
#   max_parentes.empregados = 0
# ) -> pof_ac_2002_cs.rothbarth.sample

# Faixas etárias normais, POF2002, sem sexo
pof_normais_2002_ss.rothbarth %>% 
  filter(
    !!sym(rothbarth.welfare.indicator_pof2002) > 0,
    !!sym(expenditure_pof2002) > 0,
    !!sym(iv.expenditure_pof2002) > 0
  ) -> pof_normais_2002_ss.rothbarth.sample #%>%
# sample.selection( # Seleção amostral VAZ & VAZ (2007)
#   incluir_solteiros_sem.filhos = F,
#   incluir_solteiros_com.filhos = F,
#   var.chefe_idade = 'control.chefe_idade',
#   max_idade_chefe = 69,
#   min_idade_chefe = 18,
#   qtd_morador = qtd_moradores_pof2002,
#   max_moradores = 8,
#   max_agregados = 0,
#   max_pensionistas = 0,
#   max_empregados = 0,
#   max_parentes.empregados = 0
# ) -> pof_normais_2002_ss.rothbarth.sample

# Faixas etárias normais, POF2002, com sexo
pof_normais_2002_cs.rothbarth %>% 
  filter(
    !!sym(rothbarth.welfare.indicator_pof2002) > 0,
    !!sym(expenditure_pof2002) > 0,
    !!sym(iv.expenditure_pof2002) > 0
  ) -> pof_normais_2002_cs.rothbarth.sample #%>%
# sample.selection( # Seleção amostral VAZ & VAZ (2007)
#   incluir_solteiros_sem.filhos = F,
#   incluir_solteiros_com.filhos = F,
#   var.chefe_idade = 'control.chefe_idade',
#   max_idade_chefe = 69,
#   min_idade_chefe = 18,
#   qtd_morador = qtd_moradores_pof2002,
#   max_moradores = 8,
#   max_agregados = 0,
#   max_pensionistas = 0,
#   max_empregados = 0,
#   max_parentes.empregados = 0
# ) -> pof_normais_2002_cs.rothbarth.sample

# VAZ & VAZ, POF2002, sem sexo
pof_vaz_2002_ss.rothbarth %>% 
  filter(
    !!sym(rothbarth.welfare.indicator_pof2002) > 0,
    !!sym(expenditure_pof2002) > 0,
    !!sym(iv.expenditure_pof2002) > 0
  ) -> pof_vaz_2002_ss.rothbarth.sample #%>%
# sample.selection( # Seleção amostral VAZ & VAZ (2007)
#   incluir_solteiros_sem.filhos = F,
#   incluir_solteiros_com.filhos = F,
#   var.chefe_idade = 'control.chefe_idade',
#   max_idade_chefe = 69,
#   min_idade_chefe = 18,
#   qtd_morador = qtd_moradores_pof2002,
#   max_moradores = 8,
#   max_agregados = 0,
#   max_pensionistas = 0,
#   max_empregados = 0,
#   max_parentes.empregados = 0
# ) -> pof_vaz_2002_ss.rothbarth.sample

# VAZ & VAZ, POF2002, com sexo
pof_vaz_2002_cs.rothbarth %>% 
  filter(
    !!sym(rothbarth.welfare.indicator_pof2002) > 0,
    !!sym(expenditure_pof2002) > 0,
    !!sym(iv.expenditure_pof2002) > 0
  ) -> pof_vaz_2002_cs.rothbarth.sample #%>%
# sample.selection( # Seleção amostral VAZ & VAZ (2007)
#   incluir_solteiros_sem.filhos = F,
#   incluir_solteiros_com.filhos = F,
#   var.chefe_idade = 'control.chefe_idade',
#   max_idade_chefe = 69,
#   min_idade_chefe = 18,
#   qtd_morador = qtd_moradores_pof2002,
#   max_moradores = 8,
#   max_agregados = 0,
#   max_pensionistas = 0,
#   max_empregados = 0,
#   max_parentes.empregados = 0
# ) -> pof_vaz_2002_cs.rothbarth.sample

# Deaton, POF2002, sem sexo
pof_deaton_2002_ss.rothbarth %>% 
  filter(
    !!sym(rothbarth.welfare.indicator_pof2002) > 0,
    !!sym(expenditure_pof2002) > 0,
    !!sym(iv.expenditure_pof2002) > 0
  ) -> pof_deaton_2002_ss.rothbarth.sample #%>%
# sample.selection( # Seleção amostral VAZ & VAZ (2007)
#   incluir_solteiros_sem.filhos = F,
#   incluir_solteiros_com.filhos = F,
#   var.chefe_idade = 'control.chefe_idade',
#   max_idade_chefe = 69,
#   min_idade_chefe = 18,
#   qtd_morador = qtd_moradores_pof2002,
#   max_moradores = 8,
#   max_agregados = 0,
#   max_pensionistas = 0,
#   max_empregados = 0,
#   max_parentes.empregados = 0
# ) -> pof_deaton_2002_ss.rothbarth.sample

# Deaton, POF2002, com sexo
pof_deaton_2002_cs.rothbarth %>% 
  filter(
    !!sym(rothbarth.welfare.indicator_pof2002) > 0,
    !!sym(expenditure_pof2002) > 0,
    !!sym(iv.expenditure_pof2002) > 0
  ) -> pof_deaton_2002_cs.rothbarth.sample #%>%
# sample.selection( # Seleção amostral VAZ & VAZ (2007)
#   incluir_solteiros_sem.filhos = F,
#   incluir_solteiros_com.filhos = F,
#   var.chefe_idade = 'control.chefe_idade',
#   max_idade_chefe = 69,
#   min_idade_chefe = 18,
#   qtd_morador = qtd_moradores_pof2002,
#   max_moradores = 8,
#   max_agregados = 0,
#   max_pensionistas = 0,
#   max_empregados = 0,
#   max_parentes.empregados = 0
# ) -> pof_deaton_2002_cs.rothbarth.sample

# POF2008
# Adulto e Criança, POF2008, sem sexo
pof_ac_2008_ss.rothbarth %>% 
  filter(
    !!sym(rothbarth.welfare.indicator_pof2008) > 0,
    !!sym(expenditure_pof2008) > 0,
    !!sym(iv.expenditure_pof2008) > 0
  ) -> pof_ac_2008_ss.rothbarth.sample #%>%
# sample.selection( # Seleção amostral VAZ & VAZ (2007)
#   incluir_solteiros_sem.filhos = F,
#   incluir_solteiros_com.filhos = F,
#   var.chefe_idade = 'control.chefe_idade_anos',
#   max_idade_chefe = 69,
#   min_idade_chefe = 18,
#   qtd_morador = qtd_moradores_pof2008,
#   max_moradores = 8,
#   max_agregados = 0,
#   max_pensionistas = 0,
#   max_empregados = 0,
#   max_parentes.empregados = 0
# ) -> pof_ac_2008_ss.rothbarth.sample

# Adulto e Criança, POF2008, com sexo
pof_ac_2008_cs.rothbarth %>% 
  filter(
    !!sym(rothbarth.welfare.indicator_pof2008) > 0,
    !!sym(expenditure_pof2008) > 0,
    !!sym(iv.expenditure_pof2008) > 0
  ) -> pof_ac_2008_cs.rothbarth.sample #%>%
# sample.selection( # Seleção amostral VAZ & VAZ (2007)
#   incluir_solteiros_sem.filhos = F,
#   incluir_solteiros_com.filhos = F,
#   var.chefe_idade = 'control.chefe_idade_anos',
#   max_idade_chefe = 69,
#   min_idade_chefe = 18,
#   qtd_morador = qtd_moradores_pof2008,
#   max_moradores = 8,
#   max_agregados = 0,
#   max_pensionistas = 0,
#   max_empregados = 0,
#   max_parentes.empregados = 0
# ) -> pof_ac_2008_cs.rothbarth.sample

# Faixas etárias normais, POF2008, sem sexo
pof_normais_2008_ss.rothbarth %>% 
  filter(
    !!sym(rothbarth.welfare.indicator_pof2008) > 0,
    !!sym(expenditure_pof2008) > 0,
    !!sym(iv.expenditure_pof2008) > 0
  ) -> pof_normais_2008_ss.rothbarth.sample #%>%
# sample.selection( # Seleção amostral VAZ & VAZ (2007)
#   incluir_solteiros_sem.filhos = F,
#   incluir_solteiros_com.filhos = F,
#   var.chefe_idade = 'control.chefe_idade_anos',
#   max_idade_chefe = 69,
#   min_idade_chefe = 18,
#   qtd_morador = qtd_moradores_pof2008,
#   max_moradores = 8,
#   max_agregados = 0,
#   max_pensionistas = 0,
#   max_empregados = 0,
#   max_parentes.empregados = 0
# ) -> pof_normais_2008_ss.rothbarth.sample

# Faixas etárias normais, POF2008, com sexo
pof_normais_2008_cs.rothbarth %>% 
  filter(
    !!sym(rothbarth.welfare.indicator_pof2008) > 0,
    !!sym(expenditure_pof2008) > 0,
    !!sym(iv.expenditure_pof2008) > 0
  ) -> pof_normais_2008_cs.rothbarth.sample #%>%
# sample.selection( # Seleção amostral VAZ & VAZ (2007)
#   incluir_solteiros_sem.filhos = F,
#   incluir_solteiros_com.filhos = F,
#   var.chefe_idade = 'control.chefe_idade_anos',
#   max_idade_chefe = 69,
#   min_idade_chefe = 18,
#   qtd_morador = qtd_moradores_pof2008,
#   max_moradores = 8,
#   max_agregados = 0,
#   max_pensionistas = 0,
#   max_empregados = 0,
#   max_parentes.empregados = 0
# ) -> pof_normais_2008_cs.rothbarth.sample

# VAZ & VAZ, POF2008, sem sexo
pof_vaz_2008_ss.rothbarth %>% 
  filter(
    !!sym(rothbarth.welfare.indicator_pof2008) > 0,
    !!sym(expenditure_pof2008) > 0,
    !!sym(iv.expenditure_pof2008) > 0
  ) -> pof_vaz_2008_ss.rothbarth.sample #%>%
# sample.selection( # Seleção amostral VAZ & VAZ (2007)
#   incluir_solteiros_sem.filhos = F,
#   incluir_solteiros_com.filhos = F,
#   var.chefe_idade = 'control.chefe_idade_anos',
#   max_idade_chefe = 69,
#   min_idade_chefe = 18,
#   qtd_morador = qtd_moradores_pof2008,
#   max_moradores = 8,
#   max_agregados = 0,
#   max_pensionistas = 0,
#   max_empregados = 0,
#   max_parentes.empregados = 0
# ) -> pof_vaz_2008_ss.rothbarth.sample

# VAZ & VAZ, POF2008, com sexo
pof_vaz_2008_cs.rothbarth %>% 
  filter(
    !!sym(rothbarth.welfare.indicator_pof2008) > 0,
    !!sym(expenditure_pof2008) > 0,
    !!sym(iv.expenditure_pof2008) > 0
  ) -> pof_vaz_2008_cs.rothbarth.sample #%>%
# sample.selection( # Seleção amostral VAZ & VAZ (2007)
#   incluir_solteiros_sem.filhos = F,
#   incluir_solteiros_com.filhos = F,
#   var.chefe_idade = 'control.chefe_idade_anos',
#   max_idade_chefe = 69,
#   min_idade_chefe = 18,
#   qtd_morador = qtd_moradores_pof2008,
#   max_moradores = 8,
#   max_agregados = 0,
#   max_pensionistas = 0,
#   max_empregados = 0,
#   max_parentes.empregados = 0
# ) -> pof_vaz_2008_cs.rothbarth.sample

# Deaton, POF2008, sem sexo
pof_deaton_2008_ss.rothbarth %>% 
  filter(
    !!sym(rothbarth.welfare.indicator_pof2008) > 0,
    !!sym(expenditure_pof2008) > 0,
    !!sym(iv.expenditure_pof2008) > 0
  ) -> pof_deaton_2008_ss.rothbarth.sample #%>%
# sample.selection( # Seleção amostral VAZ & VAZ (2007)
#   incluir_solteiros_sem.filhos = F,
#   incluir_solteiros_com.filhos = F,
#   var.chefe_idade = 'control.chefe_idade_anos',
#   max_idade_chefe = 69,
#   min_idade_chefe = 18,
#   qtd_morador = qtd_moradores_pof2008,
#   max_moradores = 8,
#   max_agregados = 0,
#   max_pensionistas = 0,
#   max_empregados = 0,
#   max_parentes.empregados = 0
# ) -> pof_deaton_2008_ss.rothbarth.sample

# Deaton, POF2008, com sexo
pof_deaton_2008_cs.rothbarth %>% 
  filter(
    !!sym(rothbarth.welfare.indicator_pof2008) > 0,
    !!sym(expenditure_pof2008) > 0,
    !!sym(iv.expenditure_pof2008) > 0
  ) -> pof_deaton_2008_cs.rothbarth.sample #%>%
# sample.selection( # Seleção amostral VAZ & VAZ (2007)
#   incluir_solteiros_sem.filhos = F,
#   incluir_solteiros_com.filhos = F,
#   var.chefe_idade = 'control.chefe_idade_anos',
#   max_idade_chefe = 69,
#   min_idade_chefe = 18,
#   qtd_morador = qtd_moradores_pof2008,
#   max_moradores = 8,
#   max_agregados = 0,
#   max_pensionistas = 0,
#   max_empregados = 0,
#   max_parentes.empregados = 0
# ) -> pof_deaton_2008_cs.rothbarth.sample

# 7. ESCALAS DE EQUIVALÊNCIA: POF 2002, todas as faixas etárias, sem sexo ------------------------------------------------------------
# Adulto e Criança, POF2002, sem sexo
# teste %>%
pof_ac_2002_ss.rothbarth.sample %>%
  iv.engel.rothbarth.econ_scale(
    # iv.engel.rothbarth(
    welfare.indicator = rothbarth.welfare.indicator_pof2002,
    expenditure = expenditure_pof2002,
    iv.expenditure = iv.expenditure_pof2002,
    qtd_morador = qtd_moradores_pof2002,
    
    control = control_pof2002,
    weights = F,
    weights.var = weights.var_pof2002,
    show.diagnostics = T
  ) %>%
  # iv.engel.rothbarth.quad(weights = T, weights.var = wgt2002,
  #                    iv.expenditure = 'renda_per.capita',
  #                    show.diagnostics = T) %>%
  # 
  fix.heteroskedasticity(.) %>%
  # equivalence.scales.engel.rothbarth(pessoa.referencia = ref_ac_2002_ss)
  equivalence.scales.engel.rothbarth(pessoa.referencia = ref_ac_2002_ss)

# Normais, POF2002, sem sexo  
pof_normais_2002_ss.rothbarth.sample %>% 
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
  fix.heteroskedasticity(.) %>%
  equivalence.scales.engel.rothbarth(pessoa.referencia = ref_normais_2002_ss)

# Vaz & Vaz, POF2002, sem sexo
pof_vaz_2002_ss.rothbarth.sample %>%
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
  # iv.engel.rothbarth.quad(weights = T, weights.var = wgt2002,
  #                         iv.expenditure = 'renda_per.capita',
  #                         show.diagnostics = T) %>%
  # 
  fix.heteroskedasticity(.) %>%
  equivalence.scales.engel.rothbarth(pessoa.referencia = ref_vaz_2002_ss)

# Deaton, POF2002, sem sexo
pof_deaton_2002_ss.rothbarth.sample %>%
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
  fix.heteroskedasticity(.) %>%
  equivalence.scales.engel.rothbarth(pessoa.referencia = ref_deaton_2002_ss)

# 8. ESCALAS DE EQUIVALÊNCIA: POF 2002, todas as faixas etárias, com sexo ------------------------------------------------------------
# Adulto e Criança, POF2002, com sexo
pof_ac_2002_cs.rothbarth.sample %>%
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
  # iv.engel.rothbarth.quad(weights = T, weights.var = wgt2002,
  #                    iv.expenditure = 'renda_per.capita',
  #                    show.diagnostics = T) %>%
  # 
  fix.heteroskedasticity(.) %>%
  equivalence.scales.engel.rothbarth(pessoa.referencia = ref_ac_2002_cs)

# Normais, POF2002, com sexo  
pof_normais_2002_cs.rothbarth.sample %>%
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
  fix.heteroskedasticity(.) %>%
  equivalence.scales.engel.rothbarth(pessoa.referencia = ref_normais_2002_cs)

# Vaz & Vaz, POF2002, com sexo
pof_vaz_2002_cs.rothbarth.sample %>%
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
  # iv.engel.rothbarth.quad(weights = T, weights.var = wgt2002,
  #                         iv.expenditure = 'renda_per.capita',
  #                         show.diagnostics = T) %>%
  # 
  fix.heteroskedasticity(.) %>%
  equivalence.scales.engel.rothbarth(pessoa.referencia = ref_vaz_2002_cs)

# Deaton, POF2002, com sexo
pof_deaton_2002_cs.rothbarth.sample %>%
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
  fix.heteroskedasticity(.) %>%
  equivalence.scales.engel.rothbarth(pessoa.referencia = ref_deaton_2002_cs)

# 9. ESCALAS DE EQUIVALÊNCIA: POF 2008, todas as faixas etárias, sem sexo ------------------------------------------------------------
# Adulto e Criança, POF2008, sem sexo
pof_ac_2008_ss.rothbarth.sample %>%
  iv.engel.rothbarth.econ_scale(
    # iv.engel.rothbarth(
    welfare.indicator = rothbarth.welfare.indicator_pof2008,
    expenditure = expenditure_pof2008,
    iv.expenditure = iv.expenditure_pof2008,
    qtd_morador = qtd_moradores_pof2008,
    
    control = control_pof2008,
    weights = T,
    weights.var = weights.var_pof2008,
    show.diagnostics = T
  ) %>%
  # iv.engel.rothbarth.quad(weights = T, weights.var = wgt2008,
  #                    iv.expenditure = 'renda_per.capita',
  #                    show.diagnostics = T) %>%
  # 
  fix.heteroskedasticity(.) %>%
  equivalence.scales.engel.rothbarth(pessoa.referencia = ref_ac_2008_ss)

# Normais, POF2008, sem sexo  
pof_normais_2008_ss.rothbarth.sample %>%
  iv.engel.rothbarth.econ_scale(
    # iv.engel.rothbarth(
    welfare.indicator = rothbarth.welfare.indicator_pof2008,
    expenditure = expenditure_pof2008,
    iv.expenditure = iv.expenditure_pof2008,
    qtd_morador = qtd_moradores_pof2008,
    
    control = control_pof2008,
    weights = T,
    weights.var = weights.var_pof2008,
    show.diagnostics = T
  ) %>%
  fix.heteroskedasticity(.) %>%
  equivalence.scales.engel.rothbarth(pessoa.referencia = ref_normais_2008_ss)

# Vaz & Vaz, POF2008, sem sexo
pof_vaz_2008_ss.rothbarth.sample %>%
  iv.engel.rothbarth.econ_scale(
    # iv.engel.rothbarth(
    welfare.indicator = rothbarth.welfare.indicator_pof2008,
    expenditure = expenditure_pof2008,
    iv.expenditure = iv.expenditure_pof2008,
    qtd_morador = qtd_moradores_pof2008,
    
    control = control_pof2008,
    weights = T,
    weights.var = weights.var_pof2008,
    show.diagnostics = T
  ) %>%
  # iv.engel.rothbarth.quad(weights = T, weights.var = wgt2008,
  #                         iv.expenditure = 'renda_per.capita',
  #                         show.diagnostics = T) %>%
  # 
  fix.heteroskedasticity(.) %>%
  equivalence.scales.engel.rothbarth(pessoa.referencia = ref_vaz_2008_ss)

# Deaton, POF2008, sem sexo
pof_deaton_2008_ss.rothbarth.sample %>%
  iv.engel.rothbarth.econ_scale(
    # iv.engel.rothbarth(
    welfare.indicator = rothbarth.welfare.indicator_pof2008,
    expenditure = expenditure_pof2008,
    iv.expenditure = iv.expenditure_pof2008,
    qtd_morador = qtd_moradores_pof2008,
    
    control = control_pof2008,
    weights = T,
    weights.var = weights.var_pof2008,
    show.diagnostics = T
  ) %>%
  fix.heteroskedasticity(.) %>%
  equivalence.scales.engel.rothbarth(pessoa.referencia = ref_deaton_2008_ss)

# 10. ESCALAS DE EQUIVALÊNCIA: POF 2008, todas as faixas etárias, com sexo ------------------------------------------------------------
# Adulto e Criança, POF2008, com sexo
pof_ac_2008_cs.rothbarth.sample %>%
  iv.engel.rothbarth.econ_scale(
    # iv.engel.rothbarth(
    welfare.indicator = rothbarth.welfare.indicator_pof2008,
    expenditure = expenditure_pof2008,
    iv.expenditure = iv.expenditure_pof2008,
    qtd_morador = qtd_moradores_pof2008,
    
    control = control_pof2008,
    weights = T,
    weights.var = weights.var_pof2008,
    show.diagnostics = T
  ) %>%
  # iv.engel.rothbarth.quad(weights = T, weights.var = wgt2008,
  #                    iv.expenditure = 'renda_per.capita',
  #                    show.diagnostics = T) %>%
  # 
  fix.heteroskedasticity(.) %>%
  equivalence.scales.engel.rothbarth(pessoa.referencia = ref_ac_2008_cs)

# Normais, POF2008, com sexo  
pof_normais_2008_cs.rothbarth.sample %>%
  iv.engel.rothbarth.econ_scale(
    # iv.engel.rothbarth(
    welfare.indicator = rothbarth.welfare.indicator_pof2008,
    expenditure = expenditure_pof2008,
    iv.expenditure = iv.expenditure_pof2008,
    qtd_morador = qtd_moradores_pof2008,
    
    control = control_pof2008,
    weights = T,
    weights.var = weights.var_pof2008,
    show.diagnostics = T
  ) %>%
  fix.heteroskedasticity(.) %>%
  equivalence.scales.engel.rothbarth(pessoa.referencia = ref_normais_2008_cs)

# Vaz & Vaz, POF2008, com sexo
pof_vaz_2008_cs.rothbarth.sample %>%
  iv.engel.rothbarth.econ_scale(
    # iv.engel.rothbarth(
    welfare.indicator = rothbarth.welfare.indicator_pof2008,
    expenditure = expenditure_pof2008,
    iv.expenditure = iv.expenditure_pof2008,
    qtd_morador = qtd_moradores_pof2008,
    
    control = control_pof2008,
    weights = T,
    weights.var = weights.var_pof2008,
    show.diagnostics = T
  ) %>%
  # iv.engel.rothbarth.quad(weights = T, weights.var = wgt2008,
  #                         iv.expenditure = 'renda_per.capita',
  #                         show.diagnostics = T) %>%
  # 
  fix.heteroskedasticity(.) %>%
  equivalence.scales.engel.rothbarth(pessoa.referencia = ref_vaz_2008_cs)

# Deaton, POF2008, com sexo
pof_deaton_2008_cs.rothbarth.sample %>%
  iv.engel.rothbarth.econ_scale(
    # iv.engel.rothbarth(
    welfare.indicator = rothbarth.welfare.indicator_pof2008,
    expenditure = expenditure_pof2008,
    iv.expenditure = iv.expenditure_pof2008,
    qtd_morador = qtd_moradores_pof2008,
    
    control = control_pof2008,
    weights = T,
    weights.var = weights.var_pof2008,
    show.diagnostics = T
  ) %>%
  fix.heteroskedasticity(.) %>%
  equivalence.scales.engel.rothbarth(pessoa.referencia = ref_deaton_2008_cs)
