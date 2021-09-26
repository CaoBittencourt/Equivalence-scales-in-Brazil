# 1. PACOTES --------------------------------------------------------------
pkg <- c('sandwich', 'lmtest', 'np', 'Matching', 'systemfit', 'ivreg', 'plyr', 'glue', 'tidyverse')

lapply(pkg, function(x)
  if(!require(x, character.only = T))
  {install.packages(x); require(x)})

# lapply(pkg, function(x)
#   {citation(package = x)})



# 2. FUNÇÃO DE REGRESSÃO OLS (ENGEL E ROTHBARTH) FLEXÍVEL ---------------------
lm.engel.rothbarth <- function(
  df, welfare.indicator = 'share_despesas.mensais.alimentacao',
  expenditure = 'despesas.mensais.totais_per.capita',
  log.expenditure = T,
  control = c('UF_sigla', 'urbano')
  # , .weights = 'fator_expansao1'
){
  
  if(log.expenditure){
    
    df %>%
      filter(!!sym(expenditure) > 0) %>%
      mutate(
        !!sym(expenditure) := log(!!sym(expenditure))
      ) -> df
    
  }
  
  df %>%
    select(contains(']')) %>% 
    names(.) %>% 
    {glue('`{.}`')} %>%
    paste(collapse = '+') -> classes.etarias
  
  paste(control, collapse = '+') %>%
    {glue('{welfare.indicator} ~ ',
          '{classes.etarias}',
          '+ {expenditure}',
          '+ {.}')} %>%
    as.formula(.) -> f
  
  lm(formula = f, data = df) %>%
    return(.)
  
}



# 3. FUNÇÃO DE REGRESSÃO 2SLS (ENGEL E ROTHBARTH) FLEXÍVEL ----------------
iv.engel.rothbarth <- function(
  df, welfare.indicator = 'share_despesas.mensais.alimentacao',
  expenditure = 'despesas.mensais.totais_per.capita',
  iv.expenditure = 'renda_total_per.capita',
  log.expenditure = T,
  control = c('UF_sigla', 'urbano')
  # , .weights = 'fator_expansao1'
){
  
  if(log.expenditure){
    
    df %>%
      filter(!!sym(expenditure) > 0,
             !!sym(iv.expenditure) > 0) %>%
      mutate(
        !!sym(expenditure) := log(!!sym(expenditure)),
        !!sym(iv.expenditure) := log(!!sym(iv.expenditure))
      ) -> df
    
  }
  
  df %>%
    select(contains(']')) %>%
    names(.) %>%
    {glue('`{.}`')} %>%
    paste(collapse = '+') -> classes.etarias
  
  paste(control, collapse = '+') %>%
    {glue('{welfare.indicator} ~ ', #Regressão normal
          '{classes.etarias}',
          '+ {expenditure}',
          '+ {.} |', #Regressão instrumental a partir de "|"
          '{classes.etarias}',
          '+ {iv.expenditure}',
          '+ {.}')} %>%
    as.formula(.) -> f
  
  ivreg(formula = f, data = df) -> ivreg.engel.rothbarth
  
  print(summary(ivreg.engel.rothbarth,
                diagnostics = T))
  return(ivreg.engel.rothbarth)
  
}


# 3. FUNÇÃO DE DIAGNÓSTICO E CONSERTO DE HETEROSCEDASTICIDADE -----------------------
fix.heteroskedasticity <- function(model, .type = 'HC3',
                                   significance = 0.05){
  
  if(bptest(model)$p.value <= significance){
    model %>%
      coeftest(vcov = vcovHC(., type = .type))}
  else
    model
}


# 4. FUNÇÃO DE ESCALAS DE EQUIVALÊNCIA DE ENGEL E ROTHBARTH -------------------------
equivalence.scales.engel.rothbarth <- function(
  model, 
  expenditure = 'despesas.mensais.totais_per.capita',
  pessoa.referencia, 
  na.h = 2, nc.h = 1,
  na.r = 2, nc.r = 0
  
){
  
  nr <- na.r + nc.r
  nh <- na.h + nc.h
  
  model %>% 
    broom::tidy(.) %>% 
    mutate(
      expenditure.estimate = filter(.,term == expenditure) %>% 
        pull(estimate),
      
      coef.rel = estimate/expenditure.estimate
    ) %>%
    filter(grepl(']', term)) %>%
    mutate(
      ref.estimate.rel = filter(.,term == pessoa.referencia) %>% 
        pull(coef.rel),
      
      equivalence.scale = ref.estimate.rel*(na.r - na.h) + coef.rel*(nc.r - nc.h),
      # equivalence.scale = ref.estimate.rel*(- na.r + na.h) + coef.rel*(- nc.r + nc.h),
      equivalence.scale = exp(equivalence.scale)*(nh/nr),
      member.cost = (equivalence.scale - 1)*(nr/nc.h)
    ) %>% 
    select(
      term, 
      std.error,
      p.value,
      equivalence.scale,
      member.cost
    ) %>% return(.)
}

# PROBLEMAS: ESCALAS DE EQUIVALÊNCIA ABSURDAS, MAS FUNÇÕES FUNCIONANDO CORRETAMENTE
# SOLUÇÃO: REVER A FÓRMULA DA ESCALA DE EQUIVALÊNCIA + SELEÇÃO AMOSTRAL + VARIÁVEIS DE CONTROLE
# OBS: POSSIVELMENTE O RESULTADO NÃO ESTÁ ERRADO, SÓ A INTERPRETAÇÃO (comparação (2,1) vs (2,0) etc)?
# 5. TESTES ---------------------------------------------------------------
# pof_ac_2008_ss %>%
#   iv.engel.rothbarth(.) %>%
#   fix.heteroskedasticity(.) %>%
#   equivalence.scales.engel.rothbarth(pessoa.referencia = '`(14,104] anos`')
# 
# pof_ac_2008_ss %>%
#   iv.engel.rothbarth() %>%
#   fix.heteroskedasticity(.) %>%
#   equivalence.scales.engel.rothbarth(pessoa.referencia = '`(14,104] anos`',
#                                      na.h = 1, nc.h = 1, 
#                                      na.r = 1, nc.r = 0) %>% 
#   mutate(member.cost = member.cost/max(member.cost)) # Talvez seja isso?
# 
# pof_vaz_2002_ss %>%
#   mutate(
#     adult.goods = 
#       share_despesas.mensais.bebidas.alcoolicas +
#       share_despesas.mensais.vestuario.homem_mulher +
#       share_despesas.mensais.fumo + 
#       share_despesas.mensais.jogos_apostas
#   ) %>%
#   iv.engel.rothbarth(welfare.indicator = 'adult.goods',
#                      iv.expenditure = 'renda_per.capita') %>%
#   fix.heteroskedasticity(.) %>%
#   equivalence.scales.engel.rothbarth(pessoa.referencia = '`(14,110] anos`',
#                                      na.h = 1, nc.h = 1, 
#                                      na.r = 1, nc.r = 0) 
# 
# pof_ac_2008_ss %>%
#   lm.engel.rothbarth(.) %>%
#   fix.heteroskedasticity(.) %>%
#   equivalence.scales.engel.rothbarth(pessoa.referencia = '`(14,104] anos`')