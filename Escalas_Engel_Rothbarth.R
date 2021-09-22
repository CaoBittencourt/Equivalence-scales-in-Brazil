# 1. PACOTES
pkg <- c('sandwich', 'lmtest', 'np', 'Matching', 'systemfit', 'ivreg', 'plyr', 'glue', 'tidyverse')

lapply(pkg, function(x)
  if(!require(x, character.only = T))
  {install.packages(x); require(x)})

# lapply(pkg, function(x)
#   {citation(package = x)})


# 2. REGRESSÃO DE ENGEL E ROTHBARTH FLEXÍVEL
lm.engel.rothbarth <- function(df, welfare.indicator = 'share_despesas.mensais.alimentacao',
                               expenditure = 'despesas.mensais.totais_per.capita',
                               control = c('UF_sigla', 'desc_urbano')
                               # , .weights = 'fator_expansao1'
){
  
  df %>%
    select(contains('anos')) %>% 
    names(.) %>% 
    {glue('`{.}`')} %>% 
    paste(collapse = '+') -> classes.etarias
  
  paste(control, collapse = '+') %>%
    {glue('{welfare.indicator} ~ ',
          '+ {classes.etarias}',
          '+ log({expenditure})',
          '- {expenditure}',
          '+ {.}')} %>%
    as.formula(.) -> f
  
  lm(formula = f, data = df) %>%
    return(.)
  
}

# 3. REGRESSÃO DE ENGEL E ROTHBARTH FLEXÍVEL INSTRUMENTAL EM DOIS ESTÁGIOS
iv2sls.engel.rothbarth <- function(df, welfare.indicator = 'share_despesas.mensais.alimentacao',
                                   expenditure = 'despesas.mensais.totais_per.capita',
                                   iv.expenditure = 'renda_total_per.capita',
                                   control = c('UF_sigla', 'desc_urbano')
                                   # , .weights = 'fator_expansao1'
){
  
  df %>%
    select(contains('anos')) %>% 
    names(.) %>% 
    {glue('`{.}`')} %>% 
    paste(collapse = '+') -> classes.etarias
  
  
  control %>% 
    paste(collapse = '+') %>%
    {glue('| {classes.etarias}',
          '+ log({iv.expenditure})',
          '- {iv.expenditure}',
          '+ {.}')} -> iv
  
  control %>% 
  paste(collapse = '+') %>%
    {glue('{welfare.indicator} ~ ',
          '+ {classes.etarias}',
          '+ log({expenditure})',
          '- {expenditure}',
          '+ {.}')} %>%
    paste(iv) %>%
    as.formula(.) -> iv.f
  
  ivreg(formula = iv.f, data = df) %>% 
    return(.)
}


# 3. DIAGNÓSTICO E CONSERTO DE HETEROSCEDASTICIDADE
lm.heteroskedasticity <- function(model, .type = 'HC3',
                                  significance = 0.05){
  
  if(bptest(model)$p.value <= significance){
    model %>%
      coeftest(vcov = vcovHC(., type = .type))}
  else
    model
}

# 4. ESCALAS DE EQUIVALÊNCIA DE ENGEL E ROTHBARTH
equivalence.scales.dudel <- function(model, 
                                     pessoa.referencia, 
                                     na.h = 2, nc.h = 1,
                                     na.r = 2, nc.r = 0,
                                     nr = na.r + nc.r,
                                     nh = na.h + nc.h,
                                     expenditure = 'despesas.mensais.totais_per.capita'){
  
  model %>% 
    broom::tidy(.) %>% 
    mutate(log.expenditure.estimate = filter(.,term == glue('log({expenditure})')) %>% pull(estimate),
           coef.rel = estimate/log.expenditure.estimate) %>%
    filter(grepl('anos', term)) %>%
    mutate(ref.estimate.rel = filter(.,term == pessoa.referencia) %>% pull(coef.rel),
           equivalence.scale = ref.estimate.rel*(na.r - na.h) + coef.rel*(nc.r - nc.h),
           equivalence.scale = exp(equivalence.scale)*(nh/nr),
           member.cost = (equivalence.scale - 1)*(nr/nc.h))
}