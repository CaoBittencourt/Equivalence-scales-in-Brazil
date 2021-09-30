# 1. PACOTES --------------------------------------------------------------
pkg <- c('sandwich', 'lmtest', 'np', 'Matching', 'systemfit', 'ivreg', 'plyr', 'glue', 'tidyverse')

lapply(pkg, function(x)
  if(!require(x, character.only = T))
  {install.packages(x); require(x)})

# lapply(pkg, function(x)
#   {citation(package = x)})



# 2. FUNÇÃO DE REGRESSÃO OLS (ENGEL E ROTHBARTH) FLEXÍVEL ---------------------
# [ENDOGENEIDADE MUITO FORTE => NÃO UTILIZAR OLS]
# lm.engel.rothbarth <- function(
#   df, welfare.indicator = 'share_despesas.mensais.alimentacao',
#   expenditure = 'despesas.mensais.totais_per.capita',
#   log.expenditure = T,
#   control = c('UF_sigla', 'urbano')
#   # , .weights = 'fator_expansao1'
# ){
#   
#   if(log.expenditure){
#     
#     df %>%
#       filter(!!sym(expenditure) > 0) %>%
#       mutate(
#         !!sym(expenditure) := log(!!sym(expenditure))
#       ) -> df
#     
#   }
#   
#   df %>%
#     select(contains(']')) %>% 
#     names(.) %>% 
#     {glue('`{.}`')} %>%
#     paste(collapse = '+') -> classes.etarias
#   
#   paste(control, collapse = '+') %>%
#     {glue('{welfare.indicator} ~ ',
#           '{classes.etarias}',
#           '+ {expenditure}',
#           '+ {.}')} %>%
#     as.formula(.) -> f
#   
#   lm(formula = f, data = df) %>%
#     return(.)
#   
# }
# 
# lm.engel.rothbarth2 <- function(
#   df, welfare.indicator = 'share_despesas.mensais.alimentacao',
#   expenditure = 'despesas.mensais.totais_per.capita',
#   log.expenditure = T,
#   control = c('UF_sigla', 'urbano'),
#   qtd_morador = 'qtd_morador_domc'
#   # , .weights = 'fator_expansao1'
# ){
#   
#   if(log.expenditure){
#     
#     df %>%
#       filter(!!sym(expenditure) > 0) %>%
#       mutate(
#         !!sym(expenditure) := log(!!sym(expenditure))
#       ) -> df
#     
#   }
#   
#   df %>% 
#     mutate(
#       across(.cols = contains(']'),
#              ~ . / !!sym(qtd_morador)
#       ),
#       
#       !!sym(qtd_morador) := log(!!sym(qtd_morador))
#       
#     ) -> df
#   
#   df %>%
#     select(contains(']')) %>% 
#     names(.) %>% 
#     {glue('`{.}`')} %>%
#     paste(collapse = '+') -> classes.etarias
#   
#   paste(control, collapse = '+') %>%
#     {glue('{welfare.indicator} ~ ',
#           '{classes.etarias}',
#           '+ {expenditure}',
#           '+ {qtd_morador}',
#           '+ {.}')} %>%
#     as.formula(.) -> f
#   
#   lm(formula = f, data = df) %>%
#     return(.)
#   
# }



# 3. FUNÇÃO DE REGRESSÃO 2SLS (ENGEL E ROTHBARTH) FLEXÍVEL ----------------
iv.engel.rothbarth <- function(
  df, 
  welfare.indicator,
  expenditure,
  iv.expenditure,
  show.diagnostics = F,
  control,
  weights = T,
  weights.var
){
  
  # Pesos amostrais
  if(weights){
    
    df %>% 
      mutate(
        wgt = !!sym(weights.var)
      ) -> df
    
  }
  
  # log(expenditure)
  df %>%
    filter(!!sym(expenditure) > 0,
           !!sym(iv.expenditure) > 0) %>%
    mutate(
      !!sym(expenditure) := log(!!sym(expenditure)),
      !!sym(iv.expenditure) := log(!!sym(iv.expenditure))
    ) -> df
  
  # Classes de sexo e idade
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
  
  # Regressão com pesos amostrais
  if(weights){
    
    ivreg(
      formula = f, 
      data = df,
      weights = wgt
    ) -> ivreg.engel.rothbarth
    
  }
  # Regressão sem pesos amostrais
  else {
    
    ivreg(
      formula = f, 
      data = df
    ) -> ivreg.engel.rothbarth
    
  }
  
  # Teste de endogeneidade e validade da variável instrumental
  if(show.diagnostics){
    
    ivreg.engel.rothbarth %>% 
      summary(diagnostics = T) %>%
      print(.)
    
  }
  
  return(ivreg.engel.rothbarth)
  
}

iv.engel.rothbarth.quad <- function(
  df, 
  welfare.indicator = 'share_despesas.mensais.alimentacao',
  expenditure = 'despesas.mensais.totais_per.capita',
  iv.expenditure = 'renda_total_per.capita',
  show.diagnostics = F,
  control = c('UF_sigla', 'urbano'),
  weights = T,
  weights.var = 'fator_expansao1'
){
  
  # Pesos amostrais
  if(weights){
    
    df %>% 
      mutate(
        wgt = !!sym(weights.var)
      ) -> df
    
  }
  
  # log(expenditure)
  df %>%
    filter(!!sym(expenditure) > 0,
           !!sym(iv.expenditure) > 0) %>%
    mutate(
      !!sym(expenditure) := log(!!sym(expenditure)),
      !!sym(iv.expenditure) := log(!!sym(iv.expenditure)),
      
      # Termos quadráticos
      !!sym(glue('quad_{expenditure}')) := (!!sym(expenditure))^2,
      !!sym(glue('quad_{iv.expenditure}')) := (!!sym(iv.expenditure))^2
      
    ) -> df
  
  # Classes de sexo e idade
  df %>%
    select(contains(']')) %>%
    names(.) %>%
    {glue('`{.}`')} %>%
    paste(collapse = '+') -> classes.etarias
  
  paste(control, collapse = '+') %>%
    {glue('{welfare.indicator} ~ ', #Regressão normal
          '{classes.etarias}',
          '+ {expenditure}',
          '+ quad_{expenditure}',
          '+ {.} |', #Regressão instrumental a partir de "|"
          '{classes.etarias}',
          '+ {iv.expenditure}',
          '+ quad_{iv.expenditure}',
          '+ {.}')} %>%
    as.formula(.) -> f
  
  # Regressão com pesos amostrais
  if(weights){
    
    ivreg(
      formula = f, 
      data = df,
      weights = wgt
    ) -> ivreg.engel.rothbarth
    
  }
  # Regressão sem pesos amostrais
  else {
    
    ivreg(
      formula = f, 
      data = df
    ) -> ivreg.engel.rothbarth
    
  }
  
  # Teste de endogeneidade e validade da variável instrumental
  if(show.diagnostics){
    
    ivreg.engel.rothbarth %>% 
      summary(diagnostics = T) %>%
      print(.)
    
  }
  
  return(ivreg.engel.rothbarth)
  
}

iv.engel.rothbarth.econ_scale <- function(
  df, 
  welfare.indicator,
  expenditure,
  iv.expenditure,
  qtd_morador,
  show.diagnostics = F,
  control,
  weights = T,
  weights.var
){
  
  # Pesos amostrais
  if(weights){
    
    df %>% 
      mutate(
        wgt = !!sym(weights.var)
      ) -> df
    
  }
  
  # log(expenditure)
  df %>%
    filter(!!sym(expenditure) > 0,
           !!sym(iv.expenditure) > 0) %>%
    mutate(
      !!sym(expenditure) := log(!!sym(expenditure)),
      !!sym(iv.expenditure) := log(!!sym(iv.expenditure))
    ) -> df
  
  # log(qtd_morador)
  df %>% 
    mutate(
      !!sym(qtd_morador) := log(!!sym(qtd_morador))
    ) -> df
  
  # Classes de sexo e idade
  df %>%
    select(contains(']')) %>%
    names(.) %>%
    {glue('`{.}`')} %>%
    paste(collapse = '+') -> classes.etarias
  
  paste(control, collapse = '+') %>%
    {glue('{welfare.indicator} ~ ', #Regressão normal
          '{classes.etarias}',
          '+ {expenditure}',
          '+ {qtd_morador}',
          '+ {.} |', #Regressão instrumental a partir de "|"
          '{classes.etarias}',
          '+ {iv.expenditure}',
          '+ {qtd_morador}',
          '+ {.}')} %>%
    as.formula(.) -> f
  
  # Regressão com pesos amostrais
  if(weights){
    
    ivreg(
      formula = f, 
      data = df,
      weights = wgt
    ) -> ivreg.engel.rothbarth
    
  }
  # Regressão sem pesos amostrais
  else {
    
    ivreg(
      formula = f, 
      data = df
    ) -> ivreg.engel.rothbarth
    
  }
  
  # Teste de endogeneidade e validade da variável instrumental
  if(show.diagnostics){
    
    ivreg.engel.rothbarth %>% 
      summary(diagnostics = T) %>%
      print(.)
    
  }
  
  return(ivreg.engel.rothbarth)
  
}

# 3. FUNÇÃO DE DIAGNÓSTICO E CONSERTO DE HETEROSCEDASTICIDADE -----------------------
fix.heteroskedasticity <- function(
  model, 
  .type = 'HC3',
  significance = 0.05
){
  
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
  na.r = 2, nc.r = 0,
  significance.level = 0.05
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
      
      # equivalence.scale = ref.estimate.rel*(na.r - na.h) + coef.rel*(nc.r - nc.h),
      equivalence.scale = ref.estimate.rel*(na.h - na.r) + coef.rel*(nc.h - nc.r), # Deaton e Muellbauer (1986) escrevem a escala de equivalência como Dudel (2021), i.e. com xh - xr
      equivalence.scale = exp(equivalence.scale)*(nh/nr),
      member.cost = (equivalence.scale - 1)*(nr/nc.h),
      
      !!sym(glue('p.value<={significance.level}')) := p.value <= significance.level
    ) %>% 
    select(
      term, 
      std.error,
      p.value,
      !!sym(glue('p.value<={significance.level}')),
      equivalence.scale,
      member.cost
    ) %>%
    return(.)
}

equivalence.scales.engel.rothbarth.econ_scale <- function(
  model, 
  expenditure = 'despesas.mensais.totais_per.capita',
  qtd_morador = 'n_morador',
  pessoa.referencia, 
  na.h = 2, nc.h = 1,
  na.r = 2, nc.r = 0,
  significance.level = 0.05
){
  
  nr <- na.r + nc.r
  nh <- na.h + nc.h
  
  model %>% 
    broom::tidy(.) %>% 
    mutate(
      expenditure.estimate = filter(.,term == expenditure) %>% 
        pull(estimate),
      
      qtd_morador.estimate = filter(.,term == qtd_morador) %>% 
        pull(estimate),
      
      coef.rel = estimate/expenditure.estimate
      
    ) %>%
    mutate(
      ref.estimate.rel = filter(.,term == pessoa.referencia) %>% 
        pull(coef.rel),
      
      qtd_morador.estimate.rel = filter(.,term == qtd_morador) %>% 
        pull(coef.rel)
    ) %>%
    filter(grepl(']', term)) %>%
    mutate(
      # equivalence.scale = ref.estimate.rel*(na.r - na.h) + coef.rel*(nc.r - nc.h),
      equivalence.scale = ref.estimate.rel*(na.h - na.r) + coef.rel*(nc.h - nc.r) + qtd_morador.estimate.rel*log(nh/nr), # Deaton e Muellbauer (1986) escrevem a escala de equivalência como Dudel (2021), i.e. com xh - xr
      equivalence.scale = exp(equivalence.scale)*(nh/nr),
      member.cost = (equivalence.scale - 1)*(nr/nc.h),
      economies.scale = 1 + qtd_morador.estimate.rel,
      
      !!sym(glue('p.value<={significance.level}')) := p.value <= significance.level
    ) %>% 
    select(
      term, 
      std.error,
      p.value,
      !!sym(glue('p.value<={significance.level}')),
      equivalence.scale,
      member.cost,
      economies.scale
    ) %>%
    return(.)
}

