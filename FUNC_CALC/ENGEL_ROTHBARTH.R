# 1. PACOTES --------------------------------------------------------------
c(
  'sandwich', 'lmtest', #Diagnósticos e ajustes
  'np', 'Matching', 'systemfit', 'ivreg', #Modelos 
  'plyr', 'glue', 'broom', 'purrr', 'tidyverse' #Manipulação de dados
) -> pkg

lapply(pkg, function(x)
  if(!require(x, character.only = T))
  {install.packages(x); require(x)})

# Citação dos pacotes
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



# 3. FUNÇÃO DE REtGRESSÃO 2SLS (ENGEL E ROTHBARTH) FLEXÍVEL ----------------
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
  
  if(is_empty(control)){
    glue('{welfare.indicator} ~ ', #Regressão normal
         '{classes.etarias}',
         '+ {expenditure}',
         '|', #Regressão instrumental a partir de "|"
         '{classes.etarias}',
         '+ {iv.expenditure}') %>%
      as.formula(.) -> f
  }
  else { 
    paste(control, collapse = '+') %>%
      {glue('{welfare.indicator} ~ ', #Regressão normal
            '{classes.etarias}',
            '+ {expenditure}',
            '+ {.}', 
            '|', #Regressão instrumental a partir de "|"
            '{classes.etarias}',
            '+ {iv.expenditure}',
            '+ {.}')} %>%
      as.formula(.) -> f
  }
  
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
  
  if(is_empty(control)){
    glue('{welfare.indicator} ~ ', #Regressão normal
         '{classes.etarias}',
         '+ {expenditure}',
         '|', #Regressão instrumental a partir de "|"
         '{classes.etarias}',
         '+ {iv.expenditure}') %>%
      as.formula(.) -> f
  }
  else { 
    paste(control, collapse = '+') %>%
      {glue('{welfare.indicator} ~ ', #Regressão normal
            '{classes.etarias}',
            '+ {expenditure}',
            '+ {.}', 
            '|', #Regressão instrumental a partir de "|"
            '{classes.etarias}',
            '+ {iv.expenditure}',
            '+ {.}')} %>%
      as.formula(.) -> f
  }
  
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

# 4. FUNÇÕES DE DIAGNÓSTICO E CONSERTO DE HETEROSCEDASTICIDADE -----------------------
fix.heteroskedasticity <- function(
  model, 
  .type = 'HC3',
  significance = 0.05
){
  
  if(bptest(model)$p.value <= significance){
    
    model %>%
      coeftest(
        vcov = vcovHC(., type = .type)
      )
    
  }
  else
    model
}

# 5. FUNÇÕES DE STD.ERRORS ROBUSTOS -----------------------
robust_std.errors <- function(
  model, 
  .type = 'HC3'
){
  
  model %>%
    vcovHC(type = .type) %>%
    diag(.) %>%
    sqrt(.) %>%
    return(.)
  
}

# 6. FUNÇÃO DE ESCALAS DE EQUIVALÊNCIA DE ENGEL E ROTHBARTH -------------------------
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
    filter(
      term != pessoa.referencia
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

equivalence.scales.engel.rothbarth.sem_per.capita <- function(
  model, 
  expenditure = 'despesas.mensais.totais',
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
      equivalence.scale = exp(equivalence.scale),
      member.cost = (equivalence.scale - 1)*(nr/nc.h),
      
      !!sym(glue('p.value<={significance.level}')) := p.value <= significance.level
    ) %>% 
    filter(
      term != pessoa.referencia
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

equivalence.scales.engel.rothbarth2 <- function(
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
      
      equivalence.scale = ref.estimate.rel*(na.r - na.h) + coef.rel*(nc.r - nc.h),
      # equivalence.scale = ref.estimate.rel*(na.h - na.r) + coef.rel*(nc.h - nc.r), # Deaton e Muellbauer (1986) escrevem a escala de equivalência como Dudel (2021), i.e. com xh - xr
      equivalence.scale = exp(equivalence.scale)*(nh/nr),
      member.cost = (equivalence.scale - 1)*(nr/nc.h),
      
      !!sym(glue('p.value<={significance.level}')) := p.value <= significance.level
    ) %>% 
    filter(
      term != pessoa.referencia
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

equivalence.scales.engel.rothbarth2.sem_per.capita <- function(
  model, 
  expenditure = 'despesas.mensais.totais',
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
      
      equivalence.scale = ref.estimate.rel*(na.r - na.h) + coef.rel*(nc.r - nc.h),
      # equivalence.scale = ref.estimate.rel*(na.h - na.r) + coef.rel*(nc.h - nc.r), # Deaton e Muellbauer (1986) escrevem a escala de equivalência como Dudel (2021), i.e. com xh - xr
      equivalence.scale = exp(equivalence.scale),
      member.cost = (equivalence.scale - 1)*(nr/nc.h),
      
      !!sym(glue('p.value<={significance.level}')) := p.value <= significance.level
    ) %>% 
    filter(
      term != pessoa.referencia
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

equivalence.scales.engel.rothbarth.econ_scale2 <- function(
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
      # economies.scale = 1 + qtd_morador.estimate.rel,
      economies.scale = 1 - qtd_morador.estimate.rel,
      
      !!sym(glue('p.value<={significance.level}')) := p.value <= significance.level
    ) %>% 
    filter(
      term != pessoa.referencia
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
      equivalence.scale = ref.estimate.rel*(na.r - na.h) + coef.rel*(nc.r - nc.h) - qtd_morador.estimate.rel*log(nh/nr), # Deaton e Muellbauer (1986) escrevem a escala de equivalência como Dudel (2021), i.e. com xh - xr
      equivalence.scale = exp(equivalence.scale)*(nh/nr),
      member.cost = (equivalence.scale - 1)*(nr/nc.h),
      # economies.scale = 1 + qtd_morador.estimate.rel,
      economies.scale = 1 - qtd_morador.estimate.rel,
      
      !!sym(glue('p.value<={significance.level}')) := p.value <= significance.level
    ) %>% 
    filter(
      term != pessoa.referencia
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


# # 7. FUNÇÃO DE STD. ERRORS DAS ESCALAS DE EQUIVALÊNCIA -------------------------
# scale.std_error <- function(
#   model,
#   expenditure = 'despesas.mensais.totais_per.capita',
#   qtd_morador
# ){
#   # Adaptado de DUDEL et al., 2020
#   # Standard error via Delta method
#   b1      <- coef(model)[expenditure]
#   b2      <- coef(model)[qtd_morador]
#   A       <- exp(-b2/b1)
#   
#   varb1   <- vcov(model)[expenditure, expenditure]
#   varb2   <- vcov(model)[qtd_morador, qtd_morador]
#   covb1b2 <- vcov(model)[expenditure, qtd_morador]
#   
#   dAdb1   <- A*(b2/b1^2)
#   dAdb2   <- A*(-1/b1)
#   
#   VarA    <- dAdb1^2 * varb1 + dAdb2^2 * varb2 + 2 * dAdb1 * dAdb2 * covb1b2
#   sdA     <- sqrt(VarA)
#   
#   return(sdA)
#   
# }

# 8. FUNÇÃO DE TABELAS DE REGRESSÃO ---------------------------------------

# Adjust standard errors
# cov1         <- vcovHC(model, type = "HC3")
# robust_se    <- sqrt(diag(cov1))
# 
# robust.se(teste.lista$pof_ac_2002_ss) %>% tidy(.)

# Stargazer output (with and without RSE)
# stargazer(model, model, type = "text",
#           se = list(NULL, robust_se))


# stargazer(teste.lista$pof_ac_2002_ss,
# type = 'text'
# se = coeftest(.),  
# se = list(NULL, robust_se)
# se = list(NULL, 
#           sqrt(
#             diag(
#               vcovHC(teste.lista$pof_ac_2002_ss, type = "HC3")
#             )
#           )
# )
# )


