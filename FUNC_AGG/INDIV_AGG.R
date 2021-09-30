# 1. PACOTES --------------------------------------------------------------
pkg <- c('plyr', 'glue', 'tidyverse')

lapply(pkg, function(x)
  if(!require(x, character.only = T))
  {install.packages(x); require(x)})

# Citação dos pacotes
# lapply(pkg, function(x)
#   {citation(package = x)})

select <- dplyr::select
mutate <- dplyr::mutate
recode <- dplyr::recode
summarise <- dplyr::summarise


# 2. RECODE (FUNÇÃO) ------------------------------------------------------
df.recode <- function(df, list.recode){
  
  list.recode %>% 
    imap(
      ~ quo(
        dplyr::recode(!!sym(.y), !!!.x)
        # recode(!!sym(.y), !!!.x)
      )
    ) -> recode.expressions
  
  mutate(df, !!!recode.expressions) %>% return(.)
  
  
}
# 3. AGREGAÇÃO DE DOMICÍLIOS - INDIVÍDUOS (FUNÇÃO) ------------------------------------
individuos.agg_fun <- function(
  df_individuos, 
  faixas.etarias, 
  sexo = F, 
  
  identificacao.domc,
  
  list.var.recode,
  
  var.relacao.pessoa.ref,
  var.sexo,
  var.idade,
  var.anos_estudo,
  
  var.interesse_chefe.fam,
  
  var.interesse_indv
  # , var.interesse_indv.perct = F
  
){ 
  # [FEITO]
  # Recode: descrição de variáveis de interesse
  # Obs: forma flexível (toma uma lista de listas e passa o recode descrito na lista meta-programaticamente)
  df_individuos %>%
    df.recode(list.var.recode) -> df_individuos
  
  # [FEITO]
  # Problemas em anos de estudo: quando uma pessoa não sabe quantos anos de estudo, o IBGE coloca um número elevado (e.g. 99, 80) ao invés de NAN
  # => Outliers não significativos 
  df_individuos %>% 
    mutate(
      !!sym(var.anos_estudo) := replace(!!sym(var.anos_estudo), 
                                        !!sym(var.anos_estudo) >= 50, NA)
    ) -> df_individuos
  
  # [FEITO]
  # Número de pessoas conforme o parentesco
  df_individuos %>%
    pivot_wider(id_cols = identificacao.domc,
                names_from = var.relacao.pessoa.ref,
                values_from = 1,
                values_fn = length,
                values_fill = 0,
                names_prefix = 'qtd_') -> df_individuos.agg_parentesco
  
  # [FEITO]
  # Número de indivíduos de cada classe de sexo e idade
  if(sexo)
    df_individuos %>%
    mutate(idade = cut(.data[[var.idade]],
                       breaks = faixas.etarias,
                       include.lowest = T,
                       right = T,
                       na.rm = T)) %>%
    pivot_wider(id_cols = identificacao.domc,
                names_from = c(var.sexo, idade),
                names_sort = T,
                values_from = 1,
                values_fn = length,
                values_fill = 0) -> df_individuos.agg_sexo.idade
  
  # [FEITO]
  # Número de indivíduos de cada classe de sexo e idade
  else
    df_individuos %>%
    mutate(idade = cut(.data[[var.idade]],
                       breaks = faixas.etarias,
                       include.lowest = T,
                       right = T,
                       na.rm = T)) %>%
    pivot_wider(id_cols = identificacao.domc,
                names_from = idade,
                names_glue = '{idade} anos',
                names_sort = T,
                values_from = 1,
                values_fn = length,
                values_fill = 0) -> df_individuos.agg_sexo.idade
  
  # [FEITO]
  # Variáveis de interesse individuais (fazer variáveis com o somatório de cada uma, e.g. quantas pessoas sabem ler e escrever)
  
  lapply(
    var.interesse_indv,
    function(var){
      df_individuos %>%
        pivot_wider(
          id_cols = identificacao.domc,
          names_from = var,
          values_from = 1,
          values_fn = length,
          values_fill = 0,
          names_repair = 'unique',
          names_prefix = 'control.qtd_'
        ) -> df_individuos.temp
      
      merge(df_individuos.agg_parentesco,
            df_individuos.temp) ->> df_individuos.agg_parentesco
    }
  )
  
  # [FAZER ISSO TAMBÉM?]
  # VARIÁVEIS DE CONTROLE PERCENTUAIS (%Sabe Ler, %Cartão de Crédito etc)
  # if(var.interesse_indv.perct){
  #   
  #   df_individuos.agg_parentesco %>% 
  #     mutate(
  #       across(
  #         .cols = var.interesse_indv,
  #         ~ . / qtd_morador,
  #         .names = "percent_{.col}"
  #       )
  #     ) -> df_individuos.agg_parentesco
  # }
  
  # [FEITO]
  # VARIÁVEIS DE CONTROLE DO CHEFE DA FAMÍLIA 
  df_individuos %>% 
    filter(!!sym(var.relacao.pessoa.ref) == 'chefe') %>%
    select(
      identificacao.domc,
      var.interesse_chefe.fam
    ) %>%
    rename_with(
      .cols = var.interesse_chefe.fam,
      .fn = function(x){paste0('control.chefe_', x)}
    ) %>% 
    merge(df_individuos.agg_parentesco) -> df_individuos.agg_parentesco
  
  # GERAL
  merge(df_individuos.agg_parentesco,
        df_individuos.agg_sexo.idade) %>% return(.)
}

# 4. AGREGAÇÃO DE DOMICÍLIOS - MORADIA (FUNÇÃO) ----------------------------------------
moradia.agg_fun <- function(
  df_individuos.agg, # Para unir os resultados ao domicílio agregado (tr2)
  df_moradia, # Registro de moradia (tr1)
  
  identificacao.domc,
  
  list.var.recode,
  
  var.interesse_domc
  
){ 
  # [FEITO]
  # Recode: descrição de variáveis de interesse
  # Obs: forma flexível (toma uma lista de listas e passa o recode descrito na lista meta-programaticamente)
  df_moradia %>%
    df.recode(list.var.recode) -> df_moradia
  
  # [FEITO]
  # VARIÁVEIS DE CONTROLE DO DOMICÍLIO
  df_moradia %>% 
    select(
      identificacao.domc,
      var.interesse_domc
    ) %>%
    rename_with(
      .cols = var.interesse_domc,
      .fn = function(x){paste0('control.domc_', x)}
    ) %>% 
    merge(df_individuos.agg) %>%
    return(.)
}

