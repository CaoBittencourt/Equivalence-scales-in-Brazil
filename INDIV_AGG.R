# 1. PACOTES
pkg <- c('plyr', 'glue', 'tidyverse')

lapply(pkg, function(x)
  if(!require(x, character.only = T))
  {install.packages(x); require(x)})

select <- dplyr::select
mutate <- dplyr::mutate
summarise <- dplyr::summarise

tr1_4.pof2008[[1]] %>% names(.)
# Var. interesse = cod_cond_ocup
tr1_4.pof2008[[2]] %>% names(.)

# 2. REGISTROS
# 2.1. INDIVÍDUOS
# individuos.agg_fun <- function(individuos, faixas.etarias, sexo = F, 
#                                
#                                identificacao.domc = c('cod_uf', 'num_seq',
#                                                       'num_dv', 'cod_domc'),
#                                
#                                list.var.recode = list('cod_rel_pess_refe_uc' = list('1' = 'chefe',
#                                                                                     '2' = 'conjuge',
#                                                                                     '3' = 'filhos',
#                                                                                     '4' = 'outros.parentes',
#                                                                                     '5' = 'agregados',
#                                                                                     '6' = 'pensionistas',
#                                                                                     '7' = 'empregados',
#                                                                                     '8' = 'parentes.empregados'),
#                                                       'cod_sexo' = list('1' = 'Homens',
#                                                                         .default = 'Mulheres'),
#                                                       'cod_sabe_ler' = list('1' = 'Sabe ler',
#                                                                             .default = 'Não sabe ler'),
#                                                       'cod_cor_raca' = list('1' = 'Branca',
#                                                                             '2' = 'Preta',
#                                                                             '3' = 'Amarela',
#                                                                             '4' = 'Parda',
#                                                                             '5' = 'Indígena',
#                                                                             '9' = 'Não sabe'),
#                                                       'cod_tem_cartao' = list('1' = 'Tem cartão de crédito',
#                                                                               .default = 'Não tem cartão de crédito'),
#                                                       'plano_saude' = list('1' = 'Tem plano de saúde',
#                                                                            .default = 'Não plano de saúde')),
#                                
#                                var.relacao.pessoa.ref = 'cod_rel_pess_refe_uc',
#                                var.sexo = 'cod_sexo',
#                                var.idade = 'idade_anos',
#                                
#                                var.ler_escrever = 'cod_sabe_ler',
#                                
#                                var.interesse_domc = c(),
#                                var.interesse_indv = c('cod_sabe_ler', 'cod_cor_raca', 
#                                                       'cod_tem_cartao', 'plano_saude'),
#                                var.interesse_pessoa.ref = c()
# ){ 
#   # Recode: descrição de variáveis de interesse
#   # Obs: forma flexível (toma uma lista de listas e passa o recode descrito na lista meta-programaticamente)
#   list.var.recode %>% 
#     imap(
#       ~ quo(recode(!!sym(.y), !!!.x)
#       )
#     ) -> recode.expressions
#   
#   individuos %>% 
#     mutate(!!!recode.expressions) -> individuos
#   
#   
#   # Número de pessoas conforme o parentesco
#   # Variáveis de interesse individuais e domiciliares
#   # Agregação do domicílio
#   # individuos %>%
#   #   pivot_wider(id_cols = identificacao.domc,
#   #               names_from = var.relacao.pessoa.ref,
#   #               values_from = 1,
#   #               values_fn = length,
#   #               values_fill = 0,
#   #               names_prefix = 'qtd_') -> individuos.agg_parentesco
#   
#   # Número de indivíduos de cada classe de sexo e idade
#   # if(sexo == T)
#   #   individuos %>%
#   #   mutate(idade = cut(.data[[var.idade]],
#   #                      breaks = faixas.etarias,
#   #                      include.lowest = T,
#   #                      right = T,
#   #                      na.rm = T)) %>%
#   #   pivot_wider(id_cols = identificacao.domc,
#   #               names_from = c(desc_sexo, idade),
#   #               names_glue = '{desc_sexo}: {idade} anos',
#   #               names_sort = T,
#   #               values_from = 1,
#   #               values_fn = length,
#   #               values_fill = 0) -> individuos.agg_sexo.idade
#   # 
#   # # Número de indivíduos de cada classe de sexo e idade
#   # else
#   #   individuos %>%
#   #   mutate(idade = cut(.data[[var.idade]],
#   #                      breaks = faixas.etarias,
#   #                      include.lowest = T,
#   #                      right = T,
#   #                      na.rm = T)) %>%
#   #   pivot_wider(id_cols = identificacao.domc,
#   #               names_from = idade,
#   #               names_glue = '{idade} anos',
#   #               names_sort = T,
#   #               values_from = 1,
#   #               values_fn = length,
#   #               values_fill = 0) -> individuos.agg_sexo.idade
#   
#   # Variáveis de interesse individuais (fazer dummies com o somatório de cada uma, e.g. quantas pessoas sabem ler e escrever)
#   # individuos %>%
#   # group_by(!!!syms(ver.interesse_indv)) %>%
#   var.interesse_indv
#   
#     individuos %>% 
#       pivot_wider(id_cols = identificacao.domc,
#                   )
# 
#   #     if(empty(individuos.agg_var.domc) == F)
#   #       merge(individuos.agg_parentesco,
#   #             individuos.agg_var.domc)
#   # 
#   #     if(empty(individuos.agg_var.indv) == F)
#   #       merge(individuos.agg_parentesco,
#   #             individuos.agg_var.indv)
#   
#   # GERAL
#   # merge(individuos.agg_parentesco,
#   #       individuos.agg_sexo.idade)
#   # %>% return(.)
# }

individuos.agg_fun <- function(individuos, faixas.etarias, sexo = F, 
                               
                               identificacao.domc = c('cod_uf', 'num_seq',
                                                      'num_dv', 'cod_domc'),
                               
                               list.var.recode = list('cod_rel_pess_refe_uc' = list('1' = 'chefe',
                                                                                    '2' = 'conjuge',
                                                                                    '3' = 'filhos',
                                                                                    '4' = 'outros.parentes',
                                                                                    '5' = 'agregados',
                                                                                    '6' = 'pensionistas',
                                                                                    '7' = 'empregados',
                                                                                    '8' = 'parentes.empregados'),
                                                      'cod_sexo' = list('1' = 'Homens',
                                                                        .default = 'Mulheres'),
                                                      'cod_sabe_ler' = list('1' = 'Sabe ler',
                                                                            .default = 'Não sabe ler'),
                                                      'cod_cor_raca' = list('1' = 'Branca',
                                                                            '2' = 'Preta',
                                                                            '3' = 'Amarela',
                                                                            '4' = 'Parda',
                                                                            '5' = 'Indígena',
                                                                            '9' = 'Não sabe'),
                                                      'cod_tem_cartao' = list('1' = 'Tem cartão de crédito',
                                                                              .default = 'Não tem cartão de crédito'),
                                                      'plano_saude' = list('1' = 'Tem plano de saúde',
                                                                           .default = 'Não plano de saúde')),
                               
                               var.relacao.pessoa.ref = 'cod_rel_pess_refe_uc',
                               var.sexo = 'cod_sexo',
                               var.idade = 'idade_anos',
                               var.ler_escrever = 'cod_sabe_ler',
                               
                               # var.interesse_domc = c(),
                               var.interesse_indv = c('cod_sabe_ler', 'cod_cor_raca', 
                                                      'cod_tem_cartao', 'plano_saude')
                               # , var.interesse_pessoa.ref = c()
){ 
  # Recode: descrição de variáveis de interesse
  # Obs: forma flexível (toma uma lista de listas e passa o recode descrito na lista meta-programaticamente)
  list.var.recode %>% 
    imap(
      ~ quo(recode(!!sym(.y), !!!.x)
      )
    ) -> recode.expressions
  
  individuos %>% 
    mutate(!!!recode.expressions) -> individuos
  
  
  # Número de pessoas conforme o parentesco
  # Variáveis de interesse individuais e domiciliares
  # Agregação do domicílio
  individuos %>%
    pivot_wider(id_cols = identificacao.domc,
                names_from = var.relacao.pessoa.ref,
                values_from = 1,
                values_fn = length,
                values_fill = 0,
                names_prefix = 'qtd_') -> individuos.agg_parentesco
  
  # Número de indivíduos de cada classe de sexo e idade
  # if(sexo == T)
  #   individuos %>%
  #   mutate(idade = cut(.data[[var.idade]],
  #                      breaks = faixas.etarias,
  #                      include.lowest = T,
  #                      right = T,
  #                      na.rm = T)) %>%
  #   pivot_wider(id_cols = identificacao.domc,
  #               names_from = c(desc_sexo, idade),
  #               names_glue = '{desc_sexo}: {idade} anos',
  #               names_sort = T,
  #               values_from = 1,
  #               values_fn = length,
  #               values_fill = 0) -> individuos.agg_sexo.idade
  # 
  # # Número de indivíduos de cada classe de sexo e idade
  # else
  #   individuos %>%
  #   mutate(idade = cut(.data[[var.idade]],
  #                      breaks = faixas.etarias,
  #                      include.lowest = T,
  #                      right = T,
  #                      na.rm = T)) %>%
  #   pivot_wider(id_cols = identificacao.domc,
  #               names_from = idade,
  #               names_glue = '{idade} anos',
  #               names_sort = T,
  #               values_from = 1,
  #               values_fn = length,
  #               values_fill = 0) -> individuos.agg_sexo.idade
  
  # Variáveis de interesse individuais (fazer dummies com o somatório de cada uma, e.g. quantas pessoas sabem ler e escrever)
  # individuos %>%
  # group_by(!!!syms(ver.interesse_indv)) %>%
  
  # if(missing(var.interesse_indv) == F)
    lapply(var.interesse_indv, function(var.individual)

      individuos %>%
        pivot_wider(id_cols = identificacao.domc,
                    names_from = var.individual,
                    values_from = 1,
                    values_fn = length,
                    values_fill = 0,
                    names_prefix = 'qtd_')) %>% 
    bind_cols(individuos.agg_parentesco,
              # .id = 
              .names_repair = 'unique') %>% return(.) 
  
  #     if(empty(individuos.agg_var.domc) == F)
  #       merge(individuos.agg_parentesco,
  #             individuos.agg_var.domc)
  # 
  # if(empty(individuos.agg_var.indv) == F)
  #   merge(individuos.agg_parentesco,
  #         individuos.agg_var.indv)
  
  # GERAL
  # merge(individuos.agg_parentesco,
  #       individuos.agg_sexo.idade)
  # %>% return(.)
}



tr1_4.pof2008[[2]] %>%
  individuos.agg_fun(faixas.etarias = NULL) %>% head(.)



# 2.2. OUTROS REGISTROS
# outros.registros.agg_fun <- function(registro, identificacao.domc = c('cod_uf', 'num_seq',
#                                                                       'num_dv', 'cod_domc'),
#                                      
#                                      
#                                      # var.relacao.pessoa.ref = 'cod_rel_pess_refe_uc',
#                                      # var.sexo = 'cod_sexo',
#                                      # var.idade = 'idade_anos',
#                                      
#                                      var.interesse_domc = c('cod_cond_ocup'),
#                                      var.interesse_pessoa.ref = c()){
#   
#   registro %>% 
#     mutate(desc_cond_ocup_imovel = recode(.data[[cod_cond_ocup]], #talvez pode virar um argumento
#                                           '1' = 'Imóvel próprio (já pago)',
#                                           '2' = 'Imóvel próprio (em pagamento)',
#                                           '3' = 'Imóvel cedido (por empregador)',
#                                           '4' = 'Imóvel cedido (outra forma)',
#                                           '5' = 'Outras condições de ocupação',
#                                           '6' = 'Imóvel alugado'))
#   
#   
#   
#   
#   
# }


