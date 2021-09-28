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
df_recode <- function(df, list.recode){
  
  list.recode %>% 
    imap(
      ~ quo(
        dplyr::recode(!!sym(.y), !!!.x)
        # recode(!!sym(.y), !!!.x)
      )
    ) -> recode.expressions
  
  mutate(df, !!!recode.expressions) %>% return(.)
  
  
}
# 3. RECODES (NOMES DAS VARIÁVEIS) ----------------------------------------
# POF 2002
list(
  
  'rel_chefe' = list('1' = 'chefe',
                     '2' = 'conjuge',
                     '3' = 'filhos',
                     '4' = 'outros.parentes',
                     '5' = 'agregados',
                     '6' = 'pensionistas',
                     '7' = 'empregados',
                     '8' = 'parentes.empregados'),
  'sexo' = list('1' = 'Homens',
                .default = 'Mulheres'),
  # 'cod_sabe_ler' = list('1' = 'Sabe ler',
  #                       .default = 'Não sabe ler'),
  'cor' = list('1' = 'cor.Branca',
               '2' = 'cor.Preta',
               '3' = 'cor.Amarela',
               '4' = 'cor.Parda',
               '5' = 'cor.Indígena',
               .default = 'cor.Não sabe'),
  'cartao' = list('1' = 'Tem cartão de crédito',
                  .default = 'Não tem cartão de crédito'),
  'plano_saude' = list('1' = 'Tem plano de saúde',
                       .default = 'Não tem plano de saúde')
  
) -> list.var.recode_tr2.pof2002

# POF 2008
list(
  
  'cod_rel_pess_refe_uc' = list('1' = 'chefe',
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
  'cod_cor_raca' = list('1' = 'cor.Branca',
                        '2' = 'cor.Preta',
                        '3' = 'cor.Amarela',
                        '4' = 'cor.Parda',
                        '5' = 'cor.Indígena',
                        .default = 'cor.Não sabe'),
  'cod_tem_cartao' = list('1' = 'Tem cartão de crédito',
                          .default = 'Não tem cartão de crédito'),
  'plano_saude' = list('1' = 'Tem plano de saúde',
                       .default = 'Não tem plano de saúde')
  
) -> list.var.recode_tr2.pof2008


# 4. FUNÇÕES DE AGREGAÇÃO DE REGISTROS ------------------------------------
# 4.1. INDIVÍDUOS
individuos.agg_fun2 <- function(
  individuos, 
  faixas.etarias, 
  sexo = F, 
  
  identificacao.domc = c('cod_uf', 'num_seq',
                         'num_dv', 'cod_domc'),
  
  list.var.recode = list.var.recode_tr2.pof2008,
  
  var.relacao.pessoa.ref = 'cod_rel_pess_refe_uc',
  var.sexo = 'cod_sexo',
  var.idade = 'idade_anos',
  var.ler_escrever = 'cod_sabe_ler',
  var.anos_estudo = 'anos_de_estudo',
  
  var.interesse_chefe.fam = c(var.anos_estudo,
                              var.idade),
  # var.interesse_domc = c(),
  var.interesse_indv = c('cod_sabe_ler', 'cod_cor_raca',
                         'cod_tem_cartao', 'plano_saude')
  
  # , var.interesse_indv.perct = F
  
){ 
  # [FEITO]: FUNCIONANDO CORRETAMENTE
  # Recode: descrição de variáveis de interesse
  # Obs: forma flexível (toma uma lista de listas e passa o recode descrito na lista meta-programaticamente)
  individuos %>%
    df_recode(list.var.recode) -> individuos
  
  # [FEITO]: FUNCIONANDO CORRETAMENTE
  # Problemas em anos de estudo: quando uma pessoa não sabe quantos anos de estudo, o IBGE coloca um número elevado (e.g. 99, 80) ao invés de NAN
  # => Outliers não significativos 
  individuos %>% 
    mutate(
      !!sym(var.anos_estudo) := replace(!!sym(var.anos_estudo), 
                                        !!sym(var.anos_estudo) >= 50, NA)
    ) -> individuos
  
  # [FEITO]: FUNCIONANDO CORRETAMENTE
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
  
  # # [FEITO]: FUNCIONANDO CORRETAMENTE
  # # Número de indivíduos de cada classe de sexo e idade
  # if(sexo)
  #   individuos %>%
  #   mutate(idade = cut(.data[[var.idade]],
  #                      breaks = faixas.etarias,
  #                      include.lowest = T,
  #                      right = T,
  #                      na.rm = T)) %>%
  #   pivot_wider(id_cols = identificacao.domc,
  #               names_from = c(var.sexo, idade),
  #               names_sort = T,
  #               values_from = 1,
  #               values_fn = length,
  #               values_fill = 0) -> individuos.agg_sexo.idade
  # 
  # # [FEITO]: FUNCIONANDO CORRETAMENTE
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
  # 
  # # [QUASE FEITO]: NÃO URGENTE (VARIÁVEIS DE CONTROLE DE DIFERENTES TIPOS)
  # # [FEITO]
  # # Variáveis de interesse individuais (fazer variáveis com o somatório de cada uma, e.g. quantas pessoas sabem ler e escrever)
  # 
  #   lapply(
  #     var.interesse_indv,
  #     function(var){
  #       individuos %>%
  #         pivot_wider(
  #           id_cols = identificacao.domc,
  #           names_from = var,
  #           values_from = 1,
  #           values_fn = length,
  #           values_fill = 0,
  #           names_repair = 'unique',
  #           names_prefix = 'control.qtd_'
  #         ) -> individuos.temp
  #       
  #       merge(individuos.agg_parentesco, 
  #             individuos.temp) ->> individuos.agg_parentesco
  #     } 
  #   ) 
  
  # [FAZER ISSO TAMBÉM?]: VARIÁVEIS DE CONTROLE PERCENTUAIS (%Sabe Ler, %Cartão de Crédito etc)
  # if(var.interesse_indv.perct){
  #   
  #   individuos.agg_parentesco %>% 
  #     mutate(
  #       across(
  #         .cols = var.interesse_indv,
  #         ~ . / qtd_morador,
  #         .names = "percent_{.col}"
  #       )
  #     ) -> individuos.agg_parentesco
  # }
  
  # [EM ANDAMENTO]: VARIÁVEIS DE CONTROLE DO CHEFE DA FAMÍLIA 
  # lapply(
  #   var.interesse_chefe.fam,
  #   function(var){
  individuos %>%
    filter(!!sym(var.relacao.pessoa.ref) == 'chefe') %>% 
    group_by(!!!syms(identificacao.domc)) %>% 
    summarise(
      across(
        .cols = var.interesse_chefe.fam
      )
    ) %>% return(.)
  
  # 
  # ) -> individuos.temp
  # merge(individuos.agg_parentesco,
  # individuos.temp) ->> individuos.agg_parentesco
  # }
  # )
  
  
  
  
  #     if(!empty(individuos.agg_var.domc))
  #       merge(individuos.agg_parentesco,
  #             individuos.agg_var.domc)
  # 
  # if(!empty(individuos.agg_var.indv))
  #   merge(individuos.agg_parentesco,
  #         individuos.agg_var.indv)
  
  # GERAL
  # return(individuos.agg_parentesco)
  # merge(individuos.agg_parentesco,
  # individuos.agg_sexo.idade) %>% return(.)
}


# 4.2. OUTROS REGISTROS 
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


# 5. TESTES ------------------------------------------------------------------
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

# tr1_4.pof2008[[2]] %>%
#   individuos.agg_fun(faixas.etarias = faixas.etarias.ac2008,
#                      sexo = T)
# 
# tr1_4.pof2002[[2]] %>%
#   individuos.agg_fun(faixas.etarias = c(0, 14, 110),
#                      sexo = F,
# 
#                      identificacao.domc = c('uf', 'seq',
#                                             'dv', 'domcl'),
# 
# list.var.recode = list('rel_chefe' = list('1' = 'chefe',
#                                           '2' = 'conjuge',
#                                           '3' = 'filhos',
#                                           '4' = 'outros.parentes',
#                                           '5' = 'agregados',
#                                           '6' = 'pensionistas',
#                                           '7' = 'empregados',
#                                           '8' = 'parentes.empregados'),
#                        'sexo' = list('1' = 'Homens',
#                                      .default = 'Mulheres'),
#                        # 'cod_sabe_ler' = list('1' = 'Sabe ler',
#                        #                       .default = 'Não sabe ler'),
#                        'cor' = list('1' = 'Branca',
#                                     '2' = 'Preta',
#                                     '3' = 'Amarela',
#                                     '4' = 'Parda',
#                                     '5' = 'Indígena',
#                                     .default = 'Não sabe'),
#                        'cartao' = list('1' = 'Tem cartão de crédito',
#                                        .default = 'Não tem cartão de crédito'),
#                        'plano_saude' = list('1' = 'Tem plano de saúde',
#                                             .default = 'Não tem plano de saúde')),

#                      var.relacao.pessoa.ref = 'rel_chefe',
#                      var.sexo = 'sexo',
#                      var.idade = 'idade',
#                      var.anos_estudo = 'anos_est' #,
#                      # var.ler_escrever = 'cod_sabe_ler',
# 
#                      # var.interesse_domc = c(),
#                      # var.interesse_indv = c('cod_sabe_ler', 'cor',
#                      #                        'cartao', 'plano_saude')
# 
#   ) -> teste
# 
# teste$anos_est %>%
#   qplot(.)
# 
# tr1_4.pof2008[[2]]$anos_de_estudo %>%
#   hist(.)
# 
# teste %>%
#   group_by(uf,seq,dv,domcl, cartao) %>%
#   tally(.)
# hist(.)

individuos.pof2008 %>% 
  df_recode(list.recode = list.var.recode_tr2.pof2008) %>% 
  select(
    cod_uf, num_seq, num_dv, cod_domc,
    
    cod_rel_pess_refe_uc,
    idade_anos, 
    anos_de_estudo
  ) %>% 
  filter(cod_rel_pess_refe_uc == 'chefe') %>%
  group_by(cod_uf, num_seq, num_dv, cod_domc) %>%
  summarise(across(.fns = .)) %>% View(.)
# individuos.agg_fun2(faixas.etarias = faixas.etarias.vaz2008,
# sexo = T) %>% 
glimpse(.)



# mapply(
#   var = var.interesse_indv,
#   function(var){
#     
#     individuos.pof2008 %>%
#       pivot_wider(id_cols = cod_rel_pess_refe_uc,
#                   names_from = var,
#                   values_from = 1,
#                   values_fn = length,
#                   values_fill = 0,
#                   names_prefix = 'qtd_')
#     
#   }
# )



