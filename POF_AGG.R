# 1. PACOTES
pkg <- c('plyr', 'glue', 'tidyverse')

lapply(pkg, function(x)
  if(!require(x, character.only = T))
  {install.packages(x); require(x)})

select <- dplyr::select
mutate <- dplyr::mutate
summarise <- dplyr::summarise


# 2. AGREGAÇÀO DE POF COM BASE NOS NOMES DE VARIÁVEIS DA POF 2008-2009
# Renomear outras POFs e aplicar o mesmo algoritmo
pof.agg <- function(individuos, consumo, 
                    lista.id_receitas.agg,
                    lista.id_despesas.agg,
                    lista.id_despesas.especificas,
                    faixas.etarias, sexo = T){
  
  individuos %>%
    mutate(desc_pessoa = recode(cod_rel_pess_refe_uc,
                                `1` = 'chefe',
                                `2` = 'conjuge',
                                `3` = 'filhos',
                                `4` = 'outros.parentes',
                                `5` = 'agregados',
                                `6` = 'pensionistas',
                                `7` = 'empregados',
                                `8` = 'parentes.empregados')) %>% 
    pivot_wider(id_cols = c(cod_uf, num_seq, num_dv, cod_domc),
                names_from = desc_pessoa,
                values_from = 1,
                values_fn = length,
                values_fill = 0,
                names_prefix = 'qtd_') -> individuos.agg_parentesco
  
  if(sexo == T)
    individuos %>%
    mutate(desc_sexo = recode(cod_sexo,
                              `1` = 'Homens',
                              # Obs: na POF 2002-2003, separa-se mulheres em
                              # Gestante, Não-gestante e lactante
                              # => Se 1, então Homens; e Mulheres, caso contrário
                              .default = 'Mulheres'),
           idade = cut(idade_anos,
                       breaks = faixas.etarias,
                       include.lowest = T,
                       right = T,
                       na.rm = T)) %>%
    pivot_wider(id_cols = c(cod_uf, num_seq, num_dv, cod_domc),
                names_from = c(desc_sexo, idade),
                names_glue = '{desc_sexo}: {idade} anos',
                names_sort = T,
                values_from = 1,
                values_fn = length,
                values_fill = 0) -> individuos.agg_sexo.idade
  
  else
    individuos %>%
    mutate(idade = cut(idade_anos,
                       breaks = faixas.etarias,
                       include.lowest = T,
                       right = T,
                       na.rm = T)) %>%
    pivot_wider(id_cols = c(cod_uf, num_seq, num_dv, cod_domc),
                names_from = idade,
                names_glue = '{idade} anos',
                names_sort = T,
                values_from = 1,
                values_fn = length,
                values_fill = 0) -> individuos.agg_sexo.idade
  
  # GERAL
  merge(individuos.agg_parentesco,
        individuos.agg_sexo.idade) -> individuos.agg
  
  
  # CONSUMO AGREGADO
  # Agregação das despesas específicas
  lapply(names(lista.id_despesas.especificas), function(id){
    
    consumo %>%
      mutate(!!sym(glue('despesas.mensais.{names(lista.id_despesas.especificas[id])}')) :=
               rowSums(across(.cols = contains(lista.id_despesas.especificas[[id]])),
                       na.rm = T)/12) ->> consumo
    
  }) %>% 
    invisible(.)
  
  # Agregação das despesas gerais
  lapply(names(lista.id_despesas.agg), function(id){
    
    consumo %>%
      mutate(!!sym(glue('despesas.mensais.{names(lista.id_despesas.agg[id])}')) :=
               rowSums(across(.cols = contains(lista.id_despesas.agg[[id]])),
                       na.rm = T)/12) ->> consumo
    
  }) %>% 
    invisible(.)
  
  # Despesas totais
  consumo %>%
    mutate(despesas.mensais.totais =
             rowSums(across(.cols = matches(unlist(lista.id_despesas.agg) %>% 
                                              paste(collapse = '|'))),
                     na.rm = T)/12) -> consumo
  
  # Remoção das variáveis desagregadas
  consumo %>% 
    select(-matches(unlist(lista.id_despesas.agg) %>% 
                      paste(collapse = '|')),
           -matches(unlist(lista.id_despesas.especificas) %>% 
                      paste(collapse = '|')),
           -matches(unlist(lista.id_receitas.agg) %>% 
                      paste(collapse = '|'))) -> consumo
  
  
  # Domicílios, Indivíduos e Consumo
  merge(individuos.agg, consumo) %>% 
    # Ajustes finais
    mutate(code_region = substr(cod_uf, start = 1, stop = 1),
           code_region = factor(code_region), # Código da região (utilizado para plotagem)
           
           desc_urbano = recode(urbano,
                                `0` = 'Rural',
                                `1` = 'Urbano'),
           # Estratificação social conforme o IBGE (quintis de renda per capita)
           classe_social = cut(renda_total,
                               quantile(renda_total,
                                        probs = seq(0,1,0.2),
                                        na.rm = T),
                               labels = c('Primeiro Quinto (Renda per capita)',
                                          'Segundo Quinto (Renda per capita)',
                                          'Terceiro Quinto (Renda per capita)',
                                          'Quarto Quinto (Renda per capita)',
                                          'Quinto Quinto (Renda per capita)'),
                               include.lowest = T),
           
           UF_sigla = recode(cod_uf,
                             `11`	= 'RO', `12`	= 'AC', `13`	= 'AM', `14`	= 'RR', `15`	= 'PA', `16`	= 'AP', `17`	= 'TO', 
                             `21` = 'MA', `22` = 'PI', `23` = 'CE', `24` = 'RN', `25` = 'PB', `26` = 'PE', `27` = 'AL', `28` = 'SE', `29` = 'BA',
                             `31` = 'MG', `32` = 'ES', `33` = 'RJ', `35` = 'SP',
                             `41` = 'PR', `42` = 'SC', `43` = 'RS', 
                             `50` = 'MS', `51` = 'MT', `52` = 'GO', `53` = 'DF'),
           UF_sigla = factor(UF_sigla),
           
           regiao = recode(code_region,
                           `1` = 'Norte',
                           `2` = 'Nordeste',
                           `3` = 'Sudeste',
                           `4` = 'Sul',
                           `5` = 'Centro-Oeste'),
           regiao = factor(regiao)) %>% 
    # Variáveis per capita
    mutate(across(.cols = contains('despesas'), 
                  ~ . / qtd_morador_domc, 
                  .names = '{.col}_per.capita'),
           across(.cols = contains('renda_'), 
                  ~ . / qtd_morador_domc, 
                  .names = '{.col}_per.capita'),
    # Porcentagem orçamentária 
    across(.cols = contains('despesas'), 
           ~ . / despesas.mensais.totais, 
           .names = 'share_{.col}'))
  
}