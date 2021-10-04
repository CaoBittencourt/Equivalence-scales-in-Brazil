# 1. PACOTES --------------------------------------------------------------
# Pacotes
pkg <- c('rio', 'plyr', 'glue', 'tidyverse') #Leitura e Manipulação de Dados

lapply(pkg, function(x)
  if(!require(x, character.only = T))
  {install.packages(x); require(x)})

# Citação dos pacotes
# lapply(pkg, function(x)
#   {citation(package = x)})

# Confecção própria
source('C:/Users/Sony/Documents/GitHub/TCC/FUNC_AGG/POF_AGG.R', encoding = 'UTF-8')
source('C:/Users/Sony/Documents/GitHub/TCC/FUNC_AGG/INDIV_AGG.R', encoding = 'UTF-8')

# 2. DADOS -------------------------------------------------------------------
# POF 2002-2003, 2008-2009 via Stata (DataZoom)
basepadrao.pof2002 <- rio::import('C:/Users/Sony/Documents/TCC_DADOS/POF2002/pof2002_dom_standard.dta')
basepadrao.pof2008 <- rio::import('C:/Users/Sony/Documents/TCC_DADOS/POF2008/pof2008_dom_standard.dta')

# Registros de Individuos 2002-2003, 2008-2009 via Stata (DataZoom)
individuos.pof2002 <- rio::import('C:/Users/Sony/Documents/TCC_DADOS/POF2002/pof2002_tr2.dta')
individuos.pof2008 <- rio::import('C:/Users/Sony/Documents/TCC_DADOS/POF2008/pof2008_tr2.dta')

# Registros de Moradia 2002-2003, 2008-2009 via Stata (DataZoom)
moradia.pof2002 <- rio::import('C:/Users/Sony/Documents/TCC_DADOS/POF2002/pof2002_tr1.dta')
moradia.pof2008 <- rio::import('C:/Users/Sony/Documents/TCC_DADOS/POF2008/pof2008_tr1.dta')

# 3. FAIXAS ETÁRIAS -------------------------------------------------------
# Faixas Etárias I
# Idosos (>65 anos)
# Adultos (18-65 anos)
# Adolescentes (13-17 anos)
# Crianças III (7-12 anos)
# Crianças II (4-6 anos)
# Crianças I (1-3 anos)
# Bebês (<1 anos)
# faixas.etarias.normais2002 <- c(0, 1, 4, 7, 13, 18, 65, max(individuos.pof2002$idade))
# faixas.etarias.normais2008 <- c(0, 1, 4, 7, 13, 18, 65, max(individuos.pof2008$idade_anos))

# Faixas Etárias II
# Adulto e Criança apenas
faixas.etarias.ac2002 <- c(0, 14, max(individuos.pof2002$idade))
faixas.etarias.ac2008 <- c(0, 14, max(individuos.pof2008$idade_anos))

# Faixas Etárias III
# Faixas etárias utilizadas por Vaz & Vaz (2007)
faixas.etarias.vaz2002 <- c(0, 4, 9, 14, max(individuos.pof2002$idade))
faixas.etarias.vaz2008 <- c(0, 4, 9, 14, max(individuos.pof2008$idade_anos))

# Faixas Etárias IV
# Faixas etárias utilizadas por Deaton (2018)
faixas.etarias.deaton2002 <- c(0, 4, 9, 14, 54, max(individuos.pof2002$idade))
faixas.etarias.deaton2008 <- c(0, 4, 9, 14, 54, max(individuos.pof2008$idade_anos))

# Listas
list(
  # 'normais_2002' = faixas.etarias.normais2002, 
  'ac_2002' = faixas.etarias.ac2002,
  'vaz_2002' = faixas.etarias.vaz2002,
  'deaton_2002' = faixas.etarias.deaton2002
) -> lista.faixas.etarias2002

list(
  # 'normais_2008' = faixas.etarias.normais2008, 
  'ac_2008' = faixas.etarias.ac2008,
  'vaz_2008' = faixas.etarias.vaz2008,
  'deaton_2008' = faixas.etarias.deaton2008
) -> lista.faixas.etarias2008

lista.faixas.etarias2002_ss <- lista.faixas.etarias2002
names(lista.faixas.etarias2002_ss) <- glue('pof_{names(lista.faixas.etarias2002)}_ss')

lista.faixas.etarias2002_cs <- lista.faixas.etarias2002
names(lista.faixas.etarias2002_cs) <- glue('pof_{names(lista.faixas.etarias2002)}_cs')


lista.faixas.etarias2008_ss <- lista.faixas.etarias2008
names(lista.faixas.etarias2008_ss) <- glue('pof_{names(lista.faixas.etarias2008)}_ss')

lista.faixas.etarias2008_cs <- lista.faixas.etarias2008
names(lista.faixas.etarias2008_cs) <- glue('pof_{names(lista.faixas.etarias2008)}_cs')

# 4. RECODES (NOMES DAS VARIÁVEIS) ----------------------------------------
# POF 2002
# Moradia (tr1)
list(
  
  'cond_ocup' = list('1' = 'Imóvel próprio (já pago)',
                     '2' = 'Imóvel próprio (em pagamento)',
                     '3' = 'Imóvel cedido (por empregador)',
                     '4' = 'Imóvel cedido (outra forma)',
                     '5' = 'Outras condições de ocupação',
                     '6' = 'Imóvel alugado')
  
) -> list.var.recode_tr1.pof2002

# Indivíduos (tr2)
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
  'cor' = list('1' = 'cor.Branca',
               '2' = 'cor.Preta',
               '3' = 'cor.Amarela',
               '4' = 'cor.Parda',
               '5' = 'cor.Indígena',
               .default = 'cor.Não sabe'),
  'orc_rend' = list('1' = 'Empregado',
                    .default = 'Não empregado'),
  'cartao' = list('1' = 'Tem cartão de crédito',
                  .default = 'Não tem cartão de crédito'),
  'plano_saude' = list('1' = 'Tem plano de saúde',
                       .default = 'Não tem plano de saúde')
  
) -> list.var.recode_tr2.pof2002

# POF 2008
# Moradia (tr1)
list(
  
  'cod_cond_ocup' = list('1' = 'Imóvel próprio (já pago)',
                         '2' = 'Imóvel próprio (em pagamento)',
                         '3' = 'Imóvel cedido (por empregador)',
                         '4' = 'Imóvel cedido (outra forma)',
                         '5' = 'Outras condições de ocupação',
                         '6' = 'Imóvel alugado')
  
) -> list.var.recode_tr1.pof2008

# Indivíduos (tr2)
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
  'cod_cor_raca' = list('1' = 'cor.Branca',
                        '2' = 'cor.Preta',
                        '3' = 'cor.Amarela',
                        '4' = 'cor.Parda',
                        '5' = 'cor.Indígena',
                        .default = 'cor.Não sabe'),
  'cod_sit_receita' = list('1' = 'Empregado',
                           .default = 'Não empregado'), #Obs: não consta no manual da POF2008 a legenda deste código, mas é análogo ao da POF2002. Supõe-se que 1 = empregado e os demais, de um modo ou de outro, não empregados ou indisponíveis para resposta, como na POF2002.
  'cod_tem_cartao' = list('1' = 'Tem cartão de crédito',
                          .default = 'Não tem cartão de crédito'),
  'plano_saude' = list('1' = 'Tem plano de saúde',
                       .default = 'Não tem plano de saúde')
  
) -> list.var.recode_tr2.pof2008

# 5. ARGUMENTOS PARA AGREGAÇÃO ------------------------------------------------
# POF 2002
# Argumentos para a função de agregação de domicílios POF 2002
identificacao.domc_pof2002 <- c('uf', 'seq', 'dv', 'domcl')
var.relacao.pessoa.ref_pof2002 <- 'rel_chefe'
var.sexo_pof2002 <- 'sexo'
var.idade_pof2002 <- 'idade'
var.anos_estudo_pof2002 <- 'anos_est'
# Argumentos para a função de agregação de consumo POF 2002
qtd_moradores_pof2002 <- 'n_morador'
renda_pof2002 <- 'renda'
unid_fed_pof2002 <- 'uf'

# POF 2008
# Argumentos para a função de agregação de domicílios POF 2008
identificacao.domc_pof2008 <- c('cod_uf', 'num_seq', 'num_dv', 'cod_domc')
var.relacao.pessoa.ref_pof2008 <- 'cod_rel_pess_refe_uc'
var.sexo_pof2008 <- 'cod_sexo'
var.idade_pof2008 <- 'idade_anos'
var.anos_estudo_pof2008 <- 'anos_de_estudo'
# Argumentos para a função de agregação de consumo POF 2002
qtd_moradores_pof2008 <- 'qtd_morador_domc'
renda_pof2008 <- 'renda_total'
unid_fed_pof2008 <- 'cod_uf'

# 6. VARIÁVEIS DE CONTROLE ------------------------------------------------
# POF 2002
# Variáveis de controle POF 2002
var.interesse_chefe.fam_pof2002 <- c(var.idade_pof2002, var.anos_estudo_pof2002)
var.interesse_indv_pof2002 <- c('cor', 'orc_rend', 'cartao', 'plano_saude')
var.interesse_domc_pof2002 <- c('cond_ocup')

# POF 2008
# Variáveis de controle POF 2008
var.interesse_chefe.fam_pof2008 <- c(var.idade_pof2008, var.anos_estudo_pof2008)
var.interesse_indv_pof2008 <- c('cod_cor_raca', 'cod_sit_receita', 'cod_tem_cartao', 'plano_saude')
var.interesse_domc_pof2008 <- c('cod_cond_ocup')

# 7. AGREGAÇÃO ------------------------------------------------------------
# POF 2002
# POF 2002, todas as faixas etárias, sem sexo
mapply(
  faixa.etaria = lista.faixas.etarias2002_ss,
  faixa.etaria.var_name = names(lista.faixas.etarias2002_ss),
  function(faixa.etaria,
           faixa.etaria.var_name){
    # Domicílios agregados POF 2002
    individuos.agg_fun(
      df_individuos = individuos.pof2002,
      faixas.etarias = faixa.etaria,
      sexo = F,
      
      identificacao.domc = identificacao.domc_pof2002,
      
      list.var.recode = list.var.recode_tr2.pof2002,
      
      var.relacao.pessoa.ref = var.relacao.pessoa.ref_pof2002,
      var.sexo = var.sexo_pof2002,
      var.idade = var.idade_pof2002,
      var.anos_estudo = var.anos_estudo_pof2002,
      
      var.interesse_indv = var.interesse_indv_pof2002,
      var.interesse_chefe.fam = var.interesse_chefe.fam_pof2002
    ) -> domicilio.agg.temp
    
    domicilio.agg.temp %>%
      moradia.agg_fun(df_moradia = moradia.pof2002,
                      identificacao.domc = identificacao.domc_pof2002,
                      list.var.recode = list.var.recode_tr1.pof2002,
                      var.interesse_domc = var.interesse_domc_pof2002) -> domicilio.agg.temp
    
    # Consumo domiciliar POF 2002
    pof.agg_fun(
      consumo = basepadrao.pof2002,
      qtd_moradores = qtd_moradores_pof2002,
      renda = renda_pof2002,
      unid_fed = unid_fed_pof2002
    ) -> consumo.agg.temp
    
    merge(domicilio.agg.temp,
          consumo.agg.temp) -> pof.temp
    
    # assign(x = glue('pof_{faixa.etaria.var_name}_ss'),
           # value = pof.temp,
           # envir = .GlobalEnv)
  }
) -> lista.pof2002_ss

# POF 2002, todas as faixas etárias, com sexo
mapply(
  faixa.etaria = lista.faixas.etarias2002_cs,
  faixa.etaria.var_name = names(lista.faixas.etarias2002_cs),
  function(faixa.etaria,
           faixa.etaria.var_name){
    # Domicílios agregados POF 2002
    individuos.agg_fun(
      df_individuos = individuos.pof2002,
      faixas.etarias = faixa.etaria,
      sexo = T,
      
      identificacao.domc = identificacao.domc_pof2002,
      
      list.var.recode = list.var.recode_tr2.pof2002,
      
      var.relacao.pessoa.ref = var.relacao.pessoa.ref_pof2002,
      var.sexo = var.sexo_pof2002,
      var.idade = var.idade_pof2002,
      var.anos_estudo = var.anos_estudo_pof2002,
      
      var.interesse_indv = var.interesse_indv_pof2002,
      var.interesse_chefe.fam = var.interesse_chefe.fam_pof2002
    ) -> domicilio.agg.temp
    
    domicilio.agg.temp %>%
      moradia.agg_fun(df_moradia = moradia.pof2002,
                      identificacao.domc = identificacao.domc_pof2002,
                      list.var.recode = list.var.recode_tr1.pof2002,
                      var.interesse_domc = var.interesse_domc_pof2002) -> domicilio.agg.temp
    
    # Consumo domiciliar POF 2002
    pof.agg_fun(
      consumo = basepadrao.pof2002,
      qtd_moradores = qtd_moradores_pof2002,
      renda = renda_pof2002,
      unid_fed = unid_fed_pof2002
    ) -> consumo.agg.temp
    
    merge(domicilio.agg.temp,
          consumo.agg.temp) -> pof.temp
    
    # assign(x = glue('pof_{faixa.etaria.var_name}_cs'),
    #        value = pof.temp,
    #        envir = .GlobalEnv)
  }
) -> lista.pof2002_cs

# POF 2008
# POF 2008, todas as faixas etárias, sem sexo
mapply(
  faixa.etaria = lista.faixas.etarias2008_ss,
  faixa.etaria.var_name = names(lista.faixas.etarias2008_ss),
  function(faixa.etaria,
           faixa.etaria.var_name){
    # Domicílios agregados POF 2008
    individuos.agg_fun(
      df_individuos = individuos.pof2008,
      faixas.etarias = faixa.etaria,
      sexo = F,
      
      identificacao.domc = identificacao.domc_pof2008,
      
      list.var.recode = list.var.recode_tr2.pof2008,
      
      var.relacao.pessoa.ref = var.relacao.pessoa.ref_pof2008,
      var.sexo = var.sexo_pof2008,
      var.idade = var.idade_pof2008,
      var.anos_estudo = var.anos_estudo_pof2008,
      
      var.interesse_indv = var.interesse_indv_pof2008,
      var.interesse_chefe.fam = var.interesse_chefe.fam_pof2008
    ) -> domicilio.agg.temp
    
    domicilio.agg.temp %>%
      moradia.agg_fun(df_moradia = moradia.pof2008,
                      identificacao.domc = identificacao.domc_pof2008,
                      list.var.recode = list.var.recode_tr1.pof2008,
                      var.interesse_domc = var.interesse_domc_pof2008) -> domicilio.agg.temp
    
    # Consumo domiciliar POF 2008
    pof.agg_fun(
      consumo = basepadrao.pof2008,
      qtd_moradores = qtd_moradores_pof2008,
      renda = renda_pof2008,
      unid_fed = unid_fed_pof2008
    ) -> consumo.agg.temp
    
    merge(domicilio.agg.temp,
          consumo.agg.temp) -> pof.temp
    
    # assign(x = glue('pof_{faixa.etaria.var_name}_ss'),
    #        value = pof.temp,
    #        envir = .GlobalEnv)
  }
) -> lista.pof2008_ss

# POF 2008, todas as faixas etárias, com sexo
mapply(
  faixa.etaria = lista.faixas.etarias2008_cs,
  faixa.etaria.var_name = names(lista.faixas.etarias2008_cs),
  function(faixa.etaria,
           faixa.etaria.var_name){
    # Domicílios agregados POF 2008
    individuos.agg_fun(
      df_individuos = individuos.pof2008,
      faixas.etarias = faixa.etaria,
      sexo = T,
      
      identificacao.domc = identificacao.domc_pof2008,
      
      list.var.recode = list.var.recode_tr2.pof2008,
      
      var.relacao.pessoa.ref = var.relacao.pessoa.ref_pof2008,
      var.sexo = var.sexo_pof2008,
      var.idade = var.idade_pof2008,
      var.anos_estudo = var.anos_estudo_pof2008,
      
      var.interesse_indv = var.interesse_indv_pof2008,
      var.interesse_chefe.fam = var.interesse_chefe.fam_pof2008
    ) -> domicilio.agg.temp
    
    domicilio.agg.temp %>%
      moradia.agg_fun(df_moradia = moradia.pof2008,
                      identificacao.domc = identificacao.domc_pof2008,
                      list.var.recode = list.var.recode_tr1.pof2008,
                      var.interesse_domc = var.interesse_domc_pof2008) -> domicilio.agg.temp
    
    # Consumo domiciliar POF 2008
    pof.agg_fun(
      consumo = basepadrao.pof2008,
      qtd_moradores = qtd_moradores_pof2008,
      renda = renda_pof2008,
      unid_fed = unid_fed_pof2008
    ) -> consumo.agg.temp
    
    merge(domicilio.agg.temp,
          consumo.agg.temp) -> pof.temp
    
    assign(x = glue('pof_{faixa.etaria.var_name}_cs'),
           value = pof.temp,
           envir = .GlobalEnv)
  }
) -> lista.pof2008_cs
