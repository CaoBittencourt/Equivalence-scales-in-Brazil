# 1. PACOTES --------------------------------------------------------------
# Pacotes
pkg <- c('rio', 'plyr', 'glue', 'tidyverse') #Leitura e Manipulação de Dados

lapply(pkg, function(x)
  if(!require(x, character.only = T))
  {install.packages(x); require(x)})

# Citação dos pacotes
# lapply(pkg, function(x)
#   {citation(package = x)})

# Funções próprias
source('C:/Users/Sony/Documents/GitHub/TCC/FUNC_AGG/POF_AGG.R', encoding = 'UTF-8')
source('C:/Users/Sony/Documents/GitHub/TCC/FUNC_AGG/INDIV_AGG.R', encoding = 'UTF-8')

# 2. DADOS -------------------------------------------------------------------
# POF 2002-2003, 2008-2009 via Stata (DataZoom)
basepadrao.pof2002 <- import('C:/Users/Sony/Documents/TCC_DADOS/POF2002/pof2002_dom_standard.dta')
basepadrao.pof2008 <- import('C:/Users/Sony/Documents/TCC_DADOS/POF2008/pof2008_dom_standard.dta')

# Registros de Individuos 2002-2003, 2008-2009 via Stata (DataZoom)
individuos.pof2002 <- import('C:/Users/Sony/Documents/TCC_DADOS/POF2002/pof2002_tr2.dta')
individuos.pof2008 <- import('C:/Users/Sony/Documents/TCC_DADOS/POF2008/pof2008_tr2.dta')

# 3. FAIXAS ETÁRIAS -------------------------------------------------------
# Faixas Etárias I
# Idosos (>65 anos)
# Adultos (18-65 anos)
# Adolescentes (13-17 anos)
# Crianças III (7-12 anos)
# Crianças II (4-6 anos)
# Crianças I (1-3 anos)
# Bebês (<1 anos)
faixas.etarias.normais2002 <- c(0, 1, 4, 7, 13, 18, 65, max(individuos.pof2002$idade))
faixas.etarias.normais2008 <- c(0, 1, 4, 7, 13, 18, 65, max(individuos.pof2008$idade_anos))

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
lista.faixas.etarias2002 <- list('normais_2002' = faixas.etarias.normais2002, 
                                 'ac_2002' = faixas.etarias.ac2002,
                                 'vaz_2002' = faixas.etarias.vaz2002,
                                 'deaton_2002' = faixas.etarias.deaton2002)

lista.faixas.etarias2008 <- list('normais_2008' = faixas.etarias.normais2008, 
                                 'ac_2008' = faixas.etarias.ac2008,
                                 'vaz_2008' = faixas.etarias.vaz2008,
                                 'deaton_2008' = faixas.etarias.deaton2008)

# 4. AGREGAÇÃO ------------------------------------------------------------
# POF 2002
# POF 2002, todas as faixas etárias, sem sexo
mapply(
  faixa.etaria = lista.faixas.etarias2002,
  faixa.etaria.var_name = names(lista.faixas.etarias2002),
  function(faixa.etaria,
           faixa.etaria.var_name){
    # Domicílios agregados POF 2002
    individuos.agg_fun(
      individuos = individuos.pof2002,
      faixas.etarias = faixa.etaria,
      sexo = F,
      
      identificacao.domc = c('uf', 'seq',
                             'dv', 'domcl'),
      
      var.relacao.pessoa.ref = 'rel_chefe',
      var.sexo = 'sexo',
      var.idade = 'idade',
      var.anos_estudo = 'anos_est',
      
      list.var.recode = list.var.recode_tr2.pof2002
    ) -> domicilio.agg.temp
    
    # Consumo domiciliar POF 2002
    pof.agg_fun(
      consumo = basepadrao.pof2002,
      qtd_moradores = 'n_morador',
      renda = 'renda',
      unid_fed = 'uf'
    ) -> consumo.agg.temp
    
    merge(domicilio.agg.temp,
          consumo.agg.temp) -> pof.temp
    
    assign(x = glue('pof_{faixa.etaria.var_name}_ss'),
           value = pof.temp,
           envir = .GlobalEnv)
  }
) %>% invisible(.)

# POF 2002, todas as faixas etárias, com sexo
mapply(
  faixa.etaria = lista.faixas.etarias2002,
  faixa.etaria.var_name = names(lista.faixas.etarias2002),
  function(faixa.etaria,
           faixa.etaria.var_name){
    # Domicílios agregados POF 2002
    individuos.agg_fun(
      individuos = individuos.pof2002,
      faixas.etarias = faixa.etaria,
      sexo = T,
      
      identificacao.domc = c('uf', 'seq',
                             'dv', 'domcl'),
      
      var.relacao.pessoa.ref = 'rel_chefe',
      var.sexo = 'sexo',
      var.idade = 'idade',
      var.anos_estudo = 'anos_est',
      
      list.var.recode = list.var.recode_tr2.pof2002
    ) -> domicilio.agg.temp
    
    # Consumo domiciliar POF 2002
    pof.agg_fun(
      consumo = basepadrao.pof2002,
      qtd_moradores = 'n_morador',
      renda = 'renda',
      unid_fed = 'uf'
    ) -> consumo.agg.temp
    
    merge(domicilio.agg.temp,
          consumo.agg.temp) -> pof.temp
    
    assign(x = glue('pof_{faixa.etaria.var_name}_cs'),
           value = pof.temp,
           envir = .GlobalEnv)
  }
) %>% invisible(.)


# POF 2008, todas as faixas etárias, sem sexo
mapply(
  faixa.etaria = lista.faixas.etarias2008,
  faixa.etaria.var_name = names(lista.faixas.etarias2008),
  function(faixa.etaria,
           faixa.etaria.var_name){
    # Domicílios agregados POF 2008
    individuos.agg_fun(
      individuos = individuos.pof2008,
      faixas.etarias = faixa.etaria,
      sexo = F
    ) -> domicilio.agg.temp
    
    # Consumo domiciliar POF 2008
    pof.agg_fun(
      consumo = basepadrao.pof2008
    ) -> consumo.agg.temp
    
    merge(domicilio.agg.temp,
          consumo.agg.temp) -> pof.temp
    
    assign(x = glue('pof_{faixa.etaria.var_name}_ss'),
           value = pof.temp,
           envir = .GlobalEnv)
  }
) %>% invisible(.)

# POF 2008, todas as faixas etárias, com sexo
mapply(
  faixa.etaria = lista.faixas.etarias2008,
  faixa.etaria.var_name = names(lista.faixas.etarias2008),
  function(faixa.etaria,
           faixa.etaria.var_name){
    # Domicílios agregados POF 2008
    individuos.agg_fun(
      individuos = individuos.pof2008,
      faixas.etarias = faixa.etaria,
      sexo = T
    ) -> domicilio.agg.temp
    
    # Consumo domiciliar POF 2008
    pof.agg_fun(
      consumo = basepadrao.pof2008
    ) -> consumo.agg.temp
    
    merge(domicilio.agg.temp,
          consumo.agg.temp) -> pof.temp
    
    assign(x = glue('pof_{faixa.etaria.var_name}_cs'),
           value = pof.temp,
           envir = .GlobalEnv)
  }
) %>% invisible(.)

