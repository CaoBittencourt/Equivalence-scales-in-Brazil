# 1. PACOTES
pkg <- c('tidyverse', 'plyr', 'sandwich', 'lmtest',
         'geobr', 'rgdal', 'stargazer', 'gghighlight',
         'ggridges', 'ggthemes', 'viridis', 'naniar',
         'scales', 'patchwork')

lapply(pkg, function(x)
  if(!require(x, character.only = T))
  {install.packages(x); require(x)})

select <- dplyr::select
mutate <- dplyr::mutate
summarise <- dplyr::summarise


# 2. TEMA
# 2.1. MAPAS
# 2.2. GRÁFICOS
theme_set(theme_bw(base_size = 16) +
            theme(plot.title = element_text(size = rel(1),
                                            face = 'bold',
                                            hjust = 0.5,
                                            vjust = 3),
                  plot.subtitle = element_text(size = rel(1),
                                               face = 'italic',
                                               hjust = 0.5,
                                               vjust = 3),
                  plot.margin = margin(20,20,20,20),
                  panel.grid = element_line(size = rel(0.2)),
                  axis.title.x = element_text(size = rel(0.85),
                                              hjust = 0.5,
                                              vjust = -3),
                  axis.title.y = element_text(size = rel(0.85),
                                              hjust = 0.5,
                                              vjust = 3),
                  axis.text.x = element_text(size = rel(0.8)),
                  axis.text.y = element_text(size = rel(0.8)),
                  strip.text.x = element_text(size = rel(0.8)),
                  strip.text.y = element_text(size = rel(0.8)),
                  legend.title = element_text(size = rel(0.8)),
                  legend.text = element_text(size = rel(0.7)),
                  legend.background = element_blank(),
                  legend.position = 'right',
                  legend.direction = 'vertical',
                  legend.box.background = element_blank()))

theme_ridges(font_size = 16) +
  theme(plot.title = element_text(size = rel(1),
                                  face = 'bold',
                                  hjust = 0.5,
                                  vjust = 3),
        plot.margin = margin(20,20,20,20),
        panel.grid = element_line(size = rel(0.2)),
        axis.title.x = element_text(size = rel(0.8),
                                    hjust = 0.5,
                                    vjust = -3),
        axis.title.y = element_text(size = rel(0.8),
                                    hjust = 0.5,
                                    vjust = 3),
        axis.text.x = element_text(size = rel(0.8)),
        axis.text.y = element_text(size = rel(0.8)),
        strip.text.x = element_text(size = rel(0.8)),
        strip.text.y = element_text(size = rel(0.8)),
        legend.title = element_text(size = rel(0.8)),
        legend.text = element_text(size = rel(0.7)),
        legend.background = element_blank(),
        legend.position = 'right',
        legend.direction = 'vertical',
        legend.box.background = element_blank()) -> theme_ridges.2


# 3. DADOS
# 3.1. LINK / ARQUIVO
consumo.file <- 'C:/Users/Sony/Downloads/consumo.csv'
# consumo.link <- url('')

individuos.file <- 'C:/Users/Sony/Downloads/individuos.csv'
# individuos.link <- url('')

# 3.2. BASES DE DADOS
consumo.df <- read.csv(consumo.file,
                       sep = ';')

individuos.df <- read.csv(individuos.file,
                          sep = ';')

install.packages("devtools")
devtools::install_github("lucasmation/microdadosBrasil")
library('microdadosBrasil')

# Censo Demográfico 2000
download_sourceData("PNAD", 2002, unzip = T)

download_sourceData("CENSO", 2000, unzip = T)
d <- read_CENSO('domicilios', 2000)
d <- read_CENSO('pessoas', 2000)

# Censo Demográfico 2000
download_sourceData("CENSO", 2010, unzip = T)
d <- read_CENSO('domicilios', 2010)
d <- read_CENSO('pessoas', 2010)


# 3.3. AJUSTES
# EXPLORAÇÃO I
# glimpse(consumo)
# glimpse(individuos)

# COMMON COLUMNS
# intersect(names(consumo),
# names(individuos))

# EXPLORAÇÃO II
# Fonte = Descrição dos Registros POF 2008-2009_03042014 (Disponível no site do IBGE)
# Fonte = Layout com Descrições da POF 2008 (Disponível no site do IBGE)

# IDENTIFICAÇÃO DOMICILIAR (UMA FAMÍLIA ESPECÍFICA):
# ID_DOM <=> COD_UF => NUM_SEQ => NUM_DV => COD_DOMC (USAR ID_DOM)

# Funções de Agregação
pof.agg <- function(individuos = individuos.df, 
                    consumo = consumo.df, 
                    faixas.etarias, 
                    sexo = T){
  individuos %>% 
    transmute(cod_rel_pess_refe_uc = case_when(cod_rel_pess_refe_uc == 1 ~ 'num_pessoa.ref',
                                               cod_rel_pess_refe_uc == 2 ~ 'num_conjuge',
                                               cod_rel_pess_refe_uc == 3 ~ 'num_filhos',
                                               cod_rel_pess_refe_uc == 4 ~ 'num_outros.parentes',
                                               cod_rel_pess_refe_uc == 5 ~ 'num_agregados',
                                               cod_rel_pess_refe_uc == 6 ~ 'num_pensionistas',
                                               cod_rel_pess_refe_uc == 7 ~ 'num_empregados.dom',
                                               cod_rel_pess_refe_uc == 8 ~ 'num_parentes.empregados'),
              cod_uf = cod_uf, num_seq = num_seq, 
              num_dv = num_dv, cod_domc = cod_domc,
              num_ext_renda = num_ext_renda, 
              fator_expansao1 = fator_expansao1, 
              fator_expansao2 = fator_expansao2) %>% 
    pivot_wider(id_cols = c(cod_uf, num_seq, num_dv, cod_domc, 
                            num_ext_renda, fator_expansao1, fator_expansao2),
                names_from = cod_rel_pess_refe_uc,
                values_from = 1,
                values_fn = length,
                values_fill = 0) -> individuos.agg_parentesco
  
  if(sexo == T)
    individuos %>% 
    transmute(cod_sexo = case_when(cod_sexo == 1 ~ 'Homens',
                                   cod_sexo == 2 ~ 'Mulheres'),
              idade = cut(idade_anos,
                          breaks = faixas.etarias,
                          include.lowest = T,
                          right = T,
                          na.rm = T),
              cod_uf = cod_uf, num_seq = num_seq, 
              num_dv = num_dv, cod_domc = cod_domc,
              num_ext_renda = num_ext_renda, 
              fator_expansao1 = fator_expansao1, 
              fator_expansao2 = fator_expansao2) %>% 
    pivot_wider(#names_from = idade,
      names_from = c(cod_sexo, idade),
      # names_glue = '{idade} anos',
      names_glue = '{cod_sexo}: {idade} anos',
      names_sort = T,
      values_from = 1,
      values_fn = length,
      values_fill = 0) -> individuos.agg_sexo.idade
  
  else
    individuos %>% 
    transmute(idade = cut(idade_anos,
                          breaks = faixas.etarias,
                          include.lowest = T,
                          right = T,
                          na.rm = T),
              cod_uf = cod_uf, num_seq = num_seq, 
              num_dv = num_dv, cod_domc = cod_domc,
              num_ext_renda = num_ext_renda, 
              fator_expansao1 = fator_expansao1, 
              fator_expansao2 = fator_expansao2) %>% 
    pivot_wider(names_from = idade,
                names_glue = '{idade} anos',
                names_sort = T,
                values_from = 1,
                values_fn = length,
                values_fill = 0) -> individuos.agg_sexo.idade
  
  # GERAL
  full_join(individuos.agg_parentesco,
            individuos.agg_sexo.idade,
            by = c('cod_uf', 'num_seq', 'num_dv', 'cod_domc',
                   'num_ext_renda', 'fator_expansao1', 'fator_expansao2')) -> individuos.agg
  
  # CATEGORIAS ORÇAMENTÁRIAS
  id_despesas <- list(alimentacao = 'da',
                      moradia = 'dd02',
                      vestuario = 'dd03',
                      transporte = 'dd04',
                      higiene = 'dd05',
                      saude = c('dd06', 'dd6'),
                      educacao = 'dd07',
                      lazer = 'dd08',
                      fumo = 'dd09',
                      servicos.pessoais = 'dd10',
                      despesas.diversas = 'dd11',
                      impostos_previdencia_doacoes = 'dd12',
                      imoveis_investimentos = 'dd13',
                      empresimos = 'dd14',
                      var_ativo_passivo = 'vvp')
  
  id_take.out <- c('da21', 'da22', 'da23', 'da24',
                   'da25', 'da28', 'da29')
  
  id_lanche.escolar <- 'da27'
  
  id_vestuario_infantil <- 'dd033'
  
  id_vestuario_homem.mulher <- c('dd031', 'dd032')
  
  id_bebidas_alcoolicas <- 'da26'
  
  id_jogos_apostas <- 'dd111'
  
  id_receitas <- 're'
  
  
  # CONSUMO AGREGADO
  consumo %>%
    transmute(despesas.alimentacao = rowSums(select(.,contains(id_despesas$alimentacao)), na.rm = T),
              despesas.alimentacao.fora = rowSums(select(.,contains(id_take.out)), na.rm = T),
              despesas.alimentacao.lanche.escolar = rowSums(select(.,contains(id_lanche.escolar)), na.rm = T),
              
              despesas.moradia = rowSums(select(.,contains(id_despesas$moradia)), na.rm = T),
              despesas.vestuario = rowSums(select(.,contains(id_despesas$vestuario)), na.rm = T),
              despesas.transporte = rowSums(select(.,contains(id_despesas$transporte)), na.rm = T),
              despesas.higiene = rowSums(select(.,contains(id_despesas$higiene)), na.rm = T),
              despesas.saude = rowSums(select(.,contains(id_despesas$saude)), na.rm = T),
              
              despesas.educacao = rowSums(select(.,contains(id_despesas$educacao)), na.rm = T),
              
              despesas.lazer = rowSums(select(.,contains(id_despesas$lazer)), na.rm = T),
              despesas.fumo = rowSums(select(.,contains(id_despesas$fumo)), na.rm = T),
              despesas.servicos.pessoais = rowSums(select(.,contains(id_despesas$servicos.pessoais)), na.rm = T),
              despesas.diversas = rowSums(select(.,contains(id_despesas$despesas.diversas)), na.rm = T),
              despesas.impostos_previdencia_doacoes = rowSums(select(.,contains(id_despesas$impostos_previdencia_doacoes)), na.rm = T),
              despesas.imoveis_investimentos = rowSums(select(.,contains(id_despesas$imoveis_investimentos)), na.rm = T),
              despesas.empresimos = rowSums(select(.,contains(id_despesas$empresimos)), na.rm = T),
              despesas.var_ativo_passivo = rowSums(select(.,contains(id_despesas$var_ativo_passivo)), na.rm = T),
              despesas.totais = rowSums(select(.,contains(as.character(unlist(id_despesas)))), na.rm = T),
              
              despesas.totais.nao.monetaria = rowSums(select(.,contains('nm')), na.rm = T),
              
              
              # Para o método de Rothbarth
              despesas.vestuario_infantil = rowSums(select(.,contains(id_vestuario_infantil)), na.rm = T),
              
              despesas.vestuario_homem.mulher = rowSums(select(.,contains(id_vestuario_homem.mulher)), na.rm = T),
              despesas.vestuario_adulto = ifelse((despesas.vestuario - despesas.vestuario_infantil < 0),
                                                 0, (despesas.vestuario - despesas.vestuario_infantil)),
              despesas.bebidas_alcoolicas = rowSums(select(.,contains(id_bebidas_alcoolicas)), na.rm = T),
              despesas.jogos_apostas = rowSums(select(.,contains(id_jogos_apostas)), na.rm = T),
              
              receitas.totais = rowSums(select(.,contains(id_receitas)), na.rm = T),
              
              cod_uf = cod_uf,
              num_seq = num_seq,
              num_dv = num_dv,
              cod_domc = cod_domc,
              id_dom = id_dom,
              urbano = ifelse(urbano == 0, 'Rural', 'Urbano'),
              urbano = factor(urbano),
              
              # Estratificação Social (renda per capita)
              classe.social = cut(receitas.totais,
                                  quantile(receitas.totais,
                                           probs = seq(0,1,0.2),
                                           na.rm = T),
                                  labels = c('Primeiro Quinto (Renda per capita)',
                                             'Segundo Quinto (Renda per capita)',
                                             'Terceiro Quinto (Renda per capita)',
                                             'Quarto Quinto (Renda per capita)',
                                             'Quinto Quinto (Renda per capita)'),
                                  include.lowest = T),
              
              qtd_morador_domc = qtd_morador_domc,
              renda_bruta_monetaria = renda_bruta_monetaria,
              renda_bruta_nao_monetaria = renda_bruta_nao_monetaria,
              renda_total = renda_total) %>% 
    mutate(across(.cols = contains('despesas.'), ~ . / 12, 
                  .names = '{.col}.mensal')) %>% 
    mutate(across(.cols = contains('receitas.'), ~ . / 12, 
                  .names = '{.col}.mensal')) %>% 
    mutate(across(.cols = contains('.mensal'), ~ . / qtd_morador_domc, 
                  .names = '{.col}.per_capita')) -> consumo.agg
  
  
  # CONSUMO AGREGADO POR DOMICÍLIO
  full_join(individuos.agg, consumo.agg,
            by = c('cod_uf', 'num_seq',
                   'num_dv', 'cod_domc')) %>% 
    mutate(UF.sigla = case_when(cod_uf == 11	~ 'RO', cod_uf == 12	~ 'AC',
                                cod_uf == 13	~ 'AM', cod_uf == 14	~ 'RR',
                                cod_uf == 15	~ 'PA', cod_uf == 16	~ 'AP',
                                cod_uf == 17	~ 'TO', cod_uf == 21 ~ 'MA', 
                                cod_uf == 22 ~ 'PI', cod_uf == 23 ~ 'CE', 
                                cod_uf == 24 ~ 'RN', cod_uf == 25 ~ 'PB', 
                                cod_uf == 26 ~ 'PE', cod_uf == 27 ~ 'AL',
                                cod_uf == 28 ~ 'SE', cod_uf == 29 ~ 'BA',
                                cod_uf == 31 ~ 'MG', cod_uf == 32 ~ 'ES',
                                cod_uf == 33 ~ 'RJ', cod_uf == 35 ~ 'SP',
                                cod_uf == 41 ~ 'PR', cod_uf == 42 ~ 'SC',
                                cod_uf == 43 ~ 'RS', cod_uf == 50 ~ 'MS',
                                cod_uf == 51 ~ 'MT', cod_uf == 52 ~ 'GO',
                                cod_uf == 53 ~ 'DF'),
           UF.sigla = factor(UF.sigla),
           
           UF.nome = case_when(cod_uf == 11	~ 'Rondônia', cod_uf == 12	~ 'Acre',
                               cod_uf == 13	~ 'Amazonas', cod_uf == 14	~ 'Roraima',
                               cod_uf == 15	~ 'Pará', cod_uf == 16	~ 'Amapá',
                               cod_uf == 17	~ 'Tocantins', cod_uf == 21 ~ 'Maranhão', 
                               cod_uf == 22 ~ 'Piauí', cod_uf == 23 ~ 'Ceará', 
                               cod_uf == 24 ~ 'Rio Grande do Norte', cod_uf == 25 ~ 'Paraíba', 
                               cod_uf == 26 ~ 'Pernambuco', cod_uf == 27 ~ 'Alagoas',
                               cod_uf == 28 ~ 'Sergipe', cod_uf == 29 ~ 'Bahia',
                               cod_uf == 31 ~ 'Minas Gerais', cod_uf == 32 ~ 'Espírito Santo',
                               cod_uf == 33 ~ 'Rio de Janeiro', cod_uf == 35 ~ 'São Paulo',
                               cod_uf == 41 ~ 'Paraná', cod_uf == 42 ~ 'Santa Catarina',
                               cod_uf == 43 ~ 'Rio Grande do Sul', cod_uf == 50 ~ 'Mato Grosso do Sul',
                               cod_uf == 51 ~ 'Mato Grosso', cod_uf == 52 ~ 'Goiás',
                               cod_uf == 53 ~ 'Distrito Federal'),
           UF.nome = factor(UF.nome),
           
           regiao = case_when(startsWith(as.character(cod_uf), '1') ~ 'Norte',
                              startsWith(as.character(cod_uf), '2') ~ 'Nordeste',
                              startsWith(as.character(cod_uf), '3') ~ 'Sudeste',
                              startsWith(as.character(cod_uf), '4') ~ 'Sul',
                              startsWith(as.character(cod_uf), '5') ~ 'Centro-Oeste'),
           regiao = factor(regiao),
           
           tipo.familia = paste0('(', (qtd_morador_domc - num_filhos),',',
                                 num_filhos,')')) %>% 
    arrange(qtd_morador_domc, desc(num_filhos)) %>%
    mutate(tipo.familia = factor(tipo.familia, unique(tipo.familia)))
}

# Faixas Etárias
# Idosos (>65 anos)
# Adultos (18-65 anos)
# Adolescentes (13-17 anos)
# Crianças III (7-12 anos)
# Crianças II (4-6 anos)
# Crianças I (1-3 anos)
# Bebês (<1 anos)
faixas.etarias.normais <- c(0, 1, 4, 7, 13, 18, 65, max(individuos.df$idade_anos))

# Adulto e Criança apenas
faixas.etarias.ac <- c(0, 14, max(individuos.df$idade_anos))

# Faixas etárias utilizadas por Vaz (2007)
faixas.etarias.vaz <- c(0, 4, 9, 14, max(individuos.df$idade_anos))

# Faixas etárias utilizadas por Deaton (2018)
faixas.etarias.deaton <- c(0, 4, 9, 14, 54, max(individuos.df$idade_anos))

# AGREGAÇÕES POR FAIXAS ETÁRIAS COM E SEM SEXO
# pof.agg(faixas.etarias = faixas.etarias.ac) -> pof.agg.com_sexo.ac
# pof.agg(faixas.etarias = faixas.etarias.vaz) -> pof.agg.com_sexo.vaz
# pof.agg(faixas.etarias = faixas.etarias.deaton) -> pof.agg.com_sexo.deaton
# 
# pof.agg(faixas.etarias = faixas.etarias.ac, sexo = F) -> pof.agg.sem_sexo.ac
# pof.agg(faixas.etarias = faixas.etarias.vaz, sexo = F) -> pof.agg.sem_sexo.vaz
# pof.agg(faixas.etarias = faixas.etarias.deaton, sexo = F) -> pof.agg.sem_sexo.deaton

# 4. SELEÇÃO AMOSTRAL
# pof.agg.com_sexo.ac -> pof.agg.com_sexo.ac.backup
# pof.agg.com_sexo.vaz -> pof.agg.com_sexo.vaz.backup
# pof.agg.com_sexo.deaton -> pof.agg.com_sexo.deaton.backup
# 
# pof.agg.sem_sexo.ac -> pof.agg.sem_sexo.ac.backup
# pof.agg.sem_sexo.vaz -> pof.agg.sem_sexo.vaz.backup
# pof.agg.sem_sexo.deaton -> pof.agg.sem_sexo.deaton.backup
# 
# pof.agg.com_sexo.normais <- pof.agg.com_sexo.normais.backup
# pof.agg.com_sexo.ac <- pof.agg.com_sexo.ac.backup
# pof.agg.com_sexo.vaz <- pof.agg.com_sexo.vaz.backup
# pof.agg.com_sexo.deaton <- pof.agg.com_sexo.deaton.backup
# 
# pof.agg.sem_sexo.normais <- pof.agg.sem_sexo.normais.backup
# pof.agg.sem_sexo.ac <- pof.agg.sem_sexo.ac.backup
# pof.agg.sem_sexo.vaz <- pof.agg.sem_sexo.vaz.backup
# pof.agg.sem_sexo.deaton <- pof.agg.sem_sexo.deaton.backup

# ESCOPO
sample.selection <- function(df, regioes = levels(df$regiao),
                             UF = levels(df$UF.sigla),
                             rural.urbano = levels(df$urbano),
                             max.moradores = 7, max.filhos = 4,
                             max.agregados = 0, max.pensionistas = 0,
                             max.empregados.dom = 0, max.fam.empregados.dom = 0,
                             incluir.solteiros = F){
  if(incluir.solteiros == T){
    df %>% 
      filter(regiao %in% regioes,
             UF.sigla %in% UF,
             urbano %in% rural.urbano,
             qtd_morador_domc <= max.moradores,
             num_filhos <= max.filhos,
             num_agregados <= max.agregados,
             num_pensionistas <= max.pensionistas,
             num_empregados.dom <= max.empregados.dom,
             num_parentes.empregados <= max.fam.empregados.dom) %>%
      mutate(across(c(regiao, UF.sigla, UF.nome, urbano),
                    .fns = factor))
  }
  else
    df %>% 
    filter(regiao %in% regioes,
           UF.sigla %in% UF,
           urbano %in% rural.urbano,
           num_conjuge == 1,
           qtd_morador_domc <= max.moradores,
           num_filhos <= max.filhos,
           num_agregados <= max.agregados,
           num_pensionistas <= max.pensionistas,
           num_empregados.dom <= max.empregados.dom,
           num_parentes.empregados <= max.fam.empregados.dom) %>%
    mutate(across(c(regiao, UF.sigla, UF.nome, urbano),
                  .fns = factor))
}

# 5. REGRESSÕES
# 5.1. FUNÇÕES DE REGRESSÃO E ESCALAS DE EQUIVALÊNCIA
lm.engel.rothbarth <- function(df, good.i = 'despesas.alimentacao.mensal',
                               expenditure = 'despesas.totais.mensal.per_capita',
                               control = c('UF.sigla', 'urbano')){
  
  paste(good.i, '/ despesas.totais.mensal ~ .',
        paste0('+ log(', expenditure, ')',
               '- ', expenditure)) %>%
    as.formula(.) -> f
  
  df %>%
    select(contains('anos'),
           good.i,
           control,
           despesas.totais.mensal,
           expenditure) %>%
    lm(formula = f#,
       # weights = .weights
    )
  
}


lm.heteroskedasticity <- function(model, .type = 'HC3',
                                  significance = 0.05){
  
  if(bptest(model)$p.value <= significance){
    model %>%
      coeftest(vcov = vcovHC(., type = .type))}
  else
    model
}


equivalence.scales.dudel <- function(model, 
                                     ref.a, na.h, nc.h,
                                     na.r = 1, nc.r = 0,
                                     nr = na.r + nc.r,
                                     nh = na.h + nc.h,
                                     expenditure = 'despesas.totais.mensal.per_capita'){
  
  model %>% 
    broom::tidy(.) %>% 
    mutate(log.expenditure.estimate = filter(.,term == paste0('log(', expenditure, ')')) %>% pull(estimate),
           coef.rel = estimate/log.expenditure.estimate) %>%
    filter(grepl('anos', term)) %>%
    mutate(ref.estimate.rel = filter(.,term == ref.a) %>% pull(coef.rel),
           equivalence.scale = ref.estimate.rel*(na.r - na.h) + coef.rel*(nc.r - nc.h),
           equivalence.scale = exp(equivalence.scale)*(nh/nr),
           member.cost = (equivalence.scale - 1)*(nr/nc.h))
}


# 5.2. TESTAGEM DE REGRESSÕES
# HETEROCEDACIDADE
# Heterocedasticidade em todas as regressões
list(pof.agg.com_sexo.ac,
     pof.agg.sem_sexo.ac,
     pof.agg.com_sexo.deaton,
     pof.agg.sem_sexo.deaton,
     pof.agg.com_sexo.vaz,
     pof.agg.sem_sexo.vaz) %>% 
  lapply(function(x){lm.engel.rothbarth(x)}) %>%
  lapply(bptest) %>% 
  lapply(function(x){x$p.value <= 0.05})

# Novos erros-padrão
list(pof.agg.com_sexo.ac,
     pof.agg.sem_sexo.ac,
     pof.agg.com_sexo.deaton,
     pof.agg.sem_sexo.deaton,
     pof.agg.com_sexo.vaz,
     pof.agg.sem_sexo.vaz) %>% 
  lapply(function(x){lm.engel.rothbarth(x)}) %>%
  lapply(function(x){lm.heteroskedasticity(x)})


# 5.3. ESCALAS DE EQUIVALÊNCIA
referencia.com_sexo.ac = '`Homens: (14,104] anos`'
referencia.sem_sexo.ac = '`(14,104] anos`'
referencia.com_sexo.vaz = '`Homens: (14,104] anos`'
referencia.sem_sexo.vaz = '`(14,104] anos`'
referencia.com_sexo.deaton = '`Homens: (14,54] anos`'
referencia.sem_sexo.deaton = '`(14,54] anos`'



# # Método de Engel
pof.agg.sem_sexo.vaz %>%
  filter(!(num_conjuge == 0 & num_filhos > 0)) %>%
  sample.selection(max.moradores = 5, max.filhos = 3,
                   incluir.solteiros = T) %>%
  lm.engel.rothbarth(.) %>%
  lm.heteroskedasticity(.) %>%
  equivalence.scales.dudel(ref.a = referencia.sem_sexo.vaz,
                           na.h = 2, nc.h = 1,
                           na.r = 2, nc.r = 0) %>%
  select(term, equivalence.scale, std.error,
         member.cost, p.value) %>%
  mutate(p.value = p.value <= 0.05)

lapply(1:4, function(x)
  equivalence.scales.dudel(model = m,
                           ref.a = referencia.sem_sexo.ac,
                           na.h = 2, nc.h = x,
                           na.r = 2, nc.r = 0) %>%
    select(term, equivalence.scale,
           member.cost, p.value) %>%
    mutate(nc = x,
           term = factor(term, unique(term)),
           p.value = p.value <= 0.05)) %>%
  bind_rows(.) %>%
  ggplot(aes(x = term,
             y = member.cost)) + 
  geom_bar(stat = 'identity') + 
  facet_grid(cols = vars(nc)) + 
  coord_flip()
