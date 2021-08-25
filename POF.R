# 1. PACOTES
# install.packages('devtools' ,
# repos = 'http://cran.rstudio.com/')
# library(devtools)
# remotes::install_github('ajdamico/lodown', dependencies = T)
# remotes::install_github('tomasbarcellos/pof')

pkg <- c('sandwich', 'lmtest', 'stargazer', 'rgdal', 'sidrar',
         'geobr', 'gghighlight', 'ggridges', 'ggthemes', 'viridis', 'patchwork', 'naniar', 'scales',
         'plyr', 'tidyverse' #, pof
         )

lapply(pkg, function(x)
  if(!require(x, character.only = T))
  {install.packages(x); require(x)})

select <- dplyr::select
mutate <- dplyr::mutate
summarise <- dplyr::summarise



# 2. TEMA
# 2.1. MAPAS
# 2.2. GRÁFICOS
# theme_set(theme_bw(base_size = 16) +
#             theme(plot.title = element_text(size = rel(1),
#                                             face = 'bold',
#                                             hjust = 0.5,
#                                             vjust = 3),
#                   plot.subtitle = element_text(size = rel(1),
#                                                face = 'italic',
#                                                hjust = 0.5,
#                                                vjust = 3),
#                   plot.margin = margin(20,20,20,20),
#                   panel.grid = element_line(size = rel(0.2)),
#                   axis.title.x = element_text(size = rel(0.85),
#                                               hjust = 0.5,
#                                               vjust = -3),
#                   axis.title.y = element_text(size = rel(0.85),
#                                               hjust = 0.5,
#                                               vjust = 3),
#                   axis.text.x = element_text(size = rel(0.8)),
#                   axis.text.y = element_text(size = rel(0.8)),
#                   strip.text.x = element_text(size = rel(0.8)),
#                   strip.text.y = element_text(size = rel(0.8)),
#                   legend.title = element_text(size = rel(0.8)),
#                   legend.text = element_text(size = rel(0.7)),
#                   legend.background = element_blank(),
#                   legend.position = 'right',
#                   legend.direction = 'vertical',
#                   legend.box.background = element_blank()))
# 
# theme_ridges(font_size = 16) +
#   theme(plot.title = element_text(size = rel(1),
#                                   face = 'bold',
#                                   hjust = 0.5,
#                                   vjust = 3),
#         plot.margin = margin(20,20,20,20),
#         panel.grid = element_line(size = rel(0.2)),
#         axis.title.x = element_text(size = rel(0.8),
#                                     hjust = 0.5,
#                                     vjust = -3),
#         axis.title.y = element_text(size = rel(0.8),
#                                     hjust = 0.5,
#                                     vjust = 3),
#         axis.text.x = element_text(size = rel(0.8)),
#         axis.text.y = element_text(size = rel(0.8)),
#         strip.text.x = element_text(size = rel(0.8)),
#         strip.text.y = element_text(size = rel(0.8)),
#         legend.title = element_text(size = rel(0.8)),
#         legend.text = element_text(size = rel(0.7)),
#         legend.background = element_blank(),
#         legend.position = 'right',
#         legend.direction = 'vertical',
#         legend.box.background = element_blank()) -> theme_ridges.2


# 3. DADOS
# 3.1. LINK / ARQUIVO
# PIB 2010
pib.2010_regioes <- 'C:/Users/Sony/Downloads/PIB.2010.Regioes.csv'
pib.2010_uf <- 'C:/Users/Sony/Downloads/PIB.2010.UF.csv'
pib.2010_municipios <- 'C:/Users/Sony/Downloads/PIB.2010.Municipios.csv'

# Censo 2010
censo.2010_regioes <- 'C:/Users/Sony/Downloads/Censo.2010.Regioes.csv'
censo.2010_uf <- 'C:/Users/Sony/Downloads/Censo.2010.UF.csv'
censo.2010_municipios <- 'C:/Users/Sony/Downloads/Censo.2010.Municipios.csv'
 
# # POF 2008-2009
# Consumo
pof.2008_consumo <- 'C:/Users/Sony/Downloads/consumo.csv'

# Individuos
pof.2008_individuos <- 'C:/Users/Sony/Downloads/individuos.csv'


# 3.2. BASES DE DADOS
# PIB 2010
read.csv(pib.2010_regioes, 
         fileEncoding = 'UTF-8-BOM',
         header = T, sep = ';') -> pib.2010_regioes.df

read.csv(pib.2010_uf, 
         fileEncoding = 'UTF-8-BOM',
         header = T, sep = ';') -> pib.2010_uf.df

read.csv(pib.2010_municipios, 
         fileEncoding = 'UTF-8-BOM',
         header = T, sep = ';') -> pib.2010_municipios.df

# Censo 2010
read.csv(censo.2010_regioes, 
         fileEncoding = 'UTF-8-BOM',
         header = T, sep = ';') -> censo.2010_regioes.df

read.csv(censo.2010_uf, 
         fileEncoding = 'UTF-8-BOM',
         header = T, sep = ';') -> censo.2010_uf.df

read.csv(censo.2010_municipios, 
         fileEncoding = 'UTF-8-BOM',
         header = T, sep = ';') -> censo.2010_municipios.df

# Malhas Geográficas 2010
read_region(year = 2010) %>% 
  mutate(across(.cols = -geom,
                .fns = factor)) -> geom.2010_regioes.df

read_state(year = 2010) %>% 
  mutate(across(.cols = -geom,
                .fns = factor)) -> geom.2010_uf.df

read_municipality(year = 2010) %>% 
  mutate(across(.cols = -geom,
                .fns = factor)) -> geom.2010_municipios.df


# POF 2008-2009
# pof::download_pof(2009)
# unzip_pof(2009)

# pof::download_pof(2018)
# unzip_pof(2018)
read.csv(pof.2008_consumo, 
         fileEncoding = 'UTF-8-BOM',
         header = T, sep = ';') -> pof.2008_consumo.df

read.csv(pof.2008_individuos, 
         fileEncoding = 'UTF-8-BOM',
         header = T, sep = ';') -> pof.2008_individuos.df


# SIDRA R
# Todas as idades, exceto agregações
# Obs: é necessário fazer muitas requisições por conta do limite de dados por requisição do SIDRA
# Obs: essa etapa é demorada
info_sidra(1378)$classific_category$`c287 = Idade (135):` %>% 
  filter(str_detect(desc,'ano') == T,
         str_detect(desc,' a ') == F,
         str_detect(desc,'Total') == F,
         str_detect(desc,'ou') == F | str_detect(desc,'100')) %>%
  pull(cod) %>%
  lapply(function(idades.cod){
    get_sidra(1378,
              variable = 93,
              geo = 'City',
              classific = 'C287',
              category = list('C287' = idades.cod)) %>%
      # get_sidra(1378,
      #           variable = 93,
      #           period = '2010',
      #           geo = 'City',
      #           geo.filter = list('City' = municipio),
      #           classific = c('C287','C2'),
      #           category = list('C287' = idades.cod,
      #                           'C2' = c(4,5))) %>%
      select('Ano', 'Município (Código)', 'Município',
             'Idade', 'Sexo', 'Valor')
  }) %>% bind_rows(.) -> censo.2010_municipios.df

info_sidra(1378) %>% 
  filter(str_detect(desc,'ano') == T,
         str_detect(desc,' a ') == F,
         str_detect(desc,'Total') == F) %>%
  pull(cod) -> idades.cod

get_sidra(1378,
          period = '2010',
          geo = 'State',
          classific = c('C287','C2'),
          category = list('C287' = idades.cod,
                          'C2' = c(4,5))) %>% 
  View(.)

lapply(geom.2010_municipios.df$code_muni[1:3],
       function(x){
         get_sidra(1378,
                   period = '2010',
                   geo = 'Region',
                   classific = c('C287','C2'),
                   category = list('C287' = names(x),
                                   'C2' = c(4,5))) %>% 
           View(.)
       })

df <- geom.2010_municipios.df$code_muni

mapply(function(df,cod){
  get_sidra(1378,
            period = '2010',
            geo = 'Region',
            classific = c('C287','C2'),
            category = list('C287' = cod,
                            'C2' = c(4,5))) %>% 
    View(.)}, 
  df = geom.2010_municipios.df$code_muni,
  names(df))

get_sidra(1378,
          period = '2010',
          geo = 'Region',
          classific = c('C287','C2'),
          category = list('C287' = idades.cod,
                          'C2' = c(4,5))) %>% 
  View(.)


info_sidra(1378)$geo %>%
  View()



lapply(df, function(i){
  
  get_sidra(1378,
            variable = 93,
            period = '2010',
            geo = 'City',
            geo.filter = list('City' = df[i]),
            classific = c('C287','C2') ,
            category = list('C287' = idades.cod,
                            'C2' = c(4,5))) %>% 
    select('Valor', 'Município (Código)', 
           'Município', 'Ano', 'Idade', 'Sexo') %>%
    View(.)
  
})

get_sidra(1378,
          period = '2010',
          geo = 'City',
          classific = 'C287',
          category = list('C287' = 0)) %>% 
  pull('Município (Código)') %>%
  
  lapply(function(municipio){
    
    get_sidra(1378,
              variable = 93,
              period = '2010',
              geo = 'City',
              geo.filter = list('City' = municipio),
              classific = c('C287','C2'),
              category = list('C287' = idades.cod,
                              'C2' = c(4,5))) %>% 
      select('Valor', 'Município (Código)', 
             'Município', 'Ano', 'Idade', 'Sexo') %>%
      pull(Município) %>%
      unique(.)
  })



intersect(cod.mun,df) %>% length(.)
lapply(cod.mun,function(x) print(x))
length(df)

# The geo argument can be one of "Brazil", "Region", "State", "MesoRegion", "MicroRegion", "MetroRegion", "MetroRegionDiv", "IRD", "UrbAglo", "City", 


get_sidra(1378,
          geo = c('State','City'),
          classific = c('C287'),
          category = list('C287' = c(0,93070,93071,93072,93073,
                                     93074,93075,93076,93077,
                                     93078,93079,93080,93081,
                                     93082,93084,93085,107453,
                                     111286,93087,93088,93089,
                                     93090,93091,93092,93093,
                                     93094,11942,93095,93096,
                                     496,93097,93098,93099,93100))) %>% View(.)
get_sidra(6715, 
          period = "all", 
          variable = 1204, 
          classific = c("C12190","C339"),
          category = list(c(8018,103561,103574,
                            103585,103618, 103539), 
                          c(47558, 47559,47560,
                            47561,47562,47563,
                            47564))) -> pof.2
info_sidra(1378, wb = T)

info_sidra(6717) 

?get_sidra(geo = '')
get_sidra(6715, 
          period = "all", 
          variable = 1204) %>% View()


# Listas
list(regioes = censo.2010_regioes.df,
     uf = censo.2010_uf.df,
     municipios = censo.2010_municipios.df) -> censo.2010

list(regioes = pib.2010_regioes.df,
     uf = pib.2010_uf.df,
     municipios = pib.2010_municipios.df) -> pib.2010

list(regioes = geom.2010_regioes.df,
     uf = geom.2010_uf.df,
     municipios = geom.2010_municipios.df) -> geom.2010

# 3.3. AJUSTES
# Censo
lapply(censo.2010, function(x){
  x %>% 
    mutate(Total = Homens + Mulheres) %>%
    mutate(across(.cols = c(1,2),
                  .fns = factor)) %>%
    select(-Domicílio) %>%
    # Dados Wide
    pivot_wider(id_cols = c(1,2),
                names_from = Idade,
                values_from = c(Homens, Mulheres, Total),
                values_fill = 0, names_sep = ': ', names_sort = T) %>% 
    mutate(Total.Geral = rowSums(select(.,contains('Total')), na.rm = T)) %>%
    select(-contains('NA'))
}) -> censo.2010

# PIB
lapply(pib.2010, function(x){
  x %>%
    mutate(PIB.2010_Mil.Reais = as.character(PIB.2010_Mil.Reais),
           PIB.2010_Mil.Reais = str_replace(PIB.2010_Mil.Reais,',','.'),
           PIB.2010_Mil.Reais = as.numeric(as.character(PIB.2010_Mil.Reais)),
           PIB.2010_Mil.Reais = PIB.2010_Mil.Reais*1000) %>%
    dplyr::rename(PIB.2010 = PIB.2010_Mil.Reais) %>%
    mutate(across(.cols = c(1,2),
                  .fns = factor)) %>% 
    # Obs: NA's = municípios criados após o Censo de 2010 (Sem PIB disponível)
    drop_na(.) %>% 
    mutate(across(.cols = c(1,2),
                  .fns = factor))
}) -> pib.2010

# 3.4. PIB PER CAPITA E POR ADULTO-EQUIVALENTE
Map(merge, pib.2010, censo.2010) %>%
  lapply(function(x){
    x %>%
      mutate(PIB.2010_Per.Capita = PIB.2010/Total.Geral,
             PIB.2010_SQRT = PIB.2010/sqrt(Total.Geral),
             PIB.2010_SQRT_Per.Capita.dif = PIB.2010_SQRT - PIB.2010_Per.Capita,
             PIB.2010_SQRT_Per.Capita.prop = PIB.2010_SQRT/PIB.2010_Per.Capita)
  }) -> censo.pib.2010

# 3.5. MALHAS GEOGRÁFICAS
Map(merge, censo.pib.2010, geom.2010,
    MoreArgs = list(all.x = T, by.x = 1, by.y = 1)) -> censo.pib.geom.2010


# 3.6 AGREGAÇÃO POF 2008-2009
# Função de Agregação
pof.agg.2 <- function(individuos, consumo, 
                      faixas.etarias, sexo = T){
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
    pivot_wider(names_from = cod_rel_pess_refe_uc,
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
    pivot_wider(names_from = c(cod_sexo, idade),
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
  merge(individuos.agg_parentesco,
        individuos.agg_sexo.idade) -> individuos.agg
  
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
              
              # Código regional utilizado pelo IBGE = Primeiro número do código estadual
              code_region = substr(cod_uf, start = 1, stop = 1),
              code_region = factor(code_region),
              
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
  merge(individuos.agg, consumo.agg)
  
}

# Faixas Etárias
faixas.etarias.vaz <- c(0, 4, 9, 14, max(pof.2008_individuos.df$idade_anos))
# faixas.etarias.vaz <- c(0, 4, 9, 14, max(tr1_4.pof2008[[2]]$idade_anos))

# Agregação
pof.agg.2(consumo = pof.2008_consumo.df,
          individuos = pof.2008_individuos.df,
          faixas.etarias = faixas.etarias.vaz) -> pof.2008


# 3.7. DATA FRAMES FINAL
# OBS: os dados em nível municipal da POF são limitados demais para juntá-los com os demais
# => Agregações de POF e Censo.PIB.Geom apenas em nível regional e estadual)
pof.2008 %>% 
  group_by(cod_uf, code_region,
           urbano, classe.social) %>% 
  summarise(across(.cols = contains('qtd_morador'),
                   .fns = list(mean = mean, median = median, sum = sum),
                   .names = "{col}_{fn}"),
            across(.cols = contains('despesa'),
                   .fns = list(mean = mean, median = median, sum = sum),
                   .names = "{col}_{fn}"),
            across(.cols = contains('receita'),
                   .fns = list(mean = mean, median = median, sum = sum),
                   .names = "{col}_{fn}"),
            across(.cols = contains('renda'),
                   .fns = list(mean = mean, median = median, sum = sum),
                   .names = "{col}_{fn}")) -> pof.2008.agg


full_join(pof.2008.agg, censo.pib.geom.2010$uf,
          by = c('cod_uf' = 'Cód.',
                 'code_region' = 'code_region'))




# 5. MAPAS
# Mapa genérico
map.sf <- function(dados, var = 'PIB.2010_SQRT',
                   nivel = 'regioes',
                   nivel.contorno = 'uf',
                   nivel.filtro = NULL,
                   cor.contorno = '#f1faee',
                   option.viridis = 'viridis',
                   map = 'geom'){
  
  if(missing(nivel.filtro))
    
    dados[[nivel]] %>%
    ggplot(aes(geometry = !!sym(map))) + 
    geom_sf(aes(fill = !!sym(var),
                color = !!sym(var))) +
    geom_sf(data = dados[[nivel.contorno]],
            fill = NA, color = cor.contorno,
            size = .1) +
    scale_fill_viridis(option = option.viridis) + 
    scale_color_viridis(option = option.viridis) +
    ggthemes::theme_map()
  
  else
    
    dados[[nivel]] %>%
    filter(eval(rlang::parse_expr(nivel.filtro))) %>%
    ggplot(aes(geometry = !!sym(map))) + 
    geom_sf(aes(fill = !!sym(var),
                color = !!sym(var))) +
    geom_sf(data = dados[[nivel.contorno]] %>%
              filter(eval(rlang::parse_expr(nivel.filtro))),
            fill = NA, color = cor.contorno,
            size = .1) +
    scale_fill_viridis(option = option.viridis) + 
    scale_color_viridis(option = option.viridis) +
    ggthemes::theme_map()
}


# # Nacional
# Regional
censo.pib.geom.2010 %>%
  map.sf(nivel = 'regioes',
         nivel.contorno = 'regioes')

# Estadual
censo.pib.geom.2010 %>%
  map.sf(nivel = 'uf',
         nivel.contorno = 'uf')

# Municipal
censo.pib.geom.2010 %>%
  map.sf(nivel = 'municipios',
         nivel.contorno = 'uf')

# # Regional
# Estadual
for(regiao in seq(min(censo.pib.geom.2010$uf$code_region),
                  max(censo.pib.geom.2010$uf$code_region),1)) {
  
  censo.pib.geom.2010 %>%
    map.sf(nivel = 'uf',
           nivel.contorno = 'uf',
           nivel.filtro = paste0('code_region ==',
                                 regiao)) %>%
    plot(.)
}

# Municipal
for(regiao in seq(min(censo.pib.geom.2010$uf$code_region),
                  max(censo.pib.geom.2010$uf$code_region),1)) {
  
  censo.pib.geom.2010 %>%
    map.sf(nivel = 'municipios',
           nivel.contorno = 'uf',
           nivel.filtro = paste0('code_region ==', regiao)) %>%
    plot(.)
}



# # Estadual
# Municipal


# ncol - 1 
censo.pib.geom.2010$municipios %>% 
  # filter(abbrev_state %in% c('SC', 'PR', 'SP', 'MG', 'GO')) %>%
  ggplot(aes(geometry = geom,
             fill = PIB.2010_SQRT)) + 
  geom_sf(color = NA) +
  scale_fill_viridis() + 
  scale_color_viridis()



censo.pib.geom.2010$uf %>%
  # filter(abbrev_state == 'PR') %>%
  ggplot(aes(geometry = geom,
             fill = PIB.2010_SQRT_Per.Capita.dif,
             color = PIB.2010_SQRT_Per.Capita.dif)) + 
  geom_sf(size = .15) +
  scale_fill_viridis() + 
  scale_color_viridis() 
# gghighlight(PIB.2010 >= median(PIB.2010))

censo.pib.geom.2010$municipios %>%
  filter(abbrev_state == 'PR') %>%
  ggplot(aes(geometry = geom,
             fill = PIB.2010_Per.Capita,
             color = PIB.2010_Per.Capita)) + 
  geom_sf() +
  scale_fill_viridis() + 
  scale_color_viridis()


# Estaduais
# Municipais

