# 1. PACOTES --------------------------------------------------------------

# 2. FUNÇÃO DE AJUSTE DE POF  ---------------------------------------------
# Ajuste de renda conforme escala de equivalência
pof.ajuste.escalas.ac_fun <- function(
  df.pof
  ,df.escalas.ac
  ,var.adulto
  ,var.crianca = '`[0,14] anos`'
){
  
  # Renda por adulto-equivalente
  # Diferencial entre a renda per-capita e renda por adulto-equivalente
  # Renda total equivalente ()
  
  # Despesas por adulto-equivalente
  # Diferencial entre a despesa per-capita e despesa por adulto-equivalente
  # Despesa total equivalente ()
  
  # CUIDADO: não deve-se misturar despesas equivalentes com renda equivalente!
  # Apenas deve-se ajustar ou renda, ou despesa! Do contrário o ajuste é feito 2x.
  
  # 1. TIPO DA FAMÍLIA
  a<-enquo(var.adulto)
  c<-enquo(var.crianca)
  
  df.pof$a
  df.pof$c
  df.pof$A <- df.pof$a
  df.pof$C <- df.pof$var.crianca
  
  # df.pof %>% 
  #   mutate(
  #     # ,family.type = paste0(
  #     #   strrep('A',!!sym(var.adulto)),
  #     #   strrep('C',!!sym(var.crianca))
  #     # )
  #   ) -> df.pof
  
  # df.escalas.ac %>% 
  #   select(equivalence.scale) %>% 
  #   merge(df.pof) %>% 
  return(df.pof)
  
}


sym('`a`')

pof.ajuste.escalas.ac_fun(
  df.pof = lista.pof2002_ss$pof_ac_2002_ss,
  df.escalas.ac = lalala,
  var.adulto = '`(14,110] anos`',
  var.crianca = '`[0,14] anos`'
) %>% glimpse(.)

lista.pof2002_ss$pof_ac_2002_ss   [,'`(14,110] anos`'] %>% class(.) 

dfdf[,'UF_sigla']


# %>% 
#   mutate(
#     Afamily.type = strrep('A',`(14,110] anos`),
#     Cfamily.type = strrep('C',`[0,14] anos`),
#     family.type = paste0(Afamily.type,Cfamily.type)
    # )

# adulto equivalente
5000/sqrt(5)
5000/5

(5000/5)/sqrt(5)

# per capita
5000/5
5000*5
