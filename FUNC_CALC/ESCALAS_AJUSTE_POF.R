# 1. PACOTES --------------------------------------------------------------

# 2. FUNÇÃO DE AJUSTE DE POF  ---------------------------------------------
# Ajuste de renda conforme escala de equivalência
pof.ajuste.escalas.ac_fun <- function(
  df.pof
  ,df.escalas.ac
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
  
  df.pof %>%
    select(
      contains(c('] anos', '[')), #C
      contains(c('] anos', '(')) #A
    ) %>% names(.) -> old.names
  
  c('C', 'A') -> new.names
  
  names(df.pof)[match(old.names,
                      names(df.pof))] <- new.names
  
  df.pof %>% 
    mutate(
      A.str = strrep('A', A),
      C.str = strrep('C', C),
      family.type = paste0(A.str, C.str)
    ) %>% 
    select(!c(A.str, C.str))-> df.pof
  
  df.escalas.ac %>%
    select(equivalence.scale
           ,family.type
           ,member.cost) %>%
    merge(df.pof) -> df.pof
  
  # 2. RENDA POR ADULTO-EQUIVALENTE
  
  # df.pof %>%
  #   mutate(
  #     renda.ajustada = 
  #   )
  
  # 3. DESPESAS POR ADULTO-EQUIVALENTE
  
  
  return(df.pof)
  
}


pof.ajuste.escalas.ac_fun(
  df.pof = lista.pof2002_ss$pof_ac_2002_ss
  ,df.escalas.ac = pof2002_ss.engel_scales
) %>% View(.)


# %>% 
#   mutate(
#     Afamily.type = strrep('A', A),
#     Cfamily.type = strrep('C', C),
#     family.type = paste0(Afamily.type,Cfamily.type)
# )

# adulto equivalente
5000/sqrt(5)
5000/5


(5000/5)/sqrt(5)

5000/2
(5000/2)/sqrt(2)



# per capita
5000/5
5000*5





tibble(
  b = c(
    0.42
    ,0.30
    ,0.20
  ),
  a = seq(1,3)
) %>% 
  ggplot(
    aes(x = a, y = b)
  ) +
  # geom_line()
  geom_bar(stat = 'identity')

tibble(
  b = c(
    sqrt(2)
    ,sqrt(3)
    ,sqrt(4)
    ,sqrt(5)
  ),
  a = seq(0,3)
) %>% 
  ggplot(
    aes(x = a, y = b)
  ) +
  geom_bar(stat = 'identity') + 
  ggthemes::theme_economist()




