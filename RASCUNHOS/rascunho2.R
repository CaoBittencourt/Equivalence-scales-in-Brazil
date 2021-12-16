read.csv(
  url('https://docs.google.com/spreadsheets/d/e/2PACX-1vRsrF8OaQGKav6PMpUKtnxMYJheKdti3WDkCql92BpmaXepFUUsYqmIuKuMO5h6gumDo8EyDDJWNs4o/pub?gid=1869709918&single=true&output=csv')
) -> dsds

dsds %>% glimpse()

dsds %>% 
  mutate(
    ref = ifelse(
      is.na(A)
      , yes = 2
      , no = 1
    )
  ) -> dsds

dsds %>% 
  pivot_longer(
    cols = c(A,AA,AAC,AACC,AACCC)
    ,names_to = 'scale'
    ,values_to = 'scale.val'
  ) -> dsds


dsds %>% 
  group_by(
    Abordagem
    ,Metodo
    ,Fonte
  ) %>% 
  filter(!is.na(scale.val)) %>% 
  mutate(
    # AA.cost = filter(.,scale == 'AA') %>% pull(scale.val)
    # AA.cost = filter(.,scale == 'AA') %>% pull(scale.val)
    # child.cost = scale.val - first(scale.val)
    # ,child.cost = scale.val - AA.cost
    # ,child.cost = ifelse(
    # str_count(scale,'C') == 0
    # , yes = NA
    # , no = child.cost*(ref/str_count(scale,'C'))
    # )
    # ,child.cost.bool = child.cost <= 1
    # ,
    acresc = scale.val - lag(scale.val)
    ,acresc.posit = acresc > 0
    ,acresc.inf.1 = ifelse(
      ref == 1
      , yes = acresc <= 1
      , no = acresc <= 0.5
    )
    ,acresc.decres = round(acresc,2) <= round(lag(acresc),2)
  ) %>% 
  summarise(
    across(
      .cols = contains('acresc.')
      ,.fns = function(x){sum((x-1)*(-1),na.rm = T)}
    )
  ) %>% View(.)




(2.37-1.97)/1 
(2.85-1.97)/2 
(3.41-1.97)/3 

(1.2-1)*(2/1) 
(1.45-1)*(2/2)
(1.75-1)*(2/3)

(2.66-1.72)/1 
(3.63-1.72)/2 
(4.67-1.72)/3 

(1.12-1)*(2/1)
(1.21-1)*(2/2)

(1.15-1)*(2/1)
(1.32-1)*(2/2)
(1.52-1)*(2/3)

(1.29-1)*(2/1)
(1.23-1)*(2/2)
(1.22-1)*(2/3)

(1.68-1.35)/1 
(1.84-1.35)/2 
(2.03-1.35)/3 

(1.12-1)*(2/1)
(1.24-1)*(2/2)
(1.36-1)*(2/3)

(1.78-1.65)/1 
(1.92-1.65)/2 
(2.13-1.65)/3 

(1.79-1.71)/1 
(1.99-1.71)/2 
(2.24-1.71)/3 

(2.05-1.73)/1 
(1.94-1.73)/2 
(2.28-1.73)/3 

(2.03-1.9)/1 
(1.97-1.9)/2 
(2.17-1.9)/3 

(1.08-1)*(2/1)
(1.16-1)*(2/2)
(1.24-1)*(2/3)

(1.15-1)*(2/1)
(1.28-1)*(2/2)
(1.38-1)*(2/3)

(1.12-1)*(2/1)
(1.24-1)*(2/2)
(1.36-1)*(2/3)

(1.25-1.2)/1 
(1.31-1.2)/2 
(1.39-1.2)/3 

(1.98-1.53)/1 
(2.35-1.53)/2 
(2.66-1.53)/3 

(1.31-1.2)/1 
(1.94-1.2)/2 

(1.53-1.36)/1 
(1.69-1.36)/2 

(1.76-1.58)/1 
(2.11-1.58)/2 
(2.26-1.58)/3 

(2.39-1.97)/1 
(2.75-1.97)/2 

(2.4-1.22)/1 
(2.1-1.22)/2 
(1.16-1.22)/3 

(1.9-1.65)/1 
(2.31-1.65)/2 
(2.94-1.65)/3 

(1.56-1.48)/1 
(1.74-1.48)/2 
(1.95-1.48)/3 

(1.22-1)*(2/1)
(1.28-1)*(2/2)

(1.62-1.76)/1 
(1.89-1.76)/2 
(2.15-1.76)/3

(1.77-1.66)/1 
(2.4-1.66)/2

(1.9-1.66)/1 
(2.12-1.66)/2 
(2.39-1.66)/3

(sqrt(3) - sqrt(2))/1
(sqrt(4) - sqrt(2))/2
(sqrt(5) - sqrt(2))/3


# plausibilidade
list(
  'engel' = c(4,3,3,3)
  ,'roth' = c(4,3,2)
  ,'(E)les' = c(3,4,3,3)
  ,'qes' = c(2,2)
  ,'aids'= c(4,4,4,3)
  ,'qaids'=c(4,3,4,3)
  ,'semipar'=c(4,0,3,3,4,2)
  ,'nopar'=c(3,3)
  ,'expert'=c(4,4,4)
) %>% sapply(mean)
