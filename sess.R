## Sess1

library("readr")
library(syuzhet)
library(tidyverse)

nk2008 = read_tsv('nk2007')
cfk2008 = read_tsv('cfk2008')
cfk2009 = read_tsv('cfk2009')
cfk2010 = read_tsv('cfk2010')
cfk2011 = read_tsv('cfk2011')
cfk2012 = read_tsv('cfk2012')
cfk2013 = read_tsv('cfk2013')
cfk2014 = read_tsv('cfk2014')
cfk2015 = read_tsv('cfk2015')

lexico <- read.csv("https://bitsandbricks.github.io/data/sdal.csv", 
                   stringsAsFactors = FALSE)
stopwords_es <- read.csv("https://bitsandbricks.github.io/data/stopwords_es.csv",
                         stringsAsFactors = FALSE)

get_sentiment_sdal = function(s_v){
  s_v_token1 = get_tokens(s_v)
  s_v_token1= s_v_token1[!(s_v_token1 %in% stopwords_es$STOPWORD)]
  get_sentiment(s_v_token1)
  sent = lexico %>% filter(palabra %in% s_v_token1) %>% select(media_agrado) %>% colMeans()
  sent
}
get_sentiment_discurso = function(disc){
  s_v = get_sentences(as_vector(disc))
  a = sapply(s_v,get_sentiment_sdal)
  mean(a, na.rm = T)
}
cfk = c(cfk2008,cfk2009,cfk2010,cfk2011,cfk2012,cfk2013,cfk2014,cfk2015)
cfk_a = sapply(cfk, get_sentiment_discurso)
cfk_df = enframe(cfk_a)
cfk_df$name = c(2008,2009,2010,2011,2012,2013,2014,2015)
macri2016 = read_tsv('macri2016')
macri2017 = read_tsv('macri2017')
macri2018 = read_tsv('macri2018')
macri2019 = read_tsv('macri2019')


macri = c(macri2016,macri2017,macri2018,macri2019)
macri_a = sapply(macri, get_sentiment_discurso)
macri_df = enframe(macri_a)
macri_df$name = c(2012,2013,2014,2015)+4

cfk_df = cfk_df %>% mutate(
  Presidente = 'CFK'
)

macri_df= macri_df %>% mutate(
  Presidente = 'Macri'
)

cfkmm_df = rbind(cfk_df,macri_df)


normalize <- function(x)
{
  return((x- min(x)) /(max(x)-min(x)))
}

ppc = data.frame(
  value = c(7.92,3.029,-6.854, 9.3,4.789,-2.145, 1.266, -3.579,1.63, -3.11, 1.609, -3.5)/100,
  pbi = c(9901, 10201, 9502, 10385, 10883, 10649, 10784, 10398, 10568,10239, 10404, 10040),
  anio = c(2008, 2009, 2010, 2011, 2012, 2013,2014,2015,2016,2017,2018,2019)
)

ppc = ppc %>% mutate(
  value_sc = scale(pbi)
)

cfkmm_df = cfkmm_df %>% mutate(
  value_scaled = scale(value)
)
x= cfkmm_df$value
xroc = x[-1] / x[-length(x)]-1
cfkmm_df['roc']= c(0,xroc)


x= ppc$pbi
xroc = x[-1] / x[-length(x)]-1
ppc['roc']= c(0,xroc)


cfk_df
rate <- 100*diff(cfkmm_df$value)/df[-nrow(cfkmm_df),]$price

ppc %>% ggplot()+geom_line(aes(x=anio, y = value), color = 'red')+geom_line(aes(x=anio, y = value_sc))

cfkmm_df %>% ggplot()+geom_line(aes(x = name , y = value))+scale_x_continuous()

ggplot()+geom_line(data = cfkmm_df, aes(x = name, y = roc, color = Presidente))+geom_line(data = ppc, aes(x = anio, y = roc))+scale_x_continuous(name = "Año", breaks = c(2008, 2009, 2010, 2011, 2012, 2013,2014,2015,2016,2017,2018,2019))+scale_y_continuous(name = "Optimismo/variacion PBI Per Capita")+ 
  scale_color_manual(values=c("blue", "yellow"))+theme_classic()
