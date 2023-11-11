library(tidyverse)
library(tidytext)

setwd("~/RPROJECTS/HumanFlourishing/twitter_Analysis/lists/")

all_tokens <- readRDS("all_tokens2.rds")

#recode
all_tokens$type <- recode(all_tokens$type, 
                          business="business",
                          hf="hflourishing",
                          influencers="influencers",
                          journalism="journalism",
                          startup="startup",
                          vc="venture")

all_tokens$type <- factor(all_tokens$type, levels= c(
  "hflourishing",
  "journalism",
  "startup",
  "influencers",
  "business",
  "venture"))

#scientific notation
scientific_10 <- function(x) {
  parse(text=gsub("e", " %*% 10^", scales::scientific_format()(x)))
}


#### Zipfs Law ####
fplot_1a <- all_tokens %>% 
  group_by(type) %>% 
  top_n(100000, percentage) %>%
  mutate(order=row_number()) %>% 
  ungroup() %>% 
  mutate(mode="Zipf's Law 100000 MFW") %>% 
  rename(lists=type) 

fplot_1b <- all_tokens %>% 
  anti_join(stop_words) %>% 
  group_by(type) %>% 
  top_n(100000, percentage) %>%
  mutate(order=row_number()) %>% 
  ungroup() %>% 
  mutate(mode="Zipf's Law 100000 MFW without stopwords") %>% 
  rename(lists=type) 

plot_1ab <- bind_rows(fplot_1a, fplot_1b)

plot_1ab <- plot_1ab %>% 
  ggplot(aes(order, percentage, group=lists, color=lists,
             size= lists, 
             alpha=lists))+
  geom_path()+
  scale_size_manual (values=c(2,1,1,1,1,1))+
  scale_alpha_manual (values=c(1,0.5,0.5,0.5,0.5,0.5))+
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1))+
  scale_x_log10(label=scientific_10)+
  scale_y_log10(label=scientific_10)+
  theme_bw()+
  xlab("log(rank)")+
  ylab("log(percentage frequency)")+
  #ggtitle("Zipf's Law")+
  theme(plot.title = element_text(hjust = 0.5))+
  facet_wrap(~mode, scales = "fixed")+
  theme(axis.title = element_text(size = 8))

#### heaps law####
setwd("~/RPROJECTS/HumanFlourishing/twitter_Analysis/")
types_annotate <- readRDS("types_annotate.rds")

#all tokens
g1 <- types_annotate %>% 
  group_by(type) %>% 
  mutate(row=row_number()) %>% 
  filter(row<100) %>% 
  select(token) %>% 
  distinct() %>% 
  summarise(total=n()) %>%
  mutate(rank=100) %>% 
  ungroup()

g2 <- types_annotate %>% 
  group_by(type) %>% 
  mutate(row=row_number()) %>% 
  filter(row<500) %>% 
  select(token) %>% 
  distinct() %>% 
  summarise(total=n()) %>%
  mutate(rank=500) %>% 
  ungroup()

g3 <- types_annotate %>% 
  group_by(type) %>% 
  mutate(row=row_number()) %>% 
  filter(row<1000) %>% 
  select(token) %>% 
  distinct() %>% 
  summarise(total=n()) %>%
  mutate(rank=1000) %>% 
  ungroup()

g4 <- types_annotate %>% 
  group_by(type) %>% 
  mutate(row=row_number()) %>% 
  filter(row<1500) %>% 
  select(token) %>% 
  distinct() %>% 
  summarise(total=n()) %>%
  mutate(rank=1500) %>% 
  ungroup()

g5 <- types_annotate %>% 
  group_by(type) %>% 
  mutate(row=row_number()) %>% 
  filter(row<2000) %>% 
  select(token) %>% 
  distinct() %>% 
  summarise(total=n()) %>%
  mutate(rank=2000) %>% 
  ungroup()

g6 <- types_annotate %>% 
  group_by(type) %>% 
  mutate(row=row_number()) %>% 
  filter(row<3000) %>% 
  select(token) %>% 
  distinct() %>% 
  summarise(total=n()) %>%
  mutate(rank=3000) %>% 
  ungroup()

g7 <- types_annotate %>% 
  group_by(type) %>% 
  mutate(row=row_number()) %>% 
  filter(row<4000) %>% 
  select(token) %>% 
  distinct() %>% 
  summarise(total=n()) %>%
  mutate(rank=4000) %>% 
  ungroup()

g8 <- types_annotate %>% 
  group_by(type) %>% 
  mutate(row=row_number()) %>% 
  filter(row<5000) %>% 
  select(token) %>% 
  distinct() %>% 
  summarise(total=n()) %>%
  mutate(rank=5000) %>% 
  ungroup()

g9 <- types_annotate %>% 
  group_by(type) %>% 
  mutate(row=row_number()) %>% 
  filter(row<6000) %>% 
  select(token) %>% 
  distinct() %>% 
  summarise(total=n()) %>%
  mutate(rank=6000) %>% 
  ungroup()

g10 <- types_annotate %>% 
  group_by(type) %>% 
  mutate(row=row_number()) %>% 
  filter(row<10000) %>% 
  select(token) %>% 
  distinct() %>% 
  summarise(total=n()) %>%
  mutate(rank=10000) %>% 
  ungroup()

g11 <- types_annotate %>% 
  group_by(type) %>% 
  mutate(row=row_number()) %>% 
  filter(row<100000) %>% 
  select(token) %>% 
  distinct() %>% 
  summarise(total=n()) %>%
  mutate(rank=100000) %>% 
  ungroup()


g_all <- bind_rows(g1, g2, g3, g4, g5, g6, g7, g8, g9, g10, g11)

g_all$type <- recode(g_all$type, 
                     business="business",
                     hf="hflourishing",
                     influencers="influencers",
                     journalism="journalism",
                     startup="startup",
                     vc="venture")

g_all$type <- factor(g_all$type, levels= c("hflourishing", 
                                           "journalism",
                                           "startup",
                                           "influencers",
                                           "business",
                                           "venture"))

fplot_1c <- g_all %>% 
  rename(lists=type) %>% 
  mutate(mode="Heaps' Law 100000 First Words") 

# without stopwords
types_stopwords <- types_annotate %>% 
  anti_join(stop_words, by = c("token"="word"))

gs1 <- types_stopwords %>% 
  group_by(type) %>% 
  mutate(row=row_number()) %>% 
  filter(row<100) %>% 
  select(token) %>% 
  distinct() %>% 
  summarise(total=n()) %>%
  mutate(rank=100) %>% 
  ungroup()

gs2 <- types_stopwords %>% 
  group_by(type) %>% 
  mutate(row=row_number()) %>% 
  filter(row<500) %>% 
  select(token) %>% 
  distinct() %>% 
  summarise(total=n()) %>%
  mutate(rank=500) %>% 
  ungroup()

gs3 <- types_stopwords %>% 
  group_by(type) %>% 
  mutate(row=row_number()) %>% 
  filter(row<1000) %>% 
  select(token) %>% 
  distinct() %>% 
  summarise(total=n()) %>%
  mutate(rank=1000) %>% 
  ungroup()

gs4 <- types_stopwords %>% 
  group_by(type) %>% 
  mutate(row=row_number()) %>% 
  filter(row<1500) %>% 
  select(token) %>% 
  distinct() %>% 
  summarise(total=n()) %>%
  mutate(rank=1500) %>% 
  ungroup()

gs5 <- types_stopwords %>% 
  group_by(type) %>% 
  mutate(row=row_number()) %>% 
  filter(row<2000) %>% 
  select(token) %>% 
  distinct() %>% 
  summarise(total=n()) %>%
  mutate(rank=2000) %>% 
  ungroup()

gs6 <- types_stopwords %>% 
  group_by(type) %>% 
  mutate(row=row_number()) %>% 
  filter(row<3000) %>% 
  select(token) %>% 
  distinct() %>% 
  summarise(total=n()) %>%
  mutate(rank=3000) %>% 
  ungroup()

gs7 <- types_stopwords %>% 
  group_by(type) %>% 
  mutate(row=row_number()) %>% 
  filter(row<4000) %>% 
  select(token) %>% 
  distinct() %>% 
  summarise(total=n()) %>%
  mutate(rank=4000) %>% 
  ungroup()

gs8 <- types_stopwords %>% 
  group_by(type) %>% 
  mutate(row=row_number()) %>% 
  filter(row<5000) %>% 
  select(token) %>% 
  distinct() %>% 
  summarise(total=n()) %>%
  mutate(rank=5000) %>% 
  ungroup()

gs9 <- types_stopwords %>% 
  group_by(type) %>% 
  mutate(row=row_number()) %>% 
  filter(row<6000) %>% 
  select(token) %>% 
  distinct() %>% 
  summarise(total=n()) %>%
  mutate(rank=6000) %>% 
  ungroup()

gs10 <- types_stopwords %>% 
  group_by(type) %>% 
  mutate(row=row_number()) %>% 
  filter(row<10000) %>% 
  select(token) %>% 
  distinct() %>% 
  summarise(total=n()) %>%
  mutate(rank=10000) %>% 
  ungroup()

gs11 <- types_stopwords %>% 
  group_by(type) %>% 
  mutate(row=row_number()) %>% 
  filter(row<100000) %>% 
  select(token) %>% 
  distinct() %>% 
  summarise(total=n()) %>%
  mutate(rank=100000) %>% 
  ungroup()

gs_all <- bind_rows(gs1, gs2, gs3, gs4, gs5, gs6, gs7, gs8, gs9, gs10, gs11)

gs_all$type <- recode(gs_all$type, 
                      business="business",
                      hf="hflourishing",
                      influencers="influencers",
                      journalism="journalism",
                      startup="startup",
                      vc="venture")

gs_all$type <- factor(gs_all$type, levels= c("hflourishing", 
                                             "journalism",
                                             "startup",
                                             "influencers",
                                             "business",
                                             "venture"))

fplot_1d <- gs_all %>% 
  rename(lists=type) %>% 
  mutate(mode="Heaps' Law 100000 Firsts Words without stopwords") 

plot_1cd <- bind_rows(fplot_1c, fplot_1d)

plot_1cd <- plot_1cd %>%  
  ggplot(aes(rank, total, color=lists, group=lists,
             size= lists, 
             alpha=lists))+
  geom_path()+
  theme_bw()+
  ylab("distinct vocabulary")+
  xlab("document length")+
  scale_size_manual (values=c(2,1,1,1,1,1))+
  scale_alpha_manual (values=c(1,0.5,0.5,0.5,0.5,0.5))+
  #ggtitle("Heaps' Law")+
  theme(plot.title = element_text(hjust = 0.5))+
  facet_wrap(~mode, scales = "fixed")+
  theme(axis.title = element_text(size = 8))#+ 
  # theme(plot.background = element_rect(fill = "#f1f5deff"))+
  # theme(legend.background = element_rect(fill = "#f1f5deff"))+
  # theme(legend.key = element_rect(fill = "#f1f5deff"))+
  # labs(color=NULL, size=NULL, alpha=NULL)

ggsave("heapsLawPoster.jpg", plot_1cdi, height = 1.6, width = 7, dpi = 300)

#### density ####
all_density <- all_tokens %>% 
  group_by(type) %>% 
  summarise(density=n()/sum(n)) %>% 
  ungroup()

fplot_1e <- all_density %>% 
  rename(lists=type) %>% 
  mutate(mode="Density")

all_density_2 <- all_tokens %>% 
  anti_join(stop_words) %>% 
  group_by(type) %>% 
  summarise(density=n()/sum(n)) %>% 
  ungroup()

fplot_1f <- all_density_2 %>% 
  rename(lists=type) %>% 
  mutate(mode="Density without stopwords")

plot_ef <- bind_rows(fplot_1e, fplot_1f)  

plot_1ef <- plot_ef %>% 
  ggplot(aes(lists, density, fill=lists, alpha= lists))+
  geom_col(show.legend=FALSE)+
  coord_flip()+
  #theme_bw()+
  theme(axis.ticks.y = element_blank(),
        axis.text.y = element_blank())+
  #ggtitle("Density")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme_minimal()+
  xlab(NULL)+
  ylab(NULL)+
  scale_alpha_manual (values=c(1,0.5,0.5,0.5,0.5,0.5))+
  facet_wrap(~mode, scales = "fixed")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(axis.text = element_text(size = 8))

####plot 1 arrange ####
arrange1 <- ggpubr::ggarrange(plot_1ab, 
                              plot_1cd,
                              plot_1ef,
                              common.legend = TRUE, legend = "bottom",
                              ncol = 1, nrow = 3, align = "hv", heights = c(2,2,1))

#ggsave("plots/plot1.png", arrange1, width = 7, height = 8, dpi = 300)
ggsave("Final_Plots/Fig2_alt.jpg", arrange1, width = 7, height = 8, dpi = 300)

#### NRC heatmap ####
NRC_2017_Translations <-readxl::read_excel("~/R/PROJECTS/NRC-Emotion-Lexicon/NRC-Emotion-Lexicon/NRC-Emotion-Lexicon-v0.92/NRC-Emotion-Lexicon-v0.92-In105Languages-Nov2017Translations.xlsx")
NRC <- NRC_2017_Translations %>% select(word=`English (en)...1`, Positive, Negative, Anger, Anticipation,
                                        Disgust, Fear, Joy, Sadness, Surprise, Trust)


setwd("~/R/PROJECTS/HumanFlourishing/twitter_Analysis/lists")
all_tokens <- readRDS("all_tokens2.rds")

#recode
all_tokens$type <- recode(all_tokens$type, 
                          business="business",
                          hf="hflourishing",
                          influencers="influencers",
                          journalism="journalism",
                          startup="startup",
                          vc="venture")

all_tokens$type <- factor(all_tokens$type, levels= c("hflourishing",
                                                     "journalism",
                                                     "startup",
                                                     "influencers",
                                                     "business",
                                                     "venture"))

NRC_tokens <- all_tokens %>% 
  inner_join(NRC) %>% 
  mutate(Positive=percentage*Positive) %>% 
  mutate(Negative=percentage*Negative) %>% 
  mutate(Trust=percentage*Trust) %>% 
  mutate(Surprise=percentage*Surprise) %>% 
  mutate(Anger=percentage*Anger) %>% 
  mutate(Anticipation=percentage*Anticipation) %>% 
  mutate(Disgust=percentage*Disgust) %>% 
  mutate(Fear=percentage*Fear) %>% 
  mutate(Joy=percentage*Joy) %>% 
  mutate(Sadness=percentage*Sadness)

#spliting by emotions heatmap
NRC_plot <- NRC_tokens %>% 
  gather(key = "emotion", value ="value", 5:14) %>% 
  filter(!emotion %in% c("Positive", "Negative")) %>% 
  filter(!value ==0) %>% 
  group_by(type, emotion) %>% 
  summarize(value=sum(value)) %>% 
  ggplot(aes(reorder(as_factor(emotion), -value), 
             reorder(as_factor(type), value),
             fill=value))+
  geom_tile(show.legend = FALSE)+
  geom_text(aes(label=round(value, 2)))+
  xlab(NULL)+
  ylab(NULL)+
  theme_minimal()+
  scale_fill_distiller(palette = 8,
                       direction = 1)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0, hjust=1))

setwd("~/R/PROJECTS/HumanFlourishing/twitter_Analysis")
ggsave("plots/Fig3.jpg", NRC_plot, width = 7.2, height = 3.2, dpi = 300)

#### emojis wordcloud ####
#volver a cargar datos
setwd("~/R/PROJECTS/HumanFlourishing/twitter_Analysis/lists/")
all_tokens <- readRDS("all_tokens2.rds")

library(emojifont)
library(emoji)
#library(emoGG)
emojis <- all_tokens %>% 
  filter(type=="hflourishing") %>% 
  pull(word) %>% 
  emoji_extract() 

emoji_detected <- emojis %>% as_tibble() %>% na.omit() %>% 
  count(value, sort = TRUE)

#emoji wordcloud2
emoji_detected %>% 
  top_n(100, n) %>% 
  wordcloud2::wordcloud2(size = 1, gridSize = 5)

#emoji wordcloud
#wordcloud::wordcloud(emoji_detected$value, freq= emoji_detected$n, min=3)

#export hd image
webshot::install_phantomjs()

library(wordcloud2)
hw <- emoji_detected %>% 
  top_n(100, n) %>%
  wordcloud2(size = 1, gridSize = 10)

htmlwidgets::saveWidget(hw,"1.html", selfcontained = FALSE) #no funciona
webshot::webshot("1.html","wordcloud_emojis.png", delay=120,
                 vwidth = 10000, vheight = 8000, cliprect = c(3000, 3000, 4000, 2000))

####emojis heaps law####
#volver a cargar datos
setwd("~/R/PROJECTS/HumanFlourishing/twitter_Analysis/")
types_annotate <- readRDS("types_annotate.rds")

library(emoji)
ge1 <- types_annotate %>% 
  group_by(type) %>% 
  mutate(row=row_number()) %>% 
  filter(row<100) %>% 
  select(token) %>% 
  mutate(emoji=emoji_extract(token)) %>% 
  distinct() %>% 
  summarise(total=n()) %>%
  mutate(rank=100) %>% 
  ungroup()

ge2 <- types_annotate %>% 
  group_by(type) %>% 
  mutate(row=row_number()) %>% 
  filter(row<500) %>% 
  select(token) %>% 
  mutate(emoji=emoji_extract(token)) %>% 
  distinct() %>% 
  summarise(total=n()) %>%
  mutate(rank=500) %>% 
  ungroup()

ge3 <- types_annotate %>% 
  group_by(type) %>% 
  mutate(row=row_number()) %>% 
  filter(row<1000) %>% 
  select(token) %>% 
  mutate(emoji=emoji_extract(token)) %>% 
  distinct() %>% 
  summarise(total=n()) %>%
  mutate(rank=1000) %>% 
  ungroup()

ge4 <- types_annotate %>% 
  group_by(type) %>% 
  mutate(row=row_number()) %>% 
  filter(row<1500) %>% 
  select(token) %>% 
  mutate(emoji=emoji_extract(token)) %>% 
  distinct() %>% 
  summarise(total=n()) %>%
  mutate(rank=1500) %>% 
  ungroup()

ge5 <- types_annotate %>% 
  group_by(type) %>% 
  mutate(row=row_number()) %>% 
  filter(row<2000) %>% 
  select(token) %>% 
  mutate(emoji=emoji_extract(token)) %>% 
  distinct() %>% 
  summarise(total=n()) %>%
  mutate(rank=2000) %>% 
  ungroup()

ge6 <- types_annotate %>% 
  group_by(type) %>% 
  mutate(row=row_number()) %>% 
  filter(row<3000) %>% 
  select(token) %>% 
  mutate(emoji=emoji_extract(token)) %>% 
  distinct() %>% 
  summarise(total=n()) %>%
  mutate(rank=3000) %>% 
  ungroup()

ge7 <- types_annotate %>% 
  group_by(type) %>% 
  mutate(row=row_number()) %>% 
  filter(row<4000) %>% 
  select(token) %>% 
  mutate(emoji=emoji_extract(token)) %>% 
  distinct() %>% 
  summarise(total=n()) %>%
  mutate(rank=4000) %>% 
  ungroup()

ge8 <- types_annotate %>% 
  group_by(type) %>% 
  mutate(row=row_number()) %>% 
  filter(row<5000) %>% 
  select(token) %>% 
  mutate(emoji=emoji_extract(token)) %>% 
  distinct() %>% 
  summarise(total=n()) %>%
  mutate(rank=5000) %>% 
  ungroup()

ge9 <- types_annotate %>% 
  group_by(type) %>% 
  mutate(row=row_number()) %>% 
  filter(row<6000) %>% 
  select(token) %>% 
  mutate(emoji=emoji_extract(token)) %>% 
  distinct() %>% 
  summarise(total=n()) %>%
  mutate(rank=6000) %>% 
  ungroup()

ge10 <- types_annotate %>% 
  group_by(type) %>% 
  mutate(row=row_number()) %>% 
  filter(row<10000) %>% 
  select(token) %>% 
  mutate(emoji=emoji_extract(token)) %>% 
  distinct() %>% 
  summarise(total=n()) %>%
  mutate(rank=10000) %>% 
  ungroup()

ge11 <- types_annotate %>% 
  group_by(type) %>% 
  mutate(row=row_number()) %>% 
  filter(row<25000) %>% 
  select(token) %>% 
  mutate(emoji=emoji_extract(token)) %>% 
  distinct() %>% 
  summarise(total=n()) %>%
  mutate(rank=25000) %>% 
  ungroup()

ge12 <- types_annotate %>% 
  group_by(type) %>% 
  mutate(row=row_number()) %>% 
  filter(row<50000) %>% 
  select(token) %>% 
  mutate(emoji=emoji_extract(token)) %>% 
  distinct() %>% 
  summarise(total=n()) %>%
  mutate(rank=50000) %>% 
  ungroup()

ge13 <- types_annotate %>% 
  group_by(type) %>% 
  mutate(row=row_number()) %>% 
  filter(row<100000) %>% 
  select(token) %>% 
  mutate(emoji=emoji_extract(token)) %>% 
  distinct() %>% 
  summarise(total=n()) %>%
  mutate(rank=100000) %>% 
  ungroup()


ge_all <- bind_rows(ge1, ge2, ge3, ge4, ge5, ge6, ge7, ge8, ge9, ge10, ge11, ge12)

ge_all$type <- recode(ge_all$type, 
                      business="business",
                      hf="hflourishing",
                      influencers="influencers",
                      journalism="journalism",
                      startup="startup",
                      vc="venture")

ge_all$type <- factor(ge_all$type, levels= c("hflourishing", 
                                             "journalism",
                                             "startup",
                                             "influencers",
                                             "business",
                                             "venture"))

emmojis_plot <- ge_all %>% 
  rename(lists=type) %>% 
  mutate(mode="50000 First Emojis") %>% 
  ggplot(aes(rank, total, color=lists, group=lists,
             size= lists, 
             alpha=lists))+
  geom_path()+
  theme_bw()+
  ylab("distinct emojis")+
  xlab("document length")+
  scale_size_manual (values=c(3,1.5,1.5,1.5,1.5,1.5))+
  scale_alpha_manual (values=c(1, 0.5,0.5,0.5,0.5,0.5))+
  #ggtitle("50000 Firsts Emojis")+
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 16),
        legend.text = element_text(size = 16))

ggsave("Final_Plots/Fig5.jpg", emmojis_plot, height = 1.5, width = 4.5, dpi = 300)

#### lemmatization ####
setwd("~/R/PROJECTS/HumanFlourishing/twitter_Analysis/")
types_annotate <- readRDS("types_annotate.rds")

#upos
plot_5 <- types_annotate %>% 
  group_by(type) %>% 
  count(upos, sort = TRUE) %>% 
  mutate(percentage=n/sum(n)*100) %>%
  ungroup() %>% 
  rename(lists=type) %>% 
  mutate(upos=gsub("NOUN", "Noun", upos),
         upos=gsub("PUNCT", "Punctuation", upos),
         upos=gsub("VERB", "Verb", upos),
         upos=gsub("PROPN", "Proper Noun", upos),
         upos=gsub("ADP", "Adposition", upos),
         upos=gsub("PRON", "Pronoun", upos),
         upos=gsub("ADJ", "Adjective", upos),
         upos=gsub("DET", "Determiner", upos),
         upos=gsub("AUX", "Auxiliary", upos),upos=gsub("AUX", "Auxiliary", upos),
         upos=gsub("ADV", "Adverb", upos),
         upos=gsub("PART", "Particle", upos),
         upos=gsub("SYM", "Symbol", upos),
         upos=gsub("X", "other...", upos),
         upos=gsub("CCONJ", "Coordinating Conjunction", upos),
         upos=gsub("SCONJ", "Subordinating Conjunction", upos),
         upos=gsub("NUM", "Numeral", upos),
         upos=gsub("INTJ", "Interjection", upos)) %>% 
  ggplot(aes(reorder(upos, percentage), percentage, group=lists, fill=lists))+
  geom_col(aes(alpha=lists), position = "dodge", width = 0.8)+
  coord_flip()+
  xlab(NULL)+
  ylab(NULL)+
  theme_bw()+
  #scale_discrete_manual (values=c(2,1,1,1,1,1))+
  scale_alpha_manual (values=c(1,0.4,0.4,0.4,0.4,0.4))

#hf specific
plot_5b <- types_annotate %>% 
  filter(upos%in%c("VERB", "PRON", 
                   "AUX", 
                   "X", 
                   "INTJ")) %>% 
  filter(type=="hflourishing") %>% 
  mutate(upos=gsub("VERB", "Verb", upos),
         upos=gsub("PRON", "Pronoun", upos),
         upos=gsub("AUX", "Auxiliary", upos),
         upos=gsub("X", "other...", upos),
         upos=gsub("INTJ", "Interjection", upos)) %>% 
  filter(!lemma=="https://t.co/1QTQlj6YKW") %>% 
  group_by(upos) %>%
  count(lemma, sort = TRUE) %>% 
  mutate(percentage=n/sum(n)*100) %>%
  top_n(10, percentage) %>% 
  ungroup() %>% 
  mutate(upos=as.factor(upos),
         lemma=reorder_within(lemma, percentage, upos)) %>% 
  ggplot(aes(reorder(lemma, percentage), percentage))+
  geom_col(fill="#F8766D", position = "dodge", show.legend = FALSE)+
  coord_flip()+
  scale_x_reordered()+
  xlab(NULL)+
  theme_bw()+
  facet_wrap(~reorder(upos, -percentage), scales = "free_y", ncol=5)

#ggarrange 5 y 5b
plots_5ab <- ggpubr::ggarrange(plot_5, plot_5b, common.legend = FALSE,
                               ncol = 1, nrow = 2, heights = c(2.1, 1.4))

ggsave("Final_Plots/Fig6.jpg", plots_5ab, height = 7, width=8, dpi = 300)

#check the others
types_annotate %>% 
  filter(upos%in%c("VERB", "PRON", "X", "INTJ")) %>% 
  filter(!type=="hflourishing") %>% 
  group_by(upos) %>%
  count(type, token, sort = TRUE) %>% 
  mutate(percentage=n/sum(n)*100) %>%
  top_n(10, percentage) %>% 
  ungroup() %>% 
  ggplot(aes(reorder(token, percentage), percentage))+
  geom_col(aes(fill=type), position = "dodge")+
  coord_flip()+
  xlab(NULL)+
  facet_wrap(~upos, scales = "free_y")

#feats
plot_6 <- types_annotate %>% 
  filter(upos=="VERB") %>% 
  group_by(type) %>% 
  count(feats, sort = TRUE) %>% 
  mutate(percentage=n/sum(n)*100) %>% 
  ungroup() %>% 
  rename(lists=type) %>% 
  filter(feats%in%c("VerbForm=Inf", "VerbForm=Ger", "Mood=Ind|Tense=Pres|VerbForm=Fin",
                    "Mood=Ind|Number=Sing|Person=3|Tense=Pres|VerbForm=Fin", 
                    "Mood=Imp|VerbForm=Fin","Tense=Past|VerbForm=Part", 
                    "Mood=Ind|Tense=Past|VerbForm=Fin", "Tense=Pres|VerbForm=Part", 
                    "Tense=Past|VerbForm=Part|Voice=Pass")) %>% 
  ggplot(aes(fct_reorder(feats, percentage), percentage, group=lists, fill=factor(lists, levels = levels(lists))))+
  geom_col(aes(fill=lists, alpha=lists), position = "dodge", 
           width = 0.8)+
  coord_flip()+
  xlab(NULL)+
  ylab(NULL)+
  theme_bw()+
  #scale_discrete_manual (values=c(2,1,1,1,1,1))+
  scale_alpha_manual (values=c(1,0.4,0.4,0.4,0.4,0.4))+ 
  scale_fill_discrete('lists') 

#hf specific
plot_6b <- types_annotate %>% 
  filter(feats%in%c("VerbForm=Inf", "Mood=Ind|Tense=Pres|VerbForm=Fin",
                    "Mood=Imp|VerbForm=Fin")) %>% 
  filter(type=="hflourishing") %>% 
  group_by(feats) %>%
  count(token, sort = TRUE) %>% 
  mutate(percentage=n/sum(n)*100) %>%
  top_n(10, percentage) %>% 
  ungroup() %>% 
  ggplot(aes(reorder(token, percentage), percentage))+
  geom_col(fill="#F8766D", position = "dodge", show.legend = FALSE)+
  coord_flip()+
  xlab(NULL)+
  facet_wrap(~feats, scales = "free_y")+
  theme_bw()

#ggarrange 6 y 6b
plots_6ab <- ggpubr::ggarrange(plot_6, plot_6b, common.legend = FALSE,
                               ncol = 1, nrow = 2, heights = c(2.1, 1.4))

ggsave("Final_Plots/Fig7.jpg", plots_6ab, dpi = 300, height = 7, width=8)


#check the others
types_annotate %>% 
  filter(feats%in%c("VerbForm=Inf", "Mood=Ind|Tense=Pres|VerbForm=Fin",
                    "Mood=Imp|VerbForm=Fin")) %>% 
  #filter(type=="hf") %>% 
  group_by(feats) %>%
  count(type, token, sort = TRUE) %>% 
  mutate(percentage=n/sum(n)*100) %>%
  top_n(10, percentage) %>% 
  ungroup() %>% 
  ggplot(aes(reorder(token, percentage), percentage))+
  geom_col(aes(fill=type), position = "dodge")+
  coord_flip()+
  xlab(NULL)+
  facet_wrap(~feats, scales = "free_y")

#### TOpic Modeling ####
setwd("~/R/PROJECTS/HumanFlourishing/twitter_Analysis")

library(topicmodels)
hf_lda <- readRDS("hf_lda.rds")

hf_topics <- tidy(hf_lda, matrix = "beta")

hf_gamma <- tidy(hf_lda, matrix = "gamma")

top_5_gamma <- hf_gamma %>% 
  arrange(desc(gamma)) %>%
  group_by(topic) %>%
  slice_max(order_by = gamma, n = 4) %>% 
  ungroup() %>% 
  rename(user_id=document)

#join with users definition
colors <- tibble(topic=c(1, 2,3, 4), color= c("#F8766D", "#7CAE00", "#00BFC4", "#C77CFF")) 

f_colors <- top_5_gamma %>% 
  right_join(colors)

#tablegrob format
library(gridExtra)
library(grid)

colors_rep <- rep(colors$color, each=4)

tt1 <- ttheme_minimal(
  core=list(bg_params = list(fill = colors_rep, col="white"),
            fg_params=list(fontface=3, fontsize=12)),
  colhead=list(fg_params=list(col="navyblue", fontface=4L)),
  rowhead=list(fg_params=list(col="orange", fontface=3L)))

hf_users <- readRDS("hf_tw_2list.rds")


hf_table <- hf_users %>% mutate(picture=paste0('<img src="https://cousateca.info/wp-content/uploads/hf_tw_images/', 
                                               screen_name,'" height="52"></img>')) %>% 
  select(user_id, name, description, location) %>% 
  inner_join(top_5_gamma) %>% 
  arrange(topic) %>% 
  select(name, description, topic) %>% 
  mutate(description=gsub("\n"," ", description)) %>% 
  mutate(description=gsub('(.{90})', '\\1\n', description)) %>% 
  tableGrob(theme = tt1, rows = NULL)

#join with table
plot_topics <- hf_topics %>% 
  arrange(desc(beta)) %>%
  group_by(topic) %>%
  slice_max(order_by = beta, n = 12) %>% 
  ungroup() %>%
  mutate(topic = as.factor(topic),
         term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta, fill=topic))+
  geom_col(show.legend = FALSE)+
  scale_x_reordered() +
  coord_flip()+
  xlab(NULL)+
  facet_wrap(~topic, scales = "free")+
  theme_bw()+
  theme(text = element_text(size = 14))

topic_model_plot <- ggpubr::ggarrange(plot_topics, hf_table, ncol = 1, nrow = 2, 
                                      heights = c(7.5,10))

ggsave("Final_Plots/Fig8_alt.jpg", 
       topic_model_plot, 
       height = 12.1, 
       width = 9.8, dpi = 300)

####EXTRAS####
#emoji density
emoji_detected <- emojis %>% as_tibble() %>% na.omit() %>% 
  count(value, sort = TRUE) %>% 
  mutate(lists="hflourishing")

emojis_j <- all_tokens %>% 
  filter(type=="journalism") %>% 
  pull(word) %>% 
  emoji_extract() 

emoji_j_detected <- emojis_j %>% as_tibble() %>% na.omit() %>% 
  count(value, sort = TRUE) %>% 
  mutate(lists="journalism")

emojis_s <- all_tokens %>% 
  filter(type=="startup") %>% 
  pull(word) %>% 
  emoji_extract() 

emoji_s_detected <- emojis_s %>% as_tibble() %>% na.omit() %>% 
  count(value, sort = TRUE) %>% 
  mutate(lists="startup")

emojis_i <- all_tokens %>% 
  filter(type=="influencers") %>% 
  pull(word) %>% 
  emoji_extract()

emoji_i_detected <- emojis_i %>% as_tibble() %>% na.omit() %>% 
  count(value, sort = TRUE) %>% 
  mutate(lists="influencers")

emojis_b <- all_tokens %>% 
  filter(type=="business") %>% 
  pull(word) %>% 
  emoji_extract() 

emoji_b_detected <- emojis_b %>% as_tibble() %>% na.omit() %>% 
  count(value, sort = TRUE) %>% 
  mutate(lists="business")

emojis_v <- all_tokens %>% 
  filter(type=="venture") %>% 
  pull(word) %>% 
  emoji_extract() 

emoji_v_detected <- emojis_v %>% as_tibble() %>% na.omit() %>% 
  count(value, sort = TRUE) %>% 
  mutate(lists="venture")

#juntar los datasets
all_emojis_detected <- bind_rows(emoji_detected,
                                 emoji_b_detected,
                                 emoji_i_detected,
                                 emoji_j_detected,
                                 emoji_v_detected,
                                 emoji_s_detected)
