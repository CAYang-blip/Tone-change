#------------- TONE DATA PROCESSING PROGRAM -----------------

# Tone Processing -- Takes you from the output file of the Praat F0 
# measurement script to a tone plot. The authors of this program are
# Jerry Fine and Cathryn Yang, based on Stanford (2008, 2016).  
# Program written May - August, 2020 and revised December 2022.

# Stay safe and healthy.

#  The following packages need to be installed

library(tidyverse)
library(ggpubr)
#  
# change working directory to local location of folder on your machine.
#  the working directory is assumed to contain the praat data.
#  Here we assume it is in a csv file.

data <- read_csv("praat_data_nopitchjumps.csv")

data$Pitch <- as.numeric(data$Pitch)

#  to tidy up -- change "sil" tokens to "sil8"

data$Token[data$Token == "sil"] <- "sil8"

# also get rid of random undefineds - not the ones 
# that come with the silences

data <- data %>%
  filter( (Pitch != "undefined" | Token == "sil8") )

#  add a tones column by processing the tokens

data <- data %>% mutate(Tone = str_sub(Token,-1))


# flatten out the data for further processing.  That is, take out
# the time and pitch info temporarily while we add in Preceding and Following tones, 
# and initial consonants.

data_f <- data %>% 
        distinct(Token_Number,Speaker,.keep_all = T) %>%
        select(-Time,-Pitch)

# add the preceding token column and following token
# NOTE:  Due to the annotation of our praat data,
# these preceding and following values will only be
# accurate with tones 2,4 and 7

data_f <- data_f %>% 
          group_by(Speaker) %>%
          mutate(Prev_Token = lag(Token)) %>%
          mutate(Next_Token = lead(Token))

#  We now begin to process the tokens locating and typing
#  initial consonants

tokens <- unique(data_f$Token)


# Locate the vowels in each token

vowels <- "a|e|u|o|i|A|E|I|O|U|y"

v_loc <- regexpr(vowels,tokens)

# Use a substring to get onsets

ons <- substr(tokens,1,v_loc-1)

# Initial consonant type information

va <- "p|t|k|q|c|ch"
vf <- "f|s|sh|h|x|r"
vu <- "b|d|g|j|z|zh"
vs <- "w|m|n|l|r|nj"

# token information gathered

token_info <- tibble( Token = tokens, `Initial Consonant` = ons )

# the onset type info is gathered

token_info <- token_info %>%
              mutate(Initial = case_when(
                     `Initial Consonant` == ""     ~ "vowel",
                     grepl(va,`Initial Consonant`) ~ "aspirated",
                     grepl(vu,`Initial Consonant`) ~ "unaspirated",
                     grepl(vf,`Initial Consonant`) ~ "fricative",
                     grepl(vs,`Initial Consonant`) ~ "sonorant"))

### mark silence as a special case:

silence_index <- which(token_info$Token == "sil8")
token_info$`Initial Consonant`[silence_index] <- "Silence"
token_info$Initial[silence_index] <- ""

# Assuming that just changing the sil8 entry in token_info
# will get us a fix.  I am not sure how to see if this
# works.

#  This information is now re-integrated with data_f by a left join

data_f <- left_join(data_f, token_info, by = "Token")

#  Preceding syllable tone

data_f <- data_f %>% mutate(Prec_Tone = str_sub(Prev_Token,-1))

# Following syllable tone

data_f <- data_f %>% mutate(Next_Tone = str_sub(Next_Token,-1))

# Following Syllable Initial

data_f <- data_f %>% 
  group_by(Speaker) %>%
  mutate(`Following Initial` = lead(`Initial Consonant`))

# Following Syllable Initial Type

data_f <- data_f %>%
  group_by(Speaker) %>%
  mutate(`Following Initial Type` = case_when(
    `Following Initial` == ""     ~ "none",
    `Following Initial` == "Silence" ~ "Silence",
    grepl(va,`Following Initial`) ~ "obstruent",
    grepl(vu,`Following Initial`) ~ "obstruent",
    grepl(vf,`Following Initial`) ~ "obstruent",
    grepl(vs,`Following Initial`) ~ "sonorant"))

#  We are now ready to re-join this data set to the main one

data <- data %>% left_join( data_f,
           by = c("Token_Number","Speaker","Token","Tone", "Style"))

#  We need to filter out tokens whose n-count is less than 4

data <- data %>%
        group_by(Speaker,Token_Number) %>%
        mutate(Toks = n()) %>%
        filter(Toks > 3) %>%
        select(-Toks)

#  We can now filter out the silences

data <- data %>% filter( Token != "sil8")

#for citation plot, filter out Tone 5 (ru sheng) and 6 (neutral tone) 
#and 7 (half T3 but in isolation)
data <- data %>% filter( Tone != 5, Tone != 6, Tone != 7)

#count tokens by tone
count_token <- data_f %>%
  distinct() %>%
  group_by(Tone) %>%
  summarise(number_of_occurences = n())

connected <- data_f %>% filter(Style =="connected")

#count tokens by preceding tone
data_b <- data_bal %>% 
  distinct(Token_Number,Speaker,.keep_all = T) %>%
  select(-Time,-Pitch)

count_token_env <- data_f %>%
  distinct() %>%
  group_by(Tone, Style, Prec_Tone, Next_Tone) %>%
  summarise(number_of_occurences = n())

count_token_env_3 <- data_f %>%
  distinct() %>%
  group_by(Speaker, Tone, Style) %>%
  summarise(number_of_occurences = n())

# Calculate the reference max 

#this gets the mean of each tone listed separately
#mean <- data %>%
#  group_by(Tone)%>%
#  summarise(r_mean =mean(Pitch))

#summary <- summary(data$Pitch)

#  Calculate the reference interval length for 
#  each tone
#ref_tone <- 2

#ref_int <- data %>%
#  group_by(Speaker,Token_Number,Tone) %>%
#  summarise(int_leng = max(Time)-min(Time)) %>%
#  group_by(Tone) %>%
#  summarize(t_mean = mean(int_leng)) %>%
#  mutate(t_norm = t_mean/t_mean[ref_tone])

#  Normalization of the times -- using only times from praat

data <- data %>% 
    group_by(Speaker,Token_Number) %>%
    mutate(T_int = max(Time)-min(Time)) %>%
    mutate(Time = (Time - min(Time))/T_int)

#  get the reference mean per speaker

ref_tone <- 2
ref_mean <- data %>%
  filter(Tone == ref_tone)%>%
  group_by(Speaker)%>%
  summarise(r_mean = mean(Pitch))

#semitones and Chao Number

lgmax <- log(max(data$Pitch,na.rm = TRUE))
lgmin <- log(min(data$Pitch,na.rm = TRUE))
lgdif <- lgmax - lgmin

data <- data %>% 
  left_join( ref_mean, by = "Speaker") %>%
  group_by(Speaker,Token_Number) %>%
  mutate(Semi_Tone = 12 * log(Pitch/r_mean) / log(2)) %>%
  mutate(Chao_Number = 5* (log(Pitch)- lgmin)/lgdif) %>%
  select(-r_mean)

# Preceding and following tone environment

data <- data %>% 
  mutate(`Preceding` = case_when(
    Prec_Tone == 8 ~ "Silence__", 
    is.na(Prec_Tone) ~ "Silence__",
    Prec_Tone== 4  ~ "low",
    Prec_Tone == 5 ~ "T5__", 
    Prec_Tone == 1 ~ "high",
    Prec_Tone == 2 ~ "high",
    Prec_Tone == 3 ~ "high",
    Prec_Tone ==7 ~"high"))

data <- data %>%
  mutate(`Following` = case_when(
    Next_Tone == 8 ~ "Silence", 
    Next_Tone == 1~ "other",
    Next_Tone == 4~ "T4",
    Next_Tone == 5~ "other",
    Next_Tone ==3 ~ "__T3",
    Next_Tone ==6 ~ "other",
    Next_Tone ==2 ~ "other") )

#balance the preceding tone environment for T2 and T3, so that there
#are 10-15 tokens for each preceding environment and following environment
#balanced for T2 final position, by preceding environment:
data_bal <- data %>%
  filter(Token_Number!= 36,
         Token_Number!= 39,
         Token_Number!= 48,
         Token_Number!= 51,
         Token_Number!= 151,
         Token_Number!= 154,
         Token_Number!= 175,
        Token_Number!= 178,
         Token_Number!= 199,
         Token_Number!= 202,
        Token_Number!= 205,
        Token_Number!= 287,
        Token_Number!= 407,
        Token_Number!= 415,
        Token_Number!= 424,
        Token_Number!= 332,
        Token_Number!= 335,
         )
##balanced for T2 nonfinal, by following environment:
data_bal_nonfin <- data %>%
  filter(Token_Number!= 35,
         Token_Number!= 41,
         Token_Number!= 47,
         Token_Number!= 53,
         Token_Number!= 331,
         Token_Number!= 337,
         Token_Number!= 379,
         Token_Number!= 385,
         Token_Number!= 406,
         Token_Number!= 417,
         Token_Number!= 423 )

# Plotting the data

#citation and set style of T2
#count tokens
data_cit2 <- data %>% 
  filter(Tone ==2, Style=="set"|Style=="citation")

data_cit2f <- data_cit2 %>% 
  distinct(Token_Number,Speaker,.keep_all = T) %>%
  select(-Time,-Pitch)

count_token_env_c <- data_cit2f %>%
  distinct() %>%
  group_by(Tone) %>%
  summarise(number_of_occurences = n()) 

#plot T2 citation form
T2cit <- ggplot(data = filter(data, Tone == 2, Style=="citation"|Style=="set")) +
  geom_smooth(mapping = aes(Time,Chao_Number)) +
  theme_bw()+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.title.y=element_blank(), 
        axis.ticks.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        # explicitly set the horizontal lines (or they will disappear too)
        panel.grid.major.y = element_line( size=.1, color="grey" ), 
        plot.title=element_text(size=10))+
  ylim(0,5)+
  ggtitle("1922 citation")

T2cit
ggsave("T2 cit.png", plot = T2cit,
       dpi = 300)

#citation style of T3
#count tokens
data_cit3 <- data %>% 
  filter(Tone ==3, Style=="set"|Style=="citation")

data_cit3f <- data_cit3 %>% 
  distinct(Token_Number,Speaker,.keep_all = T) %>%
  select(-Time,-Pitch)

count_token_env_c <- data_cit3f %>%
  distinct() %>%
  group_by(Tone, Speaker) %>%
  summarise(number_of_occurences = n()) 
#plot T3 citation form
T3cit <- ggplot(data = filter(data, Tone == 3, Style =="citation"|Style=="set")) +
  geom_smooth(mapping = aes(Time,Chao_Number), method = "gam", formula = y ~ s(x, bs = "cs")) +
  theme_bw()+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.title.y=element_blank(), 
        axis.ticks.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        # explicitly set the horizontal lines (or they will disappear too)
        panel.grid.major.y = element_line( size=.1, color="grey" ), 
        plot.title=element_text(size=10))+
  ylim(0,5)+
  ggtitle("1922 citation")

T3cit

ggsave("T3 cit.png", plot = T3cit,
       dpi = 300)

#plot them together
#cowplot can take out the y axis for the final plot
plotcit <- cowplot::plot_grid(T2cit, 
                             T3cit + 
                               theme(axis.text.y = element_blank(),
                                     axis.ticks.y = element_blank(),
                                     axis.title.y = element_blank() ), 
                             nrow = 1, align = "v")
plotcit
ggsave("T2 T3 cit.png", plot = plotcit,
       dpi = 300)


#just T3 non final, not before T3 or T5 or T6, minus outliers
##count tokens
data_3n <- data %>% 
  filter(Tone == 3, Next_Tone != 3, Next_Tone!=5,
         Next_Tone != 8,Style=="connected", 
         Token_Number!=55, Token != "yi3", Token != "yu3", 
         `Preceding Environment` == "Silence__")

data_3nf <- data_3n %>% 
  distinct(Token_Number,Speaker,.keep_all = T) %>%
  select(-Time,-Pitch)

mean(data_3nf$T_int)

count_token_env_3 <- data_3nf %>%
  distinct() %>%
  group_by(Tone, Style, Prec_Tone, Next_Tone) %>%
  summarise(number_of_occurences = n()) 

#plot
#filter out tokens with measurement errors                                 
T3nonfin <- ggplot(data = filter(data, Tone == 3, Style=="connected", 
                                 Next_Tone != 3, Next_Tone!=5, Next_Tone != 8,
                                 `Preceding Environment` == "Silence__",
                                 Token != "yi3", Token != "yu3", Token_Number != 55)) +
  geom_smooth(mapping = aes(Time,Chao_Number), 
              method = "gam", formula = y ~ s(x, bs = "cs")) +
  theme_bw()+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.title.y=element_blank(), 
        axis.ticks.y =element_blank(),
        axis.text.y =element_text(size = 8),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        # explicitly set the horizontal lines (or they will disappear too)
        panel.grid.major.y = element_line( size=.1, color="grey" ), 
        plot.title=element_text(size=9),
        legend.position = "right")+
  ylim(0,5)+
  ggtitle("1922 non-final")

T3nonfin
ggsave("T3 nonfin.png", plot = T3nonfin,
       dpi = 300)


#just T3 final
##count tokens
data_3fin <- data %>% 
  filter(Tone == 3, Style=="connected", 
         Next_Tone == 8, Prec_Tone !=5, `Preceding Environment` != "Silence__")

data_3f <- data_3fin %>% 
  distinct(Token_Number,Speaker,.keep_all = T) %>%
  select(-Time,-Pitch)
mean(data_3f$T_int)

count_token_env_3 <- data_3f %>%
  distinct() %>%
  group_by(Tone, Style, Prec_Tone, Next_Tone) %>%
  summarise(number_of_occurences = n()) 
#plot T3 fin
T3fin <- ggplot(data = filter(data, Tone == 3, Style=="connected", Token_Number!="38", Next_Tone == 8, Prec_Tone !=5, `Preceding` != "Silence__")) +
  geom_smooth(mapping = aes(Time,Chao_Number)) +
  theme_bw()+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.title.y=element_blank(), 
        axis.ticks.y =element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        # explicitly set the horizontal lines (or they will disappear too)
        panel.grid.major.y = element_line( size=.1, color="grey" ),
        plot.title=element_text(size=9))+
  ylim(0,5)+
  ggtitle("2018 final")
T3fin
ggsave("T3final2018.png", T3fin,
       dpi = 300)

#plot together
plotT3 <-grid.arrange(T3nonfin, T3fin+rremove("ylab"), ncol = 2)
plotT3 <- ggarrange(T3nonfin, T3fin+rremove("ylab"), ncol = 2)
#cowplot can take out the y axis for the final plot
plotT3 <- cowplot::plot_grid(T3nonfin, 
                   T3fin + 
                     theme(axis.text.y = element_blank(),
                           axis.ticks.y = element_blank(),
                           axis.title.y = element_blank() ), 
                   nrow = 1, align = "v")
plotT3
ggsave("T3 nonfinal final.png", plot = plotT3,
       dpi = 300)

#plot T3 fin with preceding environment
T3fin <- ggplot(data = filter(data, Tone == 3, Style=="connected", Token_Number!="38", Next_Tone == 8, Prec_Tone !=5, `Preceding` != "Silence__")) +
  geom_smooth(mapping = aes(Time,Chao_Number, linetype=`Preceding`)) +
  theme_bw()+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.title.y=element_blank(), 
        axis.ticks.y =element_blank(),
        axis.ticks.x =element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        # explicitly set the horizontal lines (or they will disappear too)
        panel.grid.major.y = element_line( size=.1, color="grey" ),
        plot.title=element_text(size=9))+
  theme(legend.title=element_text(size=8), legend.text = element_text(size=8),legend.position = "right")+
  ylim(0,5)
T3fin
ggsave("T3final_pre.png", T3fin,
       dpi = 300)


#T2 nonfinal
#count tokens
data_bal_nonfin2 <- data_bal_nonfin %>% 
filter(Tone == 2, Style=="connected", Next_Tone != 8, Next_Tone!=5, `Preceding Environment` == "Silence__")
  
  data_bnf <- data_bal_nonfin2 %>% 
    distinct(Token_Number,Speaker,.keep_all = T) %>%
    select(-Time,-Pitch)
  
  count_token_env_2 <- data_bnf %>%
    distinct() %>%
    group_by(Tone, Style, Prec_Tone, Next_Tone) %>%
    summarise(number_of_occurences = n())  

#plot T2 nonfinal position
T2nonfin <- ggplot(data = filter(data_bal_nonfin, Tone == 2, Style=="connected", Next_Tone != 8, Next_Tone!=5, Prec_Tone ==8)) +
geom_smooth(mapping = aes(Time,Chao_Number)) +
  theme_bw()+
  facet_wrap(Token~Token_Number)+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.title.y=element_blank(), 
        axis.ticks.y =element_blank(),
        axis.text.y =element_text(size = 8),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        # explicitly set the horizontal lines (or they will disappear too)
        panel.grid.major.y = element_line( size=.1, color="grey" ), 
        plot.title=element_text(size=10))+
  ylim(0,5)+
  ggtitle("a) 1922 non-final")

T2nonfin
ggsave("T2nonfinal.png", T2nonfin,
       dpi = 300)
#T2 final
#count tokens

data_bal_fin2 <- data_bal %>% 
  filter(Tone == 2, Style=="connected", Prec_Tone!=5, Prec_Tone != 8, Next_Tone == 8)

data_bf <- data_bal_fin2 %>% 
  distinct(Token_Number,Speaker,.keep_all = T) %>%
  select(-Time,-Pitch)

count_token_env_2 <- data_bf %>%
  distinct() %>%
  group_by(Tone, Style, Prec_Tone, Next_Tone) %>%
  summarise(number_of_occurences = n())  
#plot T2 final position
T2fin <- ggplot(data = filter(data_bal, Tone == 2, Style=="connected", Prec_Tone!=5, Prec_Tone != 8, Next_Tone == 8)) +
  geom_smooth(mapping = aes(Time,Chao_Number, linetype=`Preceding`)) +
  theme_bw()+
  facet_wrap(Token~Token_Number)+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.y =element_blank(),
        axis.title.y=element_blank(), 
        axis.text.y =element_text(size = 8),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        # explicitly set the horizontal lines (or they will disappear too)
        panel.grid.major.y = element_line( size=.1, color="grey" ),
        plot.title=element_text(size=10))+
  theme(legend.title=element_text(size=8), legend.text = element_text(size=8),legend.position = "right")+
  ylim(0,5)

T2fin
ggsave("T2final2_pre.png", T2fin,
       dpi = 300)
#Saving 1.76 x 2.17 in image

#cowplot can take out the y axis for the final plot
plotT2 <- cowplot::plot_grid(T2nonfin, 
                             T2fin + 
                               theme(axis.text.y = element_blank(),
                                     axis.ticks.y = element_blank(),
                                     axis.title.y = element_blank() ), 
                             nrow = 1, align = "v")
plotT2
ggsave("T2 nonfinal final2.png", plot = plotT2,
       dpi = 300)

#just T3 final, by Preceding Tone
my_plot <- ggplot(data = filter(data, Tone == 3, Next_Tone == 8, `Preceding Environment` != "Silence__")) +
  geom_smooth(mapping = aes(Time,Semi_Tone,
                            color = Prec_Tone)) +
  theme_bw()+
  ylab("F0 (Semitones)")+
  xlab("Average duration relative to T2 (T2 = 1.0)") 

#just T3 final after T3
my_plot <- ggplot(data = filter(data, Tone == 3, Next_Tone == 8, Prec_Tone == 3)) +
  geom_smooth(mapping = aes(Time,Semi_Tone)) +
  facet_wrap(~Token)+
  theme_bw()


#just T2 final
my_plot <- ggplot(data = filter(data, Tone == 2, Next_Tone == 8,`Preceding Environment` != "Silence", Prec_Tone != 5 )) +
  geom_smooth(mapping = aes(Time,Semi_Tone,
                            color = Prec_Tone)) +
  theme_bw()+
  ylab("F0 (Semitones)")+
  xlab("Time (normalized)") 

my_plot <- ggplot(data = filter(data, Tone == 2, Next_Tone == 8, `Preceding Environment` != "Silence", Prec_Tone != 5 )) +
  geom_smooth(mapping = aes(Time,Semi_Tone)) +
  theme_bw()+
  ylab("F0 (Semitones)")+
  xlab("Time (normalized)") 

#plot citation tone in 1922
plot_cit <- ggplot(data = filter(data, Style=="set"|Style=="citation")) +
  geom_smooth(mapping = aes(Time,Chao_Number,
                            group = Tone,
                            color = Tone))+
  theme_bw()+
  scale_color_manual(labels = c("T1", "T2", "T3","T4"), 
                     values = c("blue", "black", "red", "green"))+
  theme(axis.text.x=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_text(size=8),
        axis.text.y =element_text(size=8),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_line( size=.1, color="grey" ),
        legend.position="none")+
  ylab("f0 (T-value log transform)")+
  coord_cartesian(ylim = c(1, 5)) 
plot_cit

ggsave("Citation and set forms_notitle.png", plot = plot_cit,
       dpi = 300)

#Plot Tone 2 and 3 from Shi and Wang 2006, 7 speakers from 30-40 year old age group
#"Lao Beijing" means that both parents are Beijingers and the person grew up in Beijing

data_SW <- read_csv("Shi_and_Wang_2006_f0_data.csv")

data_SW$pitch <- as.numeric(data_SW$pitch)
data_SW$time <- as.numeric(data_SW$time)

plot_SW2 <- ggplot(data = filter(data_SW, tone == 2)) +
  geom_smooth(mapping = aes(time,pitch))+
  theme_bw()+
  theme(axis.text.x=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        # explicitly set the horizontal lines (or they will disappear too)
        panel.grid.major.y = element_line( size=.1, color="grey" ), 
        plot.title=element_text(size=10))+
  scale_x_continuous(breaks=seq(1,9,1))+
  ylab("f0 (T-value log transform)")+
  ylim(0,5)+
  ggtitle("2006 citation")

plot_SW2
T2cit
ggsave("T2_2006.png", plot = plot_SW2,
       dpi = 300)

plotT2cit <- cowplot::plot_grid(T2cit, 
                             plot_SW2 + 
                               theme(axis.text.y = element_blank(),
                                     axis.ticks.y = element_blank(),
                                     axis.title.y = element_blank() ), 
                             nrow = 1, align = "v")

plotT2cit

ggsave("T2_SW.png", plot = plotT2cit,
       dpi = 300)
#Tone 3
plot_SW <- ggplot(data = filter(data_SW, tone == 3)) +
  geom_smooth(mapping = aes(time,pitch))+
  theme_bw()+
  theme(axis.text.x=element_blank(),
        axis.title.x=element_blank(),
         axis.title.y=element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        # explicitly set the horizontal lines (or they will disappear too)
        panel.grid.major.y = element_line( size=.1, color="grey" ), 
        plot.title=element_text(size=10))+
  ylab("f0 (T-value log transform)")+
  ylim(0,5)+
  ggtitle("2006 citation")

plot_SW
ggsave("T3_SW2006.png", plot = plot_SW,
       dpi = 300)
T3cit

plotT3cit <- cowplot::plot_grid(T3cit, 
                                plot_SW + 
                                  theme(axis.ticks.y = element_blank(),
                                        axis.title.y = element_blank() ), 
                                nrow = 1, align = "v")

plotT3cit

ggsave("T3_SW.png", plot = plotT3cit,
       dpi = 300)

