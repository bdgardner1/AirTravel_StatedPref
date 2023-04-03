library(dplyr)
library(tidyverse)
library(ggplot2)
library(data.table)
library(ggridges)
library(tidyverse)
library(broom)
library(car)

options(scipen = 999)
set.seed(3141)

my_theme <- function(x){
  theme(plot.subtitle = element_text(size = 9, colour = 'darkgray'),
        axis.text.x = element_text(size = 8),
        axis.title.x = element_text(size = 9),
        axis.text.y = element_text(size = 9),
        axis.title.y = element_text(size = 9),
        panel.background = element_rect(fill = "white"),
        legend.title = element_text(size = 7),
        legend.text = element_text(size = 7),
        legend.position = "right")
}

setwd("//Poise.Homeoffice.Local/Home/TMS7/Users/GardneB/My Documents/MainProject3/Survey/HO")
#Load main weight file and set numeric data
results = read.csv("HO_Results.csv")
dems = readxl::read_xlsx("demographics.xlsx")
main = merge(results, dems, by.x = "ProlificID", by.y = "ParticipantID")
main$Age = as.numeric(main$Age)
main$Salary_Amount = as.numeric(as.character(main$Salary_Amount))


#Create Age bands
main = main %>%
  mutate(age_band = case_when(
    Age <25 ~ "18-25",
    Age >=25 & Age <35 ~ "25-34",
    Age >=35 & Age <50 ~ "35-50",
    Age >=50 & Age <66 ~ "50-65",
    Age >65 ~ "65+"
  ))

main = main %>% 
  mutate(age_band2 = case_when(
    Age <46 ~ "18 - 45",
    Age >45 ~ "45+"
  ))

#Create number of days since last flight & group
main$Last_Flight = as.Date(main$Last_Flight,
                           format = "%d/%m/%Y")
main$LastFlight_Days = as.Date("2023/03/22") - main$Last_Flight
main = main %>%
  mutate(LF_Group = case_when(
    Last_Flight >= as.Date("2022/03/22") ~ "<1 Year",
    Last_Flight >= as.Date("2021/03/22") 
    & Last_Flight < as.Date("2022/03/22") ~ "1<>2 Years",
    Last_Flight >= as.Date("2018/03/22") 
    & Last_Flight < as.Date("2021/03/22") ~ "2<>5 Years",
    Last_Flight >= as.Date("2013/03/22") 
    & Last_Flight < as.Date("2018/03/22") ~ "5<>10 Years",
    Last_Flight <= as.Date("2013/03/22") ~ ">10 Years"))

#Create Leisure and Business Groups
main = main %>%  mutate(Freq_L = case_when(
  Leisure_Freq == "Less than once a year" ~ 0.5,
  Leisure_Freq == "1 or 2 times a year" ~ 1,
  Leisure_Freq == "3 to 5 times a year" ~ 2,
  Leisure_Freq == "More than 5 times a year" ~3 
)) %>% 
  mutate(Freq_B = case_when(
    Bus_Freq == "Never" ~ 0,
    Bus_Freq == "Less than once a year" ~ 0.5,
    Bus_Freq == "1 or 2 times a year" ~ 1,
    Bus_Freq == "3 to 5 times a year" ~ 2,
    Bus_Freq == "More than 5 times a year" ~3)) 
main$Freq_travel = main$Freq_B + main$Freq_L

main$Leisure_Freq = factor(main$Leisure_Freq,
                           
                           levels = c("Less than once a year",
                                      "1 or 2 times a year",
                                      "3 to 5 times a year",
                                      "More than 5 times a year"))
main$Bus_Freq = factor(main$Bus_Freq,
                       
                       levels = c("Never","Less than once a year",
                                  "1 or 2 times a year",
                                  "3 to 5 times a year",
                                  "More than 5 times a year"))
#Leisure Freq table
main_young = main %>% 
  filter(age_band2 == "18 - 45")

prop.table(table(main_young$age_band2, year(main_young$Last_Flight)))
prop.table(table(main_young$Leisure_Freq))
prop.table(table(main_young$Bus_Freq))

main_old = main %>% 
  filter(age_band2 != "18 - 45")

prop.table(table(main_old$age_band2, year(main_old$Last_Flight)))
prop.table(table(main_old$Leisure_Freq))
prop.table(table(main_old$Bus_Freq))
#Hold bag tables

main %>%  
  filter(Hold_Bag == 'Yes') %>%
  group_by(Hold_Bag, age_band, Sex) %>% 
  summarise(avg = n())

main %>% 
  filter(Hold_Bag != "I can't remember") %>% 
  group_by(age_band, Sex) %>% 
  summarise(avg = n())


#Create overall attitude statistics
main$overall_op = (main$OP_Checkin + main$OP_ArrivalHall +
                          main$OP_Bag + main$OP_Gate + main$OP_Pass +
                          main$OP_Security)/6
main$pass_diff = main$OP_Pass - main$overall_op

#binomial distribution of last flights
main %>% 
  ggplot()+
  aes(x = LastFlight_Days/365)+
  geom_density()+
  xlim(0,10)+
  my_theme()+
  labs(x = "Years Since Last Flight",
       y = "Density")+
  scale_y_continuous(labels = scales::percent)
ggsave("last_flight.png")

#Age Barchart
total = nrow(main)
main %>% filter(age_band!="NA") %>%   
  group_by(age_band) %>% 
  summarise(band_per = round((n()/total)*100,0)) %>% 
  ggplot()+
  aes(x = age_band, y = band_per, fill = "red")+
  geom_col(alpha = 0.4)+
  labs(x = "Age Band",
       y = "Percent of Total")+
  theme(legend.position = "none")
ggsave("Age.png")

#Sex

table(main$Sex)

#Leisure Freq Barchart
main %>%  
  group_by(age_band, Leisure_Freq) %>% 
  summarise(avg = n()/total) %>% 
  na.omit() %>% 
  ggplot()+
  aes(x = age_band, y = avg, fill = Leisure_Freq) %>% 
  geom_col(position = "fill")+
  labs(x = "Age Group",
       y = "Share of Age Group",
       fill = "Leisure Freq")+
  scale_y_continuous(labels = scales::percent)
ggsave("LF.png")

#Business Freq Barchart
main %>%  
  group_by(age_band, Bus_Freq) %>% 
  summarise(avg = n()/total) %>% 
  na.omit() %>% 
  ggplot()+
  aes(x = age_band, y = avg, fill = Bus_Freq) %>% 
  geom_col(position = "fill")+
  labs(x = "Age Group",
       y = "Share of Age Group",
       fill = "Leisure Freq")+
  scale_y_continuous(labels = scales::percent)
ggsave("LF.png")

#Overall Leisure Freq
main %>%  
  group_by(Leisure_Freq) %>% 
  summarise(avg = n()/total,
            text = "All") %>% 
  na.omit() %>% 
  ggplot()+
  aes(x = text, y = avg, fill = fct_rev(Leisure_Freq)) %>% 
  geom_col()+
  labs(x = "",
       y = "Share",
       fill = str_wrap("Leisure Travel Frequency",15))+
  scale_y_continuous(labels = scales::percent)+
  theme(axis.text.x = element_text(size = 10))
ggsave("LF_Overall.png")
#Overall Business Freq
main %>%  
  group_by(Bus_Freq) %>% 
  summarise(avg = n()/total,
            text = "All") %>% 
  na.omit() %>% 
  ggplot()+
  aes(x = text, y = avg, fill = fct_rev(Bus_Freq)) %>% 
  geom_col()+
  labs(x = "",
       y = "Share",
       fill = str_wrap("Business Travel Frequency",15))+
  scale_y_continuous(labels = scales::percent)+
  theme(axis.text.x = element_text(size = 10))
ggsave("BF_Overall.png")

#Hold Bags
main %>%  
  filter(Hold_Bag != "I can't remember") %>% 
  group_by(Hold_Bag, age_band) %>% 
  summarise(avg = n(),
            text = "All") %>% 
  na.omit() %>% 
  ggplot()+
  aes(x = age_band, y = avg, fill = Hold_Bag) %>% 
  geom_col(position = "fill")+
  labs(x = "",
       y = "Share",
       fill = str_wrap("Hold Bag On Last Flight",15))+
  scale_y_continuous(labels = scales::percent)+
  scale_fill_discrete(labels = c("No", "Yes"))+
  my_theme()
ggsave("hold_overall.png")  

main %>% filter(Hold_Bag != "I can't remember") %>% 
  group_by(Hold_Bag) %>% 
  summarise(total = n()) %>% 
  mutate(prop = total/sum(total))

main %>%  
  group_by(Hold_Bag, age_band2) %>% 
  summarise(avg = n(),
            text = "All") %>% 
  na.omit() %>% 
  ggplot()+
  aes(x = age_band2, y = avg, fill = Hold_Bag) %>% 
  geom_col(position = "fill")+
  labs(x = "",
       y = "Share",
       fill = str_wrap("Hold Bag On Last Flight",15))+
  scale_y_continuous(labels = scales::percent)+
  scale_fill_discrete(labels = c("Can't Recall", "No", "Yes"))+
  theme(axis.text.x = element_text(size = 10))
ggsave("hold_overall.png")


#Attitudinal Stats analysis and visualisation
Ratings = main %>% 
  group_by(Sex) %>% 
  summarise(Check_In = mean(OP_Checkin),
            Security = mean(OP_Security),
            Gate = mean(OP_Gate),
            Passport = mean(OP_Pass),
            Baggage= mean(OP_Bag),
            Arrival_Hall = mean(OP_ArrivalHall)) %>% 
  data.frame()

rat_ls <-melt(setDT(select(main, c(1,5, 9, 12:17, 82:83, 86,87,91))), 
              id.vars = c("ProlificID", "Sex","Age", "Leisure_Freq",
                          "Salary_Amount", "age_band2","LastFlight_Days","Freq_travel"), 
              variable.name = "Type")

rat_ls %>% 
  ggplot()+
  aes(x = value, fill = Type)+
  geom_histogram()+
  facet_wrap(~ Type)+
  theme(legend.position = "none")
ggsave("AT_Opininos.png")

rat_ls %>% 
  group_by(Type) %>% 
  summarise(avg_score = mean(value))

rat_ls %>% 
  group_by(Type) %>% 
  summarise(avg_score = mean(value)) %>% 
  ggplot()+
  aes(x = Type, y = avg_score, colour = Type)+
  geom_point(shape = "diamond", size = 2)+
  ylim(2.8,4)+
  geom_errorbar(aes(ymin = 0.97*avg_score, ymax = 1.03 * avg_score), width=.2,
                position = position_dodge(),
                colour = "grey")+
  labs(x = "Waiting Area",
       y = "Average Dissatisfaction Score (1-7)")+
  scale_x_discrete(labels = c("Check-in", "Security", "Gate", 
                              "Passport", "Baggage", "Arrival Hall"))+
  my_theme()+
  guides(color=FALSE)
ggsave("Opinions_errror.png")

r_over = rat_ls %>% 
  filter(Sex != "Prefer not to say") %>% 
  group_by(ProlificID, Sex, Age, Leisure_Freq, LastFlight_Days,
           Salary_Amount, Freq_travel) %>% 
  summarise(avg_sat = mean(value))


model = lm(avg_sat ~ Sex + Age 
           + Salary_Amount + (LastFlight_Days) + Freq_travel, data = r_over)

write.csv(tidy(model),"att_model.csv") 

model <- aov(rat_ls$value~factor(rat_ls$Type))
summary(model)

model = lm(avg_sat ~ Salary_Amount + Freq_travel, data = r_over)
summary(model)

rat_ls %>% 
  filter(Sex != "Prefer not to say") %>% 
  group_by(Sex, Type) %>% 
  summarise(avg_value = mean(value)) %>% 
  ggplot()+
  aes(x = Type, y = avg_value, colour = Sex)+
  geom_point()+
  ylim(2,5)+
  labs(x = "Waiting Area",
       y = "Average Disatisfaction (1-7)"
       )+
  theme(axis.text.x = element_text(angle = 90))+
  scale_x_discrete(labels = c("Check-in", "Security", "Gate",
                              "Passport", "Baggage", "Arrival Hall"))
ggsave("Opinions_Sex.png")


###wait preference

wp = select(main,c("age_band2", "Sex", "Flight_Pass","Pass_Bag","Pass_Train"))

wpp = wp %>% 
  mutate(Flight_Passport = case_when(
    Flight_Pass == "Your flight took 45 minutes longer than scheduled, but there was no queue when arriving at UK passport control." ~ "Flight",
    Flight_Pass == "Your flight arrived on time, but you had to queue for 45 minute at UK passport control." ~ "Passport",
    Flight_Pass == "I have no preference" ~ "No Preference"))
   

wpp = wpp%>% 
  mutate(Passport_Train = case_when(
    Pass_Train == "There is no queue at passport control. However, your train journey home takes 1.5 hours, 30 minutes longer than scheduled." ~ "Train",
    Pass_Train == "There was a 30 minute queue at passport control. Your train journey home takes 1 hour as scheduled." ~ "Passport",
    Pass_Train == "I have no preference" ~ "No Preference"
  ))

wpp = wpp %>% 
  mutate(Passport_Baggage = case_when(
    Pass_Bag == "The queue at passport control was 25 minutes long, however your hold luggage is waiting for you in baggage reclaim" ~ "Passport",
    Pass_Bag == "There is no queue at passport control, however you need to wait 25 minutes for your hold luggage in baggage reclaim" ~ "Baggage Reclaim",
    Pass_Bag == "No preference" ~ "No Preference"
  ))

wppp = select(wpp, c("age_band2", "Sex", "Passport_Baggage",
                     "Flight_Passport", "Passport_Train"))

lwp = melt(setDT(wppp), 
                   id.vars = c("age_band2","Sex"), 
                   variable.name = "choice")


lwp$value = factor(lwp$value,
                   levels = c("Passport", 
                              "No Preference",
                              "Flight",
                              "Baggage Reclaim",
                              "Train"))


lwp = lwp %>% 
    mutate( value2 = case_when(
      value == "Train" ~ "Alternative",
      value == "Baggage Reclaim" ~ "Alternative",
      value == "Flight" ~ "Alternative",
      value == "Passport" ~ "Passport Control",
      value == "No Preference" ~ "No Preference"))

lwp$choice = factor(lwp$choice,
                    levels = c("Flight_Passport",
                               "Passport_Baggage",
                               "Passport_Train"))

lwp %>% 
    na.omit() %>% 
    group_by(choice, value2) %>% 
    summarise(total = n(),
              text = "ALL") %>% 
    ggplot()+
    aes(x = fct_rev(choice), y = total, fill = value2)+
    geom_col(position = 'fill')+
  coord_flip() +
  scale_y_continuous(labels = scales::percent)+
  my_theme()+
  theme(legend.position = "right")+
  scale_x_discrete(labels = c(str_wrap("Passport Control vs Train Home",16), 
                              str_wrap("Passport Control vs Baggage Reclaim",16), 
                              str_wrap("Passport Control vs Flight Time",16)))+
  labs(y = "Share of Respondents",
       x = "",
       fill = "Wait Time Preference",
       subtitle = "Where would respondents prefer to spend the same amount of additional time?")+
  scale_fill_manual(values = c("lightpink","grey","lightblue"))
ggsave("queue_prefernece.png", height = 4, width = 8)

lwp %>% 
  na.omit() %>% 
  filter(Sex != "Prefer not to say") %>% 
  group_by(value2, age_band2) %>% 
  summarise(total = n(),
            text = "ALL")

cc = table(lwp$Sex,lwp$value2)


lwp %>% 
  filter(age_band2 == "18-34") %>% 
  count(value2) %>% 
  mutate(prop = n/sum(n))

chisq = chisq.test(cc)

chisq

####Validation


mainx = filter(main, Include. == 'y')
nrow(mainx)

##Create long format of filtered database
#Select required columns and melt
mainl = select(mainx, c(1,5,15:16,19, 21:56, 82:83, 86, 91:93))

long <- melt(setDT(mainl), 
             id.vars = c("ProlificID","Pass_Bag","Age", 
                         "age_band2","Sex","Salary_Amount",
                         "OP_Pass","OP_Bag","Freq_travel",
                         "overall_op", "pass_diff"), 
             variable.name = "choice")
#Convert choice fields into time and compensation
longc = data.frame(do.call("rbind", strsplit(as.character(long$choice), "_", fixed = TRUE)))
long$type = longc$X1
long$time = as.numeric(as.character(longc$X2))
long$comp = as.numeric(as.character(longc$X3))
long$value = as.numeric(long$value)
long = na.omit(long)
long$costA = long$comp * -1
long$value = long$value -1
long$vtt = (long$comp/long$time)*60

long %>% group_by(type, value) %>% 
  summarise(total = n(),
            avg_comp = mean(vtt))

#Create groups for passport/bag queue opinion (low/high)
long = long %>% mutate(OP_Pass_group = case_when(
  OP_Pass <= 4 ~ "Low",
  OP_Pass >4 ~ "High"))

long = long %>% mutate(Bag_Pass_group = case_when(
  OP_Bag <= 4 ~ "Low",
  OP_Bag >4 ~ "High"))

#Split dataframe into 3 scenarios (passport/baggage/train)
pass = long %>% filter(type == 'PASS') %>% 
  na.omit()

bag = long %>% filter(type == 'BAG') %>% 
  na.omit()

Train = long %>% filter(type == 'TRAIN') %>% 
  na.omit()

##Pasport Analysis
#Logit model building and validation
model = glm(value ~ time + comp + Sex + Salary_Amount + Age + OP_Pass_group, data = pass,
            family = 'binomial')
summary(model)

write.csv(tidy(model), "pass_regmod.csv")

nullmodel = glm(value ~ 1, family = 'binomial', data = pass)

summary(nullmodel)

(1-logLik(model))/logLik(nullmodel)

#Model predictive accuracy
smp_size = floor(0.8 * nrow(pass))
train_ind = sample(seq_len(nrow(pass)),size = smp_size)
train = pass[train_ind,]
test = pass[-train_ind,]

mod = glm(value ~ time + comp + Age + Sex + Salary_Amount + OP_Pass_group,
          data = train, family = 'binomial')

probabilities = mod %>% predict(test, type = "response")
prediction = ifelse(probabilities >0.5, 1,0)

mean(prediction == test$value)


#Probability density function of accepting compensation
prob_cost = data.frame( comp = seq(0,45, by = 1))
prob_cost$Time = 30
prob_cost$Male = 1
prob_cost$Salary_Amount = median((na.omit(main$Salary_Amount)))
prob_cost$Age = median(main$Age, na.rm = TRUE)
compB = as.numeric(coef(model)["comp"])
timeB = as.numeric(coef(model)["time"])
AgeB = as.numeric(coef(model)["Age"])
Salary_AmountB = as.numeric(coef(model)["Salary_Amount"])
SexB = as.numeric(coef(model)["SexMale"])
intercept = as.numeric(coef(model)["(Intercept)"])

prob_cost = prob_cost %>% 
  mutate(log_odd = intercept + (timeB*Time) + 
           (compB*comp) + 
           (SexB*Male) + 
           (Salary_AmountB*Salary_Amount) + 
           (AgeB*Age)) %>% 
  mutate(prob = exp(log_odd)/(1+exp(log_odd)))

prob_cost %>% 
  ggplot()+
  aes(x = comp, y = prob)+
  geom_line()+
  labs(x = "Compensation Offered (£)",
       y = "Likelihood of Accepting",
       subtitle = "30 Minute Wait-Median Salary_Amount-Median Age-Male")+
  scale_x_continuous(breaks = (seq(0,45,5)))+
  scale_y_continuous(labels = scales::percent)+
  my_theme()
ggsave("probAccept_30min.png")


#Various Passport Choice visualisations
pass %>% 
  group_by(OP_Pass) %>% 
  summarise(avg_accept = mean(value),
            share = n()/total) %>% 
  ggplot()+
  aes(x = OP_Pass, y = avg_accept, size = share)+
  geom_point(colour = 'lightblue')+
  my_theme()+
  theme(legend.position = "right")+
  ylim(0.19,0.5)+
  scale_size_continuous(labels = scales::percent)+
  scale_y_continuous(labels = scales::percent)+
  labs(x = "Passport Wait Time Dissatisfaction Score",
       y = "Average Compensation Acceptance",
       size = str_wrap("Share of Respondents",8))
ggsave("accept_satscore.png")

pass %>% 
  group_by(time) %>% 
  summarise(avg_accept = mean(value),Avg_CompHr = mean(vtt),
            share = n()/total) %>% 
  ggplot()+
  aes(x = time, y = avg_accept, fill = Avg_CompHr)+
  geom_col(colour = 'lightblue')+
  my_theme()+
  theme(legend.position = "right")+
  ylim(0.19,0.5)+
  scale_y_continuous(labels = scales::percent)+
  labs(x = "Wait Time",
       y = "Average Compensation Acceptance",
       size = str_wrap("Share of Respondents",8))
ggsave("accept_satscore.png")

test = select(pass, c("time","comp", "vtt"))

unique(test)
pass %>% 
  group_by(vtt) %>% 
  summarise(Likelihood_Accept = mean(value)) %>% 
  ggplot()+
  aes(x = vtt, y = Likelihood_Accept)+
  geom_point()+
  geom_smooth(formula = 'y~x')+
  labs(x = "Compensation Offered (£ per Minute)",
       y = "Likelihood of Acceptance")+
  my_theme()
ggsave("accept_comp.png")

pass %>% 
  group_by(vtt, time) %>% 
  summarise(accept = mean(value)) %>%  
  ggplot()+
  aes(x = time, y = vtt, size = accept)+
  geom_point(colour = "red", alpha = 0.5)+
  labs(x = "Time Queuing (Mins)",
       y = "Compensation (£ per Minute)",
       size = "% Accept")+
  theme_classic()+
  scale_size(labels = scales::percent)
ggsave("Time_Comp.png")

pass %>% 
    group_by(vtt, time) %>% 
    summarise(accept = mean(value)) %>%  
    ggplot()+
    aes(x = vtt, y = accept, , colour = as.factor(time))+
    geom_point(size = 2) +
  labs(x = "Compensation Offered (£ per Hour)",
       y = "Likelihood of Acceptance",
       colour = str_wrap("Total Wait Time (Minutes)",14))+
    theme_classic()+
  scale_y_continuous(labels = scales::percent)
ggsave("comp_accept.png")

pass %>%
  filter(Sex != "Prefer not to say") %>% 
  group_by(Sex) %>% 
  summarise(avg_accept = mean(value),
            avg_vtt = mean(vtt)) 
#Test of correlcation between compensation VTT and likelihood of acceptanace
test = pass %>% 
  group_by(vtt, time) %>% 
  summarise(accept = mean(value))
cor(test$vtt,test$accept)

pass %>%
  group_by(age_band2) %>% 
  summarise(avg_accept = mean(value),
            avg_vtt = mean(vtt)) 

pass %>%
  group_by(Sex) %>% 
  summarise(avg_accept = mean(value),
            avg_vtt = mean(vtt)) 

mean(pass$value, na.rm = TRUE)

pass %>%
  mutate(incA30 = case_when(
    Salary_Amount <40000 ~ "30 or less",
    Salary_Amount >=40000 ~ "over 30"
  )) %>% 
  group_by(incA30) %>% 
  summarise(avg_accept = mean(value),
            avg_vtt = mean(vtt))

pass %>%
  mutate(passAtt = case_when(
    OP_Pass <5 ~ "4 or lower",
    OP_Pass >=5 ~ "5 or higher"
  )) %>% 
  group_by(passAtt) %>% 
  summarise(avg_accept = mean(value),
            avg_vtt = mean(vtt))

pass %>%
  filter(Sex != "Prefer not to say") %>% 
  group_by(Sex, age_band2) %>% 
  summarise(avg_accept = mean(value)) %>% 
  ggplot()+
  aes(x = Sex, y = avg_accept, fill = age_band2)+
  geom_col(position = 'dodge')+
  scale_y_continuous(labels = scales::percent)+
  labs(x = "Sex",
       y = "% Accept Compensation",
       fill = "Age")+
  my_theme()
ggsave("SexAge_Accept.png")

pass %>%
  group_by(Sex, age_band2) %>% 
  summarise(avg_accept = mean(value),
            total = n()/6)

pass_poor = filter(pass, Salary_Amount <30000)
pass_rich = filter(pass, Salary_Amount>=30000)

mean(pass_poor$value)
mean(pass_poor$vtt)
mean(pass_rich$value)
mean(pass_rich$vtt)

table(mainx$age_band2)
total = nrow(mainx)
pass %>% 
  filter(Salary_Amount <= 100000) %>% 
  group_by(Salary_Amount) %>% 
  summarise(avg_accept = mean(value),
            avg_cost = mean(value),
            avg_vtt = mean(vtt),
            share = n()/total) %>% 
  ggplot()+
  aes(x = Salary_Amount, y = avg_accept, size = share)+
  geom_point(colour = 'pink')+
  labs(x = "Annual Salary_Amount",
       y = "Average Compensation Acceptance",
       size = str_wrap("Total Respondents",7))+
  scale_y_continuous(labels = scales::percent)+
  my_theme()+
  scale_size_continuous(labels = scales::percent)
ggsave("Salary_Amount_Accept.png")

pass %>% 
  filter(Pass_Bag != "") %>% 
  group_by(Pass_Bag) %>% 
  summarise(avg_accept = mean(value),
            avg_vtt = mean(vtt)) %>% 
  ggplot()+
  aes(x = Pass_Bag, y = avg_accept, fill = as.factor(round(avg_vtt,1))) %>% 
  geom_col() +
  labs(x = "Queue Avoidance Preference",
       y = "Average Compensation Acceptance",
       fill = str_wrap("Average Compension Per Hour (£)",8))
ggsave("pass_ComApt_QA.png")

pass %>% 
  filter(Sex != "Prefer not to say") %>% 
  filter(Age >=18) %>% 
  group_by(Age) %>% 
  summarise(avg_accept = mean(value),
            avg_comp = mean(vtt),
            total = n()/6) %>% 
  ggplot()+
  aes(x = Age, y = avg_accept, colour = avg_comp)+
  geom_point()+
  labs(x = "Age",
       y = "Average Acceptance of Compensation",
       colour = str_wrap("Average Compensation Offered",8))+
  scale_y_continuous(labels = scales::percent)+
  my_theme()
ggsave("Age_Accept.png")

###Comparing passport VTT by queue preference
pass_b = filter(pass, Pass_Bag == "There is no queue at passport control, however you need to wait 25 minutes for your hold luggage in baggage reclaim")
pass_p = filter(pass, Pass_Bag == "The queue at passport control was 25 minutes long, however your hold luggage is waiting for you in baggage reclaim")
results = c()

dblist = list(pass_b, pass_p)

for (db in dblist){
  
  model = glm(value ~ time + costA + Sex + Salary_Amount + Age + 
                OP_Pass_group + Bag_Pass_group, data = db,
              family = 'binomial')
  
  time = as.numeric(coef(model)["time"])
  cost = as.numeric(coef(model)["costA"])
  error_time = as.numeric(sqrt(diag(vcov(model)))["time"])
  error_cost = as.numeric(sqrt(diag(vcov(model)))["costA"])
  
  vtt = (time/cost) * 60
  
  vtt_min = ((time+(2*error_time))/(cost-(2*error_cost)))*60
  
  vtt_max = ((time-(2*error_time))/(cost+(2*error_cost)))*60
  results = append(results, c(vtt, vtt_min,vtt_max))
}

results

res_df = data.frame(type = c("Passport","Baggage"),
                    VTT = results[c(1,4)],
                    Min = results[c(2,5)],
                    Max = results[c(3,6)])

res_df$type = factor(res_df$type,
                     levels = c("Passport", "Baggage"))


res_df %>% 
  ggplot()+
  aes(x = type, y = VTT)+
  geom_point(fill = "white", colour = "lightblue", size = 6,
             shape = "diamond")+
  geom_errorbar(aes(ymin = Min, ymax = Max), width=.2,
                position = position_dodge(),
                colour = "grey")+
  scale_x_discrete(labels = c(str_wrap("Passport Control",8),
                              str_wrap("Baggage Reclaim",7),
                              str_wrap("Train Home",5)))+
  scale_y_continuous(min = 15)+
  labs(x = "Queue Avoidance Preference",
       y = "Valuation of Travel Time (£ per Hour)",
       caption = "Error Bars show 90% confidence")+
  theme(axis.title.y = element_text(size = 10))
ggsave("VTT_Pass_QA.png")

#####Sex
pass_male = filter(pass, Sex == 'Male')
pass_female = filter(pass, Sex == 'Female')

results = c()

dblist = list(pass_male, pass_female)

for (db in dblist){
  
  model = glm(value ~ time + costA + Salary_Amount + Age + OP_Pass_group,
              data = db,
              family = 'binomial')
  
  time = as.numeric(coef(model)["time"])
  cost = as.numeric(coef(model)["costA"])
  dmob = deltaMethod(model, "time/costA")
  
  vtt = (time/cost) * 60
  
  vtt_min = dmob$`2.5 %`*60
  
  vtt_max = dmob$`97.5 %`*60
  results = append(results, c(vtt, vtt_min,vtt_max))
}

results

res_df = data.frame(type = c("Male","Female"),
                    VTT = results[c(1,4)],
                    Min = results[c(2,5)],
                    Max = results[c(3,6)])

res_df$CI = res_df$VTT - res_df$Min
res_df

res_df %>% 
  ggplot()+
  aes(x = type, y = VTT)+
  geom_point(fill = "white", colour = "lightblue", size = 6,
             shape = "diamond")+
  geom_errorbar(aes(ymin = Min, ymax = Max), width=.2,
                position = position_dodge(),
                colour = "grey")+
  scale_y_continuous(min = 15)+
  labs(x = "Sex",
       y = "Valuation of Travel Time (£ per Hour)",
       caption = "Error Bars show 95% confidence")+
  theme(axis.title.y = element_text(size = 10))
ggsave("VTT_Pass_Male.png")

######Age
pass_young = filter(pass, Age < 46)

pass_old = filter(pass, Age >45)

results = c()

dblist = list(pass_young, pass_old)

for (db in dblist){
  
  model = glm(value ~ time + costA + Sex + Age + OP_Pass_group,
              data = db,
              family = 'binomial')
  
  time = as.numeric(coef(model)["time"])
  cost = as.numeric(coef(model)["costA"])
  dmob = deltaMethod(model, "time/costA")
  
  vtt = (time/cost) * 60
  
  vtt_min = dmob$`2.5 %`*60
  
  vtt_max = dmob$`97.5 %`*60
  results = append(results, c(vtt, vtt_min,vtt_max))
}

results

res_df = data.frame(type = c("Age<45","Age>46"),
                    VTT = results[c(1,4)],
                    Min = results[c(2,5)],
                    Max = results[c(3,6)])

res_df$type = factor(res_df$type,
                     levels = c("Age<45","Age>46"))

res_df$CI = res_df$VTT - res_df$Min
res_df

res_df %>% 
  ggplot()+
  aes(x = type, y = VTT)+
  geom_point(fill = "white", colour = "lightblue", size = 6,
             shape = "diamond")+
  geom_errorbar(aes(ymin = Min, ymax = Max), width=.2,
                position = position_dodge(),
                colour = "grey")+
  scale_y_continuous(min = 15)+
  labs(x = "Age Group",
       y = "Valuation of Travel Time (£ per Hour)",
       caption = "Error Bars show 90% confidence")+
  theme(axis.title.y = element_text(size = 10))
ggsave("VTT_Pass_Salary_Amount.png")

######Salary_Amount

pass_poor = filter(pass, Salary_Amount <= 40000)

pass_rich = filter(pass, Salary_Amount >=40000)

results = c()

dblist = list(pass_poor, pass_rich)

for (db in dblist){
  
  model = glm(value ~ time + costA + Sex + Age + OP_Pass_group,
              data = db,
              family = 'binomial')
  
  time = as.numeric(coef(model)["time"])
  cost = as.numeric(coef(model)["costA"])
  dmob = deltaMethod(model, "time/costA")
  
  vtt = (time/cost) * 60
  
  vtt_min = dmob$`2.5 %`*60
  
  vtt_max = dmob$`97.5 %`*60
  results = append(results, c(vtt, vtt_min,vtt_max))
}

results

res_df = data.frame(type = c("Salary_Amount<£35k","Salary_Amount>£35k"),
                    VTT = results[c(1,4)],
                    Min = results[c(2,5)],
                    Max = results[c(3,6)])

res_df$type = factor(res_df$type,
                     levels = c("Salary_Amount<£35k","Salary_Amount>£35k"))

res_df$CI = res_df$VTT - res_df$Min
res_df

res_df %>% 
  ggplot()+
  aes(x = type, y = VTT)+
  geom_point(fill = "white", colour = "lightblue", size = 6,
             shape = "diamond")+
  geom_errorbar(aes(ymin = Min, ymax = Max), width=.2,
                position = position_dodge(),
                colour = "grey")+
  labs(x = "Queue Avoidance Preference",
       y = "Valuation of Travel Time (£ per Hour)",
       caption = "Error Bars show 90% confidence")+
  theme(axis.title.y = element_text(size = 10))+
  my_theme()
ggsave("VTT_Pass_Salary_Amount.png")

###Travel comp comp

results = c()

dblist = list(pass, bag,Train)

for (db in dblist){

model = glm(value ~ time + costA + Sex + Salary_Amount + Age 
            + OP_Pass_group,  data = db,
            family = 'binomial')

time = as.numeric(coef(model)["time"])
cost = as.numeric(coef(model)["costA"])

dmob = deltaMethod(model, "time/costA")

vtt = (time/cost) * 60

vtt_min = dmob$`2.5 %`*60

vtt_max = dmob$`97.5 %`*60
results = append(results, c(vtt, vtt_min,vtt_max))
}
results

res_df = data.frame(type = c("Passport","Baggage","Train"),
                        VTT = results[c(1,4,7)],
                        Min = results[c(2,5,8)],
                        Max = results[c(3,6,9)])
                        
res_df$type = factor(res_df$type,
                     levels = c("Passport", "Baggage", "Train"))

res_df


res_df %>% 
  ggplot()+
  aes(x = type, y = VTT)+
  geom_point(fill = "white", colour = "lightblue", size = 6,
             shape = "diamond")+
  geom_errorbar(aes(ymin = Min, ymax = Max), width=.2,
                position = position_dodge(),
                colour = "grey")+
  scale_x_discrete(labels = c(str_wrap("Passport Control",8),
                              str_wrap("Baggage Reclaim",7),
                              str_wrap("Train Home",5)))+
  scale_y_continuous(min = 15)+
  labs(x = "Arrival Journey Component",
       y = "Valuation of Travel Time (£ per Hour)",
       caption = "Error Bars show 95% confidence")+
  my_theme()

ggsave("VTT_Comp.png")

#Passport model analysis

model = glm(value ~ time + comp + Sex + Salary_Amount + Age, data = pass,
            family = 'binomial')
nullModel = glm(value ~ 1, data = pass, family = 'binomial')

summary(nullModel)

summary(model)  

model_broom = tidy(model)

model_broom

write.csv(model_broom,"model_output.csv")

vcov(model)
dmod = deltaMethod(model, "time/comp")

dmod$`2.5 %`

60*0.6936516
60*0.631505

