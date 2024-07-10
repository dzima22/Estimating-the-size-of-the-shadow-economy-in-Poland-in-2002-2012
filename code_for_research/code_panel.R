#libraries 
install.packages('plm')
library(plm)
library(lmtest)
library(tseries)
library(ggplot2)
library(dplyr)
#define variables 
data_for_panel$wojewodstwa_num <- as.numeric(as.factor(data_for_panel$wojewodstwa))
pdata <- pdata.frame(data_for_panel, index = c("wojewodstwa_num", "Data"))
pdata <- pdata %>%
  group_by(wojewodstwa_num) %>%
  mutate(
    przy_PKB = if_else(Data == 2002, NA_real_, log(PKB) - lag(log(PKB))),
    przy_produkcja_globalna = if_else(Data == 2002, NA_real_, log(produkcja.globalna) - lag(log(produkcja.globalna))),
    przy_zuzycie_posrednie = if_else(Data == 2002, NA_real_, log(zuzycie.posrednie) - lag(log(zuzycie.posrednie))),
    przy_Inwestycje = if_else(Data == 2002, NA_real_, log(Inwestycje) - lag(log(Inwestycje))),
    przy_wydatki_budzetowe = if_else(Data == 2002, NA_real_, log(wydatki.budzetowe) - lag(log(wydatki.budzetowe))),
    przy_konsumpcja = if_else(Data == 2002, NA_real_, log(konsumpcja) - lag(log(konsumpcja))),
    eksp = if_else(Data == 2002, NA_real_, eksport.netto / lag(eksport.netto)),
    przy_eksp = if_else(Data == 2002, NA_real_, log(eksp)),
  )
#deal with NA
pdata <- pdata[pdata$Data != 2002, ]
pdata <- pdata %>%
  mutate(across(everything(), ~ replace(., is.na(.), 0)))
#models 
model1 <- plm(przy_PKB ~ przy_produkcja_globalna + przy_zuzycie_posrednie, model = "random",data = pdata)
pdata$p_pred1 <- predict(model1)
model2 <- plm(przy_PKB ~ przy_konsumpcja + przy_Inwestycje + przy_wydatki_budzetowe + przy_eksp, data = pdata, model = "random")
pdata$p_pred2 <- predict(model2)
#summary
summary(pdata[c("przy_produkcja_globalna", "przy_zuzycie_posrednie", "przy_Inwestycje", 
                "przy_wydatki_budzetowe", "przy_konsumpcja", "przy_eksp")])
summary(pdata$przy_PKB)

#tranform predictions
mean_data <- pdata %>%
  group_by(Data) %>%
  summarise(across(everything(), mean, na.rm = TRUE))
mean_data <- mean_data %>%
  mutate(PKB_new = exp(p_pred1) * (PKB),
         PKB_new2 = exp(p_pred2) * (PKB))

mean_data <- mean_data %>%
  mutate(szara_strefa = PKB_new - PKB_new2,
         szara_strefa_pr = szara_strefa / PKB,
         szara_strefa_pr_1 = szara_strefa_pr * 100,
         szara_strefa_pr_2 = abs(szara_strefa_pr * 100))

print(mean_data %>%
        select(Data, PKB, PKB_new, PKB_new2, szara_strefa_pr_2, szara_strefa) %>%
        slice(3:11))
#adf test
adf.test(mean_data$przy_PKB)
adf.test(mean_data$przy_produkcja_globalna)
adf.test(mean_data$przy_zuzycie_posrednie)
adf.test(mean_data$przy_Inwestycje)
adf.test(mean_data$przy_wydatki_budzetowe)
adf.test(mean_data$przy_konsumpcja)
adf.test(mean_data$przy_eksp)

#plots 
mean_data <- mean_data %>%
  mutate(roznica = produkcja.globalna - zuzycie.posrednie)

ggplot(data = mean_data, aes(x = Data)) +
  geom_line(aes(y = PKB, color = "Rzeczywisty PKB"), group = 1) +
  geom_line(aes(y = produkcja.globalna, color = "produkcja globalna"), group = 1) +
  geom_line(aes(y = zuzycie.posrednie, color = "zużycie pośrednie"), group = 1) +
  geom_line(aes(y = roznica, color = "różnica Produkcja-Zużycie"), group = 1) +
  labs(title = "PKB i jego składniki", y = "mln zł", x = "Rok") +
  scale_color_manual(name = "Legenda", 
                     values = c("Rzeczywisty PKB" = "blue", 
                                "produkcja globalna" = "green", 
                                "zużycie pośrednie" = "red", 
                                "różnica Produkcja-Zużycie" = "purple"))


ggplot(data = mean_data, aes(x = Data)) +
  geom_line(aes(y = Inwestycje, color = "Inwestycje"), group = 1) +
  geom_line(aes(y = wydatki.budzetowe, color = "Wydatki budżetowe"), group = 1) +
  geom_line(aes(y = konsumpcja, color = "Konsumpcja"), group = 1) +
  geom_line(aes(y = eksport.netto, color = "Eksport netto"), group = 1) +
  labs(title = "PKB i jego składniki", y = "zł", x = "Rok") +
  scale_color_manual(name = "Legenda", 
                     values = c("Inwestycje" = "blue", 
                                "Wydatki budżetowe" = "red", 
                                "Konsumpcja" = "green", 
                                "Eksport netto" = "yellow")) +
  theme_minimal()



ggplot(data = mean_data, aes(x = Data)) +
  geom_line(aes(y = PKB, color = "Rzeczywisty PKB"), group = 1) +
  geom_line(aes(y = PKB_new, color = "PKB z Modelu I"), group = 1) +
  geom_line(aes(y = PKB_new2, color = "PKB z Modelu II"), group = 1) +
  labs(title = "PKB i predykcji PKB", y = "mln zł", x = "Rok") +
  scale_y_continuous(labels = scales::comma) +
  scale_color_manual(name = "Legenda", 
                     values = c("Rzeczywisty PKB" = "blue", 
                                "PKB z Modelu I" = "red", 
                                "PKB z Modelu II" = "green")) +
  theme_minimal()

mean_data <- mean_data %>%
  mutate(szara_GUS = case_when(
    Data == 2005 ~ 13.7,
    Data == 2006 ~ 13.7,
    Data == 2007 ~ 12.8,
    Data == 2008 ~ 11.8,
    Data == 2009 ~ 13.1,
    Data == 2010 ~ 12.6,
    Data == 2011 ~ 12.1,
    Data == 2012 ~ 14,
    TRUE ~ NA_real_
  ),
  szara_EY = case_when(
    Data == 2005 ~ 18.9,
    Data == 2006 ~ 17.4,
    Data == 2007 ~ 15.8,
    Data == 2008 ~ 14,
    Data == 2009 ~ 14.4,
    Data == 2010 ~ 14.6,
    Data == 2011 ~ 14.2,
    Data == 2012 ~ 14.1,
    TRUE ~ NA_real_
  ))
print(mean_data %>%
        select(Data, szara_strefa_pr_2, szara_GUS, szara_EY) %>%
        slice(4:11))
ggplot(data = mean_data %>% slice(4:11), aes(x = Data)) +
  geom_line(aes(y = szara_strefa_pr_2, color = "Badanie własne"), group = 1) +
  geom_line(aes(y = szara_GUS, color = "Szara strefa według GUS"), group = 1) +
  geom_line(aes(y = szara_EY, color = "Szara strefa według EY"), group = 1) +
  labs(title = "Udział szarej strefy według GUS, EY i własne badanie", y = "procent", x = "Rok") +
  scale_color_manual(name = "Legenda", 
                     values = c("Badanie własne" = "blue", 
                                "Szara strefa według GUS" = "green", 
                                "Szara strefa według EY" = "red")) +
  theme_minimal()
