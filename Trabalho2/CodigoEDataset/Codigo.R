library(tidyverse)
library(tsibble)
library(fpp3)
library(stringi)
library(stringr)
library(seasonal)
library(kableExtra)

dados <- read.csv("company.csv")
dados <- dados[1:64,]
names(dados)[3] <- "values"
names(dados)[1] <- "date"

dados$date <- as.Date(dados$date, format = "%d.%m.%Y")

dados <- dados %>% 
  mutate(date = yearmonth(date)) %>% 
  as_tsibble(index = date)

dados$values <- as.numeric(dados$values)

glimpse(dados)

fit_mean <- dados |> model(MEAN(values))
fit_mean |> forecast(h = 5) |> autoplot(dados, level = NULL) 
fit_mean %>% gg_tsresiduals()

fit_naive <- dados |>  model(NAIVE(values))
fit_naive |> forecast(h = 5) |> autoplot(dados, level = NULL) 
fit_naive %>% gg_tsresiduals()

fit_snaive <- dados |> model(SNAIVE(values ~ lag(12))) 
fit_snaive |> forecast(h = 5) |> autoplot(dados, level = NULL) 
fit_snaive %>% gg_tsresiduals()

fit_drift <- dados |> model(RW(values ~ drift())) 
fit_drift |> forecast(h = 5) |> autoplot(dados, level = NULL) 
fit_drift %>% gg_tsresiduals()

fit_dcmp <- dados |>
  model(stlf = decomposition_model(
    STL(values ~ trend(window = 7), robust = TRUE),
    NAIVE(season_adjust)))
fit_dcmp |>
  forecast(h = 5) |>
  autoplot(dados, level = NULL)
fit_dcmp %>% gg_tsresiduals()

fit_tsm <- dados |>
  model(TSLM(values ~ trend() + season())) 
fit_tsm |> forecast(h = 5) |> autoplot(dados, level = NULL) 
fit_tsm %>% gg_tsresiduals()

fit_fourier <- dados |>
  model(TSLM(values ~ trend() + fourier(K = 2))) 
fit_fourier |> forecast(h = 5) |> autoplot(dados, level = NULL) 
fit_fourier %>% gg_tsresiduals()

fit_spline <- dados |>
  model(TSLM(values ~ trend(knots = as.Date("2016-07-01"))))
fit_spline |> forecast(h = 5) |> autoplot(dados, level = NULL) 
fit_spline %>% gg_tsresiduals()

fit_ets <- dados |>
  model(ETS(values ~ error("A") + trend("N") + season("N")))
fit_ets |> forecast(h = 5) |> autoplot(dados, level = NULL) 
fit_ets %>% gg_tsresiduals()

fit_holts <- dados |>
  model(ETS(values ~ error("A") + trend("A") + season("N")))
fit_holts |> forecast(h = 5) |> autoplot(dados, level = NULL) 
fit_holts %>% gg_tsresiduals()

fit_holtsAmort <- dados |>
  model(ETS(values ~ error("A") + trend("Ad") + season("N")))
fit_holtsAmort |> forecast(h = 5) |> autoplot(dados, level = NULL) 
fit_holtsAmort %>% gg_tsresiduals()

fit_holtsWint <- dados |>
  model(ETS(values ~ error("A") + trend("A") + season("A")))
fit_holtsWint |> forecast(h = 5) |> autoplot(dados, level = NULL) 
fit_holtsWint %>% gg_tsresiduals()

dados_tr <- dados %>% 
  stretch_tsibble(.init = 50, .step = 1)

tabela <- dados_tr %>%
  model(Mean = MEAN(values),
        Naive = NAIVE(values),
        SNaive = SNAIVE(values ~ lag(12)),
        Drift = RW(values ~ drift()),
        Decomposition = decomposition_model(STL(values ~ trend(window = 7), robust = TRUE), NAIVE(season_adjust)),
        TSM = TSLM(values ~ trend() + season()),
        Fourier = TSLM(values ~ trend() + fourier(K = 2)),
        Spline = TSLM(values ~ trend(knots = as.Date("2016-07-01"))),
        ETS = ETS(values ~ error("A") + trend("N") + season("N")),
        Holts = ETS(values ~ error("A") + trend("A") + season("N")),
        Holts_Amortecido = ETS(values ~ error("A") + trend("Ad") + season("N")),
        Holts_Winter = ETS(values ~ error("A") + trend("A") + season("A"))) %>%
  forecast(h = 1) %>%
  accuracy(dados)

tabelaK <- tabela[,-2]
names(tabelaK)[1] <- "Modelo"

tabelaK <- kable(tabelaK, "html") %>%
  row_spec(7, background = "yellow") %>% 
  kable_styling(full_width = FALSE, bootstrap_options = "striped")

tabelaK

give_table <- function(dados, fore = 5){
  dados_tr <- dados %>% 
    stretch_tsibble(.init = 50, .step = fore)
  
  tabela <- dados_tr %>%
    model(Mean = MEAN(values),
          Naive = NAIVE(values),
          SNaive = SNAIVE(values ~ lag(12)),
          Drift = RW(values ~ drift()),
          Decomposition = decomposition_model(STL(values ~ trend(window = 7), robust = TRUE), NAIVE(season_adjust)),
          TSM = TSLM(values ~ trend() + season()),
          Fourier = TSLM(values ~ trend() + fourier(K = 2)),
          Spline = TSLM(values ~ trend(knots = as.Date("2016-07-01"))),
          ETS = ETS(values ~ error("A") + trend("N") + season("N")),
          Holts = ETS(values ~ error("A") + trend("A") + season("N")),
          Holts_Amortecido = ETS(values ~ error("A") + trend("Ad") + season("N")),
          Holts_Winter = ETS(values ~ error("A") + trend("A") + season("A"))) %>%
    forecast(h = fore) %>%
    accuracy(dados)
  tabelaK <- tabela[,-2]
  names(tabelaK)[1] <- "Modelo"
  vec = rep(fore, nrow(tabelaK))
  tabelaK$Passos = vec
  
  return(tabelaK)
}

tabela1 = give_table(dados, fore = 1)
tabela2 = give_table(dados, fore = 2)
tabela3 = give_table(dados, fore = 3)
tabela4 = give_table(dados, fore = 4)
tabela5 = give_table(dados, fore = 5)

tab_scale <- function(tabela){
  tab_aux = tabela[,c(1,10)]
  tabela = tabela[,2:9]
  minimuns <- apply(tabela, 2, min)
  sds <- apply(tabela, 2, sd)
  tabela = abs(tabela)
  
  df_norm <- as.data.frame(scale(tabela, center = minimuns, scale = sds))
  somas = rowSums(df_norm)
  tab_aux$Somas = somas
  return(tab_aux)
}

tabelas = rbind(tab_scale(tabela1), tab_scale(tabela2), tab_scale(tabela3), tab_scale(tabela4), tab_scale(tabela5))

tabelas |>
  ggplot()+
  geom_line(aes(x = Passos, y = Somas, color = Modelo), size = 1)+
  scale_colour_manual(values = c("green",rep("gray", 5), "red", rep("gray", 4), "blue", rep("gray", 4)))+
  labs(y = "Soma das Estatísticas de Erro sob a mesma Escala", x = "Número de Passos")

modelos <- dados %>%
  model(#Mean = MEAN(values),
    #Naive = NAIVE(values),
    #SNaive = SNAIVE(values ~ lag(12)),
    #Drift = RW(values ~ drift()),
    Decomposition = decomposition_model(STL(values ~ trend(window = 7), robust = TRUE), NAIVE(season_adjust)),
    TSM = TSLM(values ~ trend() + season()),
    #Fourier = TSLM(values ~ trend() + fourier(K = 2)),
    #Spline = TSLM(values ~ trend(knots = as.Date("2016-07-01"))),
    #ETS = ETS(values ~ error("A") + trend("N") + season("N")),
    #Holts = ETS(values ~ error("A") + trend("A") + season("N")),
    #Holts_Amortecido = ETS(values ~ error("A") + trend("Ad") + season("N")),
    Holts_Winter = ETS(values ~ error("A") + trend("A") + season("A")))

previsoes <- forecast(modelos, h = 5)
previsoes

dados |> autoplot(values) + labs(y = "Número de Vendas", x = "Mês e Ano")

names(previsoes)[4] = "values"

dados %>%
  autoplot(values) +
  autolayer(previsoes[,c(1,2,4)], size = 1.5) +
  labs(y = "Valor", x = "Mês")

fit_holtsWint <- dados |>
  model(ETS(values ~ error("A") + trend("A") + season("A")))
fit_holtsWint |> forecast(h = 5) |> autoplot(dados, level = NULL) 
fit_holtsWint %>% gg_tsresiduals()

fit_tsm <- dados |>
  model(TSLM(values ~ trend() + season())) 
fit_tsm |> forecast(h = 5) |> autoplot(dados, level = NULL) 
fit_tsm %>% gg_tsresiduals()

fit_dcmp <- dados |>
  model(stlf = decomposition_model(
    STL(values ~ trend(window = 7), robust = TRUE),
    NAIVE(season_adjust)))
fit_dcmp |>
  forecast(h = 5) |>
  autoplot(dados, level = NULL)
fit_dcmp %>% gg_tsresiduals()

teste1 = fit_holtsWint |> 
  augment() |> 
  features(.innov, ljung_box, lag = 2, dof = 0)

teste2 = fit_dcmp |> 
  augment() |> 
  features(.innov, ljung_box, lag = 2, dof = 0)

teste3 = fit_tsm |> 
  augment() |> 
  features(.innov, ljung_box, lag = 2, dof = 0)

df_testes = rbind(teste1, teste2, teste3)

df_testes$.model = c("Método de Holt", "Decomposição", "TSM")

tabela_testes = kable(df_testes, "html") %>%
  kable_styling(full_width = FALSE, bootstrap_options = "striped")

tabela_testes