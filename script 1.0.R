# INTRODUÇÃO --------------------------------------------------------------

# O objetivo do projeto é analisar a evolução histórica dos padrões de 
# emissão de gás carbônico (CO2) na atmosfera, assim como identificar o 
# melhor modelo preditivo a partir de dados do tamanho da população e do PIB 

# Esse projeto também está disponível na forma de um notebook em R Markdown
# disponível nesse link: https://douglasnovelli.github.io/co2_projections/


# BIBLIOTECAS NECESSÁRIAS --------------------------------------
library(openxlsx) #usado p/ baixar e carregar dados no formato xlsx 
library(tidyverse) #usado p/ organizar e limpar os bancos de dados
library(maps) #fornece dados e funções p/ plotar mapas, incluíndo fronteiras nacionais
library(mgcv) #usado p/ projetar o modelo GAM
library(forecast) #usado p/ projetar os modelos ARIMA e ARIMAX


# BANCOS DE DADOS ----------------------------------------------

# Emissões de CO2, extraídos do projeto EDGAR
url_co2 <- "https://raw.githubusercontent.com/DouglasNovelli/co2_projections/main/datasets/CO2_1970_2018.xlsx"
co2 <- read.xlsx(url_co2)

# População, com dados do Banco Mundial
url_pop <- "https://raw.githubusercontent.com/DouglasNovelli/co2_projections/main/datasets/API_SP.POP.TOTL_DS2_en_excel_v2_4901765.xlsx"
pop <- read.xlsx(url_pop)

# Produto Interno Bruto, com dados do Banco Mundial
url_pib <- "https://raw.githubusercontent.com/DouglasNovelli/co2_projections/main/datasets/API_NY.GDP.MKTP.CD_DS2_en_excel_v2_4901715.xlsx"
pib <- read.xlsx(url_pib)

# Dados sobre as regiões de cada país, com dados do Banco Mundial
url_metadata <- "https://raw.githubusercontent.com/DouglasNovelli/co2_projections/main/datasets/Metadata.xlsx"
metadata <- read.xlsx(url_metadata)


# LIMPEZA DOS BANCOS DE DADOS ---------------------------------------------

# Exclusão das colunas que não serão utilizadas e padronização dos nomes
co2 <- co2 %>%
  select(-IPCC_annex, -C_group_IM24_sh, -Substance) %>%
  rename(ccode = Country_code_A3, country = Name) %>%
  rename_with(~gsub("Y_", "", .x), starts_with("Y_")) #retira o Y_ do começo de cada ano

pop <- pop %>%
  select(-Indicator.Code, -Indicator.Name) %>%
  rename(ccode = Country.Code, country = Country.Name)

pib <- pib %>%
  select(-Indicator.Code, -Indicator.Name) %>%
  rename(ccode = Country.Code, country = Country.Name)

metadata <- metadata %>%
  select(-SpecialNotes) %>%
  rename(ccode = Country.Code, country = TableName, region = Region, income = IncomeGroup)

# O banco metadata usa metadados das bases do banco mundial,
# a notação faltante nesse banco se refere ao mundo como um todo.
# O banco co2 não possui o mesmo número de observações que os demais.

# É importante identificar quais observações faltam em cada banco.
# Essa identificação pode ser realizada com a função filter().
# Como os bancos pip e pop tem a mesma origem, vale a pena começar por eles:

nrow(pib) #266
pib %>%
  filter(ccode %in% pop$ccode) %>%
  nrow() #266

# Como pib manteve o mesmo número de observações, 
# sabemos que os bancos pip e pop tem os mesmos casos.
# Agora qualquer um deles pode ser usado para filtrar o banco co2.

nrow(co2) #219
co2 %>%
  filter(ccode %in% pop$ccode) %>%
  nrow() #199

# 20 casos estão presentes no banco de co2, mas não nos demais.
# O código abaixo fornece a lista

co2 %>%
  filter(!(ccode %in% pop$ccode)) %>%
  pull(country) # exrai os valores da variável country como uma lista

# A maior parte dos casos se refere a territórios ultramarinhos.
# Por conta de seu tamanho reduzido, tendem a não impactar no modelo.
# Opta-se assim pela remoção desses dados.

co2 <- co2 %>%
  filter(ccode %in% pop$ccode)

# O processo contrário revela a presença de múltiplos casos nos 
# bancos pip e pop que não constam na base co2.

pop %>%
  filter(!(ccode %in% co2$ccode)) %>%
  pull(country)

# aqui se tratam de macro regiões que tiveram seus resultados agrupados
# para a análise pretendida, os dados dos Estados bastam.

pop <- pop %>%
  filter(ccode %in% co2$ccode)

pib <- pib %>%
  filter(ccode %in% co2$ccode)

metadata <- metadata %>%
  filter(ccode %in% co2$ccode)

# Com os dados dos Estados padronizados, resta apenas colocá-los no formato tidy.
# Tidy data, ou dados organizados, se refere a um formato de dados onde cada variável 
# é uma coluna e cada observação é uma linha.
# No caso, todas as demais variáveis já estão como uma coluna,
# mas os anos, nas três bases, estão divididas em múltiplas colunas.
# Os códigos abaixo resolvem isso, 
# criando a coluna ano e reunindo os valores na coluna apropriada (co2, pop e pib)

co2 <- co2 %>%
  gather(key = "year", value = "co2", -ccode, -country) %>%
  mutate(year = as.numeric(year))
str(co2)

pop <- pop %>%
  gather(key = "year", value = "pop", -ccode, -country) %>%
  mutate(year = as.numeric(year))
str(pop)

pib <- pib %>%
  gather(key = "year", value = "pib", -ccode, -country) %>%
  mutate(year = as.numeric(year))
str(pib)

# Os dados podem agora ser unificados em um único data frame

dados <- co2 %>%
  full_join(pop, by = c("ccode", "country", "year")) %>%
  full_join(pib, by = c("ccode", "country", "year")) %>%
  drop_na() # descarta observações com valores ausentes
str(dados)


# ANÁLISE DESCRITIVA ------------------------------------------------------

# CO2

# Evolução do total de emissões entre 1970 e 2018
co2 %>% 
  group_by(year) %>%
  summarise(co2_total = sum(co2, na.rm = TRUE)) %>%
  ggplot(aes(x = year, y = co2_total)) +
  geom_line() +
  theme_minimal() +
  labs(title = "Evolução anual do total de emissões de CO2 (kt)",
       subtitle = "1970-2018",
       x = "Ano",
       y = "Total de emissões de CO2 (kt)")

# Evolução do total por região:
co2 %>%
  left_join(metadata, by = "ccode") %>%
  group_by(region, year) %>% #agrupa os dados por região
  summarise(co2_total = sum(co2, na.rm = TRUE)) %>%
  ungroup() %>%
  ggplot(aes(x = year, y = co2_total, color = region)) +
  geom_line() +
  theme_minimal() +
  labs(title = "Evolução do total das emissões de CO2 por região",
       subtitle = "1970-2018",
       x = "Ano",
       y = "Total de emissões de CO2 (kt)",
       color = "")
# O padrão de queda a partir de 2016 se replica em todas as regiões.
# Ainda assim, maior parte das regiões mostra tendencias de crescimento ao longo dos anos;
# Só a América do Norte, Europa e Ásia Central mostram uma tendencia de queda;
# O Leste Asiático e o Pacífico apresentam uma curva acentuada de crescimento das emissões.

# Nota-se que esses gráficos estão sujeito a distorções causadas por dados ausentes
# a média de emissões anuais, desconsiderando os valores ausentes, 
# pode fornecer um parâmetro mais confiável para inferir o crescimento das emissões

co2 %>%
  group_by(year) %>%
  summarise(co2_m = mean(co2, na.rm = TRUE)) %>%
  ggplot(aes(x = year, y = co2_m)) +
  geom_line() +
  theme_minimal() +
  labs(title = "Evolução anual da média de emissões de CO2 (kt)",
       subtitle = "1970-2018",
       x = "Ano",
       y = "Média das emissões de CO2 (kt)")

co2 %>%
  left_join(metadata, by = "ccode") %>%
  group_by(region, year) %>% 
  summarise(co2_m = mean(co2, na.rm = TRUE)) %>%
  ggplot(aes(x = year, y = co2_m, color = region)) +
  geom_line() +
  theme_minimal() +
  labs(title = "Evolução da média das emissões de CO2 por região",
       subtitle = "1970-2018",
       x = "Ano",
       y = "Média das emissões de CO2 (kt)",
       color = "")
  
# Os gráficos das médias indicam que realmente parece ter existido uma redução brusca nas emissões de CO2 em 2016

# plota os países que tiveram a maior queda entre 2015 e 2016
filter(co2, year >= 2014) %>%
  spread(year, co2, sep = "_") %>%
  mutate(dif = year_2016 - year_2015) %>%
  arrange(desc(dif)) %>%
  drop_na(dif) %>%
  tail(5) %>%
  select(-dif) %>%
  gather(key = "year", value = "co2", -ccode, -country) %>%
  mutate(year = as.numeric(str_remove(year, "year_"))) %>%
  ggplot(aes(x = year, y = co2, color = country)) +
  geom_line() +
  theme_minimal() +
  labs(title = " Cinco países que mais reduziram suas emissões entre 2015 e 2016",
       x = "ano",
       color = "")
# Como demostrado, a queda das emissões em 2016 parece ter sido puchada sobretudo pelos EUA e pela Índia


# Gráfico setorial das emissões geradas por esses países em relação ao total gerado entre 2010-2018
top_total_emissions <- co2 %>%
  filter(year >= 2010) %>%
  group_by(country) %>%
  summarise(co2_total = sum(co2, na.rm = TRUE)) %>%
  arrange(desc(co2_total)) %>%
  head(5)
co2 %>%
  mutate(country = if_else(country %in% top_total_emissions$country, country, "Other countries")) %>%
  filter(year >= 2010) %>%
  group_by(country) %>%
  summarize(co2 = sum(co2, na.rm = TRUE)) %>%
  mutate(country = fct_reorder(country, co2),
         co2_percent = co2 / sum(co2) * 100) %>%
  ggplot(aes(x = "", y = co2, fill = country)) +
  geom_bar(width = 1, stat = "identity") +
  geom_text(aes(label = sprintf("%.2f%%", co2_percent)), 
            position = position_stack(vjust = 0.5)) +
  theme_void() +
  labs(title = "Emissões geradas pelos países líderes no ranking no período (% relação ao total)",
       subtitle = "2010-2018",
       fill = "")
# Entre 2010 e 2018, China, EUA e India foram responsáveis por, 
# aproximadamente, metade das emissões de CO2 no mundo.


# mostra a evolução temporal das emissões desses Estados
ggplot(data = filter(co2, country %in% top_total_emissions$country),
       aes(x = year, y = co2, color = country)) +
  geom_line() +
  theme_minimal() +
  labs(title = "Evolução das emissões de CO2 dos 5 maiores emissores entre 2010 e 2018",
       x = "Ano",
       y = "Emissões de CO2 (kt)",
       color = "")
# EUA e Japão taxas estáveis no período;
# A Rússia ten taxas relativamente estáveis, com uma queda percepítivel ao fim da URSS;
# China e India apresentam aumentos exponenciais a partir de meados da década de 90,
# com uma queda perceptível em 2016, seguida de aparente retomada no crescimento.


# Mapa com a média de emissões, por país, no período entre 2010 e 2018

# calcula as médias para o período
co2_mean <- co2 %>%
  filter(year >= 2010) %>%
  group_by(country) %>% 
  summarise(co2_mean = mean(co2, na.rm = TRUE)) %>%
  drop_na()

# importa os dados geográficos do pacote `maps`
map <- map_data("world")
  
# verifica a correspondência dos dados em map e co2_mean
full_join(map, co2_mean, by = c("region" = "country")) %>%
  filter(is.na(group)) %>%
  select(region) %>%
  as.list() %>%
  print()

# padroniza o banco map para garantir correspondência com os dados importados do pacote "maps"
map <- map %>%
  # altera a grafia do nome dos países
  mutate(
    region = recode(region,
                    #não há dados para Gibraltar
                    "Brunei" = "Brunei Darussalam",
                    "Republic of Congo" = "Congo",
                    "Democratic Republic of the Congo" = "Congo_the Democratic Republic of the",
                    "Ivory Coast" = "Cote d'Ivoire",
                    "Iran" = "Iran, Islamic Republic of",
                    "North Korea" = "Korea, Democratic People's Republic of",
                    "South Korea" = "Korea, Republic of",
                    "Laos" = "Lao People's Democratic Republic",
                    "Libya" = "Libyan Arab Jamahiriya",
                    "North Macedonia" = "Macedonia, the former Yugoslav Republic of",
                    "Micronesia" = "Micronesia, Federated States of",
                    "Moldova" = "Moldova, Republic of",
                    "Russia" = "Russian Federation",
                    "Syria" = "Syrian Arab Republic",
                    "Tanzania" = "Tanzania_United Republic of",
                    "UK" = "United Kingdom",
                    "USA" = "United States",
                    "Vietnam" = "Viet Nam",
                    "Virgin Islands" = "Virgin Islands_British")) %>%
  # une Estados insulares com mais de uma ilha:
  mutate(
    region = recode(region,
                    "Antigua" = "Antigua and Barbuda",
                    "Barbuda" = "Antigua and Barbuda",
                    "Saint Kitts" = "Saint Kitts and Nevis",
                    "Nevis" = "Saint Kitts and Nevis",
                    "Saint Vincent" = "Saint Vincent and the Grenadines",
                    "Grenadines" = "Saint Vincent and the Grenadines",
                    "Trinidad" = "Trinidad and Tobago",
                    "Tobago" = "Trinidad and Tobago")) %>%
  # altera o nome dos territórios chinês de Hong Kong e Macao, com base na província
  mutate(
    region = case_when(
      subregion == "Hong Kong" ~ "Hong Kong",
      subregion == "Macao" ~ "Macao",
      TRUE ~ region
    )
  )

# define o banco co2_map, que será empregado na geração do mapa
co2_map <- left_join(map, co2_mean, by = c("region" = "country")) %>%
  group_by(region)
# Cria a escala de cores que será utilizada
color_scale <- scale_fill_gradient(low = "blue", high = "red")
# Cria o mapa
ggplot(co2_map) +
  geom_map(aes(map_id = region, fill = co2_mean), map = co2_map, color = "white") +
  expand_limits(x = co2_map$long, y = co2_map$lat) +
  color_scale +
  theme_void() +
  labs(title = "Médias de emissões anuais de CO2 (kt)",
       subtitle = "2010-2018",
       fill = "CO2 (kt)")


# Por fim, têm-se a evolução temporal das principais métricas da emissão de CO2
co2_summary <- co2 %>%
  mutate(decade = as.numeric(paste0(substr(year, 1, 3), "0"))) %>%
  # cria a variável década, selecionando os três primeiros números do ano e adicionando um 0 ao final
  group_by(decade) %>% #agrupa os dados por década, em seguida calcula as métricas de interesse
  summarise(mean_co2 = mean(co2, na.rm = TRUE),
            median_co2 = median(co2, na.rm = TRUE),
            sd_co2 = sd(co2, na.rm = TRUE)) %>%
  print()
# A média e a mediana aumentaram a cada década, indicando um aumento geral das emissões de CO2 no período.
# O desvio padrão também aumentou, o que sugere um aumento na variabilidade das emissões entre os países.


# POPULAÇÃO

# Evolução da população mundial ao longo dos anos
pop %>% 
  group_by(year) %>%
  summarise(pop_total = sum(pop, na.rm = TRUE)) %>%
  ggplot(aes(x = year, y = pop_total)) +
  geom_line() +
  theme_minimal() +
  labs(title = "Evolução da população mundial ao longo dos anos",
       subtitle = "1960-2021",
       x = "Ano",
       y = "População")

# Evolução da população mundial, divida por regiões
pop %>%
  left_join(metadata, by = "ccode") %>%
  group_by(region, year) %>% #agrupa os dados por região
  summarise(pop_total = sum(pop, na.rm = TRUE)) %>%
  ungroup() %>%
  ggplot(aes(x = year, y = pop_total, color = region)) +
  geom_line() +
  theme_minimal() +
  labs(title = "Evolução da população mundial, divida por regiões",
       subtitle = "1960-2021",
       x = "Ano",
       y = "População",
       color = "")
# Embora as taxas de crescimento variem de região para região, em todos os casos elas são estáveis e ascendentes.

# evolução temporal das principais métricas da emissão de pop
pop_summary <- pop %>%
  mutate(decade = as.numeric(paste0(substr(year, 1, 3), "0"))) %>%
  # cria a variável década, selecionando os três primeiros números do ano e adicionando um 0 ao final
  group_by(decade) %>% #agrupa os dados por década, em seguida calcula as métricas de interesse
  summarise(mean_pop = mean(pop, na.rm = TRUE),
            median_pop = median(pop, na.rm = TRUE),
            sd_pop = sd(pop, na.rm = TRUE)) %>%
  print()
# Houve um aumento significativo na população mundial ao longo das décadas.
# O desvio padrão também cresceu bastante, o que indica que a população mundial está cada vez mais heterogênea.
# É importante lembrar que essas medidas estão sujeitas a outliers e a variações que podem não ser 
# representativas da população mundial como um todo, mas dão uma ideia geral de sua evolução.

# Correlação entre as taxas de emissão de CO2 e a população
co2_pop <- inner_join(co2, pop, by = c("ccode", "country", "year"))
cor_co2_pop <- cor(co2_pop$co2, co2_pop$pop, use = "complete.obs")
r2_co2_pop <- cor_co2_pop^2

ggplot(co2_pop, aes(x = co2, y = pop)) +
  geom_point(color = "grey") +
  scale_x_log10() +
  scale_y_log10() +
  theme_minimal() +
  geom_smooth(method="lm", col="darkred", linetype="dashed") +
  geom_text(x = Inf, y = Inf, label = paste0("R² = ", round(r2_co2_pop, 2)),
            hjust = 1, vjust = 1, size = 4, col="darkred") +
  labs(title = "Correlação entre população e emissões de CO2",
       subtitle = "1970-2018",
       x = "População (log)",
       y = "Emissões de CO2 (log)")
# Uma correlação de 0,72 indica uma relação forte e positiva entre as duas variáveis.
# Já o R² de 0,521 indica que 52,1% da variação nas emissões
# de CO2 podem ser explicadas pela variação na população.
# Apesar da relação não ser perfeita, ela é forte.


#PIB

# Evolução do PIB mundial ao longo dos anos
pib %>% 
  group_by(year) %>%
  summarise(pib_total = sum(pib, na.rm = TRUE)) %>%
  ggplot(aes(x = year, y = pib_total)) +
  geom_line() +
  theme_minimal() +
  labs(title = "Evolução do PIB mundial ao longo dos anos",
       subtitle = "1960-2021",
       x = "Ano",
       y = "PIB")

# Evolução do PIB mundial, divido por regiões
pib %>%
  left_join(metadata, by = "ccode") %>%
  group_by(region, year) %>% #agrupa os dados por região
  summarise(pib_total = sum(pib, na.rm = TRUE)) %>%
  ungroup() %>%
  ggplot(aes(x = year, y = pib_total, color = region)) +
  geom_line() +
  theme_minimal() +
  labs(title = "Evolução do PIB mundial, divido por regiões",
       subtitle = "1960-2021",
       x = "Ano",
       y = "PIB",
       color = "Região")
# Embora o PIB apresente taxas de crescimento mais instáveis que a população,
# em todos os casos ele ainda apresenta uma curva ascendente.

# evolução temporal das principais métricas de crescimento do PIB
pib_summary <- pib %>%
  mutate(decade = as.numeric(paste0(substr(year, 1, 3), "0"))) %>%
  group_by(decade) %>%
  summarise(mean_pib = mean(pib, na.rm = TRUE),
            median_pib = median(pib, na.rm = TRUE),
            sd_pib = sd(pib, na.rm = TRUE)) %>%
  print()

# Correlação entre as taxas de emissão de CO2 e o PIB
co2_pib <- inner_join(co2, pib, by = c("ccode", "country", "year"))
cor_co2_pib <- cor(co2_pib$co2, co2_pib$pib, use = "complete.obs")
r2_co2_pib <- cor_co2_pib^2

ggplot(co2_pib, aes(x = co2, y = pib)) +
  geom_point(color = "grey") +
  scale_x_log10() +
  scale_y_log10() +
  theme_minimal() +
  geom_smooth(method="lm", col="darkred", linetype="dashed") +
  geom_text(x = Inf, y = Inf, label = paste0("R² = ", round(r2_co2_pib, 2)),
            hjust = 1, vjust = 1, size = 4, col="darkred") +
  labs(title = "Correlação entre PIB e emissões de CO2",
       subtitle = "1970-2018",
       x = "PIB (log)",
       y = "Emissões de CO2 (log)")
# De modo semelhante ao notado na população, 
# a correlação de 0,76 indica uma relação forte e positiva entre as duas variáveis.
# Já o R² de 0,579 indica que 57,9% da variação nas emissões de CO2 
# podem ser explicadas pela variação no PIB.
# Novamente, apesar da relação não ser perfeita, ela é forte.

# Correlação entre a população e o PIB
pop_pib <- inner_join(pop, pib, by = c("ccode", "country", "year"))
cor_pop_pib <- cor(pop_pib$pop, pop_pib$pib, use = "complete.obs")
r2_pop_pib <- cor_pop_pib^2

ggplot(pop_pib, aes(x = pop, y = pib)) +
  geom_point(color = "grey") +
  scale_x_log10() +
  scale_y_log10() +
  theme_minimal() +
  geom_smooth(method="lm", col="darkred", linetype="dashed") +
  geom_text(x = Inf, y = Inf, label = paste0("R² = ", round(r2_pop_pib, 2)),
            hjust = 1, vjust = 1, size = 4, col="darkred") +
  labs(title = "Correlação entre a população e o tamanho do PIB",
       subtitle = "1970-2018",
       x = "PIB (log)",
       y = "População (log)")
# por outro lado, a correlação de 0,38 e o R² de 0,146 entre população e PIB
# apontam uma relação menos significativa entre as duas variáveis.
# Isso sugere que elas podem estar contribuindo com informações complementares sobre as emissões de CO2.


# ANÁLISE DOS MODELOS DE PREVISÃO PARA AS EMISSÕES DE CO2 FUTURAS ---------------------

# Serão testados os seguintes modelos:
  # Modelo de regressão linear simples (RLS);
  # Modelo de regressão linear múltipla (RLM);
  # Modelo aditivo generalizado (GAM);
  # Modelo auto-regressivo integrado de médias móveis (ARIMA); e
  # Modelo auto-regressivo integrado de médias móveis com entradas de variáveis exógenas (ARIMAX).


# Separa os dados de treino e teste
dados_treino <- dados %>% filter(year <= 2015)
dados_teste <- dados %>% filter(year > 2015)


# Modelo de regressão linear simples

# Constrói o modelo de regressão linear simples
modelo_lms <- lm(co2 ~ year, data = dados_treino)
summary(modelo_lms)

# Previsões com o modelo de regressão linear simples
previsoes_lms <- predict(modelo_lms, newdata = dados_teste)

# Para avaliar o modelo, se usa os dados de teste para calculamr a Raiz do Erro Médio Quadrático (RMSE),
rmse_lms <- sqrt(mean((previsoes_lms - dados_teste$co2)^2))
# o Erro Absoluto Médio (MAE),
mae_lms <- mean(abs(previsoes_lms - dados_teste$co2))
# o Critério de Informação de Akaike (AIC),
aic_lms <- AIC(modelo_lms)
# e o Critério de Informação Bayesiano (BIC)
bic_lms <- BIC(modelo_lms)


# Modelo de regressão linear múltipla

# Constrói o modelo de regressão linear múltipla
modelo_lm <- lm(co2 ~ pop + pib, data = dados_treino)
summary(modelo_lm)

# Previsões com o modelo de regressão linear múltipla
previsoes_lm <- predict(modelo_lm, newdata = dados_teste)

# Avaliação do modelo
rmse_lm <- sqrt(mean((previsoes_lm - dados_teste$co2)^2))
mae_lm <- mean(abs(previsoes_lm - dados_teste$co2))
aic_lm <- AIC(modelo_lm)
bic_lm <- BIC(modelo_lm)


# Modelo GAM

# Embora a relação entre as variáveis pareça ser linear, vale a pena testar o modelo GAM

# Constrói variações do modelo GAM
modelo_gam1 <- gam(co2 ~ s(pop) + pib, data = dados_treino) # supõe co2~pop como relação não-linear 
summary(modelo_gam1)

modelo_gam2 <- gam(co2 ~ pop + s(pib), data = dados_treino) # supõe co2~pib como relação não-linear 
summary(modelo_gam2)

modelo_gam3 <- gam(co2 ~ s(pop) + pib, data = dados_treino) # supõe co2~pop e co2~pib como não-linear 
summary(modelo_gam3)

# Previsões com os modelos GAM
previsoes_gam1 <- predict(modelo_gam1, newdata = dados_teste)
previsoes_gam2 <- predict(modelo_gam2, newdata = dados_teste)
previsoes_gam3 <- predict(modelo_gam3, newdata = dados_teste)

# Avaliação dos modelos GAM
rmse_gam1 <- sqrt(mean((previsoes_lm - dados_teste$co2)^2))
rmse_gam2 <- sqrt(mean((previsoes_lm - dados_teste$co2)^2))
rmse_gam3 <- sqrt(mean((previsoes_lm - dados_teste$co2)^2))

mae_gam1 <- mean(abs(previsoes_gam1 - dados_teste$co2))
mae_gam2 <- mean(abs(previsoes_gam2 - dados_teste$co2))
mae_gam3 <- mean(abs(previsoes_gam3 - dados_teste$co2))

aic_gam1 <- AIC(modelo_gam1)
aic_gam2 <- AIC(modelo_gam2)
aic_gam3 <- AIC(modelo_gam3)

bic_gam1 <- BIC(modelo_gam1)
bic_gam2 <- BIC(modelo_gam2)
bic_gam3 <- BIC(modelo_gam3)


# Modelo ARIMA

# constrói o modelo ARIMA
modelo_arima <- auto.arima(dados_treino$co2)
summary(modelo_arima)

# Previsões com o modelo ARIMA
previsoes_arima <- forecast(modelo_arima, h = nrow(dados_teste))$mean
# a função forecast retorna uma lista com múltiplas informações
# o item mean é onde consta as previsões

# Avaliação do modelo
rmse_arima <- sqrt(mean((previsoes_arima - dados_teste$co2)^2))
mae_arima <- mean(abs(previsoes_arima - dados_teste$co2))
aic_arima <- AIC(modelo_arima)
bic_arima <- BIC(modelo_arima)


# Modelo ARIMAX

# constrói o modelo ARIMAX
modelo_arimax <- auto.arima(dados_treino$co2,
                            xreg = as.matrix(dados_treino[, c("pop", "pib")]))
summary(modelo_arimax)

# Previsões com o modelo ARIMA
previsoes_arimax <- forecast(modelo_arimax, h = nrow(dados_teste), 
                             xreg = as.matrix(dados_treino[, c("pop", "pib")]))$mean

# Avaliação do modelo
rmse_arimax <- sqrt(mean((previsoes_arimax - dados_teste$co2)^2))
mae_arimax <- mean(abs(previsoes_arimax - dados_teste$co2))
aic_arimax <- AIC(modelo_arimax)
bic_arimax <- BIC(modelo_arimax)


# cria um data frame com os resultados dos testes de avaliação aplicados em cada modelo
resultados <- data.frame()

# preenche as linhas do dataframe com os valores calculados para cada modelo
resultados <- rbind(resultados, c("RLS", rmse_lms, mae_lms, AIC(modelo_lms), BIC(modelo_lms)))
resultados <- rbind(resultados, c("RLM", rmse_lm, mae_lm, AIC(modelo_lm), BIC(modelo_lm)))
resultados <- rbind(resultados, c("GAM_1", rmse_gam1, mae_gam1, AIC(modelo_gam1), BIC(modelo_gam1)))
resultados <- rbind(resultados, c("GAM_2", rmse_gam2, mae_gam2, AIC(modelo_gam2), BIC(modelo_gam2)))
resultados <- rbind(resultados, c("GAM_3", rmse_gam3, mae_gam3, AIC(modelo_gam3), BIC(modelo_gam3)))
resultados <- rbind(resultados, c("ARIMA", rmse_arima, mae_arima, AIC(modelo_arima), BIC(modelo_arima)))
resultados <- rbind(resultados, c("ARIMAX", rmse_arimax, mae_arimax, AIC(modelo_arimax), BIC(modelo_arimax)))

# renomeia as colunas e exibe o dataframe com os resultados
colnames(resultados) <- c("Modelo", "RMSE", "MAE", "AIC", "BIC")
resultados


# Comparação entre os modelo - rmse
cores <- c("blue", "green", "red", "purple", "orange", "yellow", "brown")

rmse <- c(rmse_lms,
          rmse_lm,
          rmse_gam1,
          rmse_gam2,
          rmse_gam3,
          rmse_arima,
          rmse_arimax)
modelos <- c("RLS", "RLM", "GAM_1", "GAM_2", "GAM_3", "ARIMA", "ARIMAX")
barplot(rmse, names.arg = modelos, col = cores,
        main = "Comparação entre os modelos - Raiz do Erro Médio Quadrático (RMSE)",
        xlab = "Modelo", ylab = "RMSE")

mae <- c(mae_lms,
         mae_lm,
         mae_gam1,
         mae_gam2,
         mae_gam3,
         mae_arima,
         mae_arimax)
modelos <- c("RLS", "RLM", "GAM_1", "GAM_2", "GAM_3", "ARIMA", "ARIMAX")
barplot(mae, names.arg = modelos, col = cores,
        main = "Comparação entre os modelos - Erro Absoluto Médio (MAE)",
        xlab = "Modelo", ylab = "MAE")

aic <- c(aic_lms,
         aic_lm,
         aic_gam1,
         aic_gam2,
         aic_gam3,
         aic_arima,
         aic_arimax)
modelos <- c("RLS", "RLM", "GAM_1", "GAM_2", "GAM_3", "ARIMA", "ARIMAX")
barplot(aic, names.arg = modelos, col = cores,
        main = "Comparação entre os modelos - Critério de Informação de Akaike (AIC)",
        xlab = "Modelo", ylab = "AIC")

bic <- c(bic_lms,
         bic_lm,
         bic_gam1,
         bic_gam2,
         bic_gam3,
         bic_arima,
         bic_arimax)
modelos <- c("RLS", "RLM", "GAM_1", "GAM_2", "GAM_3", "ARIMA", "ARIMAX")
barplot(bic, names.arg = modelos, col = cores,
        main = "Comparação entre os modelos - Critério de Informação Bayesiano (BIC)",
        xlab = "Modelo", ylab = "BIC")


# CONCLUSÕES ---------------------------------------------------------------

# O RLS apresentou RMSE elevado, indicando baixa acurácia na previsão das emissões de CO2.

# O RLM Apresentou menor RMSE e MAE em relação ao RLS, 
# sugerindo que a inclusão das variáveis explicativas pib e pop melhorou o modelo.

# Os três modelos GAM testados apresentaram desempenho semelhante ao RLM, 
# indicando que o modelo linear múltiplo já é suficiente.

# O ARIMA apresentou RMSE elevado, sugerindo baixa acurácia na previsão das emissões de CO2.

# O ARIMAX Apresentou o maior RMSE e MAE entre todos os modelos, 
# indicando que a inclusão de pib e pop não melhorou a previsão em relação ao modelo ARIMA simples.

# Concluí-se que, entre os modelos testados, 
# o que apresentou os melhores resultados foi o modelo de regressão linear múltipla.

