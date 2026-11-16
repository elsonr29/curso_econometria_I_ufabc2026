# Aula 02 - Regressão linear múltipla: análise e estimação
#
# Prof. Elson Rodrigo de Souza Santos
# E-mail: elson.rodrigo@ufabc.edu.br ou elson129@gmail.com
#
# Universidade Federal do ABC (UFABC)
# Bacharel em Ciências Econômicas
# Econometria I - 2026.3
# Disciplina: Econometria I - 2026.3
# Universidade Federal do ABC (UFABC)
#
###
# Descrição:
# Este script constitui o material de suporte para a Aula 02 de Econometria,
# sobre regressão linear múltipla. Utilizamos os dados do pacote wooldridge.
#
###
# 1. Pacotes: instalação
#
# Instação do pacote (se necessário)
# install.packages("wooldridge")
# install.packages("psych")

# Obs. instação de pacote por pacote para evitar travamentos
#
# 2. Dados
#
# Carregar pacote
library(wooldridge)
library(psych)

# Carregamento da base de dados de renda e educação
data("wage1")

# Visualização inicial da estrutura e das primeiras observações
str(wage1)
head(wage1)

# Seleção das variáveis de interesse da base wage1
dados <- wage1[, c("wage", "educ", "exper", "tenure")]

# Estatísticas descritivas básicas (mínimo, quartis, mediana, média e máximo)
summary(dados)
describe(dados)

# Cálculo de desvio-padrão e variância para complementar a análise
sapply(dados, sd)
sapply(dados, var)
cor(wage1[, c("educ", "exper", "tenure")])

# Nota: o pacote é baseado no manual de econometria básica de Wooldridge (2010),
# sendo muito útil para reproduzir os modelos apresentados no livro.

###
# 2. Modelo: renda vs educação
#
# Variável dependente: renda (wage)
# Variáveis independentes: educação (educ); experiência (exper); tenure 
# (tempo de empresa)
#
# A estimação ocorre passo a passo, adicionando novas variáveis
# Modelo I - educação (educ)
# Modelo II - Educação (educ) e experiẽncia (exper)
# Modelo III - Educação (educ), experiência (exper) e tenure (tempo de empresa)
# Modelo IV - experiência (exper) e tenure (tempo de empresa) # vies de especificação


#
# Modelo I - Modelo simples (apenas educação)
modelo_1 <- lm(wage ~ educ, data = wage1)
summary(modelo_1)

# Modelo II - Adicionando experiência profissional
modelo_2 <- lm(wage ~ educ + exper, data = wage1)
summary(modelo_2)

# Modelo III - Modelo completo (educação, experiência e tenure)
modelo_3 <- lm(wage ~ educ + exper + tenure, data = wage1)
summary(modelo_3)

# Modelo IV - Modelo com viés de especificação
modelo_vies <- lm(wage ~ exper + tenure, data = wage1)
summary(modelo_vies)

#
###
# 3. Diagnóstico de normalidade dos resíduos (modelo_3)
#
# Extração dos resíduos estimados (u_hat)
residuos_3 <- resid(modelo_3)

# 3.1 Gráfico de diagnósitco
graphics.off()
par(mfrow = c(2, 2), mar = c(3, 3, 2, 1), mgp = c(1.8, 0.6, 0), oma = c(0, 0, 0, 0))
plot(modelo_3, pch = 19)

# 3.2. Inspeção gráfica (Histograma com curva normal teórica e Q-Q Plot)
par(mfrow = c(1, 2)) # Divide a janela gráfica em 1 linha e 2 colunas

# Histograma dos resíduos com densidade
hist(residuos_3,
     probability = TRUE,
     main = "Histograma dos Resíduos",
     xlab = "Resíduos (u_hat)",
     ylab = "Densidade",
     col = "lightblue",
     border = "white")

# Sobreposição da curva normal teórica
curve(dnorm(x, mean = mean(residuos_3), sd = sd(residuos_3)),
      add = TRUE,
      col = "darkred",
      lwd = 2)

# Gráfico Quantil-Quantil (Q-Q Plot)
qqnorm(residuos_3,
       main = "Normal Q-Q Plot",
       xlab = "Quantis Teóricos",
       ylab = "Quantis Amostrais",
       pch = 19,
       col = rgb(0, 0, 0, 0.5))
qqline(residuos_3, col = "darkred", lwd = 2)

par(mfrow = c(1, 1)) # Retorna ao layout gráfico padrão

# 3.2. Teste formal de normalidade de Shapiro-Wilk
# H0: os resíduos seguem distribuição normal
# H1: os resíduos não seguem distribuição normal
shapiro.test(residuos_3)

###
# 4. Viés de especificação
#
# Consideramos o modelo IV que omite educação

# Extração dos resíduos estimados (u_hat)
residuos_vies <- resid(modelo_vies)

# 4.1. Gráfico de diagnóstico individual (modelo_vies)
par(mfrow = c(2, 2))
plot(modelo_vies, pch = 19)
par(mfrow = c(1, 1))

# 4.2. Comparação lado a lado: modelo_3 (esquerda) vs. modelo_vies (direita)
#
# Linha 1: Resíduos vs. Valores Ajustados (Linearidade / Viés de especificação)
par(mfrow = c(1, 2))
plot(modelo_3, which = 1, pch = 19, col = rgb(0, 0, 0, 0.4), main = "modelo_3 (Completo)")
plot(modelo_vies, which = 1, pch = 19, col = rgb(0, 0, 0, 0.4), main = "modelo_vies (Omitida)")

# Linha 2: Normal Q-Q Plot (Normalidade dos resíduos)
par(mfrow = c(1, 2))
plot(modelo_3, which = 2, pch = 19, col = rgb(0, 0, 0, 0.4), main = "modelo_3 (Completo)")
plot(modelo_vies, which = 2, pch = 19, col = rgb(0, 0, 0, 0.4), main = "modelo_vies (Omitida)")

# Linha 3: Scale-Location (Homocedasticidade)
par(mfrow = c(1, 2))
plot(modelo_3, which = 3, pch = 19, col = rgb(0, 0, 0, 0.4), main = "modelo_3 (Completo)")
plot(modelo_vies, which = 3, pch = 19, col = rgb(0, 0, 0, 0.4), main = "modelo_vies (Omitida)")

# Linha 4: Resíduos vs. Alavancagem (Pontos influentes / Distância de Cook)
par(mfrow = c(1, 2))
plot(modelo_3, which = 5, pch = 19, col = rgb(0, 0, 0, 0.4), main = "modelo_3 (Completo)")
plot(modelo_vies, which = 5, pch = 19, col = rgb(0, 0, 0, 0.4), main = "modelo_vies (Omitida)")

# Restaura o layout gráfico padrão
par(mfrow = c(1, 1))

# 5. Tabela de resultados

# Lista com os modelos estimados
modelos <- list(
  "Modelo I"   = modelo_1,
  "Modelo II"  = modelo_2,
  "Modelo III" = modelo_3,
  "Modelo IV"  = modelo_vies
)

# Definição das variáveis explicativas (linhas da tabela)
variaveis <- c("(Intercept)", "educ", "exper", "tenure")

# Função para extrair coeficiente + (erro-padrão) e asteriscos de significância
tabela_coef <- sapply(modelos, function(m) {
  s <- summary(m)$coefficients
  
  sapply(variaveis, function(v) {
    if (v %in% rownames(s)) {
      est <- s[v, "Estimate"]
      ep  <- s[v, "Std. Error"]
      pv  <- s[v, "Pr(>|t|)"]
      
      # Classificação padrão de significância estatística
      sig <- ifelse(pv < 0.01, "***",
                    ifelse(pv < 0.05, "**",
                           ifelse(pv < 0.10, "*", "")))
      
      sprintf("%.4f%s (%.4f)", est, sig, ep)
    } else {
      "-"
    }
  })
})

# Extração das estatísticas de qualidade do ajuste
tabela_stats <- sapply(modelos, function(m) {
  s <- summary(m)
  c(
    "Observações (n)"   = as.character(nobs(m)),
    "R2"                = sprintf("%.4f", s$r.squared),
    "R2 Ajustado"       = sprintf("%.4f", s$adj.r.squared),
    "Erro-padrão Reg."  = sprintf("%.4f", s$sigma),
    "Estatística F"     = sprintf("%.2f", s$fstatistic[1])
  )
})

# Junção dos dois blocos e impressão alinhada no console
tabela_final <- rbind(tabela_coef, tabela_stats)
print(as.data.frame(tabela_final), right = TRUE)

###
# FIM