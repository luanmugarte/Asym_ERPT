# Resultados das estimações

[TOC]

## Comentários gerais

Talvez tentar incluir taxa de juros como uma variável de controle para a dinâmica entre inflação e taxa de câmbio.

## Opções

### Lag Switching Variable 

Nas estimações com *comm* exógena, **não ter LSV** parece ter tido resultados melhores (mais adequado à literatura). Isso também pareceu acontecer com as estimações de frequência trimestral, embora essas estimações estivessem um tanto estranhas.

Nas estimações com *comm* endógena, **não ter LSV** parece ter tido resultados melhores também. Isso também pareceu acontecer com as estimações de frequência trimestral, embora essas estimações também estivessem um tanto estranhas.

Escolha final: **não ter LSV**.

### Efeito contemporâneo e lags das variáveis exógenas

Estimações diferenciando entre o efeito contemporâneo da variável exógena *comm* apresentou diferenças quase impercetíveis. Da mesma forma, o aumento no número de lags das variáveis exógenas fez bem pouca diferença.

Escolha final: **Efeito contemporâneo presente e 1 lag das variáveis exógenas**.

### Gamma (sensibilidade da função de transição)

Gamma = 3 gera menos significância das variáveis, especialmente nos primeiros períodos. Valores do RC andam mais perto da literatura (RC de desvalorização maior, RC de valorização menor), principalmente quando *comm* é exógena. Trade off entre significância das variáveis vs. rapidez de troca de regimes.

Gamma maior que 5 passa a gerar significância das IRFs de cambio no regime de apreciação mais cedo. No entanto, com CI = 95, isso desaparece e a significancia de regime de apreciação para gamma < 5 inexiste (para DA: capacidade. Para DA: pimpf, isso ainda é valido).

### Tendência

A inclusão também gerou pouca diferença. É mais apropriado não incluir.

Escolha final: **sem tendência**

### Trim vs Mensal

#### Trimestrais

Comm exógena gerou resultados muito estranhos. Com ela endógena, os resultados foram um pouco melhores, mas as IRFs ainda apresentavam comportamentos estranhos (valores negativos de RC). As IRFs com resposta do IPCA apresentaram significância relativamente reduzida. Significância do Regime de apreciação somente ocorre perto do 7 período.

#### Mensais

Resultados melhores, porém ainda dificultosos de interpretar.

Escolha final: **mensal**

### Lags das variáveis endógenas

De acordo com VARselect, o BIC indica 1 lag para todas as especificações. No caso de AIC ou FPE, o lag indicado foi de 4 para especificação com DA:capacidade e *comm* exog (3 para especificação com DA:pimpf). No caso de *comm* exog, AIC e FPE continuaram indicando 1 lag.

#### 1 lag

Comportamentos das IRFs menos volatéis, *resultando em IRFs do IPCA melhores no CP*. *Piores IRFs de desemprego.* Para a DA de PIB, as IRFS de desemprego não são mt bons e também gera pouca significância nos regimes.

#### 2 lags

Para os modelos mensais, também gera mais significância no curto prazo para as IRFs de cambio no regime de depreciação. *Melhores IRFs de desemprego*. No entanto, torna o comportamento das IRFs mais oscilante. Para a DA de PIB, gerou os melhores resultados.

#### 3 lags

*Gerou melhores resultados para as IRFs de câmbio.* Passam a ter um período maior de significancia no CP (sem descontinuidades) a partir de 3 lags. No entanto, o IPCA passa a ter períodos de significância intercalados. Assimetria de magnitude também se torna maior.

#### 4 lags

Semelhante ao 2 lags em termos de resultados

Escolha final: **1 a 3 lags**

### Comm endógena vs. exógena

Endógena parece ter resultados mais adequados (necessário uma avaliação mais minunciosa das outras IRFs)

1. IRFs de câmbio tem uma temporalidade mais adequada (significância no regime de apreciação após vários meses, invés de no CP)
   1. No entanto, no regime de apreciação, pode começar em um nível negativo (n significativo) após choque inicial.

#### Petro vs comm

##### Endógenas

Para DA: capacidade, os resultados com *comm* pareceram melhor. Para DA: pimpf, os resultados pareceram melhor. Petro parece aumentar a magnitude do RC.

##### Exógenas

Resultados bem semelhantes  (*e piores quando exógenas*) para as duas variáveis de *infl_ext* e de DA.

Escolha final: inflação externa endógena e variável de commodities do IMF.

### Capacidade vs. pimpf vs pib vs hiato do pib

#### Capacidade e pimpf

Ambos não parecem ser boas proxys para a DA. Capacidade parece produzir resultados melhores em lags maiores, enquanto para 1 lag pimpf gera resultados mais adequados.

#### PIB

Parece ser a melhor variável de DA. No entanto, RC fica alto em ambos os regimes. Melhores em lags maiores.

Melhores resultados com configurações:

1. Lags menores (1-2)
2. Gamma menores também (menor significância de regime de apreciação)

#### Hiato do PIB

Boa variável de DA, mas suscetível à crítica (heterodoxa) pelo uso do hiato. Também gera IRFs de desemprego estranhas com lag = 1. Com petro, gera resultados melhores.

**Escolha final**: PIB

### Dummy da GFC

Pode alterar a significância, mas faz pouca diferença em termos de magnitude.

### Desemprego - nível vs. variação

Curiosamente, gerou IRFs relativamente semelhantes para lag_endog > 2. Para lag_endog = 1, gera IRFs bem comportadas. No entanto, variação tende a ir contra a teoria.

Escolha final: nível

## Resultados interessantes

### DA: capacidade, Comm: endo, lag: 3, gamma: 4, IC: 90

Bons resultados das IRFs de cambio. No entanto, IPCA apresenta momentos de valores negativos.

**Obs:** com 1 lag também pode ser interessante. Apesar das IRFs de cambio serem um pouco piores, a IRF do IPCA fica correta.

**Obs 2:** Mesma com DA: pimpf.

### DA: capacidade, Comm: endo, lag: 3, gamma: 2, IC: 90

Bons resultados das IRFs de cambio. No entanto, IPCA apresenta momentos de valores negativos.

**Obs:** com 1 lag também pode ser interessante. Apesar das IRFs de cambio serem um pouco piores, a IRF do IPCA fica correta.

### MENSAL_exo[notrend]_endo[comm_capacidade(2)]_gamma[4]

## Ótimos resultados

### MENSAL_exo[notrend]_endo[comm_pib(2)]_gamma[3]

Outras configurações:

1. Lag_exog: 1
2. Sig_IC: 95
3. LAG_SW: True
4. GFC_dummy: True ou False
