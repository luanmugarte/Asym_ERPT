# Resultados das estimações

[TOC]

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

#### 1 lag

Comportamentos das IRFs menos volatéis, *resultando em IRFs do IPCA melhores no CP*. *Piores IRFs de desemprego.*

#### 2 lags

Para os modelos mensais, também gera mais significância no curto prazo para as IRFs de cambio no regime de depreciação. *Melhores IRFs de desemprego*. No entanto, torna o comportamento das IRFs mais oscilante. 

#### 3 lags

*Gerou melhores resultados para as IRFs de câmbio.*

#### 4 lags

Semelhante ao 2 lags em termos de resultados

Escolha final: **1 ou 3 lags**

### Comm endógena vs. exógena

Endógena parece ter resultados mais adequados (necessário uma avaliação mais minunciosa das outras IRFs)

1. IRFs de câmbio tem uma temporalidade mais adequada (significância no regime de apreciação após vários meses, invés de no CP)
   1. No entanto, no regime de apreciação, pode começar em um nível negativo (n significativo) após choque inicial.

### Capacidade vs. pimpf

Capacidade parece produzir resultados melhores em lags maiores, enquanto para 1 lag pimpf gera resultados mais adequados.

### Dummy da GFC

Pode alterar a significância, mas faz pouca diferença.

## Resultados interessantes

### DA: capacidade, Comm: endo, lag: 3, gamma: 4, IC: 90

Bons resultados das IRFs de cambio. No entanto, IPCA apresenta momentos de valores negativos.

**Obs:** com 1 lag também pode ser interessante. Apesar das IRFs de cambio serem um pouco piores, a IRF do IPCA fica correta.

**Obs 2:** Mesma com DA: pimpf.

### DA: capacidade, Comm: endo, lag: 3, gamma: 2, IC: 90

Bons resultados das IRFs de cambio. No entanto, IPCA apresenta momentos de valores negativos.

**Obs:** com 1 lag também pode ser interessante. Apesar das IRFs de cambio serem um pouco piores, a IRF do IPCA fica correta.

