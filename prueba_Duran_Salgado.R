# Prueba técnica IVS
# Postulante: 
# Andrés Durán S. MAE
# duransalgado.andres@gmail.com

#  Seteo inicial  --------------------------------------------------------------
rm(list = ls())
memory.size(max = T)

# Nombre bases de datos
var1 <- "Variables.csv"
var2 <- "Variables_2.csv"

# Librerías 
library(tidyverse)
library(ggplot2)
library(readxl)

# Pregunta 3 -------------------------------------------------------------------
# 1. Carge ambas bases
var1 <- read_delim(paste0("inputs/",var1), 
                   ";", escape_double = FALSE
                   , trim_ws = TRUE)
var2 <- read_delim(paste0("inputs/",var2), 
                   ";", escape_double = FALSE
                   , trim_ws = TRUE)

# 2.	Calcule el RO/HO y el C/HT (en ambas bases) como la división 
# entre RO y HO y de C y HT, respectivamente
var1 <- var1 %>%
  mutate(roho = RO/HO
         ,cht = C/HT)

var2 <- var2 %>%
  mutate(roho = RO/HO
         ,cht = C/HT)

# 3.	Integre ambas bases en una nueva 
# (Pista opcional: cree una cadena de Rol, Sexo y Grupo para realizar una integración efectiva), 
# dejando solo las variables Rol, Sexo, Grupo, Categoría, Tamaño, División, RO/HO y C/HT (periodo actual)
# RO/HO y C/HT (periodo anterior) y la Cadena calculada anteriormente (con los nombres originales).

# Creando cadena variable 1
var1 <- var1 %>%
  unite(cadena, ID_Empresa, Sexo, Grupo, sep = "", remove = F)
# Revisando cuántas cadenas existen
var1 %>% summarise(n = n_distinct(cadena)) %>% print()

# Creando cadena variable 2
var2 <- var2 %>%
  unite(cadena, ID_Empresa, Sexo, Grupo, sep = "", remove = F)
# Revisando cuántas cadenas existen
var2 %>% summarise(n = n_distinct(cadena)) %>% print()

# Integrando bases de datos, además, ordenando por cadena, mes y a?o
var <- rbind(var1, var2) %>% 
  arrange(cadena, Mes, Ano) %>%
  select(-c(RO_t_1,HT_t_1, HO_t_1, C_t_1))

# Revisando cuántas cadenas existen
var %>% summarise(n = n_distinct(cadena))
rm(var1, var2)

# 4. Calcule la variación porcentual del RO/HO y del C/HT entre ambos periodos.
var <- var %>%
  mutate(Periodo = as.Date(paste0(1,"/",Mes,"/",Ano), '%d/%m/%Y')
         ,roho_1 = lag(roho, 1)
         ,cht_1    = lag(cht, 1)
         ,Ano = as.numeric(format(Periodo, '%Y'))
         ,Mes  = as.numeric(format(Periodo, '%m'))
         ,roho_1 = ifelse(Mes == 8, NA, roho_1)
         ,cht_1 = ifelse(Mes == 8, NA, cht_1)
         ,var_roho = 100 * ((roho - roho_1)/roho_1)
         ,var_cht = 100 * ((cht - cht_1)/cht_1)
         ) %>%
  select(-c(Periodo, roho_1, cht_1))

# 5. Realice un gráfico de cajas con la distribución de las variaciones
# segun grupo ocupacional o segun tamaño

# Por Tamaño
roho_tam <- boxplot(var_roho ~ Tamano, var ,horizontal = T)
cht_tam  <- boxplot(var_cht ~ Tamano, var ,horizontal = T)

# Por Grupo
roho_grupo <- boxplot(var_roho ~ Grupo, var ,horizontal = T)
cht_grupo  <- boxplot(var_cht ~ Grupo, var ,horizontal = T)

# 6. Cree ua nueva var que posean una variación de RO/HO y C/HT de más de 50%

# En términos absolutos, es decir, las que sean mayores que +50% o mayor a -50%
var_new <- var %>%
  mutate(var_roho_abs = abs(var_roho)
         ,var_cht_abs = abs(var_cht)) %>%
  filter(var_roho_abs > 50 | var_cht_abs > 50) %>%
  select(-c(var_roho_abs, var_cht_abs))




