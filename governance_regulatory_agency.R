#PROJETO PARA CRIAR GRÁFICOS DA MILITARIZAÇAO DAS AGENCIAS REGULADORAS
# REVISTA GOVERNANCE

#importando o dataset criado por Ademar Guedes
library(tidyverse)
library(readxl)
adeb_999_agencias_militares <- read_excel("adeb_999_agencias_militares.xlsx")
View(adeb_999_agencias_militares)

#criando o arquivo de análise
ar <- adeb_999_agencias_militares

#criar um gráfico harmônico com o atual 1 e 2
# incluir "crescimento de militares no governo" que está no gráfico 1 do apêndice
# e as demais variáveis da figura 1 do texto

glimpse(ar)


# Verificar nomes das colunas relevantes
# Esperado (pela amostra do arquivo):
# ano_arq (coluna A), total_militares_governo (coluna C),
# total_militares_ars (coluna D), total_militares_ars_com_cargo (coluna E)

# 2) Guardar valores-base de 2013 (ano base)
base_vals <- ar %>%
    filter(ano_arq == 2013) %>%
    summarise(
        base_governo = sum(total_militares_governo, na.rm = TRUE),
        base_ar      = sum(total_militares_ars, na.rm = TRUE),
        base_ar_cargo= sum(total_militares_ars_com_cargo, na.rm = TRUE)
    )

# 3) Criar variáveis de crescimento percentual versus 2013
df_out <- ar %>%
    mutate(
        cres_militares_governo   = 100 * (total_militares_governo / base_vals$base_governo - 1),
        cres_militares_ar        = 100 * (total_militares_ars / base_vals$base_ar - 1),
        cres_militares_ar_cargo  = 100 * (total_militares_ars_com_cargo / base_vals$base_ar_cargo - 1)
    )


# Bases anuais (2013)
base_ano <- ar %>%
    filter(ano_arq == 2013) %>%
    summarise(
        base_governo = total_militares_governo,
        base_ar      = total_militares_ars,
        base_ar_cargo= total_militares_ars_com_cargo
    )

df_ano_out <- ar %>%
    mutate(
        cres_militares_governo   = 100 * (total_militares_governo / base_ano$base_governo - 1),
        cres_militares_ar        = 100 * (total_militares_ars / base_ano$base_ar - 1),
        cres_militares_ar_cargo  = 100 * (total_militares_ars_com_cargo / base_ano$base_ar_cargo - 1)
    )

# 5) Visualizar
head(df_out)
head(df_ano_out)

glimpse(df_ano_out)

#Agora vou criar o gráfico 1b
#crescimento anual de militares no governo, militares me AR e em cargos de AR

library(scales)


# Filtrar período e garantir ordenação
ar_plot <- df_ano_out %>%
    filter(ano_arq_tot >= 2013, ano_arq_tot <= 2024) %>%
    arrange(ano_arq_tot)

# Deixar dados em formato longo para facilitar o ggplot
ar_long <- ar_plot %>%
    select(ano_arq_tot,
           cres_militares_governo,
           cres_militares_ar,
           cres_militares_ar_cargo) %>%
    pivot_longer(
        cols = -ano_arq_tot,
        names_to = "serie",
        values_to = "crescimento"
    )

# Rótulos legíveis
rotulos <- c(
    "cres_militares_governo" = "Officers in all Federal Executive",
    "cres_militares_ar" = "Officers in All RAs",
    "cres_militares_ar_cargo" = "Officers as Appointees in All RAs"
)

# Gráfico
fig_1b <- ggplot(ar_long, aes(x = ano_arq_tot, y = crescimento, color = serie)) +
    geom_line(size = 1) +
    geom_point(size = 2) +
    scale_color_manual(values = c("#1b9e77", "#7570b3", "#d95f02"),
                       labels = rotulos) +
    scale_x_continuous(breaks = 2013:2024) +
    scale_y_continuous(labels = label_percent(accuracy = 1, scale = 1)) +
    labs(
        title = "Annual Trajectory of Military Officers in Government and RAs (Baseline: 2013, 2013–2024)",
        x = "Year",
        y = "Variation (%)",
        color = ""
    ) +
    theme_minimal(base_size = 10) +
    theme(
        panel.grid.minor = element_blank(),
        legend.position = "bottom"
    )

fig_1b


ggsave("figures/fig_1b.pdf", fig_1b,
       width = 8.5, height = 5, units = "in")


###AGORA CRIAR O GRÁFICO 1A (identico ao que já está no texto)
# Verificar colunas necessárias
cols_ok <- c("ano_arq",
             "total_militares_ars",
             "total_militares_ars_com_cargo",
             "militares_total_agencias_alto_escalao")

missing <- setdiff(cols_ok, names(ar))
if (length(missing) > 0) {
    stop(paste("Faltam colunas no data frame ar:", paste(missing, collapse = ", ")))
}

# Opcional: agregar por ano, se houver várias linhas por ano (ex.: mensais)
ar_ano <- ar %>%
    group_by(ano_arq) %>%
    summarise(
        total_militares_ars = sum(total_militares_ars, na.rm = TRUE),
        total_militares_ars_com_cargo = sum(total_militares_ars_com_cargo, na.rm = TRUE),
        militares_total_agencias_alto_escalao = sum(militares_total_agencias_alto_escalao, na.rm = TRUE),
        .groups = "drop"
    )

# Reestruturar para formato longo
ar_long <- ar %>%
    pivot_longer(
        cols = c(total_militares_ars,
                 total_militares_ars_com_cargo,
                 militares_total_agencias_alto_escalao),
        names_to = "serie",
        values_to = "valor"
    )

# Rótulos legíveis
rotulos <- c(
    "total_militares_ars" = "Total Officers in Regulatory Agencies",
    "total_militares_ars_com_cargo" = "Officers as Appointees in Regulatory Agencies",
    "militares_total_agencias_alto_escalao" = "Officers as Top-level Appointees in Regulatory Agencies"
)

# Gráfico
fig_1a <- ggplot(ar_long, aes(x = ano_arq, y = valor, color = serie)) +
    geom_line(size = 1) +
    geom_point(size = 2) +
    scale_color_manual(values = c("#1b9e77", "#d95f02", "#7570b3"),
                       labels = rotulos) +
    scale_x_continuous(breaks = sort(unique(ar_long$ano_arq))) +
    scale_y_continuous(labels = label_number(big.mark = ".", decimal.mark = ",")) +
    labs(
        title = "Total Officers in Regulatory Agencies (2013-2023)",
        x = "Year",
        y = "Amount",
        color = ""
    ) +
    theme_minimal(base_size = 12) +
    theme(
        panel.grid.minor = element_blank(),
        legend.position = "bottom"
    )


fig_1a

ggsave("figures/fig_1a.pdf", fig_1b,
       width = 6.5, height = 5, units = "in")


glimpse(ar_long)
glimpse (df_ano_out)

names(df_ano_out)

##CRIANDO AGORA A FIGURA 2B
##PARA TANTO, CRIAR ALGUMAS VARIAVEIS SOMANDO TOTAIS DAS 3AR E AS 8AR RESTANTES

cols_8 <- c(
    "militares_aneel","militares_anatel","militares_antaq","militares_antt",
    "militares_ancine","militares_ans","militares_anm","militares_ana"
)

cols_3 <- c("militares_anac","militares_anp","militares_anvs")

df_somas <- df_ano_out %>%
    group_by(ano_arq) %>%
    summarise(
        militares_8_ars = rowSums(across(all_of(cols_8)), na.rm = TRUE),
        militares_3_ars = rowSums(across(all_of(cols_3)), na.rm = TRUE),
        .groups = "drop"
    )

df_ano_out <- df_ano_out %>%
    left_join(df_somas, by = "ano_arq")



#criando soma de 8 e 3AR militares com cargo (inclui alto escalao)
cols_8_cargos <- c(
    "militares_aneel_cargos",
    "militares_anatel_cargos",
    "militares_antaq_cargos",
    "militares_antt_cargos",
    "militares_ancine_cargos",
    "militares_ans_cargos",
    "militares_anm_cargos",
    "militares_ana_cargos"
)

cols_3_cargos <- c("militares_anac_cargos",
                   "militares_anp_cargos",
                   "militares_anvs_cargos")

df_somas_cargos <- df_ano_out %>%
    group_by(ano_arq) %>%
    summarise(
        militares_8_ars_cargos = rowSums(across(all_of(cols_8_cargos)), na.rm = TRUE),
        militares_3_ars_cargos = rowSums(across(all_of(cols_3_cargos)), na.rm = TRUE),
        .groups = "drop"
    )

df_ano_out <- df_ano_out %>%
    left_join(df_somas_cargos, by = "ano_arq")


#criando soma de 8 e 3AR militares com alto escalao
cols_8_alto_escalao <- c(
    "militares_aneel_alto_escalao",
    "militares_anatel_alto_escalao",
    "militares_antaq_alto_escalao",
    "militares_antt_alto_escalao",
    "militares_ancine_alto_escalao",
    "militares_ans_alto_escalao",
    "militares_anm_alto_escalao",
    "militares_ana_alto_escalao"
)

cols_3_alto_escalao <- c("militares_anac_alto_escalao",
                   "militares_anp_alto_escalao",
                   "militares_anvs_alto_escalao")

df_somas_alto_escalao <- df_ano_out %>%
    group_by(ano_arq) %>%
    summarise(
        militares_8_ars_alto_escalao = rowSums(across(all_of(cols_8_alto_escalao)), na.rm = TRUE),
        militares_3_ars_alto_escalao = rowSums(across(all_of(cols_3_alto_escalao)), na.rm = TRUE),
        .groups = "drop"
    )

df_ano_out <- df_ano_out %>%
    left_join(df_somas_alto_escalao, by = "ano_arq")


#grafico somando o total de militares por 3 criterios nos dois grupos (3 e 8 AR)
vars <- c("militares_8_ars.y",
          "militares_3_ars.y",
          "militares_8_ars_cargos",
          "militares_3_ars_cargos",
          "militares_8_ars_alto_escalao",
          "militares_3_ars_alto_escalao")

# Checar colunas
stopifnot(all(c("ano_arq", vars) %in% names(df_ano_out)))

# Agregar por ano (caso haja múltiplas linhas por ano)
plot_df <- df_ano_out %>%
    group_by(ano_arq) %>%
    summarise(across(all_of(vars), ~ sum(.x, na.rm = TRUE)), .groups = "drop")

# Long format para ggplot
plot_long <- plot_df %>%
    pivot_longer(cols = all_of(vars), names_to = "serie", values_to = "valor")

# Rótulos mais legíveis
rotulos <- c(
    "militares_8_ars.y" = "8 ARs (total)",
    "militares_3_ars.y" = "3 ARs (total)",
    "militares_8_ars_cargos" = "8 ARs c/ cargo",
    "militares_3_ars_cargos" = "3 ARs c/ cargo",
    "militares_8_ars_alto_escalao" = "8 ARs alto escalão",
    "militares_3_ars_alto_escalao" = "3 ARs alto escalão"
)

fig_2b <- ggplot(plot_long, aes(x = ano_arq, y = valor, color = serie)) +
    geom_line(size = 1) +
    geom_point(size = 2) +
    scale_color_manual(values = c("#1b9e77","#d95f02","#7570b3","#e7298a","#66a61e","#e6ab02"),
                       labels = rotulos) +
    scale_x_continuous(breaks = sort(unique(plot_long$ano_arq))) +
    scale_y_continuous(labels = label_number(big.mark = ".", decimal.mark = ",")) +
    labs(
        title = "Totais anuais por série",
        x = "Ano",
        y = "Quantidade",
        color = "Série"
    ) +
    theme_minimal(base_size = 12) +
    theme(
        panel.grid.minor = element_blank(),
        legend.position = "bottom"
    )

fig_2b

ggsave("figures/fig_2b.pdf", fig_2b,
       width = 6.5, height = 5, units = "in")




###CRIANDO O MESMO GRÁFICO ACIMA, MAS COMO FACETA
# Definir variáveis por grupo
vars_ars <- c("militares_8_ars.y","militares_3_ars.y")
vars_cargos <- c("militares_8_ars_cargos","militares_3_ars_cargos")
vars_alto <- c("militares_8_ars_alto_escalao","militares_3_ars_alto_escalao")

vars_all <- c(vars_ars, vars_cargos, vars_alto)

stopifnot(all(c("ano_arq", vars_all) %in% names(df_ano_out)))

# Agregar por ano (se necessário)
base_df <- df_ano_out %>%
    group_by(ano_arq) %>%
    summarise(across(all_of(vars_all), ~ sum(.x, na.rm = TRUE)), .groups = "drop")

# Long + colunas de 'grupo' e 'tipo'
plot_long <- base_df %>%
    pivot_longer(cols = all_of(vars_all), names_to = "serie", values_to = "valor") %>%
    mutate(
        grupo = case_when(
            str_detect(serie, "_ars_alto_escalao$") ~ "ars_alto_escalao",
            str_detect(serie, "_ars_cargos$") ~ "ars_cargos",
            TRUE ~ "ars"
        ),
        tipo = if_else(str_detect(serie, "^militares_8_"), "8 ARs", "3 ARs")
    )

fig_2c <- ggplot(plot_long, aes(x = ano_arq, y = valor, color = tipo)) +
    geom_line(size = 1) +
    geom_point(size = 2) +
    scale_color_manual(values = c("8 ARs" = "#1b9e77", "3 ARs" = "#d95f02")) +
    scale_x_continuous(breaks = sort(unique(plot_long$ano_arq))) +
    scale_y_continuous(labels = label_number(big.mark = ".", decimal.mark = ",")) +
    labs(
        title = "Totais anuais por grupo (facetas)",
        x = "Ano",
        y = "Quantidade",
        color = "Série"
    ) +
    facet_wrap(~ grupo, ncol = 1, scales = "free_y") +
    theme_minimal(base_size = 12) +
    theme(
        panel.grid.minor = element_blank(),
        legend.position = "bottom"
    )


fig_2c

ggsave("figures/fig_2c.pdf", fig_2c,
       width = 6.5, height = 5, units = "in")


###CRIANDO AGORA FACETAS DOS TRÊS GRUPOS EM GRÁFICO DE ÁREA DE 100%
library(stringr)

# Variáveis por grupo (mantém .y onde informado)
vars_ars    <- c("militares_8_ars.y","militares_3_ars.y")
vars_cargos <- c("militares_8_ars_cargos","militares_3_ars_cargos")
vars_alto   <- c("militares_8_ars_alto_escalao","militares_3_ars_alto_escalao")

vars_all <- c(vars_ars, vars_cargos, vars_alto)
stopifnot(all(c("ano_arq", vars_all) %in% names(df_ano_out)))

# Agregar por ano
base_df <- df_ano_out %>%
    group_by(ano_arq) %>%
    summarise(across(all_of(vars_all), ~ sum(.x, na.rm = TRUE)), .groups = "drop")

# Long + identificar grupo e tipo
plot_long <- base_df %>%
    pivot_longer(cols = all_of(vars_all), names_to = "serie", values_to = "valor") %>%
    mutate(
        grupo = case_when(
            str_detect(serie, "_ars_alto_escalao$") ~ "RAs: all officers Top-level",
            str_detect(serie, "_ars_cargos$") ~ "RAs: all officers appointees",
            TRUE ~ "RAs: all officers"
        ),
        tipo = if_else(str_detect(serie, "^militares_8_"), "8 RAs", "3 RAs")
    )

# Normalizar para 100% dentro de cada grupo-ano
plot_pct <- plot_long %>%
    group_by(grupo, ano_arq) %>%
    mutate(total_grupo_ano = sum(valor, na.rm = TRUE),
           pct = if_else(total_grupo_ano > 0, valor / total_grupo_ano, 0)) %>%
    ungroup()

fig_2d <- ggplot(plot_pct, aes(x = ano_arq, y = pct, fill = tipo)) +
    geom_area(position = "fill", alpha = 0.9) +  # fill garante 100%
    scale_fill_manual(values = c("8 RAs" = "#1b9e77", "3 RAs" = "#d95f02")) +
    scale_x_continuous(breaks = sort(unique(plot_pct$ano_arq))) +
    scale_y_continuous(labels = label_percent(accuracy = 1)) +
    labs(
        title   = "Share of officers by group of RAs (ANP, Anvisa, ANAC vs. others) (100%)",
        x       = "Year",
        y       = "Share",
        fill    = "Groups"
    ) +
    facet_wrap(~ grupo, ncol = 1) +
    theme_minimal(base_size = 12) +
    theme(
        panel.grid.minor = element_blank(),
        legend.position = "bottom"
    )

fig_2d
ggsave("figures/fig_2d.pdf", fig_2d,
       width = 6.5, height = 5, units = "in")


###FACETAS DE TODAS AS AGENCIAS, SEPARADAMENTE, POR GRUPOS (TOTAL MILITAR, COM CARGO E ALTA BUROCRA)


# Definir vetores por faceta
grp_base <- c("militares_anac","militares_aneel","militares_anatel","militares_antaq",
              "militares_antt","militares_anvs","militares_ancine","militares_anp",
              "militares_ans","militares_anm","militares_ana")

grp_cargos <- paste0(grp_base, "_cargos")
grp_alto   <- paste0(grp_base, "_alto_escalao")

vars_all <- c(grp_base, grp_cargos, grp_alto)
stopifnot(all(c("ano_arq", vars_all) %in% names(df_ano_out)))

# Agregar por ano, se necessário
base_df <- df_ano_out %>%
    group_by(ano_arq) %>%
    summarise(across(all_of(vars_all), ~ sum(.x, na.rm = TRUE)), .groups = "drop")

# Long + coluna do grupo
plot_long <- base_df %>%
    pivot_longer(cols = all_of(vars_all), names_to = "agencia", values_to = "valor") %>%
    mutate(
        grupo = case_when(
            str_detect(agencia, "_alto_escalao$") ~ "alto_escalao",
            str_detect(agencia, "_cargos$") ~ "cargos",
            TRUE ~ "base"
        ),
        agencia_label = str_replace(agencia, "militares_", "") |> 
            str_replace("_cargos$", "") |>
            str_replace("_alto_escalao$", "") |>
            toupper()
    )

ggplot(plot_long, aes(x = ano_arq, y = valor, color = agencia_label, group = agencia_label)) +
    geom_line(size = 0.9, alpha = 0.9) +
    geom_point(size = 1.8) +
    scale_x_continuous(breaks = sort(unique(plot_long$ano_arq))) +
    scale_y_continuous(labels = label_number(big.mark = ".", decimal.mark = ",")) +
    labs(
        title = "Totais anuais por agência e grupo",
        x = "Ano",
        y = "Quantidade",
        color = "Agência"
    ) +
    facet_wrap(~ grupo, ncol = 1, scales = "free_y",
               labeller = as_labeller(c(base = "ARs (base)",
                                        cargos = "ARs (cargos)",
                                        alto_escalao = "ARs (alto escalão)"))) +
    theme_minimal(base_size = 12) +
    theme(
        panel.grid.minor = element_blank(),
        legend.position = "bottom",
        legend.key.width = unit(16, "pt")
    )



## FACETAS separando por agencia

library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
library(stringr)
library(forcats)

# Lista de ARs
ars <- c("anac","aneel","anatel","antaq","antt","anvs","ancine","anp","ans","anm","ana")

# Construir nomes de colunas para cada dimensão
vars_base   <- paste0("militares_", ars)
vars_cargos <- paste0("militares_", ars, "_cargos")
vars_alto   <- paste0("militares_", ars, "_alto_escalao")

vars_all <- c(vars_base, vars_cargos, vars_alto)
stopifnot(all(c("ano_arq", vars_all) %in% names(df_ano_out)))

# Agregar por ano (se necessário)
base_df <- df_ano_out %>%
    group_by(ano_arq) %>%
    summarise(across(all_of(vars_all), ~ sum(.x, na.rm = TRUE)), .groups = "drop")

# Converter para longo e separar AR e dimensão
plot_long <- base_df %>%
    pivot_longer(cols = all_of(vars_all), names_to = "serie", values_to = "valor") %>%
    mutate(
        # Extrair a AR do nome: "militares_anac(_...)" -> "anac"
        ar  = str_remove(serie, "^militares_") %>%
            str_remove("_cargos$") %>%
            str_remove("_alto_escalao$") %>%
            toupper(),
        dim = case_when(
            str_detect(serie, "_alto_escalao$") ~ "Alto escalão",
            str_detect(serie, "_cargos$")       ~ "Cargos",
            TRUE                                ~ "Base"
        )
    ) %>%
    # Opcional: ordenar facetas por ordem desejada
    mutate(ar = factor(ar, levels = toupper(ars)))

ggplot(plot_long, aes(x = ano_arq, y = valor, color = dim)) +
    geom_line(size = 1) +
    geom_point(size = 1.8) +
    scale_color_manual(values = c("Base" = "#1b9e77", "Cargos" = "#7570b3", "Alto escalão" = "#d95f02")) +
    scale_x_continuous(breaks = sort(unique(plot_long$ano_arq))) +
    scale_y_continuous(labels = label_number(big.mark = ".", decimal.mark = ",")) +
    labs(
        title = "Totais anuais por AR e dimensão",
        x = "Ano",
        y = "Quantidade",
        color = "Dimensão"
    ) +
    facet_wrap(~ ar, ncol = 3, scales = "free_y") +
    theme_minimal(base_size = 12) +
    theme(
        panel.grid.minor = element_blank(),
        legend.position = "bottom"
    )


################################################
#CRIANDO GRÁFICO CRESCIMENTO PERCENTUAL ANUAL DOS MILITARES NO GOVERNO
#NAS AGENCIAS E EM CARGOS NAS AGENCIAS
#############################################

# 2. Selecione e padronize as colunas
cresc_anual <- df_ano_out %>%
    rename(
        ano = ano_arq,
        total_militares_governo = total_militares_governo,
        total_militares_ars = total_militares_ars,
        total_militares_ars_com_cargo = total_militares_ars_com_cargo
    ) %>%
    filter(!is.na(ano)) %>%
    group_by(ano) %>%
    summarise(
        total_militares_governo = sum(total_militares_governo, na.rm=TRUE),
        total_militares_ars = sum(total_militares_ars, na.rm=TRUE),
        total_militares_ars_com_cargo = sum(total_militares_ars_com_cargo, na.rm=TRUE)
    ) %>% 
    arrange(ano)

# 3. Calcule o percentual de crescimento anual
cresc_anual_1 <- cresc_anual %>%
    mutate(
        crescimento_militares_governo = 100 * (total_militares_governo / lag(total_militares_governo) - 1),
        crescimento_militares_ars = 100 * (total_militares_ars / lag(total_militares_ars) - 1),
        crescimento_militares_ars_com_cargo = 100 * (total_militares_ars_com_cargo / lag(total_militares_ars_com_cargo) - 1)
    )

cresc_anual_1_long <- cresc_anual_1 %>%
    pivot_longer(
        cols = starts_with("crescimento_"),
        names_to = "Variavel",
        values_to = "Percentual"
    )

# Ajuste os rótulos se quiser mais legibilidade
cresc_anual_1_long$Variavel <- recode(cresc_anual_1_long$Variavel,
                           crescimento_militares_governo = "Officers in the Government",
                           crescimento_militares_ars = "Officers in Regulatory Agencies",
                           crescimento_militares_ars_com_cargo = "Officers as appointees in RAs")

# Gráfico de barras agrupadas
fig_A_cres_anual <- ggplot(cresc_anual_1_long, aes(x = as.factor(ano), y = Percentual, fill = Variavel)) +
    geom_col(position = "dodge") +
    geom_text(
        aes(label = sprintf("%.1f", Percentual)),
        position = position_dodge(width = 0.9),
        vjust = -0.5,
        size = 2.5
    ) +
    labs(
        title = "Annual Variation - Officers in Government, in RAs and as appointees in RAs (in %)",
        x = "Year",
        y = "Growth (%)",
        fill = ""
    ) +
    theme_minimal() +
    theme(
        plot.title = element_text(size = 10),
        axis.title = element_text(size = 8),
        axis.text = element_text(size = 8),
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 8),
        legend.position = "bottom"   # <- legenda abaixo do gráfico
    )

fig_A_cres_anual

ggsave("figures/fig_A_cres_anual.pdf", fig_A_cres_anual,
       width = 8.5, height = 5, units = "in")


############################################################
######figure annual growth only 3 RAs
###########################################################


var_anual_3ras <- read_delim("var_anual_3ras.csv", 
                             delim = ";", escape_double = FALSE, col_types = cols(var_officers_3ras_percent = col_number(), 
                                                                                  var_officers_appointees_3ras_percent = col_number(), 
                                                                                  var_officers_toplevel_3ras_percent = col_number()), 
                             trim_ws = TRUE)
View(var_anual_3ras)



ras_long <- var_anual_3ras %>%
    select(year, var_officers_3ras_percent, var_officers_appointees_3ras_percent, var_officers_toplevel_3ras_percent) %>%
           pivot_longer(
               cols = starts_with("var_officers"),
               names_to = "variable",
               values_to = "percentage"
           )
           
           
           # Ajustar nomes legíveis para legenda
           ras_long$variable <- recode(ras_long$variable,
                                      "var_officers_3ras_percent" = "Officers 3ras",
                                      "var_officers_appointees_3ras_percent" = "Appointed Officers 3ras",
                                      "var_officers_toplevel_3ras_percent" = "Top-level Officers 3ras"
           )
           
           # Criar gráfico de barras agrupadas
           fig_A_var_3ras <- ggplot(ras_long, aes(x = factor(year), y = percentage, fill = variable)) +
               geom_col(position = "dodge") +
               geom_text(aes(label = sprintf("%.1f%%", percentage)),
                         position = position_dodge(width = 0.9),
                         vjust = -0.5,
                         size = 2.5) +
               labs(
                   title = "Annual Variation - Officers in the 3 selected Regulatoy Agencies",
                   x = "Year",
                   y = "Variation (%)",
                   fill = ""
               ) +
               theme_minimal() +
                   theme(
                       plot.title = element_text(size = 10, hjust = 0.5), #centraliza o título
                       axis.title = element_text(size = 8),
                       axis.text = element_text(size = 8),
                       legend.title = element_text(size = 10),
                       legend.text = element_text(size = 8),
                       legend.position = "bottom"   # <- legenda abaixo do gráfico
                   )
               
           fig_A_var_3ras
           
                  ggsave("figures/fig_A_var_3ras.pdf", fig_A_var_3ras,
                  width = 8.5, height = 5, units = "in")

                  
#####################################################
##GRAFICO COMPARANDO TRES AGENCIAS COM AS DEMAIS ####
#####################################################

library(readr)
compare_3ras_others <- read_delim("compare_3ras_others.csv", 
                                                    delim = ";", escape_double = FALSE, trim_ws = TRUE)

View(compare_3ras_others)



compare_3_8 <- compare_3ras_others %>%
    select(year, 
           `otherRA_officers_total__percent`,
           `otherRAs_officers_appointees_percent`,
           `otherRA_officers_top-level_percent`,
           `3RA_total_officers_percent`,
           `3RA_officers_appointees_percent`,
           `3RA_officers_top-level_percent`)

# Transformar para formato longo
compare_long <- compare_3_8 %>%
    pivot_longer(
        cols = -year,
        names_to = "variable",
        values_to = "percentage"
    )

# Ajustar nomes das variáveis para legenda mais legível
compare_long$variable <- recode(compare_long$variable,
                           "otherRA_officers_total__percent" = "Other RAs - Total Officers",
                           "otherRAs_officers_appointees_percent" = "Other RAs - Appointees",
                           "otherRA_officers_top-level_percent" = "Other RAs - Top-level",
                           "3RA_total_officers_percent" = "3 RAs - Total Officers",
                           "3RA_officers_appointees_percent" = "3 RAs - Appointees",
                           "3RA_officers_top-level_percent" = "3 RAs - Top-level"
)

# Criar gráfico de barras agrupadas
fig_A_compare_3_8 <- ggplot(compare_long, aes(x = factor(year), y = percentage, fill = variable)) +
    geom_col(position = "dodge") +
    geom_text(aes(label = sprintf("%.1f%%", percentage)),
              position = position_dodge(width = 0.9),
              vjust = -0.5,
              size = 2.5) +
    labs(
        title = "Annual Variation Rate Comparison - 3 RAs and the others",
        x = "Year",
        y = "Percentage (%)",
        fill = "Category",
        caption = "Variables: Other RAs - Total Officers, Appointees, Top-level | 3 RAs - Total Officers, Appointees, Top-level"
    ) +
    theme_minimal() +
    theme(
        plot.caption = element_text(hjust = 0.5, size = 9),
        legend.position = "bottom"
    )

fig_A_compare_3_8

ggsave("figures/fig_A_compare_3_8.pdf", fig_A_compare_3_8,
       width = 8.5, height = 5, units = "in")
                  
                  
###########################################################
#COMPARANDO AS 3 AGENCIAS COM AS DEMAIS, EM TOTAIS ANUAIS##
###########################################################

library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)

# Leitura do arquivo CSV

# Selecionar apenas ano e as 6 variáveis de totais
compare <- compare_3ras_others %>%
    select(year, 
           otherRA_officers_total,
           otherRAs_officers_appointees,
           `otherRA_officers_top-level`,
           `3RA_total_officers`,
           `3RA_officers_appointees`,
           `3RA_officers_top-level`)

# Transformar para formato longo
compare_long_1 <- compare %>%
    pivot_longer(
        cols = -year,
        names_to = "variable",
        values_to = "total"
    )

# Ajustar nomes das variáveis para legenda mais legível
compare_long_1$variable <- recode(compare_long_1$variable,
                           "otherRA_officers_total" = "Other RAs - Total Officers",
                           "otherRAs_officers_appointees" = "Other RAs - Appointees",
                           "otherRA_officers_top-level" = "Other RAs - Top-level",
                           "3RA_total_officers" = "3 RAs - Total Officers",
                           "3RA_officers_appointees" = "3 RAs - Appointees",
                           "3RA_officers_top-level" = "3 RAs - Top-level"
)

# Criar gráfico de linhas
fig_A_line_compare <- ggplot(compare_long_1, aes(x = year, y = total, color = variable, group = variable)) +
    geom_line(size = 1) +
    geom_point(size = 2) +
    labs(
        title = "Officer Totals Over Time - Comparison",
        x = "Year",
        y = "Number of Officers",
        color = "Category",
        caption = "Variables: Other RAs - Total Officers, Appointees, Top-level | 3 RAs - Total Officers, Appointees, Top-level"
    ) +
    theme_minimal() +
    theme(
        plot.caption = element_text(hjust = 0.5, size = 9),
        legend.position = "bottom",
        legend.text = element_text(size = 8)
    )

fig_A_line_compare

# Salvar o gráfico
ggsave("figures/fig_A_line_compare.pdf", fig_A_line_compare,
       width = 8.5, height = 5, units = "in")


################################
#FACETING EACH RA
##############################
library(stringr)
library(scales)
library(forcats)

# Vetor com os códigos das ARs
ars <- c("anac","aneel","anatel","antaq","antt","anvs","ancine","anp","ans","anm","ana")

# Colunas esperadas por dimensão
vars_base   <- paste0("militares_", ars)
vars_cargos <- paste0("militares_", ars, "_cargos")
vars_alto   <- paste0("militares_", ars, "_alto_escalao")

vars_all <- c(vars_base, vars_cargos, vars_alto)

# Verificação de colunas existentes
stopifnot(all(c("ano_arq", vars_all) %in% names(df_ano_out)))

# Se houver múltiplas linhas por ano, consolida; se já for 1 linha/ano, valores permanecem
base_df <- df_ano_out %>%
    group_by(ano_arq) %>%
    summarise(across(all_of(vars_all), ~ sum(.x, na.rm = TRUE)), .groups = "drop")

# Formato longo e identificação de AR e dimensão
plot_long <- base_df %>%
    pivot_longer(cols = all_of(vars_all), names_to = "variavel", values_to = "valor") %>%
    mutate(
        ar  = variavel |> 
            str_remove("^militares_") |>
            str_remove("_cargos$") |>
            str_remove("_alto_escalao$") |>
            toupper(),
        dim = case_when(
            str_detect(variavel, "_alto_escalao$") ~ "Top-level appointees",
            str_detect(variavel, "_cargos$")       ~ "Appointees",
            TRUE                                   ~ "Total"
        )
    ) %>%
    mutate(ar = factor(ar, levels = toupper(ars)),
           dim = factor(dim, levels = c("Total","Appointees","Top-level appointees")))

# Gráfico com facetas por AR e 3 linhas por faceta
fig_A_facetas <- ggplot(plot_long, aes(x = ano_arq, y = valor, color = dim)) +
    geom_line(linewidth = 1) +
    geom_point(size = 1.8) +  # <- era linewidth
    scale_color_manual(values = c("Total" = "#1b9e77",
                                  "Appointees" = "#7570b3",
                                  "Top-level appointees" = "#d95f02")) +
    scale_x_continuous(breaks = sort(unique(plot_long$ano_arq))) +
    scale_y_continuous(labels = label_number(big.mark = ".", decimal.mark = ",")) +
    labs(
        title = "Officers by Regulatory Agency: total, total in appointed positions, and total in the top-level",
        x = "Year", y = "Amount", color = ""
    ) +
    facet_wrap(~ ar, ncol = 4, scales = "free_y") +
    theme_minimal(base_size = 10) +
    theme(
        panel.grid.minor = element_blank(),
        plot.title = element_text(size = 10, hjust = 0.5),
        axis.title = element_text(size = 9),
        axis.text  = element_text(size = 8),
        legend.position = "bottom"
    )

fig_A_facetas


#salvar o gráfico
ggsave("figures/fig_A_facetas.pdf", fig_A_facetas,
    width = 8.5, height = 5, units = "in")


############################################################
###DADOS COM TOTAIS GERAIS CIVIS E MILITARES ARS E DEMAIS###
############################################################

#importanto da tabela de somas gerais
library(readxl)
general_sums <- read_excel("table_general_sums.xlsx")
View(general_sums)

# criando o dataset
general_sums_1 <- general_sums %>%
    pivot_longer(cols = 2:7, names_to = "variable", values_to = "value") %>%
    mutate(variable = recode(variable,
                             "total_toplevel_3RAs" = "Toplevel 3RAs",
                             "total_appointees_3RAs" = "Appointees 3RAs",
                             "total_servants_3RAs" = "Servants 3RAs",
                             "total_toplevel_otherRAs" = "Toplevel Other RAs",
                             "total_appointees_otherRAs" = "Appointees Other RAs",
                             "total_servants_otherRAs" = "Servants Other RAs"
    ))

fig_A_general_sum_1 <- ggplot(general_sums_1, aes(x = year, y = value, color = variable)) +
    geom_line(linewidth = 1) +
    geom_point(size = 1) +
    labs(
        title = "Total servants by group of RAs (2013-2024)",
        x = "Year",
        y = "Value",
        color = ""
    ) +
    theme_minimal(base_size = 10) +
    theme(
        legend.position = "bottom",   # legenda embaixo
        legend.title = element_blank(),
        legend.box = "horizontal"     # organiza legenda em linha
    )


fig_A_general_sum_1

ggsave("figures/fig_A_general_sum_1.pdf", fig_A_general_sum_1,
       width = 8.5, height = 5, units = "in")


###Novo gráfico de comparação
# Selecionar apenas colunas year, 8 e 13

general_sum_2 <- general_sums %>%
    select(year, 8:13) %>%
    pivot_longer(cols = -year, names_to = "variable", values_to = "value") %>%
    mutate(variable = recode(variable,
                             "total_toplevel_officers_3RAs" = "Toplevel Officers 3RAs",
                             "total_appointees_officers_3RAs" = "Appointees Officers 3RAs",
                             "total_servants_officers_3RAs" = "Servants Officers 3RAs",
                             "total_toplevel_officers_otherRAs" = "Toplevel Officers Other RAs",
                             "total_appointees_officers_otherRAs" = "Appointees Officers Other RAs",
                             "total_servants_officers_otherRAs" = "Servants Officers Other RAs"
    ))


# Gráfico
fig_A_general_sum_2 <- ggplot(general_sum_2, aes(x = year, y = value, color = variable)) +
    geom_line(linewidth = 1) +
    geom_point(size = 1.5) +
    labs(
        title = "Total officers by group of RAs (2013-2024)",
        x = "Year",
        y = "Value",
        color = ""
    ) +
    theme_minimal(base_size = 12) +
    theme(
        legend.position = "bottom",   # legenda embaixo
        legend.title = element_blank(),
        legend.box = "horizontal"
    )

fig_A_general_sum_2

ggsave("figures/fig_A_general_sum_2.pdf", fig_A_general_sum_2,
width = 8.5, height = 5, units = "in")


#############
#FACETAS
#########

# Pacotes
library(tidyverse)

# 1) Ler o CSV separado por ponto e vírgula
library(readr)
compare_df <- read_delim("compare_3ras_others.csv", 
                                  delim = ";", escape_double = FALSE, trim_ws = TRUE)

# --- FACETAS COM VALORES ABSOLUTOS -----------------------------------------

# 2) Seleciona apenas as colunas "absolutas"
df_abs <- compare_df %>%
    select(
        year,
        otherRA_officers_total,
        otherRAs_officers_appointees,
        `otherRA_officers_top-level`,
        `3RA_total_officers`,
        `3RA_officers_appointees`,
        `3RA_officers_top-level`
    )

# 3) Converte para formato longo com dois níveis: grupo (3RA vs otherRA) e métrica
abs_long <- df_abs %>%
    pivot_longer(
        -year,
        names_to = c("group", "metric"),
        names_pattern = "(otherRA|3RA)[_]?(.*)"  # separa "otherRA" ou "3RA" do restante
    ) %>%
    mutate(
        metric = case_when(
            metric %in% c("officers_total", "total_officers") ~ "Total officers",
            metric %in% c("officers_appointees", "officers_appointees") ~ "Officers as appointees",
            metric %in% c("officers_top-level", "officers_top-level") ~ "Top-level officers",
            TRUE ~ metric
        ),
        group = recode(group, otherRA = "Other RAs", `3RA` = "3 RAs")
    )

# 4) Gráfico com 3 facetas (uma por métrica), comparando 3 RAs vs Other RAs
fig_abs <- ggplot(abs_long, aes(x = factor(year), y = value, group = group, color = group)) +
    geom_line(size = 0.9) +
    geom_point(size = 2) +
    facet_wrap(~ metric, ncol = 1, scales = "free_y") +
    labs(
        title = "Officers in RAs: totals, appointees, and top-level (3 RAs vs Other RAs)",
        x = "Year",
        y = "Count",
        color = ""
    ) +
    theme_minimal() +
    theme(
        plot.title = element_text(size = 11, hjust = 0.5), # centraliza o título
        axis.title = element_text(size = 9),
        axis.text = element_text(size = 8),
        legend.position = "bottom"
    )

fig_abs

# --- FACETAS COM PERCENTUAIS ------------------------------------------------



df_pct <- df %>%
    select(
        year,
        otherRA_officers_total__percent,
        otherRAs_officers_appointees_percent,
        otherRA_officers_top-level_percent,
        `3RA_total_officers_percent`,
        `3RA_officers_appointees_percent`,
        `3RA_officers_top-level_percent`
    )

pct_long <- df_pct %>%
    pivot_longer(
        -year,
        names_to = c("group", "metric", "suffix"),
        names_pattern = "(otherRA|3RA)[_]?(.*)_(percent)$"
    ) %>%
    mutate(
        metric = case_when(
            metric %in% c("officers_total", "total_officers") ~ "Total officers",
            metric %in% c("officers_appointees", "officers_appointees") ~ "Officers as appointees",
            metric %in% c("officers_top-level", "officers_top-level") ~ "Top-level officers",
            TRUE ~ metric
        ),
        group = recode(group, otherRA = "Other RAs", `3RA` = "3 RAs")
    )

fig_pct <- ggplot(pct_long, aes(x = factor(year), y = value, group = group, color = group)) +
    geom_line(size = 0.9) +
    geom_point(size = 2) +
    facet_wrap(~ metric, ncol = 1) +
    scale_y_continuous(labels = scales::label_percent(accuracy = 0.1, scale = 1)) +
    labs(
        title = "Officers in RAs (Percent): totals, appointees, and top-level (3 RAs vs Other RAs)",
        x = "Year",
        y = "Percent",
        color = ""
    ) +
    theme_minimal() +
    theme(
        plot.title = element_text(size = 11, hjust = 0.5),
        axis.title = element_text(size = 9),
        axis.text = element_text(size = 8),
        legend.position = "bottom"
    )

fig_pct

# --- SALVAR ARQUIVOS --------------------------------------------------------

# cria a pasta figures caso não exista
if (!dir.exists("figures")) dir.create("figures", recursive = TRUE)

ggsave("figures/fig_facets_absolute.pdf", fig_abs, width = 8, height = 9, units = "in")
ggsave("figures/fig_facets_percent.pdf",  fig_pct, width = 8, height = 9, units = "in")
