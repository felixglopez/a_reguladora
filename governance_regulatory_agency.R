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
    "cres_militares_governo" = "Federal Executive",
    "cres_militares_ar" = "Military in All Regulatory Agencies",
    "cres_militares_ar_cargo" = "Military as Appointees in All Regulatory Agencies"
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
        y = "Growth (%)",
        color = ""
    ) +
    theme_minimal(base_size = 12) +
    theme(
        panel.grid.minor = element_blank(),
        legend.position = "bottom"
    )

fig_1b

if(!dir.exists("figures")) {
    dir.create("figures")
}

ggsave("figures/fig_1b.pdf", fig_1b,
       width = 6.5, height = 5, units = "in")


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


