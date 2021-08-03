library(readxl)

# Importando a base de dados ----------------------------------------------
df <- readxl::read_excel("data-raw/tabela1552 - Copia.xlsx",
                         sheet = 2) |>
  dplyr::mutate(
    codigo_municipio = as.numeric(codigo_municipio)
  )

col1 <- df[1] |> tidyr::drop_na() |> dplyr::pull() |>
  rep(101) |>
  dplyr::as_tibble()

col2 <- df[2] |> tidyr::drop_na() |> dplyr::pull() |>
  rep(101) |>
  dplyr::as_tibble()

col3 <- df[3] |> tidyr::drop_na() |> dplyr::pull() |>
  rep(101) |>
  dplyr::as_tibble()

df2 <- dplyr::bind_cols(col1, col2, col3) |>
  dplyr::rename(codigo_municipio = value...1,
                nome_municipio = value...2,
                nome_uf = value...3)

df2 <- df2 |> dplyr::arrange(df2, "codigo_municipio")

df_final <- dplyr::bind_cols(df2, df[4:5])

readr::write_rds(df_final, "data/indicesCidades.rds")


# Atualizado em 01/08/2021
