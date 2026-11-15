# El presente script contiene funciones auxiliares de lectura de datos
# y cálculos con estos datos. Tales funciones son invocadas en el
# reporte principal.

read.matches.df <- function(league.identifier) {
    # Recibe el nombre de una competición (league.identifier), lee
    # todos los CSV's asociados a la competición y los concatena para
    # devolver un solo tibble con datos de los partidos de la
    # competición

    matches.df <- tibble()

    for (initial.year in 2021:2025) {
        file.path <- str_glue("data/{league.identifier}/{initial.year}-{initial.year + 1}.csv")

        current.season.df <- read_csv(
            file.path,
            col_select = c("HomeTeam", "AwayTeam", "FTHG", "FTAG"),
            col_types = cols(
                HomeTeam = col_character(),
                AwayTeam = col_character(),
                FTHG = col_integer(),
                FTAG = col_integer()
            )
        )

        matches.df <- bind_rows(matches.df, current.season.df)
    }

    matches.df <- matches.df |>
        rename(home.team = HomeTeam, away.team = AwayTeam, home.goals = FTHG, away.goals = FTAG)

    matches.df
}

goal.weight <- function(matches.df) {
    # Recibe un tibble (matches.df) con los datos de partidos de una
    # competición, calcula y devuelve el "peso del gol" en la
    # competición

    summary <- matches.df |>
        mutate(total.goals = home.goals + away.goals) |>
        summarise(n = n(), league.goals = sum(total.goals))

    weight <- summary$n / summary$league.goals
    stopifnot(is.finite(weight))

    weight
}
