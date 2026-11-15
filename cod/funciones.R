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

team.performance <- function(team.identifier, matches.df, weight) {
    # Recibe el nombre de un equipo (team.identifier), un tibble
    # (matches.df) con los datos de partidos de la competción y "peso
    # del gol" (weight). Calcula y devuelve el rendimiento del equipo
    # en cuestión

    summary <- matches.df |>
        filter(when_any(home.team == team.identifier, away.team == team.identifier)) |>
        mutate(
            goal.difference = case_when(
                home.team == team.identifier ~ home.goals - away.goals,
                .default = away.goals - home.goals
            )
        ) |>
        summarise(
            n = n(),
            total.goal.difference = sum(goal.difference)
        )

    performance <- (summary$total.goal.difference / summary$n) * weight

    performance
}

performance.table <- function(matches.df) {
    # Recibe un tibble (matches.df) con los datos de los partidos de
    # una competción, calcula el rendimiento de cada equipo, para
    # construir y devolver una tabla de rendimiento

    weight <- goal.weight(matches.df)

    team.performance.application <- function(team.identifier) {
        team.performance(team.identifier, matches.df, weight)
    }

    table <- matches.df |>
        summarise(.by = "home.team") |>
        rename(team = home.team) |>
        mutate(performance = map_dbl(team, team.performance.application)) |>
        arrange(desc(performance))

    table
}
