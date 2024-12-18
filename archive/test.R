# mean()



# pipe operator ---------------------------------------------------------------------------------------------------

summary_of_dataframe <- 
    data.frame(
        "id" = 1:10,
        "numbers" = 51:60
    ) |> 
    summary()

summary_of_dataframe


# functions -------------------------------------------------------------------------------------------------------

amazing_function <- function (input) 
{
    if (input |> is.numeric())
        
    {
        return(input * 3)
    }
    
    if (input |> is.character())
        
    {
        paste(c("only numbers"))
    }
    
    else
        
    {
        paste(c("error"))
    }
        
    
}

3 |> amazing_function()
"tipsi" |> amazing_function()
TRUE |>  amazing_function()


# Load data -------------------------------------------------------------------------------------------------------

spotify_songs_2023 <- read_csv(
    file = "data/spotify-songs-2023.csv",
    col_types = cols(
        track_name = col_character(),
        `artist(s)_name` = col_character(),
        artist_count = col_double(),
        released_year = col_double(),
        released_month = col_double(),
        released_day = col_double(),
        in_spotify_playlists = col_double(),
        in_spotify_charts = col_double(),
        streams = col_character(),
        in_apple_playlists = col_double(),
        in_apple_charts = col_double(),
        in_deezer_playlists = col_number(),
        in_deezer_charts = col_double(),
        in_shazam_charts = col_number(),
        bpm = col_double(),
        key = col_character(),
        mode = col_character(),
        `danceability_%` = col_double(),
        `valence_%` = col_double(),
        `energy_%` = col_double(),
        `acousticness_%` = col_double(),
        `instrumentalness_%` = col_double(),
        `liveness_%` = col_double(),
        `speechiness_%` = col_double()
    )
)

spotify_songs_2023 |> 
    spec() # make column types explicit, 
# then copy to read_csv function and use argument col_types

rm(ess_data)

ess <- read_csv(
    file = "data/ess.csv",
    col_types =  cols(
        cntry = col_character(),
        cname = col_character(),
        cedition = col_double(),
        cproddat = col_character(),
        cseqno = col_double(),
        name = col_character(),
        essround = col_double(),
        edition = col_double(),
        idno = col_double(),
        dweight = col_double(),
        pspwght = col_double(),
        pweight = col_double(),
        tvtot = col_double(),
        rdtot = col_double(),
        nwsptot = col_double(),
        netuse = col_double(),
        polintr = col_double(),
        trstplt = col_double(),
        vote = col_double(),
        mmbprty = col_double(),
        happy = col_double(),
        health = col_double(),
        ctzcntr = col_double(),
        brncntr = col_double(),
        hhmmb = col_double(),
        gndr = col_double(),
        yrbrn = col_double(),
        edulvla = col_double(),
        edulvlb = col_double(),
        hinctnt = col_double(),
        hinctnta = col_double(),
        ipcrtiv = col_double(),
        imprich = col_double(),
        ipeqopt = col_double(),
        ipgdtim = col_double(),
        impenv = col_double(),
        imptrad = col_double()
    )
)

spec(ess)

ess_data |> problems()

str(ess_data)
colnames(ess_data)
glimpse(ess)


# save data ---------------------------------------------------------------------------------------------------------

write_rds(
    x = df_spotify,
    file = "repo/df-spotify.rds",
    compress = "gz"
)

df_spotify <- read_rds("repo/df-spotify.rds")


# exercise with real data -----------------------------------------------------------------------------------------

library(tidyverse)

imdb <- read_delim("data/imdb.csv", delim = ";", 
                   escape_double = FALSE, trim_ws = TRUE,
                   col_types = cols(
                       Title = col_character(),
                       title_year = col_double(),
                       budget = col_double(),
                       Gross = col_double(),
                       actor_1_name = col_character(),
                       actor_2_name = col_character(),
                       actor_3_name = col_character(),
                       actor_1_facebook_likes = col_double(),
                       actor_2_facebook_likes = col_double(),
                       actor_3_facebook_likes = col_double(),
                       IMDb_rating = col_double(),
                       genre_1 = col_character(),
                       genre_2 = col_character(),
                       genre_3 = col_character(),
                       MetaCritic = col_double(),
                       Runtime = col_double(),
                       CVotes10 = col_double(),
                       CVotes09 = col_double(),
                       CVotes08 = col_double(),
                       CVotes07 = col_double(),
                       CVotes06 = col_double(),
                       CVotes05 = col_double(),
                       CVotes04 = col_double(),
                       CVotes03 = col_double(),
                       CVotes02 = col_double(),
                       CVotes01 = col_double(),
                       CVotesMale = col_double(),
                       CVotesFemale = col_double(),
                       content_rating = col_character(),
                       Country = col_character()
                   ))
glimpse(imdb)

imdb |> 
    filter(Country == "USA")

imdb |>
    group_by(Country) |> 
    summarise(mean_rating = mean(IMDb_rating, na.rm = TRUE))

# Transform data -------------------------------------------------------------------------------------------------------


df_spotify |> 
    select("track" = track_name) |> 
    arrange(track)

df_spotify |> 
    rename("energy" = 'energy_%') |> 
    colnames()

df_spotify |>
    filter(bpm > 180) |> 
    select(track_name, artist_1, bpm)


df_spotify |>
    filter(bpm < 100) |> 
    select(track_name, artist_1, bpm) |> 
    arrange(bpm)

?dplyr_tidy_select # more options for selcting things

df_spotify |>
    select(-contains("%")) |> 
    colnames()
    
df_spotify |>
    mutate(
        release_date = make_date(released_year, released_month, released_day),
        after_2020 = ifelse(released_year > 2020, TRUE, FALSE)
    ) |> 
    select(release_date, after_2020)

# Summarise -------------------------------------------------------------------------------------------------------

df_spotify |> 
    summarise(
        artist_count_mean = mean(artist_count)
    )

df_spotify$streams <- as.numeric(df_spotify$streams)

df_spotify |> 
    summarise(
        streams_mean = mean(streams, na.rm = TRUE)
    )



# Recap of Data Grammar -------------------------------------------------------------------------------------------

library(tidyverse)
df_spotify <- read_rds("introduction-to-r/repo/my-tidied-data.rds")

# Let us focus on % values of the songs.
df_spotify |> colnames()

# How can we create a tibble, which only consists of %-columns?


df_spotify |> 
    select(
        ends_with("%")
    )

colnames(
    df_spotify |> 
        select(
            ends_with("%")
        ) |> 
        mutate(
            danceability = 'danceability_%' #this adds a new variable and keeps the old one
        )
)

df_spotify_plot <- df_spotify |> 
    select(
        `danceability_%`:`speechiness_%`
    )


# Select only the acousticness_% column to visualise it.

df_spotify_plot_acousticness <- df_spotify_plot |> 
    select(acousticness = `acousticness_%`)





# Visualise stuff -------------------------------------------------------------------------------------------------

# Patchwork (combine plots)
# https://patchwork.data-imaginist.com

library(ggplot2)
library(patchwork)

p1 <- ggplot(mtcars) + geom_point(aes(mpg, disp))
p2 <- ggplot(mtcars) + geom_boxplot(aes(gear, disp, group = gear))

p1 / p2



# Installing Fonts ------------------------------------------------------------------------------------------------

# # install.packages("extrafont")
# library(extrafont)
# font_import()  # Run this once to import fonts
# loadfonts(device = "win")  # For Windows users
# 
# ggplot(mpg, aes(x = displ, y = hwy)) +
# #     geom_point() +
#     theme_minimal(base_family = "Comic Sans MS")
