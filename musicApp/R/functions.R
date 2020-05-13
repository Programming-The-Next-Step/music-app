library(spotifyr)
library(geniusr)
library(dplyr)
library(plyr)
library(tidyr)
library(jpeg)
library(visNetwork)
library(tm)

Sys.setenv(GENIUS_API_TOKEN = 'yYDySPPgWepb-YB3ikWztEzGA1828BGpk9xyjZE91FR6fjfQF71zSmv6tYc3y5gt')
Sys.setenv(SPOTIFY_CLIENT_ID = 'da1a67c279f04bc6a5996f6a7144e45c')
Sys.setenv(SPOTIFY_CLIENT_SECRET = '2d4b1432f0e449a89806fd1d80409f36')


####################################################################################################
# function that returns list of nodes and edges of an artist's collaboration network based on a name
getNetwork <- function(artist, progressBar = FALSE, weighted = FALSE)
{
  artistName = search_spotify(artist, limit = 1, type = 'artist')$name
  artistId = search_spotify(artist, limit = 1, type = 'artist')$id

  nodes = get_artist_audio_features(artistName, include_groups = c('album', 'single'))[c('artists', 'track_name')] %>%
    unique() %>%
    unnest(artists) %>%
    select(name, id) %>%
    count() %>%
    filter(!id == artistId)


  # here i make a matrix of edges (edgelist) by looping thtough each feature and seeing if they have mutual features with the selected artist:
  edges = data.frame(from = character(), to = character())
  for (i in 1:nrow(nodes))
  {
    if (progressBar) incProgress(amount = 1/nrow(nodes), detail = nodes$name[i])
    collabId = nodes$id[i]
    collabName = nodes$name[i]

    tryCatch({
      nodes2 = get_artist_audio_features(collabName, include_groups = c('album', 'single'))[c('artists', 'track_name')] %>%
        unique() %>%
        unnest(artists) %>%
        select(id) %>%
        filter(id != collabId & id != artistId) %>%
        filter(id %in% nodes$id)                           # only select features that are in list of main artist features

      if (nrow(nodes2) > 0)
      {
        edges = rbind(edges, data.frame(from = collabId, to = nodes2, stringsAsFactors = FALSE)) # add edges
      }
    },error=function(e){})
  }

  # sort each edge (so they are in the same order, and count duplicates to get weighted edgelist)
  if(weighted)
  {
    edges = t(apply(edges,1,sort))
    edges = plyr::count(edges)
  }


  network = list(nodes = nodes, edges = edges)

  return(network)
}


####################################################################################################
# function that returns a dataframe containing the urls of artist images based on a vector of ids
getImages <- function(artistIds)
{
  n =  length(artistIds)
  imageList = list()

  # spotify's API allows up to 50 artists in one request so I cut the list up into n/50 parts of 50 and one final part with the remaining artists
  cutpoints = c(seq(1, n, 50), n + 1)
  for(i in 1:(length(cutpoints)-1))
  {
    # get images for the current chunk of max 50 artists:
    chunk = cutpoints[i]:(cutpoints[i + 1] - 1)
    images = get_artists(artistIds[chunk])$images

    # get url for HD image
    images = sapply(images,'[', 1,2)

    imageList = c(imageList, images)
  }

  # replace null elements by a black spotify icon
  imageList[sapply(imageList, is.null)] <- "https://cdn.onlinewebfonts.com/svg/img_424253.png"

  imagesDF = data.frame(id = artistIds, image = unlist(imageList), stringsAsFactors = FALSE)
  return(imagesDF)
}

####################################################################################################
# function that returns vector containing words used in a song based on the name of the song and artist
getSongLyrics = function(songname, artist)
{
  # find song on genius and spotify:
  song = search_genius(paste(songname, artist))$content[[1]]
  spotify = search_spotify(paste(songname, artist), type = 'track')[1,]

  # get features that we are interested in
  features = get_track_audio_features(spotify$id)[c(1,2,6:10)]
  features = as.data.frame(features)

  #get a vector of lines in the song
  lines = get_lyrics_id(song$id)$line

  lyrics = lines %>%
    removePunctuation() %>%     # remove punctuation
    tolower() %>%               # convert to lowercase
    strsplit(split = ' ') %>%   # split up into words
    unlist()                    # make vector instead of list


  lyrics = lyrics[!lyrics %in% ""] # remove empty strings (happens when there are double spaces)

  # store relevant data in the data object
  data = list(lyrics = lyrics,
              name = song$title,
              artistname = song$primary_artist$name,
              features = features)

  return(data)
}


####################################################################################################
# function that returns vector containing words used in an album based on the name of the album and artist
# this one gets errors still, dont know why
getAlbumLyrics = function(albumname, artist, progressBar = FALSE)
{
  album = search_spotify(paste(artist, albumname), type = "album")[1,]
  tracks = get_album_tracks(album$id)

  allLines = c()
  spotify_ids = c()
  for (i in 1:nrow(tracks))
  {
    if (progressBar) incProgress(1/nrow(tracks), detail = tracks$name[i])

    query = paste(tracks$name[i], tracks$artists[[i]]$name[1])
    song = search_genius(query)$content[[1]]
    track_id = search_spotify(query, type = 'track')[1,]$id
    print(get_lyrics_id(song$id)$line)
    songLines = get_lyrics_id(song$id)$line
    allLines = c(allLines, songLines)
    spotify_ids = c(spotify_ids,track_id)
  }


  lyrics = allLines %>%
    removePunctuation() %>%
    tolower() %>%
    strsplit(split=' ') %>%
    unlist()

  lyrics = lyrics[!lyrics%in%""]

  features = as.data.frame(get_track_audio_features(spotify_ids)[c(1,2,6:10)])

  data = list(lyrics = lyrics,
              name = album$name,
              artistname =  album$artists[[1]]$name,
              features = features)

  return(data)
}
