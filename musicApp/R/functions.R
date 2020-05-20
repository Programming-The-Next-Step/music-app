library(spotifyr)
library(geniusr)
library(dplyr)
library(plyr)
library(tidyr)
library(jpeg)
library(visNetwork)
library(tm)

# the code needs authorization to use the spotify and genius api. Ive hidden these because my repository is public.
# if you want to run the code you can ask me for them personally.
Sys.setenv(GENIUS_API_TOKEN = '<hidden>')
Sys.setenv(SPOTIFY_CLIENT_ID = 'da1a67c279f04bc6a5996f6a7144e45c')
Sys.setenv(SPOTIFY_CLIENT_SECRET = '<hidden>')



# getting network of colaborations -----------------------------------------------------


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



# getting images for the network -----------------------------------------


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


# getting lyrics to a song -----------------------------------------------


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


# creating a playlist based on a user selected playlist -------------------


# function that creates a playlist based on another playlist which is inputted by the user
# currently the playlist is made on my own spotify profile (https://open.spotify.com/user/1117862156?si=OMhP-ANnSae9ijUFoqT_ew)
# because i havent found out how i can let someone without a developper account log in.
# if you want to try the function you need a playlist ID, which you can get by sharing the link to a playlist
# (id is the letters/numbers in the url between 'playlist/' and '?si=') note that the playlist you use has to be public

makePlaylist <- function(playlistID){

  # create dataframe with data about the playlist and create variable with list of artists.
  playlist = get_playlist_audio_features('Karel Veldkamp', playlistID)
  artists = playlist %>%
    unnest(track.artists) %>%
    select(name, id) %>%
    unique


  # sample 30 artists (if n > 30)
  n = nrow(artists)
  if (n > 30)
  {
    sample <- sample(1:n, 30)
  }else
  {
    sample <- 1:n
  }

  # create a pool of artist that have collaborated with the artists in the playlist, or that are related to them.
  artistPool = data.frame()
  for (i in sample)
  {
    collabs = get_artist_albums(artists[i, 'id']) %>%
      select(artists) %>%
      unnest(artists) %>%
      filter(!name %in% artists$name) %>%
      select(id) %>%
      unique()

    related = get_related_artists(artists[i, 'id'])[1:5,] %>%
      filter(!name %in% artists$name) %>%
      select(id)

    artistPool = rbind(artistPool, collabs, related)
  }

  # get top tracks from each artist in the artist pool
  allTracks = data.frame()
  for (i in 1:nrow(artistPool))
  {
    toptracks <- get_artist_top_tracks(artistPool[i,])
    if (length(toptracks)>0){
      if (! toptracks$id %in% allTracks$id){
        tracks <- get_track_audio_features(toptracks$id)
        allTracks <- rbind(allTracks, tracks)
      }
    }
  }

  # get matrix with scaled audio features.
  audioFeatures = as.matrix(allTracks[features])
  audioFeatures = scale(audioFeatures)
  rownames(audioFeatures) <- allTracks$uri

  # to pick which songs are most similar to the ones in the user playlist. I use k means clustering:

  # first save the audio features of songs in user playlist (scaled to a z score)
  features = c('danceability', 'energy', 'loudness', 'speechiness', 'acousticness', 'instrumentalness', 'liveness', 'valence')
  playlistProfile = scale(playlist[features])


  # then calculate the mean silhouette coefficient for differnt numbers of k to decide the optimal k value:
  scores = 0  # first score is set to 0 because sillouette coefficient is not defined for k == 1
  maxCluster = min(nrow(playlistProfile)-1,10) # try up to 10 means

  # this loop tries k means clustering with 2 up to 10 clusters:
  for (k in 2:maxCluster)
  {
    kmeans = kmeans(playlistProfile,k)
    sCoefs = silhouette(kmeans$cluster, dist(playlistProfile))[,3]
    scores = c(scores, mean(sCoefs))
  }

  # I then see which k lead to the highest silhouette score and use that model.
  k = which.max(scores)
  model = kmeans(playlistProfile, k)

  # determine how much songs should be chosen for each cluster:
  clusterSize <- count(model$cluster)$freq / length(model$cluster)
  clusterSongs <- floor(clusterSize * 30)

  # finally, i loop through the amount of clusters to see how scose each song is to each cluster
  uris <- c()
  for (i in 1:k)
  {
    # calculate euclidean distance of each song to the cluster center:
    dist <- rowSums((audioFeatures - model$centers[i,])^2)
    # i sort the data so shortest distances come first, and save the uris of the first few songs in a variable
    bestSongs <- sort(dist)[1:clusterSongs[i]]
    uris <- c(uris, names(bestSongs))
  }

  # then all is left is to create a playlist. Right now this playlist is made on my own account. I hope to be able to let users
  # log in and create playlists on their own accounts later.
  ID <- create_playlist('1117862156',name = 'generated playlist', authorization = get_spotify_authorization_code(scope=scopes[-11]))$id
  add_tracks_to_playlist(ID, uris = uris, authorization = get_spotify_authorization_code(scope=scopes[-11]))
}


