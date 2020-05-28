#devtools::install_github('https://github.com/charlie86/spotifyr')
library(spotifyr)
library(geniusr)
library(dplyr)
library(plyr)
library(tidyr)
library(jpeg)
library(visNetwork)
library(tm)
library(httr)
library(wordcloud)
library(cluster)

# the code needs authorization to use the spotify and genius api. Ive hidden these because my repository is public.
# if you want to run the code you can ask me for them personally.
Sys.setenv(GENIUS_API_TOKEN = 'yYDySPPgWepb-YB3ikWztEzGA1828BGpk9xyjZE91FR6fjfQF71zSmv6tYc3y5gt')
Sys.setenv(SPOTIFY_CLIENT_ID = 'da1a67c279f04bc6a5996f6a7144e45c')
Sys.setenv(SPOTIFY_CLIENT_SECRET = '2d4b1432f0e449a89806fd1d80409f36')

app_uri = 'https://carlitov.shinyapps.io/feature_networks/'


# function that returns an n row data frame with artist names and ids
# is useful in the shiny app to give suggestion based on search query.
getArtistId <- function(query, n=5)
{
  results = spotifyr::search_spotify(query, type = 'artist', limit = n)
  df <- data.frame(name <- results$name,
                   id <- results$id)

  return(df)
}



# function that returns named list of genius ids based on a query (used in shiny app to select song)
getSongID <- function(query, n = 3)
{
  songs <- geniusr::search_song(query)[1:n,]
  songList <- songs$song_id
  names(songList) <- paste(songs$song_name, songs$artist_name, sep = " - ")

  return(songList)
}

# function that returns named list of spotify ids based on a query (used in shiny app to select artist/album)

getSpotifyID <- function(query, n = 3, type = c('artist', 'album', 'playlist'))
{
  results <- spotifyr::search_spotify(query, type = type, limit = n)
  resultList <- results$id

  if (type == 'album') author <- sapply(results$artists, '[[', 3)
  if (type == 'playlist') author <- results$owner.display_name
  if (type == 'artist') author <- ""

  names(resultList) <- paste(author, results$name, sep = " ")

  return(resultList)
}

# function that returns list of nodes and edges of an artist's collaboration network based on an artists id
# progressBar is used in the shiny app to create a progress bar
# if weigted, the edgelist becomes a weigted edgelist
getNetwork <- function(ID, progressBar = FALSE, weighted = FALSE)
{
  artist = spotifyr::get_artist(ID)

  nodes = spotifyr::get_artist_audio_features(artist$name, include_groups = c('album', 'single'))[c('artists', 'track_name')] %>%
    unique() %>%
    tidyr::unnest(artists) %>%
    dplyr::select(name, id) %>%
    plyr::count() %>%
    dplyr::filter(id != artist$id)


  # here i create an edgelist by looping thtough each feature and seeing if they have mutual features with the selected artist:
  edges = data.frame(from = character(), to = character(), freq = integer())
  for (i in 1:nrow(nodes))
  {
    if (progressBar) shiny::incProgress(amount = 1/nrow(nodes), detail = nodes$name[i])
    nodeId = nodes$id[i]
    nodeName = nodes$name[i]

    # this next part is wrapped in a trycatch because some artists have no collaborators on their songs which results in errors.
    tryCatch({
      collabs = spotifyr::get_artist_audio_features(nodeName, include_groups = c('album', 'single'))[c('artists', 'track_name')] %>%
        unique() %>%
        tidyr::unnest(artists) %>%
        dplyr::select(id) %>%
        dplyr::filter(id != nodeId & id != artist$id) %>%
        dplyr::filter(id %in% nodes$id)                           # only select features that are in list of main artist features

      edges = rbind(edges, data.frame(from = nodeId, to = collabs,stringsAsFactors = FALSE))
    },error=function(e){})
  }

  # sort each row (so they are easer to count), and count duplicates to get weighted edgelist
  if(weighted & nrow(edges) > 0)
  {
    edges = t(apply(edges,1,sort))
    edges = plyr::count(edges)
  }

  return(list(nodes = nodes, edges = edges))
}



# function that returns a dataframe containing the urls of artist images based on a vector of ids
# this can be used to add images as nodes to a network plot
getImages <- function(IDs)
{
  n =  length(IDs)
  imageList = list()

  # spotify's API allows up to 50 artists in one request so I have to cut the list of IDS
  # up into n/50 parts of 50 and one final part with the remaining artists.

  # cutpoints can be interpreted as the index of the first element of the next chunk, plus one final index
  # e.g. of there were 160 artists, cutpoints would be c(1,51,101,151,161)
  cutpoints = c(seq(1, n, 50), n + 1)
  for(i in 1:(length(cutpoints)-1))
  {
    # get images for the current chunk of max 50 artists:
    chunk = cutpoints[i]:(cutpoints[i + 1] - 1)
    images = get_artists(IDs[chunk])$images

    # get url for HD image and add to list of images
    images = sapply(images,'[', 1,2)
    imageList = c(imageList, images)
  }

  # replace null elements by a spotify icon (some artists have no images)
  imageList[sapply(imageList, is.null)] <- "https://cdn.onlinewebfonts.com/svg/img_424253.png"

  return(data.frame(id = IDs, image = unlist(imageList), stringsAsFactors = FALSE))
}


# function that returns vector containing words used in a song based on the name of the song and artist
getSongLyrics = function(ID)
{
  # find song on genius and spotify:
  song = geniusr::get_song(ID)$content
  spotify = search_spotify(paste(song$title, song$primary_artist$name), type = 'track')[1,]

  # get features that we are interested in
  features = get_track_audio_features(spotify$id)[c(1,2,6:10)]
  features = as.data.frame(features)

  #get a vector of lines in the song
  lines = get_lyrics_url(song$url)$line

  lyrics = lines %>%
    tm::removePunctuation() %>%     # remove punctuation
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


# function that creates a playlist based on another playlist which is inputted by the user
# returns a list of uris of the songs in the playlist
makePlaylist <- function(playlistID){

  # create dataframe with data about the playlist and create variable with list of artists.
  playlist = get_playlist_audio_features('Karel Veldkamp', playlistID)
  artists = playlist %>%
    unnest(track.artists) %>%
    dplyr::select(name, id) %>%
    unique


  # sample 30 random artists (if n > 30)
  n = nrow(artists)
  if (n > 30) sample <- sample(1:n, 30) else sample <- 1:n


  # create a pool of artist that have collaborated with the artists in the playlist, or that are related to them.
  artistPool = data.frame()
  for (i in sample)
  {
    collabs = spotifyr::get_artist_albums(artists[i, 'id']) %>%
      dplyr::select(artists) %>%
      unnest(artists) %>%
      dplyr::filter(!name %in% artists$name) %>%
      dplyr::select(id) %>%
      unique()

    if (nrow(collabs) > 10) collabs <- dplyr::sample_n(collabs, 10)

    #related = spotifyr::get_related_artists(artists[i, 'id'])[1:5] %>%
    #  dplyr::filter(! name %in% artists$name) %>%
    #  dplyr::select(id)

    artistPool = rbind(artistPool, collabs)
  }

  # get top tracks from each artist in the artist pool
  allTracks = data.frame()
  for (i in 1:nrow(artistPool))
  {
    toptracks <- spotifyr::get_artist_top_tracks(artistPool[i,])
    if (length(toptracks) > 0){
      if (!toptracks$id %in% allTracks$id){
        tracks <- spotifyr::get_track_audio_features(toptracks$id[1:3])
        allTracks <- rbind(allTracks, tracks)
      }
    }
  }
  features <- c('danceability', 'energy', 'loudness', 'speechiness', 'acousticness', 'instrumentalness', 'liveness', 'valence')
  # get matrix with scaled audio features.
  audioFeatures <- as.matrix(allTracks[features])
  audioFeatures <- scale(audioFeatures)
  rownames(audioFeatures) <- allTracks$id

  # to pick which songs are most similar to the ones in the user playlist. I use k means clustering:

  # first save the audio features of songs in user playlist (scaled to a z score)

  playlistProfile <- scale(playlist[features])


  # then calculate the mean silhouette coefficient for differnt numbers of k to decide the optimal k value:
  scores = 0  # first score is set to 0 because sillouette coefficient is not defined for k == 1
  maxCluster = min(nrow(playlistProfile)-1,10) # try up to 10 means

  # this loop tries k means clustering with 2 up to 10 clusters:
  for (k in 2:maxCluster)
  {
    kmeans = stats::kmeans(playlistProfile,k)
    sCoefs = cluster::silhouette(kmeans$cluster, dist(playlistProfile))[,3]
    scores = c(scores, mean(sCoefs))
  }

  # I then see which k lead to the highest silhouette score and use that model.
  k <- which.max(scores)
  model <- stats::kmeans(playlistProfile, k)

  # determine how much songs should be chosen for each cluster:
  clusterSize <- count(model$cluster)$freq / length(model$cluster)
  clusterSongs <- floor(clusterSize * 30)

  # finally, i loop through the amount of clusters to see how scose each song is to each cluster
  ids <- c()
  for (i in 1:k)
  {
    # calculate euclidean distance of each song to the cluster center:
    dist <- rowSums((audioFeatures - model$centers[i,])^2)
    # i sort the data so shortest distances come first, and save the uris of the first few songs in a variable
    bestSongs <- sort(dist)[1:clusterSongs[i]]
    ids <- c(ids, names(bestSongs))
  }
  tracks = spotifyr::get_tracks(ids)
  return(tracks)
}



addPlaylist <- function(userid, token, name, uris)
{
  # first create an empty playlist:
  url <- paste0('https://api.spotify.com/v1/users/', userid, '/playlists')
  body <- list(name=name)
  headers <- c('Authorization' = paste('Bearer', token), 'Content-Type' = 'application/json')

  r <- httr::POST(url = url,
                  body = body,
                  config = add_headers(headers),
                  encode = 'json')
  content(r)

  # then fill it up with songs
  url =  paste0('https://api.spotify.com/v1/playlists/', content(r)$id ,'/tracks')

  body = list(uris = uris)

  x = POST(url = url,
           config = add_headers(headers),
           body = body,
           encode = 'json')


  return(content(r))

}

# this is javascript code that is used to redirect the user to the spotify login page
jsRedirect <- "Shiny.addCustomMessageHandler('redirect', function(message) {window.location = 'https://accounts.spotify.com/authorize?client_id=da1a67c279f04bc6a5996f6a7144e45c&scope=ugc-image-upload%20user-read-playback-state%20streaming%20user-read-email%20playlist-read-collaborative%20user-modify-playback-state%20user-read-private%20playlist-modify-public%20user-library-modify%20user-top-read%20user-read-currently-playing%20playlist-read-private%20user-follow-read%20app-remote-control%20user-read-recently-played%20playlist-modify-private%20user-follow-modify%20user-library-read&redirect_uri=https://carlitov.shinyapps.io/feature_networks/&response_type=code';});"



# this function uses the authorization code for an acces token that we can use to get user data:
getAccessToken <- function(code)
{
  # request body should contain authorization flow type, the code and the redirect uri
  body <- list(grant_type = 'authorization_code',
               code = code,
               redirect_uri = app_uri)

  # the header has to contain clientID:clientsecret in base 64 encryption



  r <- httr::POST('https://accounts.spotify.com/api/token',
                  body = body,
                  authenticate(Sys.getenv("SPOTIFY_CLIENT_ID"), Sys.getenv("SPOTIFY_CLIENT_SECRET")),
                  encode = 'form',
                  accept_json(),
                  httr::config(http_version = 2))


  return(content(r)$access_token)
}

# function that gets user info
getUserInfo <- function(token)
{
  req <- GET(url = 'https://api.spotify.com/v1/me',
             config = add_headers('Authorization' = paste('Bearer', token)))

  return(content(req))
}




# function that creates list of spotify playlists
getUserPlaylists <- function(userid, token)
{
  url <- paste0("https://api.spotify.com/v1/users/", userid, '/playlists')

  # maximum playlists per request is 50 so we have to loop though cutpoints
  req <- GET(url, query = list(limit = 50), config = add_headers(c(Authorization = paste('Bearer',token))))
  n <- content(req)$total
  print(n)

  # the first chunk gets requested twice, but I thought this was more readable and it doesnt take much longer to run
  playlistList = c()
  cutpoints = seq(1, n, 50)
  for(i in 1:(length(cutpoints)))
  {
    # get images for the current chunk of max 50 artists:
    playlists = GET(url, query = list(limit = 50, offset = cutpoints[i]), config = add_headers(c(Authorization = paste('Bearer',token))))
    content = content(playlists)$items
    # get url for HD image and add to list of images
    ids = sapply(content,'[', 5)
    names(ids) <- sapply(content, '[', 7)
    playlistList = c(playlistList, ids)
  }

  return(playlistList)
}
