app_uri = 'https://carlitov.shinyapps.io/feature_networks/'

#' Return data frame with genius song names and ids
#'
#' @param query Character string that will be used to search genius
#'
#' @param n integer that specifies the number of results to return
#'
#' @export
getSongID <- function(query, n = 3)
{
  songs <- geniusr::search_song(query)[1:n,]
  songList <- songs$song_id
  names(songList) <- paste(songs$song_name, songs$artist_name, sep = " - ")

  return(songList)
}

#' Return data frame with spotify items
#'
#' @param query Character string that will be used to search spotify
#'
#' @param n integer that specifies the number of results to return
#'
#' @param type character string that specifies whether to search for artists, albums or playlists.
#'
#' @export
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



#' Return list of nodes and edges based on spotify artist id
#'
#' @param ID Character string. artist ID.
#'
#' @param progressBar logical that specifies whether a progressbar should be displayed. (only works in shiny)
#'
#' @param weighted logical that specifies whether edgelist should be a weighted edgelist
#'
#' @export
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
  edges <- data.frame(from = character(), to = character(), freq = integer())
  for (i in 1:nrow(nodes))
  {
    if (progressBar) shiny::incProgress(amount = 1/nrow(nodes), detail = nodes$name[i])
    nodeId <- nodes$id[i]
    nodeName <- nodes$name[i]

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




#' Return dataframe with urls of artist images
#'
#' @param IDs Character vector. list of spotify artist IDs
#'
#' @export
getImages <- function(IDs)
{
  n =  length(IDs)
  imageList = list()

  # spotify's API allows up to 50 artists in one request so I have to cut the list of IDS
  # up into n/50 parts of 50 and one final part with the remaining artists.

  # cutpoints can be interpreted as the index of the first element of the next chunk, plus one final index
  # e.g. of there were 160 artists, cutpoints would be c(1,51,101,151,161)
  cutpoints = c(seq(1, n, 50), n + 1)
  for(i in 1:(length(cutpoints) - 1))
  {
    # get images for the current chunk of max 50 artists:
    chunk = cutpoints[i]:(cutpoints[i + 1] - 1)
    images = spotifyr::get_artists(IDs[chunk])$images

    # edgecase: all artists in a chunk have no image, in that case we have to fill it with NULL's and skip to next iteration
    if (all(lapply(images, function(x){length(x) == 0})))
    {
      imageList[chunk] = list(NULL)
      next()
    }

    images = sapply(images,'[', 1,2)
    imageList = c(imageList, images)
  }

  # replace null elements by a spotify icon (some artists have no images)
  imageList[sapply(imageList, is.null)] <- "https://cdn.onlinewebfonts.com/svg/img_424253.png"

  return(data.frame(id = IDs, image = unlist(imageList), stringsAsFactors = FALSE))
}


#' Return list. contains vector of words in song, data frame of audio features, the title of the song and a
#'                          name of the artist
#'
#' @param ID Character. Genius ID of the song
#'
#' @export
getSongLyrics = function(ID)
{
  # find song on genius and spotify:
  song = geniusr::get_song(ID)$content
  spotify = spotifyr::search_spotify(paste(song$title, song$primary_artist$name), type = 'track')[1,]

  # get features that we are interested in
  features = spotifyr::get_track_audio_features(spotify$id)
  if (!is.na(features)) features <- features[c(1,2,6:10)]  # some trakcs have not data on audio features
  features = as.data.frame(features)

  # get a vector of lines in the song.i try multiple times because the function fails every so often
  # either something to do with the api or with the wrapper
  for (i in 1:5)
  {
    lines <- geniusr::get_lyrics_id(song$id)$line
    if (length(lines) > 0) break()
  }
  # parse lyics if the song has any
  if (length(lines) > 0)
  {
    lyrics <- lines %>%
      tm::removePunctuation() %>%     # remove punctuation
      tolower() %>%                   # convert to lowercase
      strsplit(split = ' ') %>%       # split up into words
      unlist()                        # make vector instead of list
  } else
  {
    lyrics <- NA
  }

  # remove empty strings (happens when there are double spaces)
  lyrics <- lyrics[!lyrics %in% ""]

  # return relevant data
  return(list(lyrics = lyrics,
              name = song$title,
              artistname = song$primary_artist$name,
              features = features)
  )
}


# function that returns vector containing words used in an album based on the name of the album and artist
# this one gets errors still, dont know why
#' Return list. contains vector of words in song, data frame of audio features, the title of the album and a
#'                          list of songs that could not be found
#'
#' @param ID Character. Genius ID of the song
#'
#' @export
getAlbumLyrics <- function(ID, progressBar = FALSE)
{
  tracks <- spotifyr::get_album_tracks(ID)
  album <- spotifyr::get_album(ID)

  omitted <- c()
  allLines <- c()
  for (i in 1:nrow(tracks))
  {
    shiny::incProgress(amount = 1/nrow(tracks), detail = tracks$name[i])

    if (progressBar) incProgress(1/nrow(tracks), detail = tracks$name[i])

    query <- paste(tracks$name[i], tracks$artists[[i]]$name[1])
    song <- geniusr::search_genius(query)$content[[1]]

    # sometimes the genius search result is not the actual song
    if(song$title == tracks$name[i])
    {
      songLines <- geniusr::get_lyrics_id(song$id)$line
      allLines <- c(allLines, songLines)
    } else
    {
      # add songname to list of songs that didnt match
      omitted <- c(omitted, tracks$name[i])
    }
  }


  lyrics <- allLines %>%
    tm::removePunctuation() %>%
    tolower() %>%
    strsplit(split=' ') %>%
    unlist()

  lyrics <- lyrics[!lyrics%in%""]

  features <- as.data.frame(spotifyr::get_track_audio_features(tracks$id)[c(1,2,6:10)])

  return(list(lyrics = lyrics,
              name = album$name,
              artistname =  album$artists$name,
              features = features,
              omitted = omitted)
  )
}



#' creates recommendation playlist ina tibble that contains spotify tracks.
#'
#' @param PlaylistID Character. spotify ID of the playlist to use as inspiration
#'
#' @param progressBar Logical indicating whether progressbar should be used (only words in shiny)
#'
#' @export
makePlaylist <- function(playlistID, progressBar = FALSE){

  # create dataframe with data about the playlist and create variable with list of artists.
  playlist <- spotifyr::get_playlist_audio_features('Karel Veldkamp', playlistID)
  artists <- playlist %>%
    tidyr::unnest(track.artists) %>%
    dplyr::select(name, id) %>%
    unique


  # sample 30 random artists (if n > 30)
  n <- nrow(artists)
  if (n > 30) sample <- sample(1:n, 30) else sample <- 1:n


  # create a pool of artist that have collaborated with the artists in the playlist, or that are related to them.
  artistPool <- data.frame()

  for (i in sample)
  {
    # increment progressbar (* .3 because the second loop takes longer)
    if (progressBar) shiny::incProgress(amount = .3 * 1/length(sample), detail = artists$name[i])

    collabs <- spotifyr::get_artist_albums(artists[i, 'id']) %>%
      dplyr::select(artists) %>%
      tidyr::unnest(artists) %>%
      dplyr::filter(!name %in% artists$name) %>%
      dplyr::select(id) %>%
      unique()

    if (nrow(collabs) > 10) collabs <- dplyr::sample_n(collabs, 10)

    #related <- spotifyr::get_related_artists(artists[i, 'id'])[1:5] %>%
    #  dplyr::filter(! name %in% artists$name) %>%
    #  dplyr::select(id)

    artistPool <- rbind(artistPool, collabs)
  }

  # get top tracks from each artist in the artist pool
  allTracks <- data.frame()
  for (i in 1:nrow(artistPool))
  {
    # increment progressbar in shin . (.5 because this loops takes longer then the first one)
    if (progressBar) shiny::incProgress(amount = .6 * 1/nrow(artistPool), message = 'downloading songs', detail = '')

    toptracks <- spotifyr::get_artist_top_tracks(artistPool[i,])
    if (length(toptracks) > 0){
      if (!toptracks$id %in% allTracks$id){
        tracks <- spotifyr::get_track_audio_features(toptracks$id[1:3])
        allTracks <- rbind(allTracks, tracks)
      }
    }
  }

  if (progressBar) shiny::incProgress(amount = .1, message = 'deciding which songs you might like')
  features <- c('danceability', 'energy', 'loudness', 'speechiness', 'acousticness', 'instrumentalness', 'liveness', 'valence')
  # get matrix with scaled audio features.
  audioFeatures <- as.matrix(allTracks[features])
  audioFeatures <- scale(audioFeatures)
  rownames(audioFeatures) <- allTracks$id

  # to pick which songs are most similar to the ones in the user playlist. I use k means clustering:

  # first save the audio features of songs in user playlist (scaled to a z score)

  playlistProfile <- scale(playlist[features])


  # then calculate the mean silhouette coefficient for differnt numbers of k to decide the optimal k value:
  scores <- 0  # first score is set to 0 because sillouette coefficient is not defined for k == 1
  maxCluster <- min(nrow(playlistProfile)-1,10) # try up to 10 means

  # this loop tries k means clustering with 2 up to 10 clusters:
  for (k in 2:maxCluster)
  {
    kmeans <- stats::kmeans(playlistProfile,k)
    sCoefs <- cluster::silhouette(kmeans$cluster, stats::dist(playlistProfile))[,3]
    scores <- c(scores, mean(sCoefs))
  }

  # I then see which k lead to the highest silhouette score and use that model.
  k <- which.max(scores)
  model <- stats::kmeans(playlistProfile, k)

  # determine how much songs should be chosen for each cluster:
  clusterSize <- plyr::count(model$cluster)$freq / length(model$cluster)
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

  return(spotifyr::get_tracks(ids))
}


#' Adds playlist to spotify account
#'
#' @param userid Character string. spotify ID of the user to add the list to
#'
#' @param token Character string. access token to the spotify api (has to be on behald of the userid)
#'
#' @param name Character string. name of the playlist to be made
#'
#' @param uris list of spotify song uris to add to playlist
#'
#' @export
addPlaylist <- function(userid, token, name, uris)
{
  # first create an empty playlist:
  url <- paste0('https://api.spotify.com/v1/users/', userid, '/playlists')
  body <- list(name=name)
  headers <- c('Authorization' = paste('Bearer', token), 'Content-Type' = 'application/json')

  r <- httr::POST(url = url,
                  body = body,
                  config = httr::add_headers(headers),
                  encode = 'json')

  # then fill it up with songs
  url <-  paste0('https://api.spotify.com/v1/playlists/', httr::content(r)$id ,'/tracks')

  body <- list(uris = uris)

  x <- httr::POST(url = url,
                  config = httr::add_headers(headers),
                  body = body,
                  encode = 'json')


  return(httr::content(r))

}



#' Get spotify access token
#'
#' @param code Character string. spotify authorization code
#'
#' @export
getAccessToken <- function(code)
{
  # request body should contain authorization flow type, the code and the redirect uri
  body <- list(grant_type = 'authorization_code',
               code = code,
               redirect_uri = app_uri)

  # the header has to contain clientID:clientsecret in base 64 encryption



  r <- httr::POST('https://accounts.spotify.com/api/token',
                  body = body,
                  httr::authenticate(Sys.getenv("SPOTIFY_CLIENT_ID"), Sys.getenv("SPOTIFY_CLIENT_SECRET")),
                  encode = 'form',
                  httr::accept_json(),
                  httr::config(http_version = 2))


  return(httr::content(r)$access_token)
}


#' Get current user info
#'
#' @param token Character string. Spotify access token belonging to current user.
#'
#' @export
getUserInfo <- function(token)
{
  req <- httr::GET(url = 'https://api.spotify.com/v1/me',
                   config = httr::add_headers('Authorization' = paste('Bearer', token)))

  return(httr::content(req))
}

# function that creates list of spotify playlists
#' Get list of current user's spotify playlists
#'
#' @param userid Character string. spotify user id of the current user
#'
#' @param token Character string. Spotify access token belonging to the current user
#'
#' @export
getUserPlaylists <- function(userid, token)
{

  url <- paste0("https://api.spotify.com/v1/users/", userid, '/playlists')

  # maximum playlists per request is 50 so we have to loop though cutpoints
  req <- httr::GET(url,
                   query = list(limit = 50),
                   config = httr::add_headers(c(Authorization = paste('Bearer',token)))
  )

  n <- httr::content(req)$total

  # the first chunk gets requested twice, but I thought this was more readable and it doesnt take much longer to run
  playlistList <- c()
  cutpoints <- seq(1, n, 50)

  for(i in 1:(length(cutpoints)))
  {
    # get images for the current chunk of max 50 artists:
    playlists <-httr::GET(url,
                          query = list(limit = 50, offset = cutpoints[i]),
                          config = httr::add_headers(c(Authorization = paste('Bearer',token)))
    )

    content <- httr::content(playlists)$items

    # get url for HD image and add to list of images
    ids <- sapply(content,'[', 5)
    names(ids) <- sapply(content, '[', 7)
    playlistList <- c(playlistList, ids)
  }

  return(playlistList)
}
