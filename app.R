library(shiny)
library(visNetwork)
source('functions.R')


# api details are removed in the github version of this app
Sys.setenv(GENIUS_API_TOKEN = '<hidden>')
Sys.setenv(SPOTIFY_CLIENT_ID = '<hidden>')
Sys.setenv(SPOTIFY_CLIENT_SECRET = '<hidden>')


# this is javascript code that is used to redirect the user to the spotify login page
jsRedirect <- "Shiny.addCustomMessageHandler('redirect', function(message) {window.location = 'https://accounts.spotify.com/authorize?client_id=da1a67c279f04bc6a5996f6a7144e45c&scope=ugc-image-upload%20user-read-playback-state%20streaming%20user-read-email%20playlist-read-collaborative%20user-modify-playback-state%20user-read-private%20playlist-modify-public%20user-library-modify%20user-top-read%20user-read-currently-playing%20playlist-read-private%20user-follow-read%20app-remote-control%20user-read-recently-played%20playlist-modify-private%20user-follow-modify%20user-library-read&redirect_uri=https://carlitov.shinyapps.io/feature_networks/&response_type=code';});"




# UI page 1: lyrics -------------------------------------------------------



# UI page where you can plot lyrics to a song or an album
lyricsPage <- fluidPage(

    titlePanel(h1('Visualizing Lyrics', align = 'center')),
    
    br(),
    
    # Sidebar
    fluidRow(
        column(4,
               
               wellPanel(HTML(
                   '<b> Description </b>
                   <p> This page lets you visualize the lyrics to a song. After typing the name of the song or
                   artist you like into the query box, and choosing the song you meant from the dropdown menu, 
                   the app will create three plots. from left to right and top to bottom, you will see a wordcloud
                   of the words used in the songs (more frequent words are bigger), a barchart of the audio features
                   of the songs (these are based of spotifys audio feature database) and a barchart of the frequency 
                   of words. You can interact with the graph by changing parameters in the panel below.'
               )),
               
               wellPanel(
                   selectInput('datatype', label = 'what kind of data do you want to visualize?',choices = c('song','album'))
               ),
               
               conditionalPanel(condition = 'input.datatype == "song"',
                                textInput('songSrc', label = 'song')
               ),
               
               conditionalPanel(condition = 'input.songSrc.length > 0 && input.datatype == "song"',
                                selectInput('song', label = '', choices = c('')),
                                actionButton('searchSong', label = 'go')
               ),
               
               conditionalPanel(condition = 'input.datatype == "album"',
                                textInput('albumSrc', label = 'album')
               ),
               
               conditionalPanel(condition = '(input.albumSrc.length > 0  && input.datatype == "album")',
                                selectInput('album', label = '', choices = c('')),
                                actionButton('searchAlbum', label = 'go')
               ),
               
               br(),
               
               conditionalPanel(condition = 'input.searchSong > 0 || input.searchAlbum > 0', 
                         wellPanel(
                             checkboxInput('rmStop', 'remove stop words'),
                             sliderInput('nbar', 'Words in barchart', min = 5, max = 30, value = 20),
                             sliderInput('nCloud', 'Max. words in cloud', min = 20, max = 150, value = 80),
                             actionButton('seed', label = 'randomize wordcloud')
                         )
               )
        ),
        
        column(3,
               # Show output
               fluidRow(
                   plotOutput('cloud'),
                   width = 4
               )
        ),
        
        column(5,
               fluidRow(
                   plotOutput('features')
               )
        ),
        
        column(8,
               # Show output
               fluidRow(
                   plotOutput('barplot'),
                   width = 8
               )
        )
    )
)


# UI page 2: Networks -----------------------------------------------------



# page where you can visualize artist networks
networkPage <- fluidPage(
    
    titlePanel(h1('Collaboration networks', align = 'center')),
    
    br(),
    
    # Sidebar with a slider input for number of bins 
    fluidRow(
        column(3,
               wellPanel(HTML(
                   "<b> Description </b>
                   <p> This page lets you visualise the network of collaborations of a specific artist. 
                   say that you search for Stevie Wonder, you will see an image of all the artists that
                   have appeared on Stevie's songs. People who have worked with stevie more often will have a 
                   larger image. Additionally, you will see lines between the images of artists that have 
                   collaborated with each other. This results in a network graph of artist. You can also try to
                   zoom in on artists and oven drag them around! </p>
                   
                   <p> <b>Note:</b> Downloading this data might take a while, especially for artists who like to collaborate 
                   a lot (if you have some time on your hands, try snoop dogg ;)  )</p>"
               )),
               
               wellPanel(
                   textInput('artistSrc', label = 'What artists network do you want to analyse?')
               ),
               
               conditionalPanel(condition = 'input.artistSrc.length > 0',
                                selectInput('artist', label = '', choices = c('')),
                                actionButton('getnetwork', 'Find artist')
               ),
               
               conditionalPanel(condition = 'input.getnetwork > 0', 
                                sliderInput('nodeScale', label = 'Size of the nodes', min = 10, max = 100, value = 25), 
                                checkboxInput('rmSingles', label = 'Remove artists without connections')
               )
        ),
        
        
        column(9, 
               # Show a plot of the generated distribution
               wellPanel(
                   visNetworkOutput('graph')
               )
        )
    )
)


# UI page 3: Playlists ----------------------------------------------------



# page where you can create playlists
playlistPage <- fluidPage(
    
    titlePanel(h1('Creating playlists', align = 'center')),
    
    br(),
    

    # Sidebar with a slider input for number of bins 
    fluidRow(
        column(3,
               wellPanel(HTML(
                   '<b> Description </b>
                   <p> This page allows you to select one of your playlists and let the app create a new
                   playlist for you with music you might like. After creating the playlist the app will
                   show a list of songs that are in the newly generated playlist, and if you like what
                   you see, you can add the playlist to your spotify by clicking the button on the right. </p>'
               )),
               
               wellPanel(
                   selectizeInput('playlist', label = 'What playlist do you want to use to create recomendations?', choices = c('')),
                   actionButton('getPlaylist', label = 'go')
               )
        ),
        
        column(5,
               wellPanel(
                   tableOutput('playlist')
               )
        ),
        
        column(4,
               wellPanel(
                   actionButton('addList', 'add this playlist to my spotify')
               ), 
               
               conditionalPanel(condition = 'input.addList > 0',
                                'playlist added')
        )
    )
)





# UI: navigation ----------------------------------------------------------



ui <- navbarPage(tabPanel(title = 'Artist Networks', networkPage),
                 tabPanel(title = 'Lyrics', lyricsPage),
                 tabPanel(title =  'Playlists', playlistPage),
                 tabPanel(title = 'Log in'),
                 title = 'Music app',
                 id = 'page',
                 tags$script(HTML(jsRedirect)),
                 tags$head(tags$style(HTML("
                           .navbar-nav {float: none !important;}
                           .navbar-nav > li:nth-child(4) {float: right; background: #1ed760;}"
                           )))
                 )


# Server ------------------------------------------------------------------


server <- function(input, output, session) 
{
# Header 1: everything to do with authorization ---------------------------

    
    # if user logs in to spotify, spotify redirects them to my page with a code in the url. 
    # this reactive sees if this has happened, and saves user info and access token.
    authorization  <- eventReactive(getQueryString(), 
    {
        code <- getQueryString()$code
        token <- getAccessToken(code)
        user <- getUserInfo(token)
        
        # if user is logged in, add a list of their playlists as options on the playlist page and hide the log in button
        if (length(token) > 0) 
        {
            updateSelectizeInput(session, 'playlist', choices = getUserPlaylists(user$id, token))
            hideTab(inputId = "page", target = "Log in")
        }
        
        return(list(token = token, user = user))
    })
    
    # if user is logged in this renders a welcome message, otherwise it renders 'anonymous'
    output$user <- renderText(
    {
        if (length(authorization()$token) > 0)
        {
            firstname <- strsplit(authorization()$user$display_name, split = ' ')[[1]][1]
            paste('Welcome', firstname)
        } else 
        {
            print('anonymous')
        }
    })
    
    # the output can not be suspended because it is used to check whether the user is logged in
    outputOptions(output, 'user', suspendWhenHidden = FALSE)
    
    # redirect to spotify if user clicks on login button
    observeEvent({input$login}, 
    {
        session$sendCustomMessage('redirect', 'redirect')
    })
    
    # if user navigates to the playlist page without being logged in, send them back to the home page
    observeEvent(input$page,
    {
        
        if (input$page == 'Playlists' & length(authorization()$token) == 0)
        {
            showModal(modalDialog(
                title = 'login required',
                'you need to be logged in to spotify to use the playlist creator page.'
            ))
            updateTabsetPanel(session, 'page', 'Artist Networks')
        }
        if (input$page == 'Log in')
        {
            session$sendCustomMessage('redirect', 'redirect')
        }
        
    })
    


# Header 2: updating UI (with search suggestions) -------------------------------

    
    # render suggestions based on search query for song
    observeEvent(input$songSrc,
    {
        query <- input$songSrc
        if (nchar(query) > 0) updateSelectInput(session, 'song', choices = getSongID(query, n = 5))
    })
    
    # render suggestions based on search query for album
    observeEvent(input$albumSrc,
    {
        query <- input$albumSrc
        if (nchar(query) > 0) updateSelectInput(session, 'album', choices = getSpotifyID(query, n = 5, type = 'album'))
    })
    
    # render suggestions based on search query for artist
    observeEvent(input$artistSrc,
    {
        query <- input$artistSrc
        if (nchar(query) > 0) updateSelectInput(session, 'artist', choices = getSpotifyID(query, n = 5, type = 'artist'))
    })
    

# Header 3: obtaining data based on user input ----------------------------


    
    # get nodes, edges and images for the network of the artist that is being searched
    getNetworkData <- eventReactive(input$getnetwork, 
    {
        withProgress(value = 0, message = 'downloading collab data',
        {
            network <- getNetwork(input$artist, progressBar = TRUE, weighted = TRUE)
        })
        
        # throw error if artist has no collabs
        if (nrow(network$nodes) == 0)
        {
            showModal(modalDialog(
                title = 'No collaborations', {
                    'We could not fond any people who have collaborated with this artist'
                }))
            return()
        }
        
        images <- getImages(network$nodes$id)
                                    
        return(list(network, images))
    })
    
    
    # when search button is pressed, return list containing word frequency and audio features
    getLyricData <- eventReactive(list(input$searchSong, input$searchAlbum),
    {
        if(input$searchSong == 0 & input$searchAlbum == 0) return()
        
        if(input$datatype == 'song')
        {
            data <- getSongLyrics(input$song)
        } else if (input$datatype == 'album')
        {
            withProgress(value = 0, message = 'downloading lyrics',
                {data <- getAlbumLyrics(input$album)}
            )
            if (length(data$omitted) > 0)
            {
                showModal(modalDialog(
                    title = 'Songs omitted', {
                    HTML(paste('Not all songs are included in the results. We could not find lyrics
                          for', length(data$omitted), 'songs, so these are missing from the two
                          plots about the lyrics <br> <b> missing songs: </b> <br>', paste(data$omitted, collapse = '<br>'))
                    )
                }))
            }
        }
        return(data)
    })
    
    # create a playlist for the user
    getPlaylist <- eventReactive(input$getPlaylist, 
    {
        # draw a progressbar
        
        withProgress(value = 0, message = 'finding artists', {
            makePlaylist(input$playlist, progressBar = TRUE)
        })
    })
    
    # add the playlist to spotify if user choses to do so
    observeEvent(input$addList,
    {
        addPlaylist(userid = authorization()$user$id, token = authorization()$token, name = 'myList', uris = getPlaylist()$uri)
    })


# Header 4: drawing output -------------------------------------------------

    
    
    # draw network
    output$graph <- renderVisNetwork(
    {
        if (input$getnetwork == 0) return()
        
        
        # get data:
        network <- getNetworkData()[[1]]
        edges <- network$edges
        ids <- network$nodes$id
        names <- network$nodes$name
        images <- getNetworkData()[[2]]$image
        nCollabs <- network$nodes$freq
        mainArtist <- spotifyr::get_artist(input$artist)$name
        
        # Size of nodes and edges depend on the amount of collaborations, but the size of the biggest node/edge
        # should not be more then 4x/20X bigger then the smallest one (because it doesnt look good)
        nodeSizes <- pmin(nCollabs, 4)   
        edgeSizes <- pmin(edges[,3],20)
        
        
        # remove nodes without connections:
        if(input$rmSingles)
        {
            connectedNodes <- ids %in% as.matrix(edges[,1:2])
            
            ids <- ids[connectedNodes]
            images <- images[connectedNodes]
            names <- names[connectedNodes]
            nCollabs <- nCollabs[connectedNodes]
            nodeSizes <- nodeSizes[connectedNodes]
        }
            
        
        
        # title of edges has to be an empty list if there are no edges 
        if(nrow(edges) > 0) edgeNames <- paste(edges[,3], 'collaborations') else  edgeNames <- list()
        # title of nodes is artist name plus the amount of collabs with the main artist
        nodeInfo <- paste(names, '<br>', nCollabs, 'collaborations with', mainArtist)
    
            
        graphNodes <- data.frame(id = ids,
                            title = nodeInfo,    
                            shape = 'circularImage',
                            image = images,
                            size = input$nodeScale*nodeSizes/max(nodeSizes))


        graphEdges <- data.frame(from = edges[,1], 
                                  to = edges[,2],
                                  width = edgeSizes,
                                  title = edgeNames)

        
        
        visNetwork(graphNodes,graphEdges) %>% 
            visNodes(shapeProperties = list(useBorderWithImage = TRUE)) %>%
            visLayout(randomSeed = 2) %>%
            visPhysics(solver='repulsion')
            
        })
        

    
    # draw barplot of song lyrics
    output$barplot <- renderPlot(
    {
        # only run after click
        if (input$searchSong == 0 & input$searchAlbum == 0) return()
        
        lyrics = getLyricData()$lyrics
        if (input$rmStop) lyrics <- lyrics[!lyrics %in% tm::stopwords()]
        
        
        #get frequency of words
        frequencies <- table(lyrics)
        words <- names(frequencies)
        nbar <- input$nbar
        barplot(sort(frequencies,decreasing = T)[1:nbar], 
                las = 2, 
                cex.lab = 100,
                col = '#1ed760')
    })
    
    
    # draw wordcloud of song lyrics
    output$cloud <- renderPlot(
    {
        
        # only run after click
        if (input$searchSong == 0 & input$searchAlbum == 0) return()
        
        lyrics <- getLyricData()$lyrics
        if (input$rmStop) lyrics <- lyrics[!lyrics %in% tm::stopwords()]
        # get frequency of words
        frequencies <- table(lyrics)
        words <- names(frequencies)
        
        # set borders around plot
        par(mar = c(1,1,1,1))
        set.seed(input$seed)
        wordcloud::wordcloud(words, frequencies, max.words = input$nCloud, random.order = F, scale = c(4,.5))
    })
    
    # draw barplot of audio features
    output$features <- renderPlot(
    {
        
        # only run after click
        if (input$searchSong == 0 & input$searchAlbum == 0) return()
        
        # get features and calculate their means
        features <- getLyricData()$features
        features <- colMeans(features)
        
        # set plot margins and create plot
        par(mar = c(5,8,4,1))
        barplot(features, 
                horiz = TRUE, 
                las = 1, 
                xlim = c(0,1),
                col = '#1ed760')
    })
    
    # show a data frame of songs in the generated playlist
    output$playlist <- renderTable(
    {
        
        # only run after click
        if (input$getPlaylist == 0) return()
        
        playlist <- getPlaylist()
        
        # select the 3rd element of each element in the limit (the artists) and then take the first ones
        artists <- sapply(playlist$artists, '[', 3)
        primArtist <- sapply(artists, '[', 1)
        
        # return data frame with name and artists
        data.frame(name = playlist$name,
                   artist = primArtist)

    })
}

# Run the application 
shinyApp(ui = ui, server = server)


