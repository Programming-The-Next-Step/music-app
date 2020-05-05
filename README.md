*music-app*
I'm going to make a shiny application with several features based around music. these features include
* Visualising the most frequent used words in a song or album
* Visualising the network of collaborations of a particular artist
* Creating a Spotify playlist for the user based on their preferences

The goal is to write helper functions for these tasks and combine them into a package, which can then be used to create the shiny app. 

If  i run out of time, i will drop the playlist creator (because i have no experience with this and my idea is the least concrete). 
If i have time left i will focus on extra features. Maybe i could get the images from an album cover to use them in visualizations, 
also i could make the visualisations more graphically attractive and make different interactive graphical parameters in shiny. 

*the code*
I will use R, and the app will make use of spotifyR and geniusR to interact with the spotify and genius APIs. I will use visNetwork for 
the network graphs, and tm for parsing lyric data. I will probably find more packages that i need in the process of making the application.

These are the most important functions i will need:

* Scraping song lyrics
	function that returns vector of words based on an artist and song name

* Scraping album lyrics
	function that returns vector of words based on artsit and album name (possibly using the scrape song function)

* Scraping collaboration network
	Function that returns nodes and edges based on an artist's name. Nodes being all the artists he/she has collaborated with 
	and edges being all the collaborations that those artist have with eachother. 


Then for the playlist maker, i want the user to enter on of their playlists on spotify and make one that has the same taste.

* Get similar songs
	A function that gets a list of songs from artists that are found using the 'related artists' feature of the spotify api

* match songs
	A function that sees what songs closely resemble the songs in the users playlist. I want to do this using the song features in the spotify database,
	But i dont know yet how i will match them exactly. I might first try songs that are close to the multivariate mean of the users playlist,
	and try to use something like clustering if i have time left. 

Of course i also need to do the visualisations themselves, but i think it would be better to code those in the shiny app main function and not in 
seperate files/packages.