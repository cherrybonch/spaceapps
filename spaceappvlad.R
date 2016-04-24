Text.Document <- read.csv("C:/Users/Home/Desktop/Text Document.txt", header=FALSE) #names of all asteroids in http://space.frieger.com/asteroids/asteroids/
text <- data.frame(NA)
for(i in 1:nrow(Text.Document)){
  if(as.character(Text.Document[i, 1]) != " ") {
    colnames(text) <- "name"
    t <- as.data.frame(as.character(Text.Document[i, 1]))
    colnames(t) <- "name"
    text <- rbind(text, t)
  }
}
text <- data.frame(text[2:930, ])

#################### парсинг сайта, создаётся датафрейм с периодом и названием астероида
aster.period <- apply(text, 1, function(x){
  x <- substr(x, start = 1, stop = nchar(as.character(x)) - 1 )
  x <- gsub(pattern = '\\(', x = x, replacement = "")
  x <- gsub(pattern = '\\)', x = x, replacement = "")
  x <- gsub(pattern = ' ', x = x, replacement = "-")
  site <- readHTMLTable(paste("http://space.frieger.com/asteroids/asteroids/",as.character(x), sep = ""), header = "<td>Rotation period</td>", as.data.frame = TRUE)
  if(length(site) > 1){
    t <- site[[2]][2]
    for(i in 1:nrow(t)){
      if(!is.null(grep(pattern = "hours", x = t[i ,1]))) period <- str_extract(t[i ,1], pattern = "[[:digit:]]+[:punct:][[:digit:]]+") 
    }
  } else period <- NA
  return(list(period, x))
})

data <- data.frame()
for(i in 1:929){
  data[i, 1] <- aster.period[[i]][1]
  data[i, 2] <- aster.period[[i]][2]
}

