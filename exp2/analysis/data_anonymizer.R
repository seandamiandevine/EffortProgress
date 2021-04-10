set.seed(12563)
mappings <- list()
for(exp in c('a', 'b', 'c')) {
  files = list.files(paste0(exp, '/data'), '.csv')
  for(f in files){
    cat('experiment', exp, ':', match(f, files), '/', length(files), '\n')
    thisdata <- read.csv(paste0(exp, '/data/', f))
    thisid = strsplit(f, '_')[[1]][1]
    if(length(mappings[[thisid]]) != 0) {
      newid <- mappings[[thisid]]
    } else {
      newid <- paste(sample(c(letters, LETTERS, 1:9), 15), collapse = '')
      mappings[[thisid]] = newid
    }
    thisdata$id <- newid
    filename <- paste0(newid, '_',  paste0(strsplit(f, '_')[[1]][-1], collapse = '_'))
    write.csv(thisdata, paste0(exp, '/anonymized_data/', filename))
  }
}

mappingsdf = data.frame(unmasked_id = names(mappings), 
                        masked_id = as.character(unlist(mappings)))
write.csv(mappingsdf, 'anonymization_key.csv')
