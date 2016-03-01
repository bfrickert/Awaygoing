
random.forest <- read.table('data/random.forest.tsv', sep='\t', stringsAsFactors = T, header = T)
joind.not.awaygoing <- read.table('data/joind.not.awaygoing.tsv', sep='\t', stringsAsFactors = T, header = T)
hate.per.city <- read.table('data/hate.per.city.tsv', sep='\t', stringsAsFactors = F, header=T)
kpi.color <- function(x){
  
  if (x < 101) return( 'green')
  else if (x < 201) return( 'orange')
  else return( 'red')
}