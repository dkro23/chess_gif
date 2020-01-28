#####
### Create network for chess starting position
#####

# Load packages

library(tidyverse)
library(igraph)
library(ggraph)
library(dplyr)
library(stringr)
library(png)
library(magick)

### Attempt with adjancency matrix

## Create adjacency matrix for white

# Create blank matrix

opening<-matrix(0,nrow=64,ncol = 64)
letters<-c("a","b","c","d","e","f","g","h")
squares<-c()
for (i in seq_along(1:8)){
  x<-paste(letters[i],seq(1:8),sep="")
  squares<-c(squares,x)
}
squares

rownames(opening)<-squares
colnames(opening)<-squares

# Create a board matrix (=1 signifies a piece occupies that square)

opening.board<-t(matrix(c(rep(1,16),rep(0,32),rep(1,16)),nrow = 8,ncol = 8))
rownames(opening.board)<-c(8:1)
colnames(opening.board)<-letters


# Define some functions

name2row<-function(x){
  z<-c()
  for(i in seq_along(1:NROW(x))){
  if(is.na(x[i])){z[i]<-NA}else{
    if(nchar(x[i])!=2){z[i]<-NA}else{
  require(stringr)
  letter<-substr(x[i],1,1)
  number<-as.numeric(substr(x[i],2,2))
  number<-ifelse(number %in% c(1:8),number,NA)
  letters<-c("a","b","c","d","e","f","g","h")
  num<-((grep(letter,letters)-1)*8)+number
  z[i]<-num}}
  }
  return(z)
}
name2row("e0")
name2row("a3")
name2row(c("a3","c2","f1"))
row2name(name2row(c("a3","c2","f1")))
name2row("1")
name2row("aaa")

row2name<-function(x){
  z<-c()
  for (i in seq_along(1:NROW(x))){
  if(is.na(x[i]==T)){z[i]<-NA}else{
  letnum<-ceiling(x[i]/8)
  letter<-letters[letnum]
  num<-x[i]-(8*(letnum-1))
  z[i]<-paste(letter,num,sep="")}
  }
  return(z)
}
row2name(63)
colnames(opening)[63]

# Start filling it in: Pawns

pawn2attack<-function(x){ #enter pawn location, ie "b3"
  q<-name2row(x)
  attack1<-q-8+1
  attack2<-q+8+1
  attack1<-ifelse(substr(x,1,1)=="a",NA,attack1)
  attack2<-ifelse(substr(x,1,1)=="h",NA,attack2)
  return(c(attack1,attack2))
}
pawn2attack("a2")
row2name(pawn2attack("a2"))


pawns<-paste(letters,2,sep = "")
for (i in seq_along(1:8)){
  attack.to<-pawn2attack(pawns[i])
  attack.from<-name2row(pawns[i])
  opening[attack.from,attack.to[1]]<-1
  opening[attack.from,attack.to[2]]<-1
}
view(opening)


# Start filling it in: King

king2attack<-function(z){
  x<-substr(z,1,1)
  y<-as.numeric(substr(z,2,2))
  letnum<-grep(x,letters)
  attack.name<-c(paste(x,y+1,sep=""),paste(x,y-1,sep="")
                 ,paste(letters[letnum-1],y,sep=""),paste(letters[letnum+1],y,sep=""),
                 paste(letters[letnum-1],y-1,sep=""),paste(letters[letnum-1],y+1,sep=""),
                 paste(letters[letnum+1],y+1,sep=""),paste(letters[letnum+1],y-1,sep=""))
  return(name2row(attack.name))
}
w<-king2attack("a1")
row2name(w)


v<-"e1"
w<-king2attack(v)
row2name(w)

opening[name2row("e1"),king2attack("e1")]<-1
view(opening)



# Start filling it in: Knight

knight2attack<-function(z){
  x<-substr(z,1,1)
  y<-as.numeric(substr(z,2,2))
  letnum<-grep(x,letters)
  attack.name<-c(paste(letters[letnum-1],y+2,sep=""),paste(letters[letnum+1],y+2,sep="")
                 ,paste(letters[letnum-2],y+1,sep=""),paste(letters[letnum-2],y-1,sep=""),
                 paste(letters[letnum+2],y+1,sep=""),paste(letters[letnum+2],y-1,sep=""),
                 paste(letters[letnum-1],y-2,sep=""),paste(letters[letnum+1],y-2,sep=""))
  return(name2row(attack.name))
}
knight2attack("g1")
row2name(knight2attack("g1"))

opening[name2row("b1"),knight2attack("b1")]<-1
opening[name2row("g1"),knight2attack("g1")]<-1

# Start filling it in: Rook 

chess_cutter<-function(vec,position){ #vector of 0s and 1s be able to subset vector into cutoff by 1s with position within the subset
    if(sum(vec)==0){
      m<-1
      m<-position-m
      n<-8-position
      return(c(m,n))
    }else{
  if(position==1){
    #lower
    lower.vec<-vec[position+1:NROW(vec)]
    n<-min(grep(1,lower.vec))
    
    #combine
    
    return(c(0,n))
  }else{
    if(position==8){
      #upper
      upper.vec<-vec[1:position-1]
      m<-max(grep(1,upper.vec))
      m<-position-m
      #combine
      
      return(c(m,0))
    }else{
      #upper
      upper.vec<-vec[1:position-1]
      m<-max(grep(1,upper.vec))
      m<-position-m
      
      #lower
      lower.vec<-vec[position+1:NROW(vec)]
      n<-min(grep(1,lower.vec))
      
      #combine
      
      return(c(m,n))
    }}
  }
  
  
}
t<-c(1,1,0,0,0,0,1,1)
chess_cutter(t,5)

t<-rep(0,8)
chess_cutter(t,1)


rook2attack<-function(rook,board){
  x<-substr(rook,1,1)
  y<-as.numeric(substr(rook,2,2))
  
  letnum<-grep(x,letters)
  column<-board[,letnum]
  rankc<-board[9-y,]
  position<-board[9-y,letnum]
  
  w1<-chess_cutter(column,9-y)
  w2<-chess_cutter(rankc,letnum)
  
  k<-c(paste(x,y-w1[2],sep=""),
  paste(x,y+w1[1],sep=""),
  paste(letters[letnum-w2[1]],y,sep=""),
  paste(letters[letnum+w2[2]],y,sep=""))
  
  r<-y-w1[2]
  p<-y+w1[1]
  q<-letnum-w2[1]
  s<-letnum+w2[2]
  
  k<-c(paste(x,c(r:p),sep=""),
  paste(letters[q:s],y,sep = ""))
  
  return(k)
  #return(w2)
  
  a<-c(9-y-w1[1]:9-y+w1[2])
  
  #return(c(r:p))
}
rook2attack("a1",opening.board)
rook2attack("e4",opening.board)
name2row(rook2attack("e4",opening.board))
row2name(name2row(rook2attack("e4",opening.board)))


opening[name2row("a1"),name2row(rook2attack("a1",opening.board))]<-1
opening[name2row("h1"),name2row(rook2attack("h1",opening.board))]<-1

# Start filling it in: Bishop

squares.matrix<-matrix(squares,nrow = 8,ncol=8)

diag_vector<-function(x){ #function for taking diagonal on vector or matrix
  if (is.vector(x)==T){return(x[1])}else{
    return(diag(x))
  }
}
diag_vector(squares.matrix)
diag_vector(diag_vector(squares.matrix))


chess_diag<-function(x){  # function to get diagonals that a piece covers from its location
  w<-substr(x,1,1)
  c<-grep(w,letters)
  r<-as.numeric(substr(x,2,2))
  
  # upper left
  upleft<-mirror.matrix(t(mirror.matrix(squares.matrix[1:r,1:c])))
  # lower right
  lowerright<-squares.matrix[r:8,c:8]
  #upper right
  upperright<-squares.matrix[1:r,c:8]
  if(is.vector(upperright)==T){upperright<-x}else{
    upperright<-mirror.matrix(t(upperright))
  }
  # lower left
  lowerleft<-squares.matrix[r:8,1:c]
  if (is.vector(lowerleft)==T){lowerleft<-x}else{
    lowerleft<-mirror.matrix(lowerleft)
  }

  #z<-c(diag(upleft),diag(lowerright),diag(upperright),diag(lowerleft))
  
  #z<-z[!duplicated(z)]
  #return(z)
  
  z<-list(diag_vector(upleft),diag_vector(lowerright),diag_vector(upperright),diag_vector(lowerleft))
  return(z)
}
chess_diag("c1")
chess_diag("d8")
chess_diag("a1")

occupied_squares<-function(board){ # get list of names of squares occupied by pieces from a board configuration
  occ<-c()
  for(i in seq_along(1:8)){
    temp<-c()
    for (j in seq_along(1:8)){
      if (board[i,j]==1){temp[j]<-paste(colnames(board)[j],rownames(board)[i],sep="")}else{
        temp[j]<-NA}
    }
    occ<-c(occ,temp)
  }
  occ<-occ[!is.na(occ)]
  return(occ)
}
occupied_squares(opening.board)



bishop2attack<-function(bishop,board){
  x<-substr(bishop,1,1)
  y<-as.numeric(substr(bishop,2,2))
  letnum<-grep(x,letters)
  
  sq<-occupied_squares(board)
  
  poss<-chess_diag(bishop)
  
  poss2<-list()
  for (i in c(1:4)){
    poss2[[i]]<-ifelse(poss[[i]] %in% sq,1,0)  
  }
  
  poss3<-list()
  for (i in c(1:4)){
    s<-grep(1,poss2[[i]])
    if(length(s)==1){poss3[[i]]<-poss[[i]]}else{
      poss3[[i]]<-poss[[i]][1:s[2]]
    }  
  }
  
  z<-c(poss3[[1]],poss3[[2]],poss3[[3]],poss3[[4]])
  z<-z[!duplicated(z)]

  return(z)
}
bishop2attack("c1",opening.board)

opening[name2row("c1"),name2row(bishop2attack("c1",opening.board))]<-1
opening[name2row("f1"),name2row(bishop2attack("f1",opening.board))]<-1


# Start filling it in: Queen

queen2attack<-function(queen,board){
  a<-rook2attack(queen,board)
  b<-bishop2attack(queen,board)
  c<-c(a,b)
  c<-c[!duplicated(c)]
  return(c)
}
queen2attack("d1",opening.board)

opening[name2row("d1"),name2row(queen2attack("d1",opening.board))]<-1

# Fill in blacks pieces

opening_black<-matrix(0,nrow=64,ncol = 64)
rownames(opening_black)<-squares
colnames(opening_black)<-squares

pawn2attack_black<-function(x){ #enter pawn location, ie "b3"
  q<-name2row(x)
  attack1<-q-9
  attack2<-q+7
  attack1<-ifelse(substr(x,1,1)=="a",NA,attack1)
  attack2<-ifelse(substr(x,1,1)=="h",NA,attack2)
  return(c(attack1,attack2))
}
pawn2attack_black("a7")
row2name(pawn2attack_black("a7"))

pawns_black<-paste(letters,7,sep = "")
for (i in seq_along(1:8)){
  attack.to<-pawn2attack_black(pawns_black[i])
  attack.from<-name2row(pawns_black[i])
  opening_black[attack.from,attack.to[1]]<-1
  opening_black[attack.from,attack.to[2]]<-1
}

opening_black[name2row("a8"),name2row(rook2attack("a8",opening.board))]<-1
opening_black[name2row("h8"),name2row(rook2attack("h8",opening.board))]<-1

opening_black[name2row("b8"),knight2attack("b8")]<-1
opening_black[name2row("g8"),knight2attack("g8")]<-1

opening_black[name2row("c8"),name2row(bishop2attack("c8",opening.board))]<-1
opening_black[name2row("f8"),name2row(bishop2attack("f8",opening.board))]<-1

opening_black[name2row("d8"),name2row(queen2attack("d8",opening.board))]<-1
opening_black[name2row("e8"),king2attack("e8")]<-1

# Create a network object

opening_network<-graph_from_adjacency_matrix(opening,mode = "directed")
opening_network
plot(opening_network)
plot(opening_network,vertex.cex=1)

opening_network_black<-graph_from_adjacency_matrix(opening_black,mode = "directed")


# Get Edgelist and nodelist

edge_list<-get.edgelist(opening_network)
edge_list<-as.data.frame(edge_list)
edge_list$color<-"white"
names(edge_list)[1:2]<-c("to","from")

edge_list_black<-get.edgelist(opening_network_black)
edge_list_black<-as.data.frame(edge_list_black)
edge_list_black$color<-"black"
names(edge_list_black)[1:2]<-c("to","from")

edge_list_full<-rbind(edge_list,edge_list_black)

node_list<-squares

opening_network2<-graph_from_data_frame(d = edge_list_full, vertices = node_list, directed = T)
  
# Plot in ggraph

ggraph(opening_network,layout = "kk") +  geom_node_point(size=3) +
  geom_edge_link() +  geom_edge_link(arrow = arrow(length = unit(3, 'mm')), end_cap = circle(1.5, 'mm'))  +
  geom_node_text(label=colnames(opening),size=3,repel = T) 
ggsave("opening_test.pdf")

# Plot with edge and node list; this the way

ggraph(opening_network2,layout = "kk") +  geom_node_point(size=3) +
   geom_edge_link(arrow = arrow(length = unit(3, 'mm')), end_cap = circle(1.5, 'mm'),aes(edge_colour=color))  +
  geom_node_text(label=colnames(opening),size=3,repel = T)
ggsave("opening_test.png")


#######################################################
#######################################################

######
### Now to get networks from positions other than the opening
### Need to build a function that converts 
#####

### Starting out

occupied_squares(opening.board)
opening.board<-t(matrix(c(rep(1,16),rep(0,32),rep(1,16)),nrow = 8,ncol = 8))
rownames(opening.board)<-c(8:1)
colnames(opening.board)<-letters

### Index of initial piece and postion

pawns_white<-paste(letters,2,"_white",sep = "")
pawns_black<-paste(letters,7,"_black",sep = "")
pawn_loc<-c(paste(letters,2,sep = ""),paste(letters,7,sep = ""))

y<-as.data.frame(cbind(c(pawns_white,pawns_black),pawn_loc),stringsAsFactors = F)
names(y)<-c("id","square")

bishop<-c("b_white_c1","b_white_f1","b_black_c8","b_black_f8")
bishop_loc<-c("c1","f1","c8","f8")

knight<-c("n_white_b1","n_white_g1","n_black_b8","n_black_g8")
knight_loc<-c("b1","g1","b8","g8")

rook<-c("r_white_a1","r_white_h1","r_black_a8","r_black_h8")
rook_loc<-c("a1","h1","a8","h8")

royal<-c("q_white","q_black","k_white","k_black")
royal_loc<-c("d1","d8","e1","e8")

x<-as.data.frame(cbind(c(bishop,knight,rook,royal),c(bishop_loc,knight_loc,rook_loc,royal_loc)),stringsAsFactors = F)
names(x)<-c("id","square")


opening_index<-rbind(y,x)

### Function to create new board after moves
opening.board<-t(matrix(c(rep(1,16),rep(0,32),rep(1,16)),nrow = 8,ncol = 8))
rownames(opening.board)<-c(8:1)
colnames(opening.board)<-letters

chess_move_board<-function(from,to,board){
  r<-as.numeric(substr(from,2,2))
  c<-grep(substr(from,1,1),letters)
  board[9-r,c]<-0
  
  r<-as.numeric(substr(to,2,2))
  c<-grep(substr(to,1,1),letters)
  board[9-r,c]<-1
  
  return(board)
}
chess_move_board("e2","e4",opening.board)
opening_index
opening.board

chess_move_index<-function(from,to,index){
  if (to %in% index[,2]){
    w<-grep(to,index[,2])
    index<-index[-w,]
    a<-grep(from,index[,2])
    index[a,2]<-to}else{
  a<-grep(from,index[,2])
  index[a,2]<-to}
  
  return(index)
}
chess_move_index("e2","e7",opening_index)


### Function to generate network from position

chess_generator<-function(board,index){
  white<-index[grep("white",index[,1]),]
  black<-index[grep("black",index[,1]),]
  
  ###### Adj Matrix
  opening_white<-matrix(0,nrow=64,ncol = 64)
  rownames(opening_white)<-squares
  colnames(opening_white)<-squares
  
  opening_black<-matrix(0,nrow=64,ncol = 64)
  rownames(opening_black)<-squares
  colnames(opening_black)<-squares
  
  ###### White
  pawns<-white[grep("2_",white[,1]),2]
  for (i in seq_along(1:NROW(pawns))){
    attack.to<-pawn2attack(pawns[i])
    attack.from<-name2row(pawns[i])
    opening_white[attack.from,attack.to[1]]<-1
    opening_white[attack.from,attack.to[2]]<-1
  }
  
  bishops<-white[grep("b_",white[,1]),2]
  for (i in seq_along(1:NROW(bishops))){
    opening_white[name2row(bishops[i]),name2row(bishop2attack(bishops[i],board))]<-1
  }
  
  rooks<-white[grep("r_",white[,1]),2]
  for (i in seq_along(1:NROW(rooks))){
    opening_white[name2row(rooks[i]),name2row(rook2attack(rooks[i],board))]<-1
  }
  
  knights<-white[grep("n_",white[,1]),2]
  for (i in seq_along(1:NROW(knights))){
    opening_white[name2row(knights[i]),knight2attack(knights[i])]<-1
  }
  
  q<-white[grep("q_",white[,1]),2]
  k<-white[grep("k_",white[,1]),2]
  
  opening_white[name2row(q),name2row(queen2attack(q,board))]<-1
  opening_white[name2row(k),king2attack(k)]<-1
  
  ###### Black
  
  pawns<-black[grep("7_",black[,1]),2]
  for (i in seq_along(1:NROW(pawns))){
    attack.to<-pawn2attack_black(pawns[i])
    attack.from<-name2row(pawns[i])
    opening_black[attack.from,attack.to[1]]<-1
    opening_black[attack.from,attack.to[2]]<-1
  }
  
  bishops<-black[grep("b_",black[,1]),2]
  for (i in seq_along(1:NROW(bishops))){
    opening_black[name2row(bishops[i]),name2row(bishop2attack(bishops[i],board))]<-1
  }
  
  rooks<-black[grep("r_",black[,1]),2]
  for (i in seq_along(1:NROW(rooks))){
    opening_black[name2row(rooks[i]),name2row(rook2attack(rooks[i],board))]<-1
  }
  
  knights<-black[grep("n_",black[,1]),2]
  for (i in seq_along(1:NROW(knights))){
    opening_black[name2row(knights[i]),knight2attack(knights[i])]<-1
  }
  
  q<-black[grep("q_",black[,1]),2]
  k<-black[grep("k_black",black[,1]),2]
  
  opening_black[name2row(q),name2row(queen2attack(q,board))]<-1
  opening_black[name2row(k),king2attack(k)]<-1
  
  ###### Create Network
  
  opening_network<-graph_from_adjacency_matrix(opening_white,mode = "directed")
  opening_network_black<-graph_from_adjacency_matrix(opening_black,mode = "directed")
  
  edge_list<-get.edgelist(opening_network)
  edge_list<-as.data.frame(edge_list)
  edge_list$color<-"white"
  names(edge_list)[1:2]<-c("to","from")
  
  edge_list_black<-get.edgelist(opening_network_black)
  edge_list_black<-as.data.frame(edge_list_black)
  edge_list_black$color<-"black"
  names(edge_list_black)[1:2]<-c("to","from")
  
  edge_list_full<-rbind(edge_list,edge_list_black)
  
  node_list<-squares
  
  chess_network<-graph_from_data_frame(d = edge_list_full, vertices = node_list, directed = T)
  
  ###### Create graph
  
  z<-ggraph(chess_network,layout = "kk") +  geom_node_point(size=3) +
    geom_edge_link(arrow = arrow(length = unit(3, 'mm')), end_cap = circle(1.5, 'mm'),aes(edge_colour=color))  +
    geom_node_text(label=colnames(opening),size=3,repel = T)
  
  return(z)
  #return(
    #c(pawns,rooks,bishops,knights,k,q))
  #return(opening_black[name2row(bishops[1]),name2row(bishop2attack(bishops[1],board))])
}
chess_generator(opening.board,opening_index)


#### Function to generate gif of position change

chess_gif<-function(moves){
  
  chess_generator(opening.board,opening_index)
  ggsave("opening.png")
  x<-opening.board
  y<-opening_index
  
  pics<-c()
  pics[1]<-"opening.png"
  
  for (i in seq_along(1:NROW(moves))){
    x<-chess_move_board(moves[[i]][1],moves[[i]][2],x)
    y<-chess_move_index(moves[[i]][1],moves[[i]][2],y)
    chess_generator(x,y)
    ggsave(paste("move",i,".png",sep=""))
    pics[1+i]<-paste("move",i,".png",sep="")
  }
  
  GIF.convert <- function(x, fps, output){
    image_read(x) %>%
      image_animate(fps = fps) %>%
      image_write(output)
  }
  
  GIF.convert(pics,.5,"moves.gif")
}
moves<-list(c("e2","e4"),c("e7","e5"),c("g1","f3"),c("b8","c6"),c("f1","b5"),c("a7","a6"),
            c("b5","c6"),c("b7","c6"),c("b1","c3"),c("d7","d5"))
chess_gif(moves)

