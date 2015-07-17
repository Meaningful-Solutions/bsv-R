
read.bsv <- function( file, sep = c("][", ")(", "}{") ){
  bsv = data.frame(c());
  names.bsv = c();
  
  regex.chars <- c("[", "]","\\","^","$", ".", "|", "?", "*");
  regex.chars.safe <- paste("\\", regex.chars, sep="");
  sep.flat <- paste(sep, collapse=",");
  sep.safe <- sep.flat;
  for (rcs in regex.chars.safe){
    sep.safe <- gsub(rcs, paste("\\", rcs, sep=""), sep.safe);
  }
  sep.safe <- unlist(strsplit(sep.safe, ","));
  
  
  unsplit.bsv <- readLines(file);
  first.sep<- substr(unsplit.bsv, 1, 2);
  
  
  if(first.sep == sep[3]){
    split.bsv <- unlist(strsplit(unsplit.bsv, sep.safe[1]));
    hdr <- paste(sep[3],sep[2], sep="");
    eol <- match(hdr, split.bsv );
    unsplit.bsv <- split.bsv[eol:length(split.bsv)];
    unsplit.bsv[1] <- sep[2];
    first.sep<-sep[2];
    names.bsv <- split.bsv[3:eol-1]; #not sure why it is 3 instead of 2.
  }#(first.sep == sep[3])
  
  
  if(first.sep == sep[2]){
    split.bsv <- unlist(strsplit(unsplit.bsv, sep.safe[1]));
    bsv <- 
      data.frame(matrix(
        split.bsv[split.bsv!=sep[2]]
        , nrow=(sum(split.bsv==sep[2]) - 1)
        , byrow=TRUE
      ));
    
    if(length(names.bsv)>0){
      names(bsv)<-names.bsv;
    }
    
  }#(first.sep == sep[2])
  
  
  if(first.sep == sep[1]){
    split.bsv <- unlist(strsplit(unsplit.bsv, sep.safe[1]));
    bsv<-
      data.frame(matrix(
        split.bsv[split.bsv!=""]
        , nrow=sum(split.bsv=="")
        , byrow=TRUE
      ));
  }#(first.sep == "sep[1]")
  
  
  return(bsv);
}#read.bsv
