convert2Matrix <- function(txt, dic)
{
    row_n = length(txt)
    col_n = length(dic)
    mm = matrix(0, row_n, col_n)
    colnames(mm) = dic
    x = strsplit(txt,"")
    for (i in 1:row_n){
      nms = intersect(x[[i]],dic)
      if (length(nms)>0){
        mm[i,nms] = 1
      }
    }
    return(mm)
}
embeddingMatrix <- function(txt, emb, nsize = 10)
{
    emb = t(emb)
    row_n = length(txt)
    col_n = nrow(emb) * nsize
    mm = matrix(0,row_n,col_n)
    nms = colnames(emb)
    xx = strsplit(txt,"")
    for (i in 1:row_n){
      idx = ifelse(xx[[i]] %in% nms, xx[[i]],"空白")
      idx_n = length(idx)
      
      if(idx_n <nsize)
        idx = c(idx , rep('空白',nsize-idx_n))
      else
        idx = idx[1:nsize]
        
      mm[i,] = as.vector(emb[,idx])
      
    }
    return(mm)
    
}