#'Generate a 96-well plate map
#'
#'This function takes a vector of sample identifiers and creates an arbitrary number of 96 well plates.
#'
#'@param ids Vector: sample identifiers (IDs)
#'@param wtfile Logical: Write 96-well plate templates to file in csv format
#'@param path Character: File directory where plate templates should be saved
#'@param des Character: Project description which will serve as a file prefix for all plates.
#'@param partial Logical: Designating if you want partial plates generated or not.
#'@author Zak Robinson, Contact: zachary.robinson(at)umontana.com
#'@return List: 1st object is a list of plate matrices generated, 2nd object is list of plated samples, thier associated plate, and thier well coordinate. The 3rd object is any samples not plated when the argument partial is false.
#'@examples
#'samp_IDs<-paste("ind",1:1000,sep = "-")
#'plate_map_extract(ids = samp_IDs,wtfile = F,des = "Project_1",path = getwd(), partial = T)
#'@export


plate_map_extract<-function(ids,wtfile=TRUE,path=getwd(),des="site",partial=T){
  ids<-as.character(ids)
  plates<-ceiling(length(ids)/95)
  fullplates<-floor(length(ids)/95)
  numsamps<-c(rep(95,fullplates),(length(ids)-(fullplates*95)))
  out<-list()
  samp_coords<-data.frame(samp_ID=NA,plate_ID=NA,plate_Coord=NA)

if(partial== FALSE & fullplates != plates){
  plates<-plates-1
}

for(i in 1:plates){

    mat<-matrix(nrow=8,ncol = 12)
    mat[1:numsamps[i]]<-ids[(1:numsamps[i])+((i-1)*95)]
    mat[numsamps[i]+1]<-"Neg Control"
    colnames(mat)<-1:12
    row.names(mat)<-c("A","B","C","D","E","F","G","H")
    out[i]<-list(mat)
    names(out)[i]<-paste0(des,"_",format(Sys.time(),"%m-%d-%y"),"_PLATE_",i,".csv")
    if(wtfile==TRUE){
    write.csv(mat,file = paste0(path,"/",des,"_",format(Sys.time(),"%m-%d-%y"),"_PLATE_",i,".csv"))
    }

    samp_coords_temp<-cbind.data.frame(ids[(1:numsamps[i])+((i-1)*95)],sapply(ids[(1:numsamps[i])+((i-1)*95)],FUN = function(x){
    paste0(row.names(which(mat==x,arr.ind = T)),which(mat==x,arr.ind = T)[2])}))
    samp_coords_temp$plate_ID<-paste0(des,"_",format(Sys.time(),"%m-%d-%y"),"_PLATE_",i,".csv")
    colnames(samp_coords_temp)[1:2]<-c("samp_ID","plate_Coord")
    rownames(samp_coords_temp)<-NULL
    samp_coords<-rbind(samp_coords,samp_coords_temp)

  }
  not_plated<-ids[!(ids %in% samp_coords$samp_ID)]
  final_out<-list(out,samp_coords[-1,],not_plated)
  names(final_out)<-c("plates_list","Sample_Coords","Samps_Not_Plated")
  return(final_out)

}


