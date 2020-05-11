#Function extract_Medfish to download MedFish datasets
extract_MedFish <- function(dataset = "Pres.abs.data"){
  
  table.names <- c("Pres.abs.data","Functional.data","Climate.data",
                 "Phylogenetic.data")
  
  if(!(dataset %in% c(table.names, "all"))){
    stop('Please choose one of the following datasets:
         "Pres.abs.data","Functional.data","Climate.data",
         "Phylogenetic.data"')
  }
  
  temp <- tempfile()
  if(dataset=="Pres.abs.data"){
  download.file("https://ndownloader.figshare.com/files/5635563",temp)
  Obs.1980.Medfish.dat <- read.table(unz(temp, "Presence_absence_data/Observed_grid_1980.csv"),sep=';',
                                     header=T)
  Proj.2040.2059.Medfish.dat<- read.table(unz(temp, "Presence_absence_data/Projected_grid_2040_2059.csv"),sep=';',
                                          header=T)
  Proj.2080.2099.Medfish.dat<- read.table(unz(temp, "Presence_absence_data/Projected_grid_2080_2099.csv"),sep=';',
                                          header=T)
  
  unlink(temp)
  return(list(Obs.1980.Medfish.dat=Obs.1980.Medfish.dat,
              Proj.2040.2059.Medfish.dat=Proj.2040.2059.Medfish.dat,
              Proj.2080.2099.Medfish.dat=Proj.2080.2099.Medfish.dat))
  }
  
  if(dataset=="Functional.data"){
    
    download.file("https://ndownloader.figshare.com/files/5635566",temp)
    Medfish.func.dat <- read.table(unz(temp, "Functional_data.csv"),sep=';',
                                       header=T)
    unlink(temp)
    return(list(Medfish.func.dat=Medfish.func.dat))
  }
  
  if(dataset=="Phylogenetic.data"){
    require(ape)
    download.file("http://www.esapubs.org/archive/ecol/E096/203/Phylogenetic_data.zip",temp)
    Medfish.tree <- read.tree(unz(temp, "Phylogenetic_data/chronogram.tre"))
    unlink(temp)
    return(list(Medfish.tree=Medfish.tree))
  }
  
  if(dataset=="Climate.data"){
    
    download.file("https://ndownloader.figshare.com/files/5635572",temp)
    Obs.1980.Climate.dat <- read.table(unz(temp, "Climate_data/Observed_climatic_data_1960.csv"),sep=';',
                                       header=T)
    Proj.2040.2059.Climate.dat<- read.table(unz(temp, "Climate_data/Projected_climatic_data_2040_2059.csv"),sep=';',
                                            header=T)
    Proj.2080.2099.Climate.dat<- read.table(unz(temp, "Climate_data/Projected_climatic_data_2080_2099.csv"),sep=';',
                                            header=T)
    unlink(temp)
    
    return(list(Obs.1980.Climate.dat=Obs.1980.Climate.dat,
                Proj.2040.2059.Climate.dat=Proj.2040.2059.Climate.dat,
                Proj.2080.2099.Climate.dat=Proj.2080.2099.Climate.dat))
  }
  
}

