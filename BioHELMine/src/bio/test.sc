package bio

object test {
 
 
  val x=new Sampling(5)                           //> x  : bio.Sampling = bio.Sampling@1644cd9a
  
  x.MAX                                           //> res0: Int = 5
  x.num                                           //> res1: Int = 5
  for(i<-0 to 9) println(x.getSample())           //> 3
                                                  //| 2
                                                  //| 1
                                                  //| 0
                                                  //| 4
                                                  //| 0
                                                  //| 3
                                                  //| 1
                                                  //| 2
                                                  //| 4
  
  
  
  
  
  
}