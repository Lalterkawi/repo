package bio
import Array._
class Sampling(max:Int) extends Serializable{
 val MAX=max
 var sample= (0 to MAX-1).asInstanceOf[IndexedSeq[Int]]             //range(0,MAX,1)
 var num=MAX;    

 def initSample(){  
   num=MAX;
   sample= 0 to MAX -1
}
 
def getSample():Int={
    
    var r=new scala.util.Random
 
    var pos=0
    if(num>1)
       pos=r.nextInt(num-1)   
    var value=sample(pos)
   sample= sample.filterNot{ x => x==value }
    num=num-1
    if(num==0)
      initSample()
      
   return value   
 } 
 
def numSampleLeft():Int={return num}

}