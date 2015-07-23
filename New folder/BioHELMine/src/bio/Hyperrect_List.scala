package bio

import org.apache.hive.service.cli.thrift.TGetCatalogsReq
import org.apache.ivy.core.module.descriptor.ExtendsDescriptor

class Hyperrect_List (MAXIMIZE:Int,  tGlobal:TimerGlobal,is:instanceSet,empty:Int) extends Serializable{

  var classValue=0
  var predicates:List[Double]=List()
  var offsetPredicates:List[(Int,Int)]=List()
  var numAtt=tGlobal.attInfo.numAttributes
  var whichAtt:List[Int]=List()
  var fitness=0.0; //double 
  var ruleSize=0
  var scaledFitness=0.0; //double 
  var exceptionsLength=0.0;  //For MDL fitness function double
  var accuracy=0.0;  //double
  var accuracy2=0.0;  //double
  var coverage=0.0;  //double
  var coverageTerm=0.0; //double
  var recall=0.0; //double
  var theoryLength = 0.0;
 // Int numDiscrete;
 // Int *listDiscretePos;
 // Int *listDiscreteAtt;
 // Int numReal;
 // Int *listRealPos;
 // Int *listRealAtt;
  var chromosome:List[(Int,String)]=List()
  var modif=1;
  initializeChromosome(empty)
  
 // def fitnessComputation(is:instanceSet){
 //   modif = 0
 //  fitness =is.classifierFitness(this)
 // }
  def activateModified() {
    modif = 1;
  }
  
  
  def initializeChromosome(empty:Int){
    var r=new scala.util.Random
    var ins:Instance=null
    if(tGlobal.defaultClassPolicy!="DISABLED") {
      ins=is.getInstanceInit(tGlobal.defaultClass);
    } else {
      ins=is.getInstanceInit(tGlobal.attInfo.getNumClasses());
    }
   var selectedAtts= 0 to numAtt-1 toList;   //:List[Int]=List() 
   
   for(i<- 0 to (numAtt-1)) {

    if (tGlobal.attInfo.getTypeOfAttribute(i) ==tGlobal.attInfo.REAL) {
      var max=0.0
      var min=0.0
      var sizeD=tGlobal.attInfo.sizeDomain(i);
      var minD=tGlobal.attInfo.minDomain(i);
      var maxD=tGlobal.attInfo.maxDomain(i);
      var size=(r.nextDouble()*0.5+0.25)*sizeD;

      if(ins!=null) {
        var value=ins.realValues(i)
        min=value-size/2.0;
        max=value+size/2.0;
        if(min<minD) {
          max=max+(minD-min);
          min=minD
        }
        if(max>maxD) {
          min-=(max-maxD);
          max=maxD;
        }
      } else {
        min=r.nextDouble()*(sizeD-size)+minD;
        max=min+size;
      }
  
      chromosome=(i,min+","+max)::chromosome
    } else {
      var value=0.0
      if(ins!=null) 
           value=ins.realValues(i)
      else value=(-1)  
      var part=new Array[Int](tGlobal.attInfo.getNumValuesAttribute(i))
        for(j<- 0 to part.length-1){         
         var dump=tGlobal.attInfo.valuesOfNominalAttributes.filter(x => x._1==i).apply(0)._2.filter(x => x._1==j).apply(0)._1
         if(value==dump)
           part(j)=1
         else {
           if(r.nextDouble()<0.5)
             part(j)=1
           else 
             part(j)=0 
         }
           
        }  
    chromosome=  ((i,part.toString()))::chromosome 
      
    }
     
  
  } 
      
    if(empty==0) {
     if(is!=null) {
      classValue=ins.getclass();
     } 
     else {
      do {
        classValue=r.nextInt(tGlobal.attInfo.getNumClasses()-1)
      } while(!tGlobal.defaultClassPolicy.equals("DISABLED") && classValue==tGlobal.defaultClass);
    }
  } else {
    if(!tGlobal.defaultClassPolicy.equals("DISABLED")) {
      classValue=is.getMajorityClassExcept(tGlobal.defaultClass);
    } else {
      classValue=is.getMajorityClass()
    }
  }

  }
  
  
  
 def computeTheoryLength():Double=
{

  var ptr=chromosome
  

  for (j <- 0 to numAtt-1) {
    
    if (tGlobal.attInfo.getTypeOfAttribute(j) == tGlobal.attInfo.REAL) {
      var size=tGlobal.attInfo.sizeDomain(j)
      if(size>0) {
        var pt=chromosome.filter(x =>x._1==j)(0)._2.split(",")
        theoryLength += 1.0 - (pt(1).toDouble-pt(0).toDouble)/size;
      }

      
    } 
    else {
      var numFalse = 0;
      var numValues = tGlobal.attInfo.getNumValuesAttribute(j); 
      for (k <- 0 to  numValues-1) {
        var dump= chromosome.filter(x=>x._1==j).apply(0)._2.toCharArray()(k)
        if ( dump=='0') numFalse=numFalse+1  
      }
       theoryLength = theoryLength +numFalse.toDouble/numValues;    
    }
     
    }
   
  theoryLength /= tGlobal.numAttributesMC.toDouble

  return theoryLength
}

 def mutationOffset(geneValue:String, offsetMin:Double, offsetMax:Double):Double=
{
  var newValue=0.0
  var r=new scala.util.Random
  var replace=0
  var temp=geneValue.split(",")
  if(r.nextDouble>0.5)
    replace=1
   
  if(r.nextDouble>0.5)  
    newValue =( temp(replace).toDouble + r.nextDouble() * offsetMax) 
  else
    newValue =( temp(replace).toDouble - r.nextDouble() * offsetMin ) 
  
 
  return newValue
}
 def swapD(a:Double,b:Double)=(b,a) 

 def  mutation():Hyperrect_List=
  {
  
   var attribute=0 
   var value=0 
   var attIndex=0
   modif = 1;
   var r=new scala.util.Random
   
   if(tGlobal.numClasses>1 && !(r.nextDouble()<0.10)) {
    var newValue=(-1)
    do {
      newValue = r.nextInt(tGlobal.attInfo.getNumClasses()-1)
    } while (newValue == classValue || !tGlobal.defaultClassPolicy.equals("DISABLED") && newValue==tGlobal.defaultClass);
    classValue=newValue;
  } 
   else {
   
      attribute=r.nextInt(numAtt-1);
      value=r.nextInt(tGlobal.attInfo.getNumValuesAttribute(attribute));
      var index=value
    
      if (tGlobal.attInfo.getTypeOfAttribute(attribute) == tGlobal.attInfo.REAL) {
        
        var minOffset = 0.5 * tGlobal.attInfo.sizeDomain(attribute)
        var maxOffset = 0.5 * tGlobal.attInfo.sizeDomain(attribute)
        var thevalue=chromosome.filter(x => x._1==attribute)(0)._2
        var newValue = mutationOffset(thevalue, minOffset, maxOffset)
        if (newValue < tGlobal.attInfo.minDomain(attribute)) newValue = tGlobal.attInfo.minDomain(attribute);
        if (newValue > tGlobal.attInfo.maxDomain(attribute)) newValue = tGlobal.attInfo.maxDomain(attribute);
        var update=thevalue.split(",").map { x => x.toDouble }
        update(index)=newValue 
        if(value==1) index=index-1
        if(update(index)>update(index+1)) {
        var (u,p) = swapD(update(index),update(index+1))
        update(0)=u
        update(1)=p
        }
       chromosome=chromosome.map(x => if(x._1==attribute) {(x._1,update(0)+","+update(1))} else x ) } 
      else {
        
    	  if (attribute != tGlobal.numAttributesMC ) {
    		  if (getGene(attribute, value) == '0')
    			  setGene(attribute, value, '1');
    		  else
    			  setGene(attribute, value, '0');
    	  } else {
    		  var oldValue=getGene(attribute,value);
    		  var newValue='0'
    				  do {
    					  var newValue = r.nextInt(tGlobal.attInfo.getNumClasses()-1);
    				  } while(newValue==oldValue || tGlobal.defaultClassPolicy!=3 && newValue==tGlobal.defaultClass);

    		  setGene(attribute, value, newValue);
    	  }
      }
    
    }
   return this
  }

 def getGene( att:Int,  value:Int):Char=
	 {
		 return chromosome.filter(x=>x._1==att).apply(0)._2.toCharArray()(value)
	 }
 def setGene( att:Int,  value:Int, nfo:Char)
 {
	 var dump= chromosome.filter(x=>x._1==att).apply(0)._2
			 var dump2=dump.toCharArray()
			 dump2(value)=nfo
			 dump=dump2.mkString("")
			 chromosome=chromosome.map(x => if(x._1==att) {(x._1,dump)} else x )
			 modif = 1;
 } 
 

  def getclass() = classValue
  
  def doMatch(ins:Instance):Boolean=
  {
    
    for(i<-0 to numAtt-1) {
      println("passed "+i)
      var value=ins.realValues(i)
      if(tGlobal.attInfo.typeOfAttribute(i)==tGlobal.attInfo.REAL)
      {  
        var range= chromosome.filter(x =>x._1==i)(0)._2.split(",")
        if(value<range(0).toDouble || value> range(0).toDouble) 
          return false
      }
      else {      
        if(chromosome.filter(x=>x._1==i).apply(0)._2.toCharArray()(value.toInt)  =='0')
          return false     
    }
    }
    return true;
  }

 def setScaledFitness(pFitness:Double) {
    scaledFitness = pFitness
 }
 
 def getFitness():Double= {
    return fitness;
 }
  def setFitness(pFit:Double) {
    fitness=pFit;
  }
  def getScaledFitness():Double= {
    return scaledFitness;
  }
  def setAccuracy( acc:Double) {
    accuracy = acc;
  }
  def getAccuracy():Double= {
    return accuracy;
  }
  def setAccuracy2( acc:Double) {
    accuracy2 = acc;
  }
  def getAccuracy2():Double= {
    return accuracy2;
  }
  def setCoverage( cov :Double) {
    coverage = cov;
  }
  def getCoverage():Double= {
    return coverage;
  }


def getTheoryLength():Double= {
    return theoryLength
}

        def getRecall():Double= {
            return recall;
        }

       def setRecall( r:Double) {
            recall=r;
       }
  
 
 def crossover(in:Hyperrect_List):(Hyperrect_List,Hyperrect_List)={
    
 return crossover_1px(this, in)
}      
 
 def crossover_1px(in1:Hyperrect_List, in2 : Hyperrect_List): (Hyperrect_List,Hyperrect_List)=
  {
     var out1=in1
     var out2=in2
     var r=new scala.util.Random
     var point=r.nextInt(numAtt+1)
     
     if(point==numAtt-1){
       var temp= out1.classValue
       out1.classValue=in2.classValue
       out2.classValue=temp
     }
     else if(point==numAtt){
       out1=in2
       out2=in1
     }
     else{
       var where=r.nextInt(2)
       if(where==0){
          var offspring1:List[(Int,String)]=List()
          var offspring2:List[(Int,String)]=List()
          val (left1, right1) = in1.chromosome.splitAt(point)
          val (left2, right2) = in2.chromosome.splitAt(point)
          offspring1=(offspring1.:::(left1)).:::(right2)
          offspring2=(offspring2.:::(left2)).:::(right1)
          out1.chromosome=offspring1
          out2.chromosome=offspring2
       }
       else if(where==1){
         var temp:(Int,String)=null
         var temp2:(Int,String)=null
         if(tGlobal.attInfo.typeOfAttribute(point)==tGlobal.attInfo.REAL)
         {
           if(in1.chromosome.filter(x=> x._1==point)(0)._2.split(",")(0).toDouble >  in2.chromosome.filter(x=> x._1==point)(0)._2.split(",")(1).toDouble)
           {
             temp=(point, in2.chromosome.filter(x=> x._1==point)(0)._2.split(",")(0)+"," +in1.chromosome.filter(x=> x._1==point)(0)._2.split(",")(0))
             temp2=(point, in2.chromosome.filter(x=> x._1==point)(0)._2.split(",")(1)+"," +in1.chromosome.filter(x=> x._1==point)(0)._2.split(",")(1))
           }
           else if(in2.chromosome.filter(x=> x._1==point)(0)._2.split(",")(0).toDouble >  in1.chromosome.filter(x=> x._1==point)(0)._2.split(",")(1).toDouble)
           {
              temp=(point, in1.chromosome.filter(x=> x._1==point)(0)._2.split(",")(0)+"," +in2.chromosome.filter(x=> x._1==point)(0)._2.split(",")(0))
             temp2=(point, in1.chromosome.filter(x=> x._1==point)(0)._2.split(",")(1)+"," +in2.chromosome.filter(x=> x._1==point)(0)._2.split(",")(1))
           }
           else  {
             if(in2.chromosome.filter(x=> x._1==point)(0)._2.split(",")(0).toDouble >  in1.chromosome.filter(x=> x._1==point)(0)._2.split(",")(0).toDouble)
           {
              temp=(point, in1.chromosome.filter(x=> x._1==point)(0)._2.split(",")(0)+"," +in2.chromosome.filter(x=> x._1==point)(0)._2.split(",")(1))
             temp2=(point, in2.chromosome.filter(x=> x._1==point)(0)._2.split(",")(0)+"," +in1.chromosome.filter(x=> x._1==point)(0)._2.split(",")(1))
           }
             else{
               temp=(point, in2.chromosome.filter(x=> x._1==point)(0)._2.split(",")(0)+"," +in1.chromosome.filter(x=> x._1==point)(0)._2.split(",")(1))
               temp2=(point, in1.chromosome.filter(x=> x._1==point)(1)._2.split(",")(0)+"," +in2.chromosome.filter(x=> x._1==point)(0)._2.split(",")(1))
             } 
             
           }
           //replace att
          var offspring1:List[(Int,String)]=List()
          var offspring2:List[(Int,String)]=List()
          val (left1, right1) = in1.chromosome.splitAt(point)
          val (left2, right2) = in2.chromosome.splitAt(point)
          offspring1=(offspring1.:::(left1)).:::(right2)
          offspring2=(offspring2.:::(left2)).:::(right1)
          out1.chromosome=offspring1.updated(point, temp)
          out2.chromosome=offspring2.updated(point, temp2)
         }//nominal
         else{
          var splitting=r.nextInt(tGlobal.attInfo.getNumValuesAttribute(point)) 
          if(splitting==0) splitting=splitting +1
          temp=(point, in1.chromosome.filter(x=> x._1==point)(0)._2.substring(0,splitting)+in2.chromosome.filter(x=> x._1==point)(0)._2.substring(splitting))
          temp2=(point, in2.chromosome.filter(x=> x._1==point)(0)._2.substring(0,splitting) +in1.chromosome.filter(x=> x._1==point)(0)._2.substring(splitting))
          var offspring1:List[(Int,String)]=List()
          var offspring2:List[(Int,String)]=List()
          val (left1, right1) = in1.chromosome.splitAt(point)
          val (left2, right2) = in2.chromosome.splitAt(point)
          offspring1=(offspring1.:::(left1)).:::(right2)
          offspring2=(offspring2.:::(left2)).:::(right1)        
          out1.chromosome=offspring1.updated(point, temp)
          out2.chromosome=offspring2.updated(point, temp2)      
         }
       }
       
       
     }
  return (out1,out2)
  }
  
  def numSpecialStages():Int={return 2}
  
   def compareToIndividual(  i2:Hyperrect_List, maxmin:Int):Int= {
      if (maxmin == MAXIMIZE) {
        if (fitness>i2.fitness)
          return +69;
        if (fitness<i2.fitness)
          return -69;
        return 0;
      }
      if (fitness<i2.fitness)
        return +69;
      if (fitness>i2.fitness)
        return -69;
      return 0;
  }

  def compareToIndividual2(  i2:Hyperrect_List, maxmin:Int):Int={
     if (maxmin !=MAXIMIZE) {
        if (fitness>i2.fitness)
          return -69;
        if (fitness<i2.fitness)
          return +69;
        return 0;
      }
      if (fitness<i2.fitness)
        return -69;
      if (fitness>i2.fitness)
        return +69;
      return 0;
  }

  
  
 // def doSpecialStage(Int){
    
//  }
   
  
//  this(const classifier_hyperrect_list &orig,int son=0){
    
//  }

}