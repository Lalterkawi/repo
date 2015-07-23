package bio

import org.apache.spark.SparkConf
import org.apache.spark.SparkContext


class Classifier_Aggregated(tclassifiers:Array[Hyperrect_List], AttInfo:Attributes) {
  var classifiers:List[Hyperrect_List]=List()
  if(tclassifiers!=null)
  classifiers.++(tclassifiers.toList)
  var classiferType=(-1) 
  var defaultclass=0
  var accuracy=0.0
  val cm =scala.io.Source.fromFile("myfile.txt").getLines.toList
  
//    val logFile = "README.md" // Should be some file on your system
 //   val conf = new SparkConf().setMaster("local").setAppName("sampleApp")
 //   val sc = new SparkContext(conf)
  //  val data = sc.textFile(logFile)
    var r=new scala.util.Random
  var defaultClassPolicy=""
def  classifier_agg(){
    
     defaultClassPolicy=cm.filter{ line => line.contains("DEFAULT_CLASS") }.apply(0).split(" ")(1)
    
   
    println(defaultClassPolicy)
  
    
    if(defaultClassPolicy.contains("MAJOR")){
    
      defaultclass=AttInfo.mostFrequentClass
      
    }
    else if(defaultClassPolicy.contains("MINOR"))
    
    defaultclass=AttInfo.leastFrequentClass

    else
      defaultclass=(-1)
    
  }
    
def addClassifier(c1:Hyperrect_List):Array[Hyperrect_List]={

 classifiers= classifiers.+:(c1)
 return classifiers.toArray  
 }    
    
 def getclass(classifier:Int):Int={
   if(defaultclass!=(-1) && classifier==classifiers.size)
     return defaultclass
   else
     return classifiers(classifier).getclass()
 } 
 
 def setDefaultRule(ins:instanceSet){
   if(! defaultClassPolicy.equals("DISABLED")) return
   var nc=AttInfo.getNumClasses
   var byclassCount=ins.countsByClass.toList.max
   defaultclass=byclassCount
 }
 
   

def classify(ins:Instance):Int={
  var theclass=classifiers.filter { x => x.doMatch(ins)}.apply(0)
 if(theclass!=null) return theclass.getclass()
 else if(defaultclass!=(-1)) return classifiers.size
 else 
   return -1
}  
 
 
}