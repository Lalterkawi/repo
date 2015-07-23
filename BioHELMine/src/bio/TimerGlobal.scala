package bio

class TimerGlobal(att:Attributes) extends Serializable{
  var probOne=0.0
  var attInfo=att
  val cm =scala.io.Source.fromFile("myfile.txt").getLines.toList
  var minClassifiersInit= cm.filter {x=>x.contains("MIN_CLASSIFIERS") }.apply(0).split(" ")(1).toInt
  var maxClassifiersInit =cm.filter {x=>x.contains("MAX_CLASSIFIERS") }.apply(0).split(" ")(1).toInt
  var penalizeMin=0
   if (cm.contains("PENALIZE_MIN_SIZE")) {
    penalizeMin=cm.filter {x=>x.contains("PENALIZE_MIN_SIZE") }.apply(0).split(" ")(1).toInt
  } 
  var ignoreMissingValues=0
   if (cm.contains("IGNORE_MISSING_VALUES")) {
    ignoreMissingValues = 1;
  } 
 
  var numClasses=0
  var defaultClass=(-1)
  var defaultClassPolicy=cm.filter {x=>x.contains("DEFAULT_CLASS") }.apply(0).split(" ")(1)
  


  if(defaultClassPolicy=="MAJOR") {
          
                numClasses=attInfo.getNumClasses()-1;
                //defaultClass=0;
                defaultClass=attInfo.mostFrequentClass
  }
  else if(defaultClassPolicy== "MINOR"){
                numClasses=attInfo.getNumClasses()-1;
                //defaultClass=0;
                defaultClass=attInfo.leastFrequentClass
  }
  else  if(defaultClassPolicy== "FIXED"){
                numClasses=attInfo.getNumClasses()-1;
                defaultClass=cm.filter {x=>x.contains("FIXED_DEFAULT_CLASS") }.apply(0).split(" ")(1).toInt
  }
  else  if(defaultClassPolicy== "DISABLED"){
                numClasses=attInfo.getNumClasses();
  }
  else  if(defaultClassPolicy== "AUTO"){
                numClasses=attInfo.getNumClasses()-1;
  }

  var elitismEnabled=1
  var smartInit=cm.contains("SMART_INIT")
  
  var numAttributes=attInfo.numAttributes
  var numAttributesMC=numAttributes-1;
 
  var doTrainAndClean=0
  var cleanProb=0.0;
  if(cm.contains("RULE_CLEANING_PROB")) {
    doTrainAndClean=1;
    cleanProb=cm.filter {x=>x.contains("RULE_CLEANING_PROB") }.apply(0).split(" ")(1).toDouble
  } 
  
  var generalizingProb=0.0
 var numRepetitionsLearning=cm.filter {x=>x.contains("REPETITIONS_RULE_LEARNING") }.apply(0).split(" ")(1).toInt


  
  if(cm.contains("RULE_GENERALIZING_PROB")) {
    doTrainAndClean=1;
    generalizingProb=cm.filter {x=>x.contains("RULE_GENERALIZING_PROB") }.apply(0).split(" ")(1).toDouble
    
  } 
  
  
  

}