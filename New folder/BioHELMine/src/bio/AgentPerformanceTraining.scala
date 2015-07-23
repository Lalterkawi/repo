package bio

class AgentPerformanceTraining(pNumInstances:Int,pRuleClass:Int) {
 var ruleClass=pRuleClass;

 var numInstancesTotal = pNumInstances;
 var  numInstancesPosOK = 0;
 var  numInstancesMatched = 0;
 var  numInstancesPos = 0;

  def addMatch(realClass:Int,predictedClass:Int) {
    if(realClass==ruleClass) numInstancesPos=numInstancesPos+1
    numInstancesMatched=numInstancesMatched+1

    if (predictedClass == realClass) {
      numInstancesPosOK=numInstancesPosOK+1
    }
  }

  def addNoMatch(realClass:Int) {
    if(realClass==ruleClass) numInstancesPos=numInstancesPos+1
  }

  def getAccuracy():Double= { return numInstancesPosOK.toDouble/numInstancesTotal.toDouble }
  
  def getAccuracy2():Double= {
    if(numInstancesMatched==0) return 0;
    return numInstancesPosOK.toDouble/numInstancesMatched.toDouble
    
  }
 
  def getCoverage():Double= { return numInstancesMatched.toDouble/numInstancesTotal.toDouble}
  def getCoverage2():Double= {  return numInstancesPosOK.toDouble/numInstancesTotal.toDouble}
  def getNumOK():Int= { return numInstancesPosOK}
  def getNumPos():Int= { return numInstancesPos}
  def getNumMatched():Int= { return numInstancesMatched}
  def getNumKO():Int= { return numInstancesMatched-numInstancesPosOK}
  def getNumTotal():Int= { return numInstancesTotal}
  def getNC():Double={return (1-numInstancesMatched).toDouble/numInstancesTotal.toDouble}
  def getRecall():Double={ return numInstancesPosOK.toDouble/numInstancesPos.toDouble }
  def getFMeasure():Double= {
    var precision=getAccuracy2();
    var recall=getRecall();
    return 2*precision*recall/(precision+recall)
  }
 
 

  def setNumMatched( i:Int) {
    numInstancesMatched = i;
  }

  def setNumPos(i:Int) {
    numInstancesPos = i;
  }

  def setNumOK( i:Int) {
    numInstancesPosOK = i;
  }

 def getFitness():Double=
 {
    return getFMeasure()   
  }
  
  
  
}