package bio

class AgentPerformance(pNumClassifiers:Int,pNumClasses:Int ){

  var numClassifiers=pNumClassifiers;
  var numClasses=pNumClasses;

  var numInstancesOK=0.0;
  var numInstancesKO=0.0;
  var numInstancesNC=0.0;
  var numInstancesTotal=0.0;
  var statisticsForEachClass=Array.ofDim[Int](numClasses, 3) ;
   for (i <- 0 to numClasses-1) {
         for ( j <- 0 to 2) {
           statisticsForEachClass(i)(j) = 0;
         }
      }
  var statisticsConfusionMatrix=Array.ofDim[Int](numClasses, numClasses) ;
  for (i <- 0 to numClasses-1) {
         for ( j <- 0 to numClasses-1) {
          statisticsConfusionMatrix(i)(j) = 0;
         }
      }
  
  var classifierActivated=Array.fill[Int](numClassifiers)(0)
  var classifierCorrect=Array.fill[Int](numClassifiers)(0)
  var classifierWrong=Array.fill[Int](numClassifiers)(0)
  var aliveClassifiers=0;

  
  
  def getAccuracy():Double= { return numInstancesOK/numInstancesTotal; }
  def getError():Double={return numInstancesKO/numInstancesTotal;}
  def getNC():Double={return numInstancesNC/numInstancesTotal;}

  def getNumError():Int={return numInstancesKO.toInt;}
  def getNumNC():Int={return numInstancesNC.toInt;}
  def getActivationsOfClassifier(classifier:Int):Int= { 
    return classifierActivated(classifier); 
  }
  
  def getCorrectPredictionsOfClassifier(classifier:Int):Int= {
    return classifierCorrect(classifier);
  }
  def getAccOfClassifier(classifier:Int):Double= {
    return classifierCorrect(classifier).toDouble/classifierActivated(classifier).toDouble;
  }

  def disableClassifier(classifier:Int) {
    classifierActivated(classifier)=0;
  }

  def getLSacc(classifier:Int):Double= {
    if(classifierActivated(classifier)==0) return 0;
    var acc=getAccOfClassifier(classifier);
    var laplaceAcc=(classifierCorrect(classifier)+1.0)/(classifierActivated(classifier)+numClasses);
    return if(acc<laplaceAcc) return acc else return laplaceAcc
  }

def addPrediction(realClass:Int,predictedClass:Int,usedClassifier:Int)
{
  numInstancesTotal=numInstancesTotal+1;
  if(usedClassifier!=(-1)) {
    if(classifierActivated(usedClassifier)==0) {
      aliveClassifiers=aliveClassifiers+1;
    }
    classifierActivated(usedClassifier)=classifierActivated(usedClassifier)+1
   statisticsConfusionMatrix(realClass)(predictedClass)= statisticsConfusionMatrix(realClass)(predictedClass)+1
    if (predictedClass == realClass) {
      numInstancesOK=numInstancesOK+1;
     statisticsForEachClass(realClass)(0)= statisticsForEachClass(realClass)(0)+1
      classifierCorrect(usedClassifier)= classifierCorrect(usedClassifier)+1
    } else {
      classifierWrong(usedClassifier)=classifierWrong(usedClassifier)+1
      numInstancesKO=numInstancesKO+1
       statisticsForEachClass(realClass)(1)= statisticsForEachClass(realClass)(1)+1
    }
  } else {
    numInstancesNC= numInstancesNC+1
    statisticsForEachClass(realClass)(2)= statisticsForEachClass(realClass)(2)+1
  }
}

  
}