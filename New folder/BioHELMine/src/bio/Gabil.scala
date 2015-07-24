package bio

class Gabil (MAXIMIZE:Int,  tGlobal:TimerGlobal,is:instanceSet) {
  var length=0;   // Length of the individual in genes int
  var scaledFitness=0.0; //double 
  var exceptionsLength=0.0;  //For MDL fitness function double
  var accuracy=0.0;  //double
  var accuracy2=0.0;  //double
  var coverage=0.0;  //double
  var coverageTerm=0.0; //double
  var recall=0.0; //double
  var numAttributesMC=0; //int
  var theoryLength=0.0; //double

  var numAttributes=0; //int
  var fitness=0.0; //double
  var modif=1; //int
 
  var chromosome:List[(Int,String)]=List()
  initializeChromosome()

 // def fitnessComputation(is:instanceSet){
//	  modif = 0
//			  fitness =is.classifierFitness(this)
 // }

 
 
 
  def doMatch(ins:Instance):Boolean={
		  for (j <- 0 to tGlobal.attInfo.numAttributesMC-1){
			  if(tGlobal.attInfo.typeOfAttribute(j)==tGlobal.attInfo.NOMINAL) {  
				  if(chromosome.filter(x=>x._1==j).apply(0)._2.toCharArray()(ins.realValues(j).toInt)  =='0')
					  return false }
		  }

		  return true}    

  def initializeChromosome(){
	  var ins:Instance=null
			  if(tGlobal.defaultClassPolicy!="DISABLED") {
				  ins=is.getInstanceInit(tGlobal.defaultClass);
			  } else {
				  ins=is.getInstanceInit(tGlobal.attInfo.getNumClasses());
			  }

  for (i<-0 to tGlobal.attInfo.numAttributes -1 ){

	  if(tGlobal.attInfo.typeOfAttribute(i)==tGlobal.attInfo.NOMINAL){
		  var part=new Array[Int](tGlobal.attInfo.getNumValuesAttribute(i))
				  for(j<- 0 to part.length-1){

					  var dump=tGlobal.attInfo.valuesOfNominalAttributes.filter(x => x._1==i).apply(0)._2.filter(x => x._1==j).apply(0)._1
							  if(ins.realValueOfAttribute(i)==dump)
								  part(j)=dump
								  else
									  part(j)=0
				  }  
		  chromosome= chromosome.+:((i,part.toString()))
	  }//END OF NOMINAL
	  else 
		  chromosome=  chromosome.+:((i,ins.realValueOfAttribute(i)+""))
  }
  chromosome= chromosome.+:((tGlobal.attInfo.numAttributes,ins.getclass()+"")) 
  }  



  def getclass():Int={return  chromosome.filter(x => x._1==tGlobal.attInfo.numAttributes).apply(0)._2.toInt }

  def compareToIndividual(  i2:Gabil, maxmin:Int):Int= {
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

  def compareToIndividual2(  i2:Gabil, maxmin:Int):Int={
		  if (maxmin == MAXIMIZE) {
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


  def activateModified() {
	  modif = 1;
  }


  def getLength():Int= {
		  return length;
  }





  def setScaledFitness(pFitness:Double) {
	  scaledFitness = pFitness;
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
		  return theoryLength;
  }

  def getRecall():Double= {
		  return recall;
  }

  def setRecall( r:Double) {
	  recall=r;
  }


  def computeTheoryLength():Double=
	  {


		  var theoryLength = 0.0;

		  for (j <- 0 to tGlobal.numAttributesMC -1) {
			  var numFalse = 0;
			  var numValues = tGlobal.attInfo.getNumValuesAttribute(j); 
			  for (k <- 0 to  numValues-1) {
				  var dump= chromosome.filter(x=>x._1==j).apply(0)._2.toCharArray()(k)
						  if ( dump=='0') numFalse=numFalse+1  
			  }

			  theoryLength = theoryLength +numFalse.toDouble/numValues; 

		  }
		  theoryLength=theoryLength/tGlobal.numAttributesMC.toDouble;

		  return theoryLength;
	  }



  def mutation():Gabil=
	  {
		  var r=new scala.util.Random

				  var attribute=0
				  var value=0

				  // Modificarem el chromosome
				  modif = 1;

		  if(tGlobal.numClasses>1 && !(r.nextDouble()<0.10)) {
			  attribute = tGlobal.numAttributesMC;

		  } else {
			  attribute=r.nextInt(tGlobal.numAttributesMC-1);
			  value=r.nextInt(tGlobal.attInfo.getNumValuesAttribute(attribute)-1);
		  }

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
			  chromosome=chromosome.map(x => if(x._1==att) {(x._1,dump)} else (x._1,dump) )
			  modif = 1;
  }


}