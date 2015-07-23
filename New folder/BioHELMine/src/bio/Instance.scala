package bio

class Instance(IPD:Int, ptrainTest:Int, attributesInfo:Attributes,string:String) extends Serializable{
	val REAL =1
	val NOMINAL =2 

	var traintest=ptrainTest  //int
	var missing:String=null
	//missing =null//string
	var id=IPD; //int
	var missingValues=0 //int

	var noRealAtt=(-1) //int
	var numAttributes=attributesInfo.numAttributes; //int


	var instanceClass=(-1) //int

	var num=numAttributes

	//		if(num%4!=0) //??
	//			num=num+(4-num%4) //??

	var  realValues=Array.fill(num)(0.0); //list of double
	parseInstance(string)

	def extractNominalValue(ins:String, attribute:Int):Int={

		var value=attributesInfo.valueOfNominalAttribute(attribute, ins)
				return value    
	}


	def parseInstance(ins:String){
		println(numAttributes)
		attributesInfo.numOfInstance=attributesInfo.numOfInstance+1    /// to move to instance set
		var insArray=ins.split(',')  //assume attributes organized
		var length=insArray.size
    //attributesInfo.insertNominalValue(length-1, insArray(length-1))
		println("the instance")
		insArray.foreach { x=>println(x)}
		println(length)
		for(i <- 0 to (length-1) ){ // we can make sure that all attributes filled with at least ?

			if(attributesInfo.typeOfAttribute(i)==attributesInfo.NOMINAL)
			{

				if(insArray(i).equals("?"))
				{ 

					if(missing==null)
						missing= (Array.fill(numAttributes)(0)).mkString("")

						var seqChar=missing.toCharArray()
						seqChar(i)='1'
						missing= seqChar.mkString("")
						missingValues=1;
				}
				else{
         
					attributesInfo.insertNominalValue(i, insArray(i))
					if(i<numAttributes) 
						  realValues(i)=extractNominalValue(insArray(i), i)
						else 
              instanceClass=extractNominalValue(insArray(i), numAttributes)
				}
			}
			else { //not nominal

				if(insArray(i)=="?")
				{
					if(missing==null)
						missing= (Array.fill(numAttributes)(0)).mkString("")

						var seqChar=missing.toCharArray()
						seqChar(i)='1'
						missing= seqChar.mkString("")
						missingValues=1;
				}
				else{
					realValues(i)=(insArray(i)).toDouble
				}  



			} 
		}

		//println("printing the class if instrance "+instanceClass)
	}

	def updatMissing(){

		if(missingValues==0) return;

		for(i<-0 to numAttributes-2) {
			if(missing(i)=='1') {
				if(attributesInfo.typeOfAttribute(i)==REAL) {
					var value=attributesInfo.getAverageOfAttribute(instanceClass,i);
					realValues(i)=value;

				} 
        else if(attributesInfo.typeOfAttribute(i)==NOMINAL) {
					var value=attributesInfo.getMostFrequentValueOfAttribute(instanceClass,i);

				realValues(i)=value;
				}

				var seqChar=missing.toCharArray()
						seqChar(i)='0'
						missing= seqChar.mkString("")

			}
		}

		missingValues=0;
		missing=null;

	}

	def normalize(){
		for(i<-0 to numAttributes-1){
			if(attributesInfo.typeOfAttribute(i)==REAL)
				realValues(i)=(realValues(i)-attributesInfo.minDomain(i))/attributesInfo.sizeDomain2(i) -1
		}
	}


	def valueOfAttribute(attribute:Int):Int={return realValues(attribute).toInt}
	def getclass():Int={return instanceClass}
	def hasMissingValues():Int={return missingValues}

	def isMissing(atr:Int):Int={if ( missing ==null ||missing.toCharArray()(atr)==0) return 0; else return 1}
	def realValueOfAttribute(attribute:Int):Double={return realValues(attribute)}
	def getID():Int={return id}
  
  override def toString():String={
    
    return numAttributes +""
  }


}