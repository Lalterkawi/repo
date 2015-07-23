package bio
import java.io.Serializable
import org.apache.spark.SparkConf
import org.apache.spark.SparkContext
import org.apache.spark.rdd.RDD
import scala.util.control.Breaks._

class instanceSet(fileName:String,cmfile:String ,att:Attributes,Train:Int) extends Serializable {
	    var attributesInfo=att
			var countsByClass = Array.fill[Int](numOfClasses)(0)
			var initSamplings =new  Array[Sampling](numOfClasses)
			var instByClass = new Array[Array[Int]](numOfClasses)
      var ListOfinstances:List[Instance]=List()
			var instances: RDD[Instance]=null
			var  Indexinstances:RDD[(Instance,Long)]=null
			var id=0
			var numInstances=0
			var numInstancesOrig=0
			var numOfClasses=0
			val cm =scala.io.Source.fromFile(cmfile).getLines.toList    
			var classWiseInit=cm.filter {x=>x.contains("CLASS_WISE_INIT") }.apply(0).split(" ")(1).toInt
			readFile(fileName) 

			def readFile(fileName:String){

        
				val lineSample=cm.apply(0).split(',')
				lineSample.foreach { x => {var splits=x.split(' '); println(splits(0))}}
				for(i<-0 to lineSample.size-1){
					attributesInfo.InserAttributeName(i,(lineSample(i).split(' '))(0))

					if(lineSample(i).contains("Real"))
						      attributesInfo.setTypeOfAttribute(i, 1)
					else 	if(lineSample(i).contains("Integer")) 
								  attributesInfo.setTypeOfAttribute(i, 1)
					else  if(lineSample(i).contains("Nominal")){
									attributesInfo.setTypeOfAttribute(i, 2)
									println("ok")}
                //else if(lineSample(i).contains("class"))  
								   
                  //	 attributesInfo.insertNominalValue(i,lineSample((lineSample.size-1) )) 
								 
				}
        
        val logFile = fileName // Should be some file on your system
        val conf = new SparkConf().setMaster("local").setAppName("BioHEL")
        val sc = new SparkContext(conf)
        val data = sc.textFile(logFile)

				println(attributesInfo.numAttributes +"  *** "+attributesInfo.typeOfAttribute.toList)
				//we have to insert all instances to attributes

			 val mydata =scala.io.Source.fromFile(fileName).getLines.toList
			//	println(mydata)
      //  data.map { line => {new Instance(0, Train, attributesInfo,line)  }}
				//  var x	:List[Instance]=List()	//=data.map { line => {new Instance(0, Train, attributesInfo,line)  }}

			 instances= data.map(xd => parseLine(xd))     
     //  ListOfinstances=  mydata.map{ xd => {parseLine(xd)}}
			 numInstances=att.numOfInstance
			Indexinstances=instances.zipWithIndex		
						//Indexinstances=instances.zipWithIndex()
        
		//	instances.foreach{ x => { attributesInfo.insertInsance(x); x.updatMissing() }}
    attributesInfo.insertInsance(instances)     
     //  ListOfinstances.foreach{ x => { attributesInfo.insertInsance(x); x.updatMissing() }}
       numInstancesOrig=numInstances
						
						if(Train==1) {						
              attributesInfo.calculateAverage()
              numOfClasses=attributesInfo.getNumClasses() 
              println("number of classes "+numOfClasses)
						  initInstanceLists()
						} 
     
			}



			def parseLine(line:String):Instance={ 
					var hereid=id
					var ins=new Instance(id, Train, attributesInfo,line)
					id=id+1
					//  println(ins.realValues.toList)
					return ins
			}



			def restart()
			{
				initInstanceLists()
			}


			def initInstanceLists(){
       
				var numInst=numInstances
        countsByClass = Array.fill[Int](numOfClasses)(0)
				initSamplings =new  Array[Sampling](numOfClasses)
				instByClass = new Array[Array[Int]](numOfClasses)
        
				var i=0
				//ListOfinstances.foreach { x =>{countsByClass(x.getclass())=countsByClass(x.getclass())+1 ; }}  //1
        
        
      //  (0 to numOfClasses).map (z=> for {x<-ListOfinstances  if(x.getclass()==z ) } yield countsByClass(z)+=1 ) // or 2
            
     //   var p=   instances.map(x => (x.getclass(),1)) reduceByKey ( _+_ )   //equ 1,2 to countsByClass  
        
   
		//		for(i<-0 to numOfClasses-1) {
		//					var num=countsByClass(i)
		//					initSamplings(i) = new Sampling(num);
		//					instByClass(i) = new Array[Int](num)
		//					countsByClass(i)=0
		//				}
		//		i=0
					
				//ListOfinstances.foreach{ x =>{ instByClass(x.getclass())(countsByClass(x.getclass()))=x.id; 
				
        //countsByClass(x.getclass())=countsByClass(x.getclass())+1 ;}}
        
        var zz=instances.map{ x => (x.getclass(),x.id)} combineByKey (
            
         v => (1,List(v)),
        (acc:(Int,List[Int]),v) => ( acc._1+1,acc._2.::(v)),
        (acc1: (Int, List[Int]), acc2: (Int, List[Int])) => (acc1._1 + acc2._1, acc1._2 ::: acc2._2)
        
        )   //equ to instByclass and countsByClass
        
         
       zz.map( x => countsByClass(x._1)=x._2._1)
       zz.map(x=> initSamplings(x._1)= new Sampling(x._2._2.size))
       zz.map(x => instByClass(x._1)= x._2._2.toArray)
        
        
        for (j<- 0 to numOfClasses-1) println(" the count by class "+countsByClass(j)) 
			}


			def  removeInstancesAndRestart(cla:Hyperrect_List)
			{


				var numRemoved=0;
				var index=0;
				var countClassRem=new Array[(Int,Int)](numOfClasses)
				for(i<-0 to numOfClasses-1) countClassRem(i)=(i,0)


				var numOrig=numInstances;
				instances=instances.map { x => if(cla.doMatch(x)) { numRemoved=numRemoved+1 ;numInstances=numInstances-1; x} 
								                       else { countClassRem(x.instanceClass)=(x.instanceClass, countClassRem(x.instanceClass)._2+1); x}}
								                 instances=instances.filter { x => !cla.doMatch(x) }

				Indexinstances=instances.zipWithIndex()
				attributesInfo.updateClassCounters(countClassRem.toList);
				initInstanceLists();
			}


			def isMajority( ind:Hyperrect_List,cov:Double):Boolean=
				{

					var classCounts = Array.fill[Int](attributesInfo.getNumClasses())(0)
          var cl=ind.getclass();
          var nc=attributesInfo.getNumClasses();

					var numPos=0;
					instances.map { x => if(cl== x.instanceClass) numPos=numPos+1 else 0 }
					instances.map { x => if(ind.doMatch(x)) classCounts(x.instanceClass)= classCounts(x.instanceClass)+1 else 0}




					var ratio=classCounts(cl).toDouble/numPos.toDouble
							if(ratio<adjustCovBreak(cl,cov)/3) return false

									var max=classCounts.toList.max

									var posMax=(classCounts.toList).indexOf(max)
									var tie=0;

					if(classCounts.toList.indexOf(max, posMax+1) != (-1))
						tie=1


						return (max>0 && tie==0 && posMax==cl)
				}
			def adjustCovBreak(classInd:Int,cov:Double):Double={
					var coverageBreaks = cov/  attributesInfo.getInstancesOfClass(classInd) * numInstances
							if (coverageBreaks > 1) {
								coverageBreaks = 1;
							}

					return coverageBreaks 
			}

			def getInstanceInit( forbiddenCL:Int):Instance= 
				{
					var r=new scala.util.Random
							if(classWiseInit==0) {
								if(forbiddenCL!=numOfClasses) {
									var allEmpty=1

											for(i<-0 to numOfClasses-1) {
												if(i!=forbiddenCL) {
													if(countsByClass(i)>0) {
														allEmpty=0;
														break;
													}}
												else 
													allEmpty=1
											}
									if(allEmpty==1) {
										return null
									}
								}


								var cl=0
										do {
											if(forbiddenCL!=numOfClasses) {
												cl=r.nextInt(numOfClasses-2)
														if(cl>=forbiddenCL) cl=cl+1
											} else {
												cl=r.nextInt(numOfClasses-1)
											}
										} while(countsByClass(cl)==0)

											var pos=initSamplings(cl).getSample()
											var insIndex=instByClass(cl)(pos)
											var ins=Indexinstances.filter(x => x._2==insIndex).take(0).apply(0)._1

											return ins
							} 
							else 
							{
								var nc=numOfClasses;
                
								var count=new Array[Int](nc)
								var total=0;

								if(forbiddenCL!=numOfClasses) nc=nc-1

										for(i<-0 to nc-1) {
											if(i<forbiddenCL) 
												count(i)=initSamplings(i).numSampleLeft()
												else 
													count(i)=initSamplings(i+1).numSampleLeft()
										
                    total=total+count(i)
                    
										}
								var pos=r.nextInt(total-1)
										var acum=0;
								var j=0
										var found=0;
								for(i<-0 to nc-1  ) {
									if(found==0){
										acum=acum+count(i)
												if(pos<acum) {
													found=1;
												}
										j=i}
								}

								if(j<forbiddenCL) j=j-1

										pos=initSamplings(j).getSample()
										var insIndex=instByClass(j)(pos)
									//	var ins=Indexinstances.filter(x => x._2==insIndex).take(1).apply(0)._1
                    var ins=ListOfinstances(0)
										return ins
							}
				}

			def classifierStats(ind:Classifier_Aggregated)
			{

				var ap=new AgentPerformance(ind.classifiers.size, attributesInfo.getNumClasses())



				for (i <- 0  to numInstances-1) {
					var ins = Indexinstances.filter(x => x._2==i).take(0).apply(0)._1
							var realClass = ins.getclass()
							var predictedClass = -1;
					var whichClassifier=ind.classify(ins);
					if (whichClassifier != -1) {
						predictedClass = ind.getclass(whichClassifier);
					}
					ap.addPrediction(realClass, predictedClass,
							whichClassifier);
				}

				ind.accuracy=(ap.getAccuracy())


			}

            
          def classifierFitness( inds:List[Hyperrect_List])
        {
         

         var intermed= instances.map { x =>  for( i<-0 to inds.size-1;  
                                     ind<- inds)
                                       yield (i,ind.doMatch(x),x.instanceClass,ind.getclass()) 
                        }  //    (ap.addMatch(x.instanceClass,cl) else ap.addNoMatch(x.instanceClass)}

       var inter2=   intermed.flatMap(x => for{i <- 0 to inds.size-1}  yield (i,(                       //ref to ind
                                                                         if(x(i)._2) 1 else 0,     // numInstancesMatched=numInstancesMatched+1
                                                                         if(x(i)._2 && x(i)._3==x(i)._4) 1 else 0, //numInstancesPosOK=numInstancesPosOK+1
                                                                         if(!x(i)._2 && x(i)._3==x(i)._4) 1 else 0))) //numInstancesPos=numInstancesPos+1
          
         
       var total=   inter2.reduceByKey((x,y)=> (x._1+y._1,x._2+y._2,x._3+y._3))
       
       for (ind <- inds; t<-total) { ind.setAccuracy(t._2._2.toDouble/numInstances.toDouble) ;   
                                         if(t._2._1==0) ind.setAccuracy2(0) else ind.setAccuracy2( t._2._2.toDouble/t._2._1.toDouble) 
                                         ind.setCoverage(t._2._1.toDouble/numInstances.toDouble)
                                         var precision=ind.getAccuracy2()
                                         var recall=t._2._2.toDouble/t._2._3.toDouble
                                         ind.fitness= 2*precision*recall/(precision+recall)
                                   }
                                                                         

        }    
            
                     
 def getMajorityClass():Int=
{
 //var ordered= (for( i<-0 to countsByClass.size-1) yield (i,countsByClass(i))) toList
 var zipped= (0 to (countsByClass.size-1) ) zip countsByClass.toList toList
 
 var ordered=zipped sortWith(_._2 >_._2) 
return ordered(0)._1
}

def getMajorityClassExcept(cl:Int):Int=
{
  var zipped= (0 to (countsByClass.size-1) ) zip countsByClass.toList toList
  var ordered=(zipped sortWith(_._2 >_._2)) filter(x =>x._1 !=cl) 
  return ordered(0)._1
}
     
      
      

}