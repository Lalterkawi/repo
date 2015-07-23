package bio

import org.apache.spark.SparkConf
import org.apache.spark.SparkContext
import org.apache.spark.rdd.RDD
class Attributes (NumAtt:Int) extends Serializable{
    	    val REAL =1
    			val NOMINAL =2
    
    			var numAttributes=NumAtt
    			var numAttributesMC=NumAtt-1
    			var typeOfAttribute=Array.fill[Int](NumAtt+1)(-1)
    			var minDomain=Array.fill[Double](NumAtt-1)(0)
    			var maxDomain=Array.fill[Double](NumAtt-1)(0)
    			var sizeDomain=Array.fill[Double](NumAtt-1)(0)
    			var sizeDomain2=Array.fill[Double](NumAtt-1)(0)
    			var averageOfAttribute:List[(Int,(Int,Double))]=List() 
    			var deviationOfAttribute:List[(Int,(Int,Double))]=List() 
    			var countNumValuesForRealAttributes:List[(Int,(Int,Int))]=List() 
    			var classOfInstances:List[(Int,Int)]=List()  //first Int the class the second the count
    			var valueFrequenciesForNominalAttributes:List[(Int,List[(Int,List[Double])])]=List() //int is the attribute; int is the class, list of 
    			//double freq of each value of attributes for that class
    			var mostFrequentValueForNominalAttributes:List[(Int,List[(Int,Int)])]=List()  
    			var valuesOfNominalAttributes:List[(Int,List[(Int,String)])]=List()
    			var attributeNames:List[(Int,String)]=List()
    			var numExamples=0;
    			var mostFrequentClass=0;
    			var leastFrequentClass=0;
    			var thereAreNominal=0
    			var thereAreRealValued=0
    			var numOfInstance=0


					def getNumAttributes():Int={ return numAttributes}
					def getNumAttributesMC():Int={ return numAttributesMC}

					def setTypeOfAttribute(attribute:Int,typeAtt:Int){
						if(typeOfAttribute(attribute)!=(-1))
							println("attribute "+attribute +" already defined");
						else{  if(typeAtt==REAL)
							thereAreRealValued=1;
						if(typeAtt==NOMINAL && attribute!=numAttributesMC)
							thereAreNominal=1;
						}
						typeOfAttribute(attribute)=typeAtt;
					}

					def getTypeOfAttribute(att:Int):Int={return typeOfAttribute(att)}

					def insertNominalValue(attribute:Int,value:String){


						if(typeOfAttribute(attribute)==NOMINAL){

							thereAreNominal= thereAreNominal+1;

							if( valuesOfNominalAttributes.filter(_._1==attribute).isEmpty){
               
								var realvalue =0
								var dump:List[(Int,String)]=List()
                dump=dump.+:((realvalue,value))
                println("the dump "+dump)
								valuesOfNominalAttributes=((attribute,dump))::valuesOfNominalAttributes
                println("the values of nominal "+valuesOfNominalAttributes)
							}

							else {
								var x2= valuesOfNominalAttributes.filter(_._1==attribute).apply(0)._2.filter(p => p._2.equals(value))
                
								if(x2.isEmpty){
                    println("second adding "+value +" to att "+attribute)
                    var item= valuesOfNominalAttributes.filter(p => p._1==attribute).apply(0)
                    var pos=valuesOfNominalAttributes.indexOf(item)       
										var realvalue=valuesOfNominalAttributes.filter(p => p._1==attribute).apply(0)._2.reverse.last._1+1
										var v= (attribute,(realvalue,value)::( valuesOfNominalAttributes.filter(_._1==attribute).apply(0)._2))
										valuesOfNominalAttributes= valuesOfNominalAttributes.updated(pos, v)
										//valuesOfNominalAttributes=valuesOfNominalAttributes.+:(v)
										}
							  else thereAreNominal= thereAreNominal-1;
							}
						}
					//	if(attribute==numAttributes)  thereAreNominal= thereAreNominal-1;
					}

					def getNominalValue(attribute:Int,value:Int):String={

							if(typeOfAttribute(attribute)==NOMINAL){
								var v= valuesOfNominalAttributes.filter(_._1==attribute).apply(0)._2.filter(_._1==value).apply(0)._2
							  return v 
							}
              
							return null
					}


					def InserAttributeName(id:Int,name:String) { attributeNames=(id,name)::(attributeNames)}

					def getAttributeName(att:Int):String={return attributeNames.filter(_._1==att).apply(0)._2}

					def updateClassCounters(counts:List[(Int,Int)]){classOfInstances=counts}

					def onlyNominalAttributes():Boolean={return (thereAreRealValued==0 && thereAreNominal!=0)}
          
					def onlyRealAttributes():Boolean={return (thereAreRealValued!=0 && thereAreNominal==0)}


					def getNumValuesAttribute(attribute:Int):Int={
							 if(valuesOfNominalAttributes.filter(_._1==attribute).isEmpty) 
                 return 0
							 else 
								 return valuesOfNominalAttributes.filter(_._1==attribute).apply(0)._2.size
					} 


					def getNumClasses() :Int={
						
          // return valuesOfNominalAttributes.filter(x =>x._1==numAttributes).apply(0)._2.size
           return  getNumValuesAttribute(numAttributes)
					}

					def valueOfNominalAttribute(attribute:Int,string:String):Int= {
							//var values=0
							var value=(-1);
							//if(attribute==numAttributes) //values=getNumClasses()

						//		value= valuesOfNominalAttributes.filter(_._1==numAttributes).apply(0)._2.filter(p => p._2.equals(string)).apply(0)._1

					//	 else       // if(valuesOfNominalAttributes.filter(_._1==attribute).apply(0)._2.size!=0)

							  value= valuesOfNominalAttributes.filter(_._1==attribute).apply(0)._2.filter(p => p._2.equals(string)).apply(0)._1


						return value;
					}
          
          
          def insertInsance(instances:RDD[Instance]){
          
            
            instances.map { x => insertInsance(x); x.updatMissing()}
            
            
          }
          

					def insertInsance(ins:Instance){
						if(numExamples==0) 
						{  
							var numClasses=getNumValuesAttribute(numAttributes)
								for(j<-0 to numClasses-1) classOfInstances=((j,0))::classOfInstances
                
                for(i <- 0 to numAttributes-1)
										if(typeOfAttribute(i)==NOMINAL) {
											mostFrequentValueForNominalAttributes=((i,List.fill(numClasses)((0,0))))::mostFrequentValueForNominalAttributes
											var x:List[(Int,List[Double])]=List()
											for(k<-0 to numClasses){ 
												averageOfAttribute=(i,(k,0.0))::averageOfAttribute
												deviationOfAttribute=(i,(k,0.0))::deviationOfAttribute
												countNumValuesForRealAttributes=(i,(k,0))::countNumValuesForRealAttributes
											  x=( k, List.fill(getNumValuesAttribute(i))(0.0) )::x
                        }

											  valueFrequenciesForNominalAttributes=(i,x)::valueFrequenciesForNominalAttributes

										}

						}

						 var instanceclass=ins.getclass()
            
						 classOfInstances=classOfInstances.map(x => if(x._1==instanceclass) (x._1,x._2+1) else x)  //increment
               
						 for(i <- 0 to numAttributes-1) 
								if((ins.isMissing(i)==0 )){   //&& ins.hasMissingValues()==1) ||ins.hasMissingValues()==0) { 
										if(typeOfAttribute(i)==NOMINAL)
											valueFrequenciesForNominalAttributes= valueFrequenciesForNominalAttributes.map(x =>
										  if(x._1==i ){
                      var temp= x._2.filter(y => y._1==instanceclass)(0);
                      var pos=x._2.indexOf(temp);
                       // println("instance class="+instanceclass +" pos= "+pos)
                      var z=temp._2.toArray; 
                      z(ins.valueOfAttribute(i))=z(ins.valueOfAttribute(i))+1.0;  
											(x._1, x._2.updated(pos,(instanceclass,z.toList)))
											} 
                      else x )
										else {
												averageOfAttribute=averageOfAttribute.map(   x => if(x._1==i && x._2._1==instanceclass)  
												{var z=x._2._2; z=z+ins.realValueOfAttribute(i);  (x._1, (instanceclass, z))}
                        else x)
												
                        deviationOfAttribute=deviationOfAttribute.map(   x => if(x._1==i && x._2._1==instanceclass)  
												{var z=x._2._2; z=z+(ins.realValueOfAttribute(i)*ins.realValueOfAttribute(i));  (x._1, (instanceclass, z))}
                        else x)
												
                        countNumValuesForRealAttributes=countNumValuesForRealAttributes.map(x => 
												if(x._1==i && x._2._1==instanceclass) 
												{var z=x._2._2; z=z+1;  (x._1, (instanceclass, z))}
                        else x)
												
                        if(numExamples==0) { 
                          minDomain(i)= ins.realValueOfAttribute(i); maxDomain(i)=ins.realValueOfAttribute(i)
                             }
												else {
													if(ins.realValueOfAttribute(i)<minDomain(i)) minDomain(i)= ins.realValueOfAttribute(i)
															if(ins.realValueOfAttribute(i)>maxDomain(i)) maxDomain(i)= ins.realValueOfAttribute(i)

												     }
												sizeDomain(i)=maxDomain(i)-minDomain(i)   
												sizeDomain2(i)=sizeDomain(i)/2
											}

									}

						numExamples=numExamples+1  

					}

					def calculateAverage(){
            println("trying to build values of nominal "+valuesOfNominalAttributes)
						var numClass=getNumValuesAttribute(numAttributes)
            println(classOfInstances)
					//	mostFrequentClass=classOfInstances.map(x =>x._2).max
					//	var most=classOfInstances.indexOf( classOfInstances.find( x=> x._2==mostFrequentClass))
					//	leastFrequentClass=classOfInstances.map(x =>x._2).min
					//	var least=classOfInstances.indexOf( classOfInstances.find( x=> x._2==leastFrequentClass))

						var nominalOnly= typeOfAttribute.toList.map( x=> if(x==NOMINAL) typeOfAttribute.indexOf(x) else -1).filter { x => x!=(-1) }
						var realOnly=typeOfAttribute.toList.map( x=> if(x==REAL) typeOfAttribute.indexOf(x) else -1).filter { x => x!=(-1) }

						var temp=valueFrequenciesForNominalAttributes.map(x => (x._1, x._2.map(y => y._2)))
            var max=temp.map(x => maxiOf(x))  //we found for every att most freq value
					
            valueFrequenciesForNominalAttributes.foreach(x => println(x._2)) 
            
            var valuesCount=valueFrequenciesForNominalAttributes.map(x => (x._1,(x._2.map(y=>(y._1, (y._2.sum))  ))))    
                

					  mostFrequentValueForNominalAttributes=valueFrequenciesForNominalAttributes.map{x =>  (x._1, maxforEachClass(x._1,x._2,max))     }
                        
          }

					def maxforEachClass(att:Int,freq:List[(Int,List[Double])],max:List[(Int,Int)] ):List[(Int,Int)]={

						var dump=   freq.map(x => (x._1, 
                if(x._2.max.toInt!=0) 
							    x._2.max.toInt 
								else 
									max.filter(y => y._1==att).apply(0)._2))
						return dump
					}

					def maxiOf(item:(Int,List[List[Double]])):(Int,Int)={

						var att=item._1  
						var total=Array.fill(getNumValuesAttribute(att))(0.0)
						var x= item._2.foreach { x => {var y=x.toArray; for(i <- 0 to y.size-1) total(i)=total(i)+ y(i)} }     

						return (att,total.indexOf(total.max))
					}  

					def getInstancesOfClass( pClass:Int):Int= {return classOfInstances.filter(x => x._1==pClass).apply(0)._2}


        def getAverageOfAttribute(instanceClass :Int,i:Int):Double={
        	return   (averageOfAttribute.find(x=> x._1==i).toList)(0)._2._2
        }

        def getFrequencyOfValueOfAttribute(instanceClass :Int,i:Int , value: Int):Double=
        {
        	return (valueFrequenciesForNominalAttributes.find(x=> x._1==i)toList)(0)._2.find(y=>y._1==instanceClass).toList(0)._2(value)
        }


        def getMostFrequentValueOfAttribute(instanceClass:Int,i:Int):Int={ 
        	if(mostFrequentValueForNominalAttributes.filter(p=> p._1==i).isEmpty ) 
        		return -1
        	else{
            if(mostFrequentValueForNominalAttributes.filter(p=> p._1==i).apply(0)._2.isEmpty)
              return -1
            else  
        		  return mostFrequentValueForNominalAttributes.filter(p=> p._1==i).apply(0)._2.filter(x => x._1==instanceClass).apply(0)._2
              }
        }


        def setBounds(min:Array[Int], max:Array[Int]){
        	for (i<- 0 to numAttributes-1){
        		minDomain(i)=min(i)
        		maxDomain(i)=max(i)
        		sizeDomain(i)=max(i)-min(i)
        		sizeDomain2(i)=sizeDomain(i)/2
        	}
        }

}