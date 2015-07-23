package bio


class GA(POP_SIZE:Int, is:instanceSet,tglobal:TimerGlobal ) {

	val att=is.attributesInfo
			val cm =scala.io.Source.fromFile("myfile.txt").getLines.toList
			var optimizationMethod = cm.filter {x=>x.contains("MAX_MIN") }.apply(0).split(" ")(1).toInt

			var selectionAlg = cm.filter {x=>x.contains("SELECTION_ALGORITHM") }.apply(0).split(" ")(1)
			var  tournamentSize = cm.filter {x=>x.contains("TOURNAMENT_SIZE") }.apply(0).split(" ")(1).toInt 
      println("the tournament "+tournamentSize)
			var showFronts = cm.contains("SHOW_FRONTS") 
			var mutationProb = cm.filter {x=>x.contains("PROB_INDIVIDUAL_MUTATION") }.apply(0).split(" ")(1).toDouble
			var crossoverProb = cm.filter {x=>x.contains("PROB_CROSSOVER") }.apply(0).split(" ")(1).toDouble 

			var population=Array.ofDim[Hyperrect_List](POP_SIZE)
			var offspringPopulation :List[Hyperrect_List]=List()
			var populationRanked =Array.ofDim[Hyperrect_List](POP_SIZE)
			var maxFitness=0.0
			var minFitness=0.0
			var currentIteration=0;
			var flagResetBest=0;
			var timer= tglobal
      geneticAlgorithm()
      
			def geneticAlgorithm()
			{


				initializePopulation();

				doFitnessComputations(is)
				createPopulationRank()
				checkBestIndividual()
			}

			def doIterations( n:Int)
			{
				for (i<- 0 to n-1) {


         
					selectionAlgorithm()
					crossover()
					mutation()

					replacementAlgorithm()

					checkBestIndividual();
					currentIteration=currentIteration+1;
				}
			}

			def createPopulationRank() {


				populationRanked= population.sortBy { x => x.fitness }
			}


			def  doFitnessComputations(is:instanceSet){
			//	population.map { x => x.fitnessComputation(is) }
			is.classifierFitness(population.toList)
        populationRanked= population.sortBy { x => x.fitness }
        maxFitness=populationRanked(POP_SIZE-1).fitness
						minFitness=populationRanked(0).fitness
			} 

			def initializePopulation() {
				var i=0;


				for (i <- 0 to POP_SIZE-1) {
					population(i) = new Hyperrect_List(optimizationMethod,timer,is,0)
        println("a chromosome "+ population(i).chromosome)
				}



				flagResetBest=0;
				currentIteration = 0;
			}

			def crossover(){
				var r=new scala.util.Random
						var p1=  r.nextInt(population.size-1)
						var p2=  r.nextInt(population.size-1)
						do{ 
							p2= r.nextInt(population.size-1) 
						}while(p1==p2);

				if(r.nextDouble()>crossoverProb) 
					crossoverParents(p1,p2)

					else {
						    offspringPopulation=offspringPopulation.::(population(p1))   
								offspringPopulation=offspringPopulation.::(population(p2))   
					}

			}


//			def crossoverTwoParents(p1:Int, p2:Int){
//				var r=new scala.util.Random
//
//						var offspring1:List[(Int,String)]=List()
//						var offspring2:List[(Int,String)]=List() 
//						var m1=r.nextInt(population(p1).length)          // select the crossover point
//						//   var m2=r.nextInt(ind2.length) 
//						val (left, right) = population(p1).chromosome.splitAt(m1)
//						val (left2, right2) = population(p2).chromosome.splitAt(m1)
//
//						population(p1).chromosome=(offspring1.:::(left)).:::(right2)
//						population(p2).chromosome=(offspring2.:::(left2)).:::(right)    
//
//						offspringPopulation=offspringPopulation.::(population(p1))   
//						offspringPopulation=offspringPopulation.::(population(p2)) 
//
//			}
      
      def crossoverParents(p1:Int, p2:Int){
        var r=new scala.util.Random

      val(off1,off2)= population(p1).crossover(population(p2))
        offspringPopulation=off1::offspringPopulation  
        offspringPopulation=off2::offspringPopulation 
      }
      
      
			//use the selection only to copy from prev generation to new one
			def selectionAlgorithm(){
				var r=new scala.util.Random
						var winner =r.nextInt(population.size-1)
						for(i<-0 to tournamentSize-1)
						{
							var candidate =r.nextInt(population.size-1)
									if(population(candidate).compareToIndividual(population(winner), optimizationMethod)>0)
										winner=candidate
						}
				offspringPopulation=(population(winner))::offspringPopulation

			}

			def mutation(){
				var r=new scala.util.Random
						var pos=r.nextInt(offspringPopulation.size)
						var ind=offspringPopulation(pos)
						ind= ind.mutation()
						offspringPopulation=offspringPopulation.updated(pos,ind)
			}

			def checkBestIndividual():Hyperrect_List={
					return populationRanked(POP_SIZE-1)
			}

			def replacementAlgorithm(){
        println("i'm replacing my generation")
        // to be changed 
        var p=(population.toList:::(offspringPopulation) toArray ) .sortBy { x => x.fitness }
        
				population=p.take(POP_SIZE)
        offspringPopulation=List()
        println("*********************new population size "+population.size)
						doFitnessComputations(is)
						createPopulationRank()
			}
			def resetBest(){
				flagResetBest=1
			}
}
