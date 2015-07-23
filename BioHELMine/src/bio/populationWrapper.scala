package bio

class populationWrapper(POP:Int,att:Attributes,is:instanceSet,tglobal:TimerGlobal) {
var ga=new GA(POP,is,tglobal);

var popSize=POP

def activateModifiedFlag()
{

   var rk=ga.populationRanked
   rk.map { x => x.activateModified() }
   ga.resetBest();
}

def getPopulationRank():Array[Hyperrect_List]=
{

    return ga.populationRanked
}

def doFitnessCalculations() {
	  ga.doFitnessComputations(is);
}

def createPopulationRank() {
	  ga.createPopulationRank();
}

def gaIteration()
{
	  ga.doIterations(1)
}



def getBestOverall():Hyperrect_List=
{
		return ga.checkBestIndividual()
}

def getPopulation():Array[Hyperrect_List]=
{
		return ga.population
}


def getAverageAccuracies():(Double,Double)=
{

		var ave1=0.0;
		var ave2=0.0;
		var rk=ga.populationRanked

	  var av1= rk.map { x => x.accuracy}.sum
    var av2=rk.map { x => x.accuracy2}.sum
		
    ave1=av1/popSize;
		ave2=av2/popSize;
		return (ave1,ave2)
}


def getMaxAccuracy():Double=
{

		var rk=ga.populationRanked
		var max=rk.map { x => x.accuracy}.max
		return max;
}


def createClassifier( empty:Int):Hyperrect_List=
{
		return new Hyperrect_List(ga.optimizationMethod,ga.timer,is,0);
}


}