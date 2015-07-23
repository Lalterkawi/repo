package bio
import scala.util.control.Breaks._


object biohel {




	var percentageOfLearning = 0.0;
	var lastIteration = 0;

	var minAcc = 0.5;
	var cm:List[String]=List()
			var stop=0
			var tGlobals:TimerGlobal=null
			var is :instanceSet=null
			def runGA(att:Attributes):Hyperrect_List={
		      stop = 0;
		      lastIteration = 0;
		      percentageOfLearning = 0;
		      var popsize=   cm.filter {x=>x.contains("POP_SIZE") }.apply(0).split(" ")(1).toInt
				  var pw= new  populationWrapper(popsize,att,is,tGlobals)


		// var(ave1,ave2)= pw.getAverageAccuracies() called just to check the initial accuracy for the initial population

		      var countIt = 0
		      var numIterations =cm.filter {x=>x.contains("ITERATIONS") }.apply(0).split(" ")(1).toInt
		      if (numIterations == 0)
			       stop = 1;

		      while( stop==0) {


			      pw.gaIteration()


			      countIt=countIt+1

			      if (countIt == numIterations)
				       stop = 1;
			      else if (countIt == numIterations - 1)
				       lastIteration = 1;
			      percentageOfLearning = countIt.toDouble / (numIterations).toDouble
		}

		     if (stop==1 && countIt < numIterations)
			      return null;

		     var ind = pw.getBestOverall()

				 var clone = ind   //clone is a way to copy the individual inorder not to pass the original
				 return clone
	}

	def main(args: Array[String] ) {

		val instanceTrainFileName="myinput.txt" //readLine("enter train name ")
				val instanceTestFileName= "myinput.txt" //readLine("enter test name ")
				val configFileName= "myfile.txt"//readLine("enter config name ")


				cm=scala.io.Source.fromFile(configFileName).getLines.toList
				var classifierl:Array[Hyperrect_List]=null
				var config=cm.filter {x=>x.contains("NO_ATTRIBUTES") }.apply(0).split(" ")(1)
				println(config)
				var att=new Attributes(Integer.parseInt(config))
				var r=new scala.util.Random
				is = new instanceSet(instanceTrainFileName,configFileName,att ,1)

			  tGlobals=new TimerGlobal(att)
    var ruleSet=new Classifier_Aggregated(classifierl, att)
    var optimizationMethod = cm.filter {x=>x.contains("MAX_MIN") }.apply(0).split(" ")(1).toInt
    println("optm "+optimizationMethod)
   var countRepeat = 0


    do {
        var cancelled = 0;
        var best:Hyperrect_List =null //classifier

               for ( i <- 0  to tGlobals.numRepetitionsLearning-1) {
          var   bestIt = runGA(att);
            if (bestIt==null) {
                cancelled = 1;
                break;
            }

            if (best == null || bestIt.compareToIndividual(best,optimizationMethod) > 0) {
                if (best!=null)

                best = bestIt;
            }

            if (i < tGlobals.numRepetitionsLearning-1) {
                is.restart()
               // timers.reinit();
            }

        }
                if (cancelled==1)
                break;


        if (is.isMajority(best,cm.filter {x=>x.contains("COVERAGE_BREAKPOINT") }.take(1).toString().split(" ")(1).toDouble)) {


            is.removeInstancesAndRestart(best)
        classifierl= ruleSet.addClassifier(best)

            countRepeat = 0;

            if (is.numInstances == 0)
                break;
        } else {
            countRepeat=countRepeat+1

            if (countRepeat == 3) {
                ruleSet.setDefaultRule(is);
                break;
            } else {
                is.restart();

            }
        }


    } while (true)





   // is.classifierStats(ruleSet)
   // is = new instanceSet(instanceTestFileName,configFileName,att ,2)
   // is.classifierStats(ruleSet)

				  }




}