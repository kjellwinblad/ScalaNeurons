package me.winsh.scalaneurons.trainers

import me.winsh.scalaneurons.Trainer
import me.winsh.scalaneurons.Neuron
import me.winsh.scalaneurons.NeuronLink
import me.winsh.scalaneurons.BasicNeuronLink
import me.winsh.scalaneurons.NeedsTrigeringNeuron
import me.winsh.scalaneurons.InputOutputPattern
import me.winsh.scalaneurons.networks.FeedforwardNetwork
import scala.math.pow

import scala.collection.mutable.HashMap

class PackpropagationTrainer(network: FeedforwardNetwork, val learningRate: Double = 0.1) extends Trainer[FeedforwardNetwork](network) {

	//println(network.layers)
	//println(network.layers.map(_.map(n=>(n.inputLinks, n.outputLinks))))
	
  private val neuronActivationFuncDerivateMap = HashMap[NeedsTrigeringNeuron, Double]()

  private val errorPartialDerivativeMap = HashMap[BasicNeuronLink, (Double, Double)]()

  def train(trainingPatterns: Iterable[InputOutputPattern], nrOfTrainingIterations: Int = 1, randomOrder: Boolean = true): Iterable[Double] = {

    val patterns = trainingPatterns.toArray
    
    List.fill(nrOfTrainingIterations) {
    	
      if (randomOrder) shuffle(patterns)      
      
      (patterns.map(train(_)).foldLeft(0.0)(_+_))/patterns.size
    }

  }

  def train(trainingPattern: InputOutputPattern): Double = {

    val neuronActivationFuncDerivateMap = performFeedforwardComputation(trainingPattern.input)

    val (errorSum, errorPartialDerivativeMap) = performPackpropagationComputation(neuronActivationFuncDerivateMap, trainingPattern.output)

    updateWeights(errorPartialDerivativeMap)
//println("TRAIN RESULT")
//    	println(network.layers)
//	println(network.layers.map(_.map(n=>(n.inputLinks, n.outputLinks))))
//println("TRAIN STOP")    
    errorSum
  }

  private def performFeedforwardComputation(inputPattern: Iterable[Double]) = {

    //Feed the input to the network
    //This will propagate the input through the network to the output
    network.input = inputPattern
    //Calculate the derivative at every node
    network.layers.foreach(_.foreach((neuron) => {
      neuronActivationFuncDerivateMap.put(neuron, neuron.activationFunction.derivative(neuron.latestSum))
    }))

    neuronActivationFuncDerivateMap
  }

  private def performPackpropagationComputation(neuronActivationFuncDerivateMap: HashMap[NeedsTrigeringNeuron, Double], outputPattern: Iterable[Double]) = {

    //Compute the error vector and bind each error derivate to the corresponding output link 
    val errors = outputPattern.zipWithIndex.map { outputIndex =>
      val perferedOutput = outputIndex._1
      val index = outputIndex._2
      val actualPreferedDiff = network.output(index) - perferedOutput
      val error = 0.5 * pow(actualPreferedDiff, 2.0)
      val dervate = actualPreferedDiff

      //The second value will never be used here
      errorPartialDerivativeMap.put(network.outputLinks(index), (dervate, dervate))

      error
    }

    //Calculate partial derivatives for the modifiable weights in the network
    network.layers.reverse.foreach(_.foreach { neuron =>

      val neuronDerivate = neuronActivationFuncDerivateMap.get(neuron).get

      val additionRule = neuron.outputLinks.foldLeft(0.0)((sum, link) => errorPartialDerivativeMap.get(link).get._1)

      //Chain rule
      val tmpValue = neuronDerivate * additionRule

      //Save the calculated info in all output links
      //(This could be made more effective, the value only needs to be saved in the node)
      neuron.inputLinks.foreach { link =>
        errorPartialDerivativeMap.put(link, (link.weight * tmpValue, link.output * tmpValue))
      }
    })

    (errors.foldLeft(0.0)(_ + _), errorPartialDerivativeMap)
  }

  private def updateWeights(errorPartialDerivativeMap: HashMap[BasicNeuronLink, (Double, Double)]) =
    network.layers.foreach(_.foreach(_.inputLinks.foreach { link =>

      errorPartialDerivativeMap.get(link) match {
    	  case Some((_, derivative)) =>link.weight = link.weight - learningRate * derivative//;println("delta " + (- learningRate * derivative))
    	  case None =>Unit
      }

      
    }))

  //Taken from: http://stackoverflow.com/questions/1259223/how-to-use-java-collections-shuffle-on-a-scala-array
  // Fisher-Yates shuffle, see: http://en.wikipedia.org/wiki/Fisher–Yates_shuffle
  private def shuffle[T](array: Array[T]): Array[T] = {
    val rnd = new java.util.Random
    for (n <- Iterator.range(array.length - 1, 0, -1)) {
      val k = rnd.nextInt(n + 1)
      val t = array(k); array(k) = array(n); array(n) = t
    }
    return array
  }

}