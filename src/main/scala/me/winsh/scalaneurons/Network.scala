package me.winsh.scalaneurons

abstract class Network {

	def nrOfInputSlots:Int
	
	def nrOfOutputSlots:Int
	
	def setInput(inputNr:Int, value:Double)
	
	def input:Iterable[Double]
	
	def input_=(input:Iterable[Double])
	
	def eval(input:Iterable[Double]):Iterable[Double]
	
	def output(outputNr:Int):Double
	
	def getOutput:Array[Double]
	
}

object Network{
	
	/**
	 * The function connects all neurons in a layer with all neurons in the next layer
	 * @param layers list of layers where every layer is represented as a list of neurons
	 * @param weightInitializer A function used to initialize the weights for the links.
	 *                          The default is a random value between -1 and 1
	 */
  def connectLayers(
		  layers:List[List[BasicNeuron]], 
		  weightInitializer:()=>Double = (()=>BasicNeuronLink.nextRandomValue(-1.0, 1.0))):List[List[BasicNeuronLink]] =
		layers match {
			case Nil => Nil
			case l::Nil => Nil
			case l1::l2::rest => (for (l1Neuron <- l1 ; l2Neuron <- l2) yield {
				val link = new NeuronLink(weight=weightInitializer())
				l1Neuron.addOutputLink(link)
				l2Neuron.addInputLink(link)
				link
			}).toList::connectLayers(l2::rest)
		}
	
	
}