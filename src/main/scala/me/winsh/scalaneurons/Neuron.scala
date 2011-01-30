package me.winsh.scalaneurons.core

import scala.collection.mutable.Set

abstract class BasicNeuron {
	var activationFunction:ActivationFunction
	var inputLinks:Iterable[BasicNeuronLink]
	var outputLinks:Iterable[BasicNeuronLink]
	def computeOutput()
}

case class Neuron(
	var activationFunction:ActivationFunction = HyperbolicTangentAproxFunction(),
	var inputLinks:Iterable[BasicNeuronLink] = Set[BasicNeuronLink](),
	var outputLinks:Iterable[BasicNeuronLink] = Set[BasicNeuronLink]()) extends BasicNeuron {
	
	def computeOutput() = {
		
		implicit def basicNeuronLinkToDouble(i:BasicNeuronLink)=i.output
		
		val sum = inputLinks.foldLeft(0.0)(_+_)

		activationFunction.function(sum)
	}

}