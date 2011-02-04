package me.winsh.scalaneurons

import scala.collection.mutable.Set

abstract class BasicNeuron{
	var activationFunction:ActivationFunction
	def inputLinks:Iterable[BasicNeuronLink]
	def outputLinks:Iterable[BasicNeuronLink]
	
	def addInputLink(link:BasicNeuronLink)
	def removeInputLink(link:BasicNeuronLink)
	
	def addOutputLink(link:BasicNeuronLink)
	def removeOutputLink(link:BasicNeuronLink)
}

abstract class NeedsTrigeringNeuron extends BasicNeuron{
	/**
	 * Triggers the neuron and pushes the output to the output links
	 * @return (calculatedSum, calculatedOutput)
	 */
	def trigger():(Double, Double)
	var latestOutput = 0.0
	var latestSum = 0.0
}

class Neuron(
	var activationFunction:ActivationFunction = HyperbolicTangentAproxFunction(),
	initialInputLinks:Iterable[BasicNeuronLink] = Set[BasicNeuronLink](),
	initialOutputLinks:Iterable[BasicNeuronLink] = Set[BasicNeuronLink]()) extends NeedsTrigeringNeuron {
	
	private val inputLinksSet:Set[BasicNeuronLink] = Set() ++ initialInputLinks
	private val outputLinksSet:Set[BasicNeuronLink] = Set() ++ initialOutputLinks
	

	
	def inputLinks:Iterable[BasicNeuronLink] = inputLinksSet
	def outputLinks:Iterable[BasicNeuronLink] = outputLinksSet
	
	def trigger() = {
		
		implicit def basicNeuronLinkToDouble(i:BasicNeuronLink)=i.output
		
		val sum = inputLinks.foldLeft(0.0)(_+_)

		latestSum = sum
		
		val out = activationFunction.function(sum)
		
		latestOutput = out
		
		for(link <- outputLinks)
			link.input = out	
			
		(sum, out)
	}

	
	def addInputLink(link:BasicNeuronLink){inputLinksSet.add(link)}
	def removeInputLink(link:BasicNeuronLink){inputLinksSet.remove(link)}
		
	def addOutputLink(link:BasicNeuronLink){outputLinksSet.add(link)}
	def removeOutputLink(link:BasicNeuronLink){outputLinksSet.remove(link)}
	
	override def toString = "Neuron (latest output= " + latestOutput + ")"
}

