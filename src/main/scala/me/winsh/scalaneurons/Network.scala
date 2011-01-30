package me.winsh.scalaneurons

abstract class Network {

	val nrOfInputNeurons:Int
	
	val nrOfOutputNeurons:Int
	
	def setInput(inputNr:Int, value:Double)
	
	def setInput(input:Iterable[Double])
	
	def getOutput(outputNr:Int)
	
	def getOutput:Array[Double]

}