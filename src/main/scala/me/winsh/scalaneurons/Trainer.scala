package me.winsh.scalaneurons

import me.winsh.scalaneurons.networks.FeedforwardNetwork



abstract class Trainer[A <: Network](val network:A) {
	
	
	def train(trainingPatterns:Iterable[InputOutputPattern], nrOfTrainingIterations:Int=40, randomOrder:Boolean=true):Iterable[Double]
	
	def train(trainingPattern:InputOutputPattern):Double
	
}