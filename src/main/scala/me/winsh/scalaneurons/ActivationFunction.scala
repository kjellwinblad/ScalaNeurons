package me.winsh.scalaneurons.core

import scala.math.E
import scala.math.pow

abstract class ActivationFunction {
	
	val description:String
	
	def function(sum:Double):Double

}

case class LinearFunction(val slope:Double, val center:Double=0.0) extends ActivationFunction{
	
	val description = "Linear Function"
	
	def function(sum:Double) = slope * (sum - center)
}
	
case class StepFunction(val leftValue:Double= -1.0,val rightValue:Double=1.0, val center:Double=0.0) extends ActivationFunction{
	
	val description = "Step Function"
	
	def function(sum:Double) = 
		if(sum < center) leftValue else rightValue
			
}


case class RampFunction(val gap:Double = 1.0, val leftValue:Double= -1.0,val rightValue:Double=1.0, val center:Double=0.0) extends ActivationFunction{
	
	val description = "Ramp Function"
	
	def function(sum:Double) = 
		if(sum - center <= -gap) leftValue 
		else if(sum - center < gap) sum - center
		else rightValue
			
}

case class SigmoidFunction(val steepness:Double = 1.0, val center:Double=0.0) extends ActivationFunction{
	
	val description = "Sigmoid Function"
	
	def function(sum:Double) = 
		1.0/(1.0 + pow(E,-steepness*(sum-center)))
			
}

case class HyperbolicTangentFunction(val steepness:Double = 1.0, val center:Double=0.0) extends ActivationFunction{
	
	val description = "Hyperbolic Tangent Function"
	
	def function(sum:Double) = 
		(pow(E, steepness*(sum-center)) - pow(E, -steepness*(sum-center))) /
        (pow(E, steepness*(sum-center)) + pow(E, -steepness*(sum-center)))
			
}

case class HyperbolicTangentAproxFunction(val steepness:Double = 1.0, val center:Double=0.0) extends ActivationFunction{
	
	val description = "Hyperbolic Tangent Aprox Function"
	
	def function(sum:Double) = 
		2.0 / (1.0 + pow(E, -steepness*(sum-center))) - 1.0
			
}

case class GaussianFunction(val standardDist:Double = 1.0, val center:Double=0.0) extends ActivationFunction{
	
	val description = "Gaussian Function"
	
	def function(sum:Double) = 
		pow(E, -pow(sum-center, 2.0)) / pow(standardDist, 2.0)
			
}

