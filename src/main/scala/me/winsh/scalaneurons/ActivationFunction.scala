package me.winsh.scalaneurons

import scala.math.E
import scala.math.pow

abstract class ActivationFunction {
	
	val description:String
	
	def function(sum:Double):Double
	
	def derivative(sum:Double):Double

}

trait HasNoDerivative{
	def derivative(sum:Double):Double = throw new Exception("The activation function is not differentiable")
}

case class LinearFunction(val slope:Double = 1.0, val center:Double=0.0) extends ActivationFunction{
	
	val description = "Linear Function"
	
	def function(sum:Double) = slope * (sum - center)
	
	def derivative(sum:Double):Double = slope
}
	
case class StepFunction(val leftValue:Double= -1.0,val rightValue:Double=1.0, val center:Double=0.0) extends ActivationFunction with HasNoDerivative{
	
	val description = "Step Function"
	
	def function(sum:Double) = 
		if(sum < center) leftValue else rightValue
			
}


case class RampFunction(val gap:Double = 1.0, val leftValue:Double= -1.0,val rightValue:Double=1.0, val center:Double=0.0) extends ActivationFunction with HasNoDerivative{
	
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
		
	def derivative(sum:Double):Double = {
		val x = (sum-center)
		
		val e_exp = pow(E,-steepness*x)
		
		(steepness*e_exp)/pow((1.0 + e_exp), 2.0)
	}
			
}

case class HyperbolicTangentFunction(val steepness:Double = 1.0, val center:Double=0.0) extends ActivationFunction{
	
	val description = "Hyperbolic Tangent Function"
	
	def function(sum:Double) = {
	val x = (sum-center)
		val s = steepness
		(pow(E, s*x) - pow(E, -s*x)) / (pow(E, s*x) + pow(E, -s*x))
	}
	
	def derivative(sum:Double):Double = {
		val x = (sum-center)
		val s = steepness
		
		val e_exp_minus = pow(E,-s*x)
		val e_exp_plus = pow(E,s*x)
		
		((s*e_exp_minus + s*e_exp_plus) /
		(e_exp_minus + e_exp_plus)) -
		((e_exp_plus-e_exp_minus) * (s*e_exp_plus-s*e_exp_minus)) /
		pow(e_exp_plus+e_exp_minus, 2.0)
	}
}

case class HyperbolicTangentAproxFunction(val steepness:Double = 1.0, val center:Double=0.0) extends ActivationFunction{
	
	val description = "Hyperbolic Tangent Aprox Function"
	
	def function(sum:Double) = 
		2.0 / (1.0 + pow(E, -steepness*(sum-center))) - 1.0
		
	def derivative(sum:Double):Double = {
		val x = (sum-center)
		val s = steepness
		
		val e_exp = pow(E,-s*x)
		
		(2.0*s*e_exp)/pow(1+e_exp,2.0)
	}
			
}

case class GaussianFunction(val standardDist:Double = 1.0, val center:Double=1.0) extends ActivationFunction{
	
	val description = "Gaussian Function"
	
	def function(sum:Double) = 
		pow(E, -pow(sum-center, 2.0)) / pow(standardDist, 2.0)
	  //exp(-x^2)/(s^2)
		
	def derivative(sum:Double):Double = {
		val x = (sum-center)
		val s = standardDist
		
		(2.0*x*pow(E, -pow(x, 2.0)))/pow(s, 2.0)
	}
			
}

