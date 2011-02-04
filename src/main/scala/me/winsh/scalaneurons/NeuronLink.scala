package me.winsh.scalaneurons

import scala.util.Random

abstract class BasicNeuronLink {
  var input: Double
  var weight: Double
  def output: Double
}

object BasicNeuronLink {

  private val randomizer = new Random()

  def nextRandomValue(from: Double, to: Double) = {
    require(from < to)
    synchronized {
      randomizer.nextDouble() * (to - from) + from
    }
  }
}

class NeuronLink(
  var input: Double = 0.0,
  var weight: Double = BasicNeuronLink.nextRandomValue(-1.0, 1.0)) extends BasicNeuronLink {
  
  def output= input * weight
  
  override def toString = "NeuronLink(input=" + input + ",weight=" + weight + ",output=" + output + ")"
  
}