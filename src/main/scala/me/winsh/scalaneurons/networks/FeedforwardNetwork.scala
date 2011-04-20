package me.winsh.scalaneurons.networks

import scala.collection.Iterable
import me.winsh.scalaneurons.Network
import me.winsh.scalaneurons.NeedsTrigeringNeuron
import me.winsh.scalaneurons.Neuron
import me.winsh.scalaneurons.NeuronLink
import me.winsh.scalaneurons.BasicNeuronLink
import me.winsh.scalaneurons.LinearFunction
import me.winsh.scalaneurons.ActivationFunction
import me.winsh.scalaneurons.HyperbolicTangentAproxFunction

abstract class FeedforwardNetwork(
  val nrOfInputSlots: Int,
  initLayers: Iterable[Iterable[NeedsTrigeringNeuron]],
  weightInitializer: () => Double,
  enableBiasInputs: Boolean = true) extends Network {

  //Public Declarations

  val nrOfOutputSlots = initLayers.last.size

  val layers = initLayers.map(_.toList).toList

  //Private Declarations

  private val inputLinks = Array.fill(nrOfInputSlots)(new NeuronLink(weight = 1.0))

  private val inputNeurons = Array.fill(nrOfInputSlots)(new Neuron(LinearFunction()))

  val outputLinks = Array.fill(initLayers.last.size)(new NeuronLink(weight = 1.0))

  //Initialization

  require(initLayers.size > 0, "A Feedforward network needs at least one layer.")

  Network.connectLayers(inputNeurons.toList :: layers, weightInitializer)

  //Add bias link to all neurons
  if (enableBiasInputs)
    layers.foreach(_.foreach(_.addInputLink(new NeuronLink(input = 1.0, weight = weightInitializer()))))

  inputNeurons.zipWithIndex.foreach { (param) =>
    val index = param._2
    val neuron = param._1
    neuron.addInputLink(inputLinks(index))
  }

  initLayers.last.zipWithIndex.foreach { (param) =>
    val index = param._2
    val neuron = param._1
    neuron.addOutputLink(outputLinks(index))
  }

  //Methods

  def setInput(inputNr: Int, value: Double): Unit = {
    inputLinks(inputNr).input = value
    inputNeurons(inputNr).trigger()
    layers.foreach(_.foreach(_.trigger()))
  }

  def input_=(input: Iterable[Double]): Unit = input.zipWithIndex.foreach { param =>
    inputLinks(param._2).input = param._1
    inputNeurons(param._2).trigger()
    layers.foreach(_.foreach(_.trigger()))
  }

  def input = inputLinks.map(_.input)

  def output(outputNr: Int): Double = outputLinks(outputNr).output

  def getOutput: Array[Double] = outputLinks.map(_.output)

  def eval(in: Iterable[Double]): Iterable[Double] = {
    input = in
    getOutput
  }

}

object FeedforwardNetwork {

  protected class FeedforwardNetworkImpl(
    nrOfInputSlots: Int,
    initLayers: Iterable[Iterable[NeedsTrigeringNeuron]],
    weightInitializer: () => Double) extends FeedforwardNetwork(nrOfInputSlots, initLayers, weightInitializer) {}

  def apply(
    nrOfInputSlots: Int,
    initLayers: Iterable[Iterable[NeedsTrigeringNeuron]],
    weightInitializer: () => Double) =
    new FeedforwardNetworkImpl(nrOfInputSlots, initLayers, weightInitializer)

  def apply(
    nrOfInputSlots: Int,
    nrOfNodesActivFuncInLayers: List[(Int, ActivationFunction)],
    weightInitializer: () => Double) = {

    val initLayers = nrOfNodesActivFuncInLayers.map { param => List.fill(param._1)(new Neuron(param._2)) }

    new FeedforwardNetworkImpl(nrOfInputSlots, initLayers, weightInitializer)
  }

  def apply(
    nrOfInputSlots: Int,
    nrOfNodesInLayers: List[Int],
    activationFunction: ActivationFunction = HyperbolicTangentAproxFunction(),
    weightInitializer: () => Double = (() => BasicNeuronLink.nextRandomValue(-1.0, 1.0))) = {

    val initLayers = nrOfNodesInLayers.map { param => List.fill(param)(new Neuron(activationFunction)) }

    new FeedforwardNetworkImpl(nrOfInputSlots, initLayers, weightInitializer)
  }

}