package me.winsh.scalaneurons.trainers

import org.junit._
import Assert._
import me.winsh.scalaneurons._
import me.winsh.scalaneurons.networks._

@Test
class PackpropagationTrainerTest {

  @Test
  def xorTest {
    //input1, input2, output
    val trainingData = List(
      (0, 0, 0),
      (0, 1, 1),
      (1, 0, 1),
      (1, 1, 0)).map { example =>
      val (i1, i2, o) = example
      InputOutputPattern(List(i1, i2), List(o))
    }

    val r = (1 to 100).map { _ =>

      val network = FeedforwardNetwork(nrOfInputSlots = 2,
        nrOfNodesInLayers = List(6, 1),
        activationFunction = SigmoidFunction(steepness = 2.5, center = 1.0),
        weightInitializer = () => BasicNeuronLink.nextRandomValue(0.0, 1.0))

      val trainer = new PackpropagationTrainer(network, 0.1)

     trainer.train(trainingData, 150)

      (0 == network.eval(List(0, 0)).first.round) &&
        (1 == network.eval(List(1, 0)).first.round) &&
        (1 == network.eval(List(0, 1)).first.round) &&
        (0 == network.eval(List(1, 1)).first.round)

    }

        assert(r.map(if(_)1 else 0).foldLeft(0.0)(_+_)>10)
  }

  @Test
  def andTest {
    //input1, input2, output
    val trainingData = List(
      (0, 0, 0),
      (0, 1, 0),
      (1, 0, 0),
      (1, 1, 1)).map { example =>
      val (i1, i2, o) = example
      InputOutputPattern(List(i1, i2), List(o))
    }

    val r = (1 to 100).map { _ =>

      val network = FeedforwardNetwork(nrOfInputSlots = 2,
        nrOfNodesInLayers = List(1),
        activationFunction = SigmoidFunction(steepness = 10.0, center = 1.0),
        weightInitializer = () => BasicNeuronLink.nextRandomValue(0.0, 1.0))

      val trainer = new PackpropagationTrainer(network, 0.1)

      trainer.train(trainingData, 60)

      (network.eval(List(0, 0)).first.round == 0) &&
        (network.eval(List(1, 0)).first.round == 0) &&
        (network.eval(List(0, 1)).first.round == 0) &&
        (network.eval(List(1, 1)).first.round == 1)

    }

    assert(r.map(if(_)1 else 0).foldLeft(0.0)(_+_)>90)

  }
}