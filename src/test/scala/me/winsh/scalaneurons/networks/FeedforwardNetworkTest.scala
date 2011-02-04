package me.winsh.scalaneurons.networks

import org.junit._
import Assert._
import me.winsh.scalaneurons._

@Test
class FeedforwardNetworkTest {

	@Test
	def creationTest{
	  val network = FeedforwardNetwork(nrOfInputSlots = 2,
      nrOfNodesInLayers = List(5, 2),
      activationFunction = SigmoidFunction(),
      weightInitializer = () => BasicNeuronLink.nextRandomValue(0.0, 1.0))
	}
	
}