package bandit

import org.deeplearning4j.nn.conf.NeuralNetConfiguration
import org.deeplearning4j.nn.conf.layers.DenseLayer
import org.deeplearning4j.nn.conf.layers.OutputLayer
import org.deeplearning4j.nn.multilayer.MultiLayerNetwork
import org.deeplearning4j.nn.weights.WeightInit
import org.nd4j.linalg.activations.Activation
import org.nd4j.linalg.api.ndarray.INDArray
import org.nd4j.linalg.factory.Nd4j
import org.nd4j.linalg.learning.config.Adam
import org.nd4j.linalg.lossfunctions.LossFunctions
import java.util.*
import kotlin.math.exp

// todo running averages

fun makeNetwork(numArms: Int): MultiLayerNetwork {

    return MultiLayerNetwork(NeuralNetConfiguration.Builder()
            .seed(12345)
            .updater(Adam())
//                .l2(1e-4)
            .list()
            .layer(DenseLayer.Builder()
                    .nIn(numArms)
                    .nOut(numArms * 10)
                    .activation(Activation.RELU)
                    .weightInit(WeightInit.XAVIER)
                    .build())
            .layer(OutputLayer.Builder(LossFunctions.LossFunction.MSE)
                    .nIn(numArms * 10)
                    .nOut(numArms)
                    .activation(Activation.RELU)
                    .weightInit(WeightInit.XAVIER)
                    .build())
            .build())
}

inline fun <reified T>singleton(t: T): Array<T> {
    return Array(1) { t }
}

fun randprob(): Double {
    return (0..99).random() / 100.0
}

fun makeArms(arms: Int): Array<Array<Double>> {
    return Array(arms) { Array(arms) { randprob() } }
}

fun printArms(arms: Array<DoubleArray>) {
    for (i in 0..ContextualBandit.numArms -1) println(Arrays.toString(arms.get(i)))
}

fun reward(prob: Double): Int {
    return ((0..9).map { if (randprob() < prob) { 1 } else { 0 } }).sum()
}

fun rewardForState(arms: Array<DoubleArray>, state: Int, arm: Int): Int {
    return reward(arms.get(state).get(arm))
}

fun oneHot(N: Int, pos: Int): Array<Double> {
    return Array(N) { i -> if (i == pos) {1.0} else {0.0} }
}

fun asNDArrayRow(xs: Array<Double>): INDArray {
    val curState = singleton(xs)
    return Nd4j.createFromArray(curState)
}

fun output(network: MultiLayerNetwork, input: INDArray): Array<Double> {
    val result = network.feedForward(input)
    return result
            .get(result.size - 1)
            .toDoubleMatrix()
            .get(0)
            .toTypedArray()
}

// softm = ( np.exp(av / tau) / np.sum( np.exp(av / tau) ) )
fun softmax(av: Array<Double>, tau: Double = 2.0 /* 1.12 */): Array<Double> {
    val divided = av.map { it / tau }
    val exponentiated = divided.map { exp(it) }
    val sum = exponentiated.sum()
    return (exponentiated.map { it / sum })
            .toDoubleArray()
            .toTypedArray()
}

fun choose(softmaxedValues: Array<Double>): Int {

    val size = softmaxedValues.size
    val prob = randprob()

    var acc = 0.0
    for (i in 0..size) {
        acc = acc + softmaxedValues.get(i)
        if (acc > prob) return i
    }
    return 0 // todo review
}

fun getReward(arms: Array<Array<Double>>, curState: Int, arm: Int): Int {
    return reward(arms.get(curState).get(arm))
}

object ContextualBandit {

    val numArms = 10;

    @JvmStatic fun main(args: Array<String>) {

        val arms = makeArms(numArms)
        var curState = (0..numArms-1).random()

        try {
            val network = makeNetwork(numArms)
            network.init()

            for (i in 0..100000) {
                val inp = oneHot(numArms, curState)
                val pred = output(network, asNDArrayRow(inp))
                val choice = choose(softmax(pred)) // todo flow
                val reward = getReward(arms, curState, choice)

                println(reward)

                val clonedPred = pred.clone()
                clonedPred.set(choice, reward.toDouble())

                network.fit(asNDArrayRow(inp), asNDArrayRow(clonedPred))

                curState = (0..numArms-1).random() // todo dedup
            }
        } catch (e: Exception) {
            println("Error: " + e.getLocalizedMessage())
        }
    }
}