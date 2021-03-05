package contextual;

import org.deeplearning4j.nn.conf.NeuralNetConfiguration;
import org.deeplearning4j.nn.conf.layers.DenseLayer;
import org.deeplearning4j.nn.conf.layers.OutputLayer;
import org.deeplearning4j.nn.multilayer.MultiLayerNetwork;
import org.deeplearning4j.nn.weights.WeightInit;
import org.nd4j.linalg.activations.Activation;
import org.nd4j.linalg.learning.config.Adam;
import org.nd4j.linalg.lossfunctions.LossFunctions;

public class BanditModel {

    public static MultiLayerNetwork create(int numArms) {

        return new MultiLayerNetwork(new NeuralNetConfiguration.Builder()
                .seed(12345)
                .updater(new Adam())
//                .l2(1e-4)
                .list()
                .layer(new DenseLayer.Builder()
                        .nIn(numArms)
                        .nOut(numArms * 10)
                        .activation(Activation.RELU)
                        .weightInit(WeightInit.XAVIER)
                        .build())
                .layer(new OutputLayer.Builder(LossFunctions.LossFunction.MSE)
                        .nIn(numArms * 10)
                        .nOut(numArms)
                        .activation(Activation.RELU)
                        .weightInit(WeightInit.XAVIER)
                        .build())
                .build());
    }
}