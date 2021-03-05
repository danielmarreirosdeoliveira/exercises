package gridworld;

import java.io.IOException;
import org.deeplearning4j.nn.conf.NeuralNetConfiguration;
import org.deeplearning4j.nn.conf.layers.DenseLayer;
import org.deeplearning4j.nn.conf.layers.OutputLayer;
import org.deeplearning4j.nn.multilayer.MultiLayerNetwork;
import org.deeplearning4j.nn.weights.WeightInit;
import org.deeplearning4j.util.ModelSerializer;
import org.nd4j.linalg.activations.Activation;
import org.nd4j.linalg.api.ndarray.INDArray;
import org.nd4j.linalg.learning.config.Adam;
import org.nd4j.linalg.lossfunctions.LossFunctions;


public class GridWorldModel {

    public static void save(MultiLayerNetwork nn, String path) throws IOException {

        ModelSerializer.writeModel(nn, path, true);
    }

    public static MultiLayerNetwork restore(String path) throws IOException {

        return ModelSerializer.restoreMultiLayerNetwork(path);
    }

    public static void fit(MultiLayerNetwork nn, INDArray X, INDArray Y) {

        nn.fit(X, Y);
    }

    public static MultiLayerNetwork create() {

        return new MultiLayerNetwork(new NeuralNetConfiguration.Builder()
                .seed(12345)
                .updater(new Adam())
                .l2(1e-3)
                .list()
                .layer(new DenseLayer.Builder()
                        .nIn(64)
                        .nOut(150)
                        .activation(Activation.RELU)
                        .weightInit(WeightInit.XAVIER)
                        .build())
                .layer(new DenseLayer.Builder()
                        .nIn(150)
                        .nOut(100)
                        .activation(Activation.RELU)
                        .weightInit(WeightInit.XAVIER)
                        .build())
                .layer(new OutputLayer.Builder(LossFunctions.LossFunction.MSE)
                        .nIn(100)
                        .nOut(4)
                        .activation(Activation.IDENTITY) // <-
                        .weightInit(WeightInit.XAVIER)
                        .build())
                .build());
    }

    public static void init(MultiLayerNetwork nn) {

        nn.init();
    }
}