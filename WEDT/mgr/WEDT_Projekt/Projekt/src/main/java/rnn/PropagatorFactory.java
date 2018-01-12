package rnn;

import utils.Constants;

/**
 * Created by b.rychalska on 30.10.15.
 */
public class PropagatorFactory {

    int architecture;

    public PropagatorFactory(int architecture) {
        this.architecture = architecture;
    }

    public Propagator createPropagator() {
        return createPropagator(architecture);
    }

    public static Propagator createPropagator(int id) {
        Propagator propagator = null;
        if (id == Constants.VANILLA) {
            propagator = new VanillaRNNPropagator();
        }
//      else if (id == Constants.TEST) {
//            propagator = new TestRNNPropagator();
//        }
        else if (id == Constants.GRU) {
            propagator = new GRUPropagator();
        } else if (id == Constants.VANILLA_MULTI_H) {
            propagator = new VanillaMultiHPropagator();
        }
        return propagator;
    }

}
