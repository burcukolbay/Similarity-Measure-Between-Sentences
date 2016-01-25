import edu.sussex.nlp.jws.HirstAndStOnge;
import edu.sussex.nlp.jws.JWS;
import java.util.TreeMap;
import java.io.IOException;

public class Distance {
    private JWS jws;

    public Distance(String path_lib_worldnet, String version) throws IOException {
	this.jws = new JWS(path_lib_worldnet, version);
    }

    public double getMaxScoreHSO(String word1, String word2, String type){//TYPE -> n : noun **** v : verb **** r : adverb **** a : adjective
        HirstAndStOnge hso = jws.getHirstAndStOnge();
        TreeMap<String, Double> scores	= hso.hso(word1, word2, type);
        double max = 0;

	// System.out.println(scores);
	
        for(String s : scores.keySet()){
            if(max < scores.get(s)){
                max = scores.get(s);
            }
        }
        return max;
    }
    
    public static void main(String args[]) throws IOException {
	String w1, w2;

	if (args.length != 4) {
	    System.out.println("Usage java Distance <path to worldnet> <worldnet version> <word1> <word2>");
	    System.exit(1);
	}
	
	String path = args[0];
	String version = args[1];
	w1 = args[2];
	w2 = args[3];

	Distance d = new Distance(path, version);

		System.out.println("hso("+w1+", "+w2+") = "
				   +d.getMaxScoreHSO(w1, w2, "n"));
	
    }
}
